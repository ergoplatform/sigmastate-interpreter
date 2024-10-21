package sigmastate

import org.ergoplatform._
import org.ergoplatform.validation.ValidationRules._
import org.scalatest.BeforeAndAfterAll
import sigma.{Colls, SigmaTestingData}
import sigma.ast._
import sigma.ast.SPrimType.MaxPrimTypeCode
import sigma.ast.TypeCodes.LastConstantCode
import sigma.data.AvlTreeData
import sigma.serialization.SerializerException
import sigma.validation.ValidationRules.{CheckPrimitiveTypeCode, CheckSerializableTypeCode, CheckTypeCode, CheckTypeWithMethods, trySoftForkable}
import sigma.validation.{ChangedRule, ReplacedRule, SigmaValidationSettings, ValidationException, ValidationRule}
import ErgoTree.{EmptyConstants, HeaderType, ZeroHeader, setSizeBit}
import sigmastate.helpers.TestingHelpers._
import sigmastate.helpers.{CompilerTestingCommons, ErgoLikeContextTesting, ErgoLikeTestInterpreter, ErgoLikeTestProvingInterpreter}
import sigmastate.interpreter.Interpreter.{ScriptNameProp, emptyEnv}
import sigma.interpreter.{ContextExtension, ProverResult}
import sigma.ast.syntax._
import sigma.eval.ErgoTreeEvaluator
import sigma.eval.ErgoTreeEvaluator.DataEnv
import sigma.exceptions.InterpreterException
import sigma.serialization.SigmaSerializer.startReader
import sigma.serialization._
import sigma.serialization.ValueCodes.OpCode
import sigmastate.utils.Helpers._

class SoftForkabilitySpecification extends SigmaTestingData
    with CompilerTestingCommons
    with BeforeAndAfterAll {

  val IR: TestingIRContext = new TestingIRContext
  lazy val prover = new ErgoLikeTestProvingInterpreter()
  lazy val verifier = new ErgoLikeTestInterpreter
  val deadline = 100
  val boxAmt = 100L

  lazy val booleanPropV1 = compile(emptyEnv + ("deadline" -> deadline),
    """{
     |  HEIGHT > deadline && OUTPUTS.size == 1
     |}""".stripMargin)(IR).asBoolValue

  // cast Boolean typed prop to SigmaProp (which is invalid) // ErgoTree v0
  lazy val invalidPropV1: ErgoTree =
    ErgoTree.fromProposition(
      ErgoTree.headerWithVersion(ZeroHeader, 0),
      booleanPropV1.asSigmaProp)

  lazy val invalidTxV1 = createTransaction(createBox(boxAmt, invalidPropV1, 1))
  lazy val invalidTxV1bytes = invalidTxV1.messageToSign

  lazy val propV1 = booleanPropV1.toSigmaProp
  lazy val txV1 = createTransaction(
    createBox(boxAmt,
      ErgoTree.fromProposition(ErgoTree.headerWithVersion(ZeroHeader, 0), propV1), // ErgoTree v0
      1))
  lazy val txV1bytes = txV1.messageToSign

  val blockHeight = 110

  def createContext(h: Int, tx: ErgoLikeTransaction, vs: SigmaValidationSettings) =
    ErgoLikeContextTesting(h,
      AvlTreeData.dummy, ErgoLikeContextTesting.dummyPubkey, IndexedSeq(fakeSelf),
      tx, fakeSelf, vs = vs, activatedVersion = activatedVersionInTests)

  def proveTx(name: String, tx: ErgoLikeTransaction, vs: SigmaValidationSettings): ProverResult = {
    val env = Map(ScriptNameProp -> (name + "_prove"))
    val ctx = createContext(blockHeight, tx, vs)
    val prop = tx.outputs(0).ergoTree
    val proof1 = prover.prove(env, prop, ctx, fakeMessage).get
    proof1
  }

  def verifyTx(name: String, tx: ErgoLikeTransaction, proof: ProverResult, vs: SigmaValidationSettings) = {
    val env = Map(ScriptNameProp -> (name + "_verify"))
    val ctx = createContext(blockHeight, tx, vs)
    val prop = tx.outputs(0).ergoTree
    verifier.verify(env, prop, ctx, proof, fakeMessage).map(_._1).getOrThrow shouldBe true
  }

  def proveAndVerifyTx(name: String, tx: ErgoLikeTransaction, vs: SigmaValidationSettings) = {
    val proof = proveTx(name, tx, vs)
    verifyTx(name, tx, proof, vs)
  }

  def checkTxProp[T <: ErgoLikeTransaction, R](tx1: T, tx2: T)(p: T => R) = {
    p(tx1) shouldBe p(tx2)
  }

  property("node v1, received tx with script v1, incorrect script") {
    assertExceptionThrown({
      // CheckDeserializedScriptIsSigmaProp rule violated
      ErgoLikeTransaction.serializer.parse(startReader(invalidTxV1bytes))
    }, {
      case se: SerializerException if se.cause.isDefined =>
        val ve = se.cause.get.asInstanceOf[ValidationException]
        ve.rule == CheckDeserializedScriptIsSigmaProp
      case _ => false
    })
  }

  property("node v1, received tx with script v1, correct script") {
    // able to parse
    val tx = ErgoLikeTransaction.serializer.parse(startReader(txV1bytes))

    // validating script
    proveAndVerifyTx("propV1", tx, vs)
  }

  val Height2Code: OpCode = OpCode @@ (LastConstantCode + 56).toByte
  /** Same as Height, but new opcode to test soft-fork */
  case object Height2 extends NotReadyValueInt with ValueCompanion {
    override def companion = this
    override val opCode: OpCode = Height2Code // use reserved code
    override val opType = SFunc(SContext, SInt)
    override val costKind = Height.costKind
    protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
      addCost(this.costKind)
      E.context.HEIGHT
    }
  }
  val Height2Ser = CaseObjectSerialization(Height2, Height2)

  // prepare soft-fork settings for v2
  val v2vs = vs.updated(CheckValidOpCode.id, ChangedRule(Array(Height2Code)))

  /** Runs the block as if on the v2 node. */
  def runOnV2Node[T](block: => T): T = {
    ValueSerializer.addSerializer(Height2Code, Height2Ser)
    val res = block
    ValueSerializer.removeSerializer(Height2Code)
    res
  }

  lazy val booleanPropV2 = GT(Height2, IntConstant(deadline))

  lazy val invalidPropV2: ErgoTree = ErgoTree.fromProposition(
    header = ErgoTree.headerWithVersion(ZeroHeader, 0),  // ErgoTree v0
    prop = booleanPropV2.asSigmaProp)

  lazy val invalidTxV2 = createTransaction(createBox(boxAmt, invalidPropV2, 1))


  lazy val propV2 = booleanPropV2.toSigmaProp
  // prepare bytes using special serialization WITH `size flag` in the header
  lazy val propV2tree = ErgoTree.withSegregation(setSizeBit(ZeroHeader),  propV2)
  lazy val propV2treeBytes = runOnV2Node {
    propV2tree.bytes
  }

  lazy val txV2 = createTransaction(createBox(boxAmt, propV2tree, 1))
  lazy val txV2messageToSign = runOnV2Node {
    txV2.messageToSign
  }

  val newTypeCode = (SGlobal.typeCode + 1).toByte

  property("node v1, soft-fork up to v2, script v2 without size bit") {
    // try prepare v2 script without `size bit` in the header
    assertExceptionThrown({
      new ErgoTree(HeaderType @@ 1.toByte, EmptyConstants, Right(propV2))
    }, {
      case _: IllegalArgumentException  => true
      case _ => false
    } )

    // prepare bytes using default serialization and then replacing version in the header
    val v2tree_withoutSize_bytes = runOnV2Node {
      val tree = ErgoTree.fromProposition(
        ErgoTree.headerWithVersion(ZeroHeader, 0), propV2)  // ErgoTree v0
      val bytes = tree.bytes
      // set version to v2 while not setting the size bit,
      // we cannot do this using ErgoTree constructor (due to require() check)
      bytes(0) = 1.toByte
      bytes
    }

    // v1 node should fail
    assertExceptionThrown(
      {
        val r = startReader(v2tree_withoutSize_bytes)
        ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(r, SigmaSerializer.MaxPropositionSize)
      },
      {
        case ve: ValidationException if ve.rule == CheckHeaderSizeBit => true
        case _ => false
      }
    )

    // v2 node should fail
    runOnV2Node {
      assertExceptionThrown(
        {
          val r = startReader(v2tree_withoutSize_bytes)
          ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(r, SigmaSerializer.MaxPropositionSize)
        },
        {
          case ve: ValidationException if ve.rule == CheckHeaderSizeBit => true
          case _ => false
        } )
    }
  }

  property("node v1, soft-fork up to v2, script v2 with `size bit`") {
    val treeBytes = propV2treeBytes
    val txV2bytes = txV2messageToSign

    // parse and validate tx with v2 settings
    val tx = ErgoLikeTransaction.serializer.parse(startReader(txV2bytes))
    proveAndVerifyTx("propV2", tx, v2vs)

    // and with v1 settings
    assertExceptionThrown(
      proveAndVerifyTx("propV2", tx, vs),
      { t =>
        t.isInstanceOf[InterpreterException] &&
        t.getMessage.contains("Script has not been recognized due to ValidationException, and it cannot be accepted as soft-fork.")
      }
    )

    // also check that transaction prop was trivialized due to soft-fork
    tx.outputs(0).ergoTree.root.left.get.bytes.array shouldBe treeBytes
    tx.outputs(0).ergoTree.root.left.get.isInstanceOf[UnparsedErgoTree] shouldBe true

    // check deserialized tx is otherwise remains the same
    checkTxProp(txV2, tx)(_.inputs)
    checkTxProp(txV2, tx)(_.dataInputs)
    checkTxProp(txV2, tx)(_.outputs.length)
    checkTxProp(txV2, tx)(_.outputs(0).creationHeight)
    checkTxProp(txV2, tx)(_.outputs(0).value)
    checkTxProp(txV2, tx)(_.outputs(0).additionalTokens)
  }

  property("node v1, no soft-fork, received script v2, raise error") {
    assertExceptionThrown({
      val invalidTxV2bytes = runOnV2Node { invalidTxV2.messageToSign }
      ErgoLikeTransaction.serializer.parse(startReader(invalidTxV2bytes))
    },{
      case se: SerializerException if se.cause.isDefined =>
        val ve = se.cause.get.asInstanceOf[ValidationException]
        ve.rule == CheckValidOpCode
      case _ => false
    })
  }

  property("our node v2, was soft-fork up to v2, received script v2") {
    val txV2bytes = txV2.messageToSign

    // run as on node v2
    runOnV2Node {

      // parse and validate tx with v2 script (since serializers were extended to v2)
      val tx = ErgoLikeTransaction.serializer.parse(startReader(txV2bytes))
      tx shouldBe txV2

      // fails evaluation of v2 script (due to the rest of the implementation is still v1)
      assertExceptionThrown({
        proveAndVerifyTx("propV2", tx, v2vs)
      },{
        case _: Exception => true
        case _ => false
      })
    }
  }

  property("our node v1, was soft-fork up to v2, received v1 script, DeserializeContext of v2 script") {
    // script bytes for context variable containing v2 operation
    val propBytes = runOnV2Node {
      ValueSerializer.serialize(booleanPropV2)
    }

    // v1 main script which deserializes v2 script from context
    val mainProp = BinAnd(GT(Height, IntConstant(deadline)), DeserializeContext(1, SBoolean)).toSigmaProp
    val mainTree = ErgoTree.fromProposition(
      header = ErgoTree.headerWithVersion(ZeroHeader, 0), // ErgoTree v0
      prop = mainProp)

    val tx = createTransaction(createBox(boxAmt, mainTree, 1))
    val bindings = Map(1.toByte -> ByteArrayConstant(Colls.fromArray(propBytes)))
    val proof = new ProverResult(Array.emptyByteArray, ContextExtension(bindings))

    // verify transaction on v1 node using v2 validation settings
    verifyTx("deserialize", tx, proof, v2vs)
  }

  def checkRule(rule: ValidationRule, v2vs: SigmaValidationSettings, action: => Unit) = {
    // try SoftForkable block using current vs (v1 version)
    assertExceptionThrown({
      trySoftForkable(false) {
        action
        true
      }
    }, {
      case ve: ValidationException if ve.rule == rule => true
      case _ => false
    })

    val res = trySoftForkable(false)({
      action
      true
    })(v2vs)
    res shouldBe false
  }

  property("CheckPrimitiveTypeCode rule") {
    val typeBytes = Array[Byte](MaxPrimTypeCode)
    val v2vs = vs.updated(CheckPrimitiveTypeCode.id, ChangedRule(Array[Byte](MaxPrimTypeCode)))
    checkRule(CheckPrimitiveTypeCode, v2vs, {
      val r = startReader(typeBytes)
      TypeSerializer.deserialize(r)
    })
  }

  property("CheckTypeCode rule") {
    val typeBytes = Array[Byte](newTypeCode)
    val v2vs = vs.updated(CheckTypeCode.id, ChangedRule(Array[Byte](newTypeCode)))
    checkRule(CheckTypeCode, v2vs, {
      val r = startReader(typeBytes)
      TypeSerializer.deserialize(r)
    })
  }

  property("CheckSerializableTypeCode rule") {
    val newType = SFunc(SInt, SInt)
    val dataBytes = Array[Byte](1, 2, 3) // any random bytes will work
    val v2vs = vs.updated(CheckSerializableTypeCode.id, ReplacedRule(0))
    checkRule(CheckSerializableTypeCode, v2vs, {
      val r = startReader(dataBytes)
      DataSerializer.deserialize(newType, r)
    })
  }

  property("CheckTypeWithMethods rule") {
    val freeMethodId = 1.toByte
    val mcBytes = Array[Byte](OpCodes.PropertyCallCode, newTypeCode, freeMethodId, Outputs.opCode)
    val v2vs = vs.updated(CheckTypeWithMethods.id, ChangedRule(Array(newTypeCode)))
    checkRule(CheckTypeWithMethods, v2vs, {
      ValueSerializer.deserialize(mcBytes)
    })
  }

  property("CheckMethod rule") {
    val freeMethodId = 16.toByte
    val mcBytes = Array[Byte](OpCodes.PropertyCallCode, SCollection.typeId, freeMethodId, Outputs.opCode)
    val v2vs = vs.updated(CheckAndGetMethod.id, ChangedRule(Array(SCollection.typeId, freeMethodId)))
    checkRule(CheckAndGetMethod, v2vs, {
      ValueSerializer.deserialize(mcBytes)
    })
  }

  override protected def afterAll(): Unit = {
  }

}
