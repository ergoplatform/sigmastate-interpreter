package sigma.serialization

import org.ergoplatform.ErgoBox
import org.ergoplatform.validation.ValidationRules.CheckDeserializedScriptIsSigmaProp
import sigma.SigmaProp
import sigma.ast._
import sigma.ast.syntax.SigmaPropValue
import sigma.data.CBigInt
import sigma.util.Extensions.SigmaPropOps
import sigma.validation.ValidationException
import ErgoTree.EmptyConstants
import ErgoTree.HeaderType
import sigma.compiler.ir.IRContext
import sigma.eval.Extensions.SigmaBooleanOps
import sigmastate._
import sigmastate.helpers.CompilerTestingCommons
import sigma.serialization.ErgoTreeSerializer.DefaultSerializer

import java.math.BigInteger

class ErgoTreeSerializerSpecification extends SerializationSpecification
  with CompilerTestingCommons with CompilerCrossVersionProps {

  implicit lazy val IR: TestingIRContext = new TestingIRContext {
    beginPass(noConstPropagationPass)
  }

  private def extractConstants(prop: SigmaPropValue)(implicit IR: IRContext): Seq[ErgoTree] = {
    import ErgoTree._
    val env = Map[String, Any]()
    val res = compiler.compileTyped(env, prop)
    checkCompilerResult(res)
    val calcF = res.compiledGraph
    val constantsStore = new ConstantStore()
    val outExpr = IR.buildTree(calcF, Some(constantsStore))
    val constants = constantsStore.getAll
    val trees = if (constants.isEmpty) {
      Seq(ErgoTree(ergoTreeHeaderInTests, constants, outExpr))
    } else {
      Seq(
        ErgoTree(setConstantSegregation(ergoTreeHeaderInTests), constants, outExpr),
        ErgoTree(ergoTreeHeaderInTests, EmptyConstants, prop)
      )
    }
    trees
  }

  property("(de)serialization round trip using treeBytes()") {
    val exprs = Seq(
      EQ(Plus(10.toByte, 20.toByte), ByteConstant(30)).toSigmaProp,
      EQ(Plus(10.toShort, 20.toShort), ShortConstant(30)).toSigmaProp,
      EQ(Plus(10, 20), IntConstant(30)).toSigmaProp,
      EQ(Plus(CBigInt(BigInteger.valueOf(10L)), BigIntConstant(20L)), BigIntConstant(30L)).toSigmaProp
    )
    exprs.foreach { expr =>
      extractConstants(expr).foreach { ergoTree =>
        val bytes = DefaultSerializer.serializeErgoTree(ergoTree)
        val (_, _, deserializedConstants, treeBytes) = DefaultSerializer
          .deserializeHeaderWithTreeBytes(SigmaSerializer.startReader(bytes))
        deserializedConstants shouldEqual ergoTree.constants
        val r = SigmaSerializer.startReader(
          treeBytes,
          new ConstantStore(deserializedConstants),
          resolvePlaceholdersToConstants = true)
        val deserializedTree = ValueSerializer.deserialize(r)
        deserializedTree shouldEqual expr
      }
    }
  }

  property("Constant extraction via compiler pass: (de)serialization round trip") {
    val prop = EQ(Plus(10, 20), IntConstant(30)).toSigmaProp
    extractConstants(prop).foreach { ergoTree =>
      val bytes = DefaultSerializer.serializeErgoTree(ergoTree)
      val deserializedTree = DefaultSerializer.deserializeErgoTree(bytes)
      deserializedTree shouldEqual ergoTree
    }
  }

  property("failed type check on tree deserialization") {
    forAll(numExprTreeNodeGen) { numProp =>
      val prop = numProp.asInstanceOf[SigmaPropValue] // this typecast doesn't check the actual type
      extractConstants(prop).foreach { ergoTree =>
        val bytes = DefaultSerializer.serializeErgoTree(ergoTree)

        if (ergoTreeVersionInTests == 0) {
          assertExceptionThrown(
            DefaultSerializer.deserializeErgoTree(bytes),
            rootCauseLike[SerializerException]("Failed deserialization, expected deserialized script to have type SigmaProp;"))
        } else {
          val tree = DefaultSerializer.deserializeErgoTree(bytes)
          tree.root match {
            case Left(UnparsedErgoTree(unparsedBytes,
                ValidationException(_, CheckDeserializedScriptIsSigmaProp, _, Some(cause)))) =>
              unparsedBytes shouldBe bytes
              rootCauseLike[SerializerException](
                "Failed deserialization, expected deserialized script to have type SigmaProp;")
                .apply(cause) shouldBe true
            case _ => fail()
          }
        }
      }
    }
  }

  property("Constant extraction during serialization: (de)serialization round trip") {
    val tree = mkTestErgoTree(EQ(Plus(10, 20), IntConstant(30)).toSigmaProp)
    val bytes = DefaultSerializer.serializeErgoTree(tree)
    val (_, _, deserializedConstants, _) = DefaultSerializer.
      deserializeHeaderWithTreeBytes(SigmaSerializer.startReader(bytes))
    deserializedConstants.length shouldBe 3
    val deserializedTree = DefaultSerializer.deserializeErgoTree(bytes)
    deserializedTree shouldEqual tree
  }

  property("tree with placeholders bytes should be equal if only constants are different") {
    val tree1 = mkTestErgoTree(EQ(Plus(10, 20), IntConstant(30)).toSigmaProp)
    val tree2 = mkTestErgoTree(EQ(Plus(30, 40), IntConstant(70)).toSigmaProp)
    val bytes1 = DefaultSerializer.serializeErgoTree(tree1)
    val bytes2 = DefaultSerializer.serializeErgoTree(tree2)
    val (_, _, _, treeBytes1) = DefaultSerializer
      .deserializeHeaderWithTreeBytes(SigmaSerializer.startReader(bytes1))
    val (_, _, _, treeBytes2) = DefaultSerializer
      .deserializeHeaderWithTreeBytes(SigmaSerializer.startReader(bytes2))
    treeBytes1 shouldEqual treeBytes2
  }

  property("(de)serialize round trip") {
    // increased minimum number of successes
    // for better coverage of all possible combinations (with/without constants, segregation option, etc.)
    forAll(ergoTreeGen, minSuccessful(500)) { tree: ErgoTree =>
      val bytes = DefaultSerializer.serializeErgoTree(tree)
      val deserializedTree = DefaultSerializer.deserializeErgoTree(bytes)
      deserializedTree shouldEqual tree
    }
  }

  property("max ergo tree byte size check") {
    val tree = mkTestErgoTree(EQ(Plus(10, 20), IntConstant(30)).toSigmaProp)
    val r = SigmaSerializer.startReader(DefaultSerializer.serializeErgoTree(tree))
    if (ergoTreeVersionInTests == 0) {
      assertExceptionThrown({
        DefaultSerializer.deserializeErgoTree(r, 1)
      }, {
        case e: SerializerException => rootCause(e).isInstanceOf[ReaderPositionLimitExceeded]
      })
    } else {
      val tree = DefaultSerializer.deserializeErgoTree(r, 1)
      tree.root match {
        case Left(UnparsedErgoTree(_, ve: ValidationException)) =>
          rootCauseLike[ReaderPositionLimitExceeded]().apply(ve.cause.get) shouldBe true
        case _ => fail()
      }
    }
  }

  property("restore reader's positionLimit") {
    val tree = mkTestErgoTree(EQ(Plus(10, 20), IntConstant(30)).toSigmaProp)
    val r = SigmaSerializer.startReader(DefaultSerializer.serializeErgoTree(tree))
    r.positionLimit = 1
    DefaultSerializer.deserializeErgoTree(r, SigmaSerializer.MaxPropositionSize) shouldEqual tree
    r.positionLimit shouldBe 1
  }

  property("should compute hasDeserialize during parsing") {
    val const = IntConstant(10)
    val dc = DeserializeContext(1.toByte, SInt)
    val dr = DeserializeRegister(ErgoBox.R4, SInt)

    val samples = Table(("exp", "hasDeserialize"),
      const -> false,
      dc -> true,
      dr -> true,
      Plus(Plus(const, dc), dr) -> true,
      Plus(Plus(const, const), const) -> false
    )

    forAll(samples) { (exp, hasDeserialize) =>
      val t = new ErgoTree(
        HeaderType @@ 16.toByte,
        Array(IntConstant(1)),
        Right(BoolToSigmaProp(EQ(ConstantPlaceholder(0, SInt), exp)))
      )
      t._hasDeserialize shouldBe None

      val parsedTree = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(t.bytes)
      parsedTree shouldBe t
      parsedTree._hasDeserialize.isDefined shouldBe true
      parsedTree.hasDeserialize shouldBe hasDeserialize
    }
  }

  property("getPositionsBackref") {
    def test(positions: Array[Int], expected: Array[Int]) = {
      val backrefs = ErgoTreeSerializer.DefaultSerializer.getPositionsBackref(positions, expected.length)
      backrefs shouldBe expected
    }

    test(positions = Array(), expected = Array()) // no positions, no constants
    test(positions = Array(), expected = Array(-1)) // no positions, 1 constant
    test(positions = Array(0), expected = Array())  // 1 position, no constants
    test(positions = Array(1), expected = Array(-1)) // 1 position, but out of range
    test(positions = Array(0), expected = Array(0))  // 1 position, 1 constant
    test(positions = Array(-1), expected = Array())  // 1 invalid (out of range) position, no constants
    test(positions = Array(-2), expected = Array(-1))  // 1 invalid position, 1 constants

    test(positions = Array(0, 0), expected = Array(0))  // duplicate positions, 1 constant
    test(positions = Array(-1, 0), expected = Array(1))  // invalid positions ignored
    test(positions = Array(-1, 0, 0), expected = Array(1))  // only first of the duplicates used
     
    test(positions = Array(), expected = Array(-1, -1, -1, -1, -1))  // no positions => no backrefs

    test(positions = Array(1, 2), expected = Array(-1, 0, 1, -1, -1))
    test(positions = Array(1, 2, 4), expected = Array(-1, 0, 1, -1, 2))
  }

  property("SigmaProp.propBytes vs ErgoTree.serializer equivalence") {
    forAll(MinSuccessful(100)) { sp: SigmaProp =>
      val propBytes = sp.propBytes
      val ergoTree = new ErgoTree(ErgoTree.DefaultHeader, EmptyConstants, Right(sp.toSigmaBoolean.toSigmaPropValue), null, None, None)
      val treeBytes = DefaultSerializer.serializeErgoTree(ergoTree)
      treeBytes shouldBe propBytes.toArray
    }
  }
}
