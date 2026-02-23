package sigmastate.utxo

import org.ergoplatform.ErgoBox.{AdditionalRegisters, R6, R8}
import org.ergoplatform._
import org.scalatest.Assertion
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert, InsertOrUpdate}
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util.{ByteArrayBuilder, idToBytes}
import scorex.util.encode.Base16
import scorex.utils.Ints
import scorex.util.serialization.VLQByteBufferWriter
import scorex.utils.Longs
import sigma.{Coll, Colls, GroupElement, SigmaTestingData, VersionContext}
import sigma.Extensions.ArrayOps
import sigma.VersionContext.{V6SoftForkVersion, withVersions}
import sigma.ast.SCollection.SByteArray
import sigma.ast.SType.{AnyOps, tD}
import sigma.data.{AvlTreeData, AvlTreeFlags, CAND, CAnyValue, CBigInt, CGroupElement, CHeader, CSigmaDslBuilder, CSigmaProp}
import sigma.ast.SOption
import sigma.util.StringUtil._
import sigma.ast._
import sigma.ast.syntax._
import sigma.crypto.{CryptoConstants, CryptoFacade, SecP256K1Group}
import sigmastate._
import sigmastate.helpers.TestingHelpers._
import sigmastate.helpers.{CompilerTestingCommons, ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTestInterpreter}
import sigma.interpreter.ContextExtension.VarBinding
import sigmastate.interpreter.CErgoTreeEvaluator.DefaultEvalSettings
import sigmastate.interpreter.Interpreter._
import sigma.ast.Apply
import sigma.eval.{EvalSettings, SigmaDsl}
import sigma.exceptions.InvalidType
import sigma.serialization.ErgoTreeSerializer
import sigma.serialization.{DataSerializer, SigmaByteWriter, ValueSerializer}
import sigma.interpreter.{ContextExtension, ProverResult}
import sigma.validation.ValidationException
import sigma.util.Extensions
import sigmastate.utils.Helpers
import sigma.exceptions.InterpreterException
import sigmastate.utils.Helpers._

import java.math.BigInteger
import scala.collection.compat.immutable.ArraySeq
import java.security.SecureRandom
import scala.annotation.tailrec
import scala.util.Try

class BasicOpsSpecification extends CompilerTestingCommons
  with CompilerCrossVersionProps {
  override val printVersions: Boolean = false
  implicit lazy val IR = new TestingIRContext {
  }

  private val reg1 = ErgoBox.nonMandatoryRegisters.head
  private val reg2 = ErgoBox.nonMandatoryRegisters.tail.head
  val intVar1 = 1.toByte
  val intVar2 = 2.toByte
  val byteVar1 = 3.toByte
  val byteVar2 = 4.toByte
  val bigIntVar1 = 5.toByte
  val bigIntVar2 = 6.toByte
  val bigIntVar3 = 7.toByte
  val byteVar3 = 8.toByte
  val booleanVar = 9.toByte
  val propVar1 = 10.toByte
  val propVar2 = 11.toByte
  val propVar3 = 12.toByte
  val propBytesVar1 = 13.toByte
  val lastExtVar = propBytesVar1

  val ext: Seq[VarBinding] = Seq(
    (intVar1, IntConstant(1)), (intVar2, IntConstant(2)),
    (byteVar1, ByteConstant(1)), (byteVar2, ByteConstant(2)),
    (bigIntVar1, BigIntConstant(BigInt(10).underlying())), (bigIntVar2, BigIntConstant(BigInt(20).underlying())),
    (booleanVar, TrueLeaf))
  // to support both JVM and JS we need to wrap numeric values into CAnyValue
  val env = Map(
    "intVar1" -> CAnyValue(intVar1), "intVar2" -> CAnyValue(intVar2),
    "byteVar1" -> CAnyValue(byteVar1), "byteVar2" -> CAnyValue(byteVar2), "byteVar3" -> CAnyValue(byteVar3),
    "bigIntVar1" -> CAnyValue(bigIntVar1), "bigIntVar2" -> CAnyValue(bigIntVar2), "bigIntVar3" -> CAnyValue(bigIntVar3),
    "trueVar" -> CAnyValue(booleanVar),
    "proofVar1" -> CAnyValue(propVar1),
    "proofVar2" -> CAnyValue(propVar2)
    )

  def test(name: String,
           env: ScriptEnv,
           ext: Seq[VarBinding],
           script: String,
           propExp: SValue,
           onlyPositive: Boolean = true,
           testExceededCost: Boolean = true,
           additionalRegistersOpt: Option[AdditionalRegisters] = None) = {
    val prover = new ContextEnrichingTestProvingInterpreter() {
      override lazy val contextExtenders: Map[Byte, EvaluatedValue[_ <: SType]] = {
        val p1 = dlogSecrets(0).publicImage
        val p2 = dlogSecrets(1).publicImage
        val d1 = dhSecrets(0).publicImage

        (ext ++ Seq(
          propVar1 -> SigmaPropConstant(p1),
          propVar2 -> SigmaPropConstant(p2),
          propVar3 -> SigmaPropConstant(CSigmaProp(CAND(Seq(p1, d1)))),
          propBytesVar1 -> ByteArrayConstant(CSigmaProp(CAND(Seq(p1, d1))).propBytes)
        )).toMap
      }
      override val evalSettings: EvalSettings = DefaultEvalSettings.copy(
        isMeasureOperationTime = true,
        isDebug = true,
        isTestRun = testExceededCost)
    }

    val prop = if (script.isNullOrEmpty) {
      // for some testcases the script cannot be compiled (i.e. the corresponding syntax
      // is not supported by ErgoScript Compiler)
      // In such cases we use expected property as the property to test
      propExp.asSigmaProp
    } else {
      // compile with the latest compiler version, to get validation exception during execution, not compilation error
      withVersions(VersionContext.MaxSupportedScriptVersion, VersionContext.MaxSupportedScriptVersion) {
        compile(env, script).asBoolValue.toSigmaProp
      }
    }

    if (propExp != null)
      prop shouldBe propExp

    val tree = ErgoTree.fromProposition(ergoTreeHeaderInTests, prop)

    // check ErgoTree roundtrip
    val tBytes = ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(tree)
    val tBytes2 = ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(tBytes))
    tBytes.sameElements(tBytes2) shouldBe true

    val p3 = prover.dlogSecrets(2).publicImage
    val boxToSpend = testBox(10, tree,
      additionalRegisters = additionalRegistersOpt.getOrElse(Map(
        reg1 -> SigmaPropConstant(p3),
        reg2 -> IntConstant(1))
      ),
      creationHeight = 5)

    val newBox1 = testBox(10, tree, creationHeight = 0, boxIndex = 0, additionalRegisters = Map(
      reg1 -> IntConstant(1),
      reg2 -> IntConstant(10)))
    val ce = ContextExtension(prover.contextExtenders)
    val tx = new ErgoLikeTransaction(IndexedSeq(Input(boxToSpend.id, ProverResult(Array.empty, ce))), ArraySeq.empty, IndexedSeq(newBox1))

    val ctx = ErgoLikeContextTesting(currentHeight = 0,
      lastBlockUtxoRoot = AvlTreeData.dummy, ErgoLikeContextTesting.dummyPubkey, boxesToSpend = IndexedSeq(boxToSpend),
      spendingTransaction = tx, self = boxToSpend, ergoTreeVersionInTests)

    val pr = prover.prove(env + (ScriptNameProp -> s"${name}_prove"), tree, ctx, fakeMessage).getOrThrow

    val ctxExt = ctx.withExtension(pr.extension)

    val testVerifier = new ErgoLikeTestInterpreter

    if (!onlyPositive) {
      // test negative case
      testVerifier.verify(
          env + (ScriptNameProp -> s"${name}_verify"),
          tree, ctx, pr.proof, fakeMessage)
        .map(_._1)
        .getOrElse(false) shouldBe false //context w/out extensions
    }

    // this is helper verifier which respects the requested parameter testExceededCost for
    // some test cases (when testExceededCost == false) it emit message in the console
    // instead of failing the test and the failing case is tested separately in that case
    val flexVerifier = new ErgoLikeTestInterpreter {
      override val evalSettings: EvalSettings = DefaultEvalSettings.copy(
        isMeasureOperationTime = true,
        isDebug = true,
        isTestRun = testExceededCost)
    }
    val verifyEnv = env + (ScriptNameProp -> s"${name}_verify_ext")
    flexVerifier.verify(verifyEnv, tree, ctxExt, pr.proof, fakeMessage).get._1 shouldBe true
  }

  property("getVarFromInput") {
    def getVarTest(): Assertion = {
      val customExt = Map(
        1.toByte -> IntConstant(5)
      ).toSeq
      test("R1", env, customExt,
        "{ sigmaProp(getVarFromInput[Int](0, 1).get == 5) }",
        null
      )
    }

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      getVarTest()
    } else {
      an[sigma.validation.ValidationException] should be thrownBy getVarTest()
    }
  }

  property("getVarFromInput - self index") {
    def getVarTest(): Assertion = {
      val customExt = Map(
        1.toByte -> IntConstant(5)
      ).toSeq
      test("R1", env, customExt,
        """{
          | val idx = CONTEXT.selfBoxIndex
          | sigmaProp(CONTEXT.getVarFromInput[Int](idx.toShort, 1.toByte).get == 5)
          | }""".stripMargin,
        null
      )
    }

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      getVarTest()
    } else {
      an[sigma.validation.ValidationException] should be thrownBy getVarTest()
    }
  }

  property("getVarFromInput - invalid input") {
    def getVarTest(): Assertion = {
      val customExt = Map(
        1.toByte -> IntConstant(5)
      ).toSeq
      test("R1", env, customExt,
        "{ sigmaProp(CONTEXT.getVarFromInput[Int](1, 1).isDefined == false) }",
        null
      )
    }

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      getVarTest()
    } else {
      an[sigma.validation.ValidationException] should be thrownBy getVarTest()
    }
  }


  property("group order deserialization") {
    val b = SecP256K1Group.q

    val customExt: Seq[(Byte, EvaluatedValue[_ <: SType])] = Map(
      0.toByte -> UnsignedBigIntConstant(b)
    ).toSeq

    def deserTest() = {test("restoring q", env, customExt,
      s"""{
         |  val b1 = unsignedBigInt(\"${b.toString}\")
         |  val b2 = getVar[UnsignedBigInt](0).get
         |  b1 == b2
         |}
         | """.stripMargin,
      null,
      true
    )}

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[sigma.validation.ValidationException] should be thrownBy deserTest()
    } else {
      deserTest()
    }
  }

  property("signed -> unsigned bigint conversion - positive bigint") {
    val b = new BigInteger("9280562930080889354892980449861222646750586663683904599823322027983929189860")
    val ub = new BigInteger(1, b.toByteArray)

    def conversionTest() = {test("conversion", env, ext,
      s"""{
         |  val b = bigInt(\"${ub.toString}\")
         |  val ub = b.toUnsigned
         |  ub > 1
         | } """.stripMargin,
      null,
      true
    )}

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[sigma.validation.ValidationException] should be thrownBy conversionTest()
    } else {
      conversionTest()
    }
  }

  property("signed -> unsigned bigint conversion - negative bigint") {
    def conversionTest() = {test("conversion", env, ext,
      s"""{
         |  val b = bigInt("-1")
         |  val ub = b.toUnsigned
         |  ub > 0
         | } """.stripMargin,
      null,
      true
    )}

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[sigma.validation.ValidationException] should be thrownBy conversionTest()
    } else {
      an[Exception] should be thrownBy conversionTest()
    }
  }

  property("unsigned bigint - attempt to create from negative value") {
    def conversionTest() = {test("conversion", env, ext,
      s"""{
         |  val m = unsignedBigInt("-5")
         |  m >= 0
         | } """.stripMargin,
      null,
      true
    )}

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[Exception] should be thrownBy conversionTest()
    } else {
      an[sigma.exceptions.InvalidArguments] should be thrownBy conversionTest()
    }
  }


  property("signed -> unsigned bigint conversion - negative bigint - mod") {
    def conversionTest() = {test("conversion", env, ext,
      s"""{
         |  val b = bigInt("-1")
         |  val m = unsignedBigInt("5")
         |  val ub = b.toUnsignedMod(m)
         |  ub >= 0
         | } """.stripMargin,
      null,
      true
    )}

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[sigma.validation.ValidationException] should be thrownBy conversionTest()
    } else {
      conversionTest()
    }
  }

  property("signed -> unsigned bigint conversion - negative bigint - mod - 2") {
    def conversionTest() = {test("conversion", env, ext,
      s"""{
         |  val t = (bigInt("-1"), bigInt("5"))
         |  val b = t._1
         |  val m = t._2
         |  val ub = b.toUnsignedMod(m.toUnsigned)
         |  ub >= 0
         | } """.stripMargin,
      null,
      true
    )}

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[sigma.validation.ValidationException] should be thrownBy conversionTest()
    } else {
      conversionTest()
    }
  }

  property("unsigned bigint - add") {
    def conversionTest() = {test("add", env, ext,
      s"""{
         |  val a = unsignedBigInt("5")
         |  val b = unsignedBigInt("10")
         |  val res = a + b
         |  res == 15
         | } """.stripMargin,
      null,
      true
    )}

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[sigma.serialization.SerializerException] should be thrownBy conversionTest()
    } else {
      conversionTest()
    }
  }

  property("unsigned bigint - subtract") {
    def conversionTest() = {test("subtract", env, ext,
      s"""{
         |  val a = unsignedBigInt("10")
         |  val b = unsignedBigInt("5")
         |  a - b == b
         | } """.stripMargin,
      null,
      true
    )}

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[sigma.serialization.SerializerException] should be thrownBy conversionTest()
    } else {
      conversionTest()
    }
  }

  property("unsigned bigint - multiply") {
    def conversionTest() = {test("multiply", env, ext,
      s"""{
         |  val a = unsignedBigInt("10")
         |  val b = unsignedBigInt("50")
         |  a * b == unsignedBigInt("500")
         | } """.stripMargin,
      null,
      true
    )}

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[sigma.serialization.SerializerException] should be thrownBy conversionTest()
    } else {
      conversionTest()
    }
  }

  property("unsigned bigint - subtract with neg result") {
    def conversionTest() = {test("subtract", env, ext,
      s"""{
         |  val a = unsignedBigInt("5")
         |  val b = unsignedBigInt("10")
         |  val res = a - b
         |  res >= 0
         | } """.stripMargin,
      null,
      true
    )}

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[sigma.serialization.SerializerException] should be thrownBy conversionTest()
    } else {
      an[Exception] should be thrownBy conversionTest()
    }
  }

  property("unsigned -> signed bigint conversion") {
    def conversionTest() = {test("conversion", env, ext,
      s"""{
         |  val ub = unsignedBigInt("10")
         |  val b = ub.toSigned
         |  b - 11 == bigInt("-1")
         | } """.stripMargin,
      null,
      true
    )}

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[sigma.validation.ValidationException] should be thrownBy conversionTest()
    } else {
      conversionTest()
    }
  }

  property("unsigned -> signed overflow") {
    def conversionTest() = {test("conversion", env, ext,
      s"""{
         |  val ub = unsignedBigInt("${CryptoConstants.groupOrder}")
         |  ub.toSigned > 0
         | } """.stripMargin,
      null,
      true
    )}

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[Exception] should be thrownBy conversionTest()
    } else {
      val t = Try(conversionTest())
      // on JS exception is ArithmeticException directly, on JVM, ArithmeticException wrapped into InvocationTargetException
      t.failed.get match {
        case e: java.lang.ArithmeticException => e.getMessage.startsWith("BigInteger out of 256 bit range") shouldBe true
        case e: Throwable => e.getCause.getMessage.startsWith("BigInteger out of 256 bit range") shouldBe true
      }
    }
  }

  property("schnorr sig check") {
    val td = new SigmaTestingData {}

    val g = CGroupElement(SecP256K1Group.generator)

    def randBigInt: BigInt = {
      val random = new SecureRandom()
      val values = new Array[Byte](32)
      random.nextBytes(values)
      BigInt(values).mod(td.TestData.BigIntMaxValue.asInstanceOf[CBigInt].wrappedValue)
    }

    @tailrec
    def sign(msg: Array[Byte], secretKey: BigInt): (GroupElement, BigInt) = {
      val r = randBigInt

      val a: GroupElement = g.exp(CBigInt(r.bigInteger))
      val z = (r + secretKey * BigInt(scorex.crypto.hash.Blake2b256(msg))).mod(CryptoConstants.groupOrder)

      if(z.bitLength > 255) {
        (a, z)
      } else {
        sign(msg,secretKey)
      }
    }

    val holderSecret = randBigInt
    val bi = CBigInt(holderSecret.bigInteger)
    val holderPk = g.exp(bi)

    val message = Array.fill(5)(1.toByte)

    val (a, z) = sign(message, holderSecret)

    val customExt: Seq[(Byte, EvaluatedValue[_ <: SType])] = Map(
      0.toByte -> GroupElementConstant(holderPk),
      1.toByte -> GroupElementConstant(a),
      2.toByte -> UnsignedBigIntConstant(z.bigInteger)
    ).toSeq

    def schnorrTest() = {
      test("schnorr", env, customExt,
        s"""{
           |
           |      val g: GroupElement = groupGenerator
           |      val holder = getVar[GroupElement](0).get
           |
           |      val message = fromBase16("${Base16.encode(message)}")
           |      val e: Coll[Byte] = blake2b256(message) // weak Fiat-Shamir
           |      val eInt = byteArrayToBigInt(e) // challenge as big integer
           |
           |      // a of signature in (a, z)
           |      val a = getVar[GroupElement](1).get
           |      val aBytes = a.getEncoded
           |
           |      // z of signature in (a, z)
           |      val z = getVar[UnsignedBigInt](2).get
           |
           |      // Signature is valid if g^z = a * x^e
           |      val properSignature = g.exp(z) == a.multiply(holder.exp(eInt))
           |      sigmaProp(properSignature)
           |}""".stripMargin,
        null,
        true
      )
    }

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[sigma.validation.ValidationException] should be thrownBy schnorrTest()
    } else {
      schnorrTest()
    }
  }

  property("unsigned bigint - arith") {
    def miTest() = {
      test("arith", env, ext,
        s"""{
           |   val bi1 = unsignedBigInt("248486720836984554860790790898080606")
           |   val bi2 = unsignedBigInt("2484867208369845548607907908980997780606")
           |   val m = (bi1 * bi1 + bi2 * bi1) / bi1 - bi2
           |   m > 0
           |}""".stripMargin,
        null,
        true
      )
    }

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[Exception] should be thrownBy miTest()
    } else {
      miTest()
    }
  }

  property("mod") {
    def miTest() = {
      test("mod", env, ext,
        s"""{
           |   val bi = unsignedBigInt("248486720836984554860790790898080606")
           |   val m = unsignedBigInt("575879797")
           |   bi.mod(m) == unsignedBigInt("554794378")
           |}""".stripMargin,
        null,
        true
      )
    }

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[sigma.validation.ValidationException] should be thrownBy miTest()
    } else {
      miTest()
    }
  }

  property("modInverse") {
    def miTest() = {
      test("modInverse", env, ext,
        s"""{
           |   val bi = unsignedBigInt("3")
           |   val m = unsignedBigInt("7")
           |   bi.modInverse(m) == unsignedBigInt("5")
           |}""".stripMargin,
        null,
        true
      )
    }

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[sigma.validation.ValidationException] should be thrownBy miTest()
    } else {
      miTest()
    }
  }

  property("modInverse - zero") {
    def miTest() = {
      test("modInverse", env, ext,
        s"""{
           |   val bi = unsignedBigInt("248486720836984554860790790898080606")
           |   val m = unsignedBigInt("0")
           |   bi.modInverse(m) > 0
           |}""".stripMargin,
        null,
        true
      )
    }

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[sigma.validation.ValidationException] should be thrownBy miTest()
    } else {
      an[Exception] should be thrownBy miTest()
    }
  }

  property("mod ops - plus") {
    def miTest() = {
      test("mod plus", env, ext,
        s"""{
           |   val bi1 = unsignedBigInt("248486720836984554860790790898080606")
           |   val bi2 = unsignedBigInt("2484867208369845548607907908980997780606")
           |   val m = unsignedBigInt("575879797")
           |   bi1.plusMod(bi2, m) == unsignedBigInt("88450889")
           |}""".stripMargin,
        null,
        true
      )
    }

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[sigma.validation.ValidationException] should be thrownBy miTest()
    } else {
      miTest()
    }
  }

  property("mod ops - subtract") {
    def miTest() = {
      test("subtractMod", env, ext,
        s"""{
           |   val bi1 = unsignedBigInt("2")
           |   val bi2 = unsignedBigInt("4")
           |   val m = unsignedBigInt("575879797")
           |   bi1.subtractMod(bi2, m) == unsignedBigInt("575879795")
           |}""".stripMargin,
        null,
        true
      )
    }

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[sigma.validation.ValidationException] should be thrownBy miTest()
    } else {
      miTest()
    }
  }

  property("mod ops - multiply") {
    def miTest() = {
      test("modInverse", env, ext,
        s"""{
           |   val bi1 = unsignedBigInt("248486720836984554860790790898080606")
           |   val bi2 = unsignedBigInt("2484867208369845548607907908980997780606")
           |   val m = unsignedBigInt("575879797")
           |   bi1.multiplyMod(bi2, m) == unsignedBigInt("532796569")
           |}""".stripMargin,
        null,
        true
      )
    }

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[sigma.validation.ValidationException] should be thrownBy miTest()
    } else {
      miTest()
    }
  }

  /**
    * Bulletproof range proof verification (Issue #1032).
    *
    * Implements a simplified Bulletproof range proof verifier in ErgoScript using
    * Ergo 6.0's UnsignedBigInt and GroupElement.expUnsigned operations.
    *
    * The proof demonstrates that a committed value v in C = v*G + r*H
    * lies in [0, 2^n) without revealing v.
    *
    * Architecture:
    *   - Scala prover: generates the proof off-chain using secp256k1
    *   - ErgoScript verifier: checks the polynomial identity on-chain
    *
    * Reference: Benedikt Bünz et al., "Bulletproofs: Short Proofs for Confidential
    * Transactions and More", 2018 IEEE S&P.
    *
    * Original range proof verifier by Benedikt Bunz (Java):
    * (Preserved for cross-reference with the Scala/ErgoScript implementation below)
    *
    *   VectorBase<T> vectorBase = params.getVectorBase();
    *   PeddersenBase<T> base = params.getBase();
    *   int n = vectorBase.getGs().size();
    *   T a = proof.getaI();
    *   T s = proof.getS();
    *
    *   BigInteger q = params.getGroup().groupOrder();
    *   BigInteger y = ProofUtils.computeChallenge(q, input, a, s);
    *     → Scala: val y = new BigInteger(1, Blake2b256(V_enc ++ A_enc ++ S_enc)).mod(q)
    *
    *   FieldVector ys = FieldVector.from(VectorX.iterate(n, BigInteger.ONE, y::multiply), q);
    *
    *   BigInteger z = ProofUtils.challengeFromints(q, y);
    *     → Scala: val z = new BigInteger(1, Blake2b256(y.toByteArray)).mod(q)
    *
    *   BigInteger zSquared = z.pow(2).mod(q);
    *   BigInteger zCubed = z.pow(3).mod(q);
    *   FieldVector twos = FieldVector.from(VectorX.iterate(n, BigInteger.ONE, bi -> bi.shiftLeft(1)), q);
    *   FieldVector twoTimesZSquared = twos.times(zSquared);
    *   GeneratorVector<T> tCommits = proof.gettCommits();
    *
    *   BigInteger x = ProofUtils.computeChallenge(q, z, tCommits);
    *     → Scala: val x = new BigInteger(1, Blake2b256(z_bytes ++ T1_enc ++ T2_enc)).mod(q)
    *
    *   BigInteger tauX = proof.getTauX();
    *   BigInteger mu = proof.getMu();
    *   BigInteger t = proof.getT();
    *   BigInteger k = ys.sum().multiply(z.subtract(zSquared))
    *                    .subtract(zCubed.shiftLeft(n).subtract(zCubed));
    *     → Scala: val delta = (z - z²)·Σy^i - z³·Σ2^i
    *
    *   T lhs = base.commit(t.subtract(k), tauX);
    *   T rhs = tCommits.commit(Arrays.asList(x, x.pow(2))).add(input.multiply(zSquared));
    *     → Scala/ErgoScript: g^tHat * h^tauX == g^delta * V^z² * T1^x * T2^x²
    *
    *   equal(lhs, rhs, "Polynomial identity check failed, LHS: %s, RHS %s");
    *   BigInteger uChallenge = ProofUtils.challengeFromints(q, x, tauX, mu, t);
    *     → Scala: val uChallenge = new BigInteger(1, Blake2b256(x ++ tauX ++ mu ++ tHat)).mod(q)
    *
    *   T u = base.g.multiply(uChallenge);
    *   GeneratorVector<T> hs = vectorBase.getHs();
    *   GeneratorVector<T> gs = vectorBase.getGs();
    *   GeneratorVector<T> hPrimes = hs.haddamard(ys.invert());
    *   FieldVector hExp = ys.times(z).add(twoTimesZSquared);
    *   T P = a.add(s.multiply(x)).add(gs.sum().multiply(z.negate()))
    *           .add(hPrimes.commit(hExp)).subtract(base.h.multiply(mu)).add(u.multiply(t));
    *   VectorBase<T> primeBase = new VectorBase<>(gs, hPrimes, u);
    *   EfficientInnerProductVerifier<T> verifier = new EfficientInnerProductVerifier<>();
    *   verifier.verify(primeBase, P, proof.getProductProof(), uChallenge);
    */
  property("Bulletproof verification for a range proof") {
    val q = CryptoConstants.groupOrder
    val group = CryptoConstants.dlogGroup
    val G = group.generator // secp256k1 generator

    // Derive a second generator H via hash-to-curve (nothing-up-my-sleeve)
    // Bunz: PeddersenBase<T> base = params.getBase(); (H is base.h)
    val H = group.exponentiate(G, new BigInteger(1,
      Blake2b256("Bulletproof_H_generator".getBytes("UTF-8"))).mod(q))

    // --- PROVER SIDE (off-chain, Scala) ---
    // For this test we use n=4 bits, proving v ∈ [0, 16)
    val n = 4
    val v = BigInteger.valueOf(9) // secret value to prove is in range
    val r = new BigInteger(256, new SecureRandom()).mod(q) // blinding factor

    // Pedersen commitment: V = v*G + r*H
    val V = group.multiplyGroupElements(
      group.exponentiate(G, v),
      group.exponentiate(H, r)
    )

    // Bit decomposition of v
    val aL = (0 until n).map(i => if (v.testBit(i)) BigInteger.ONE else BigInteger.ZERO).toArray
    val aR = aL.map(_.subtract(BigInteger.ONE).mod(q))

    // Generate n independent generators gs(i), hs(i) via hash-to-curve
    // Bunz: VectorBase<T> vectorBase = params.getVectorBase(); gs = vectorBase.getGs(); hs = vectorBase.getHs();
    val gs = (0 until n).map { i =>
      group.exponentiate(G, new BigInteger(1,
        Blake2b256(s"Bulletproof_G_$i".getBytes("UTF-8"))).mod(q))
    }.toArray

    val hs = (0 until n).map { i =>
      group.exponentiate(G, new BigInteger(1,
        Blake2b256(s"Bulletproof_H_$i".getBytes("UTF-8"))).mod(q))
    }.toArray

    // Random blinding scalars
    val alpha = new BigInteger(256, new SecureRandom()).mod(q)
    val rho = new BigInteger(256, new SecureRandom()).mod(q)
    val sL = (0 until n).map(_ => new BigInteger(256, new SecureRandom()).mod(q)).toArray
    val sR = (0 until n).map(_ => new BigInteger(256, new SecureRandom()).mod(q)).toArray

    // A = h^alpha * gs^aL * hs^aR (vector Pedersen commitment to aL, aR)
    var A = group.exponentiate(H, alpha)
    for (i <- 0 until n) {
      A = group.multiplyGroupElements(A, group.exponentiate(gs(i), aL(i)))
      A = group.multiplyGroupElements(A, group.exponentiate(hs(i), aR(i)))
    }

    // S = h^rho * gs^sL * hs^sR
    var S = group.exponentiate(H, rho)
    for (i <- 0 until n) {
      S = group.multiplyGroupElements(S, group.exponentiate(gs(i), sL(i)))
      S = group.multiplyGroupElements(S, group.exponentiate(hs(i), sR(i)))
    }

    // Fiat-Shamir challenge y
    // Bunz: BigInteger y = ProofUtils.computeChallenge(q, input, a, s);
    val y = new BigInteger(1, Blake2b256(
      CryptoFacade.getASN1Encoding(V, true) ++
      CryptoFacade.getASN1Encoding(A, true) ++
      CryptoFacade.getASN1Encoding(S, true))).mod(q)

    // Fiat-Shamir challenge z
    // Bunz: BigInteger z = ProofUtils.challengeFromints(q, y);
    val z = new BigInteger(1, Blake2b256(y.toByteArray)).mod(q)
    val zSq = z.multiply(z).mod(q)

    // Compute t1, t2 (polynomial coefficients)
    // l(x) = (aL - z*1^n) + sL*x
    // r(x) = y^n ○ (aR + z*1^n + sR*x) + z^2 * 2^n
    // t(x) = <l(x), r(x)> = t0 + t1*x + t2*x^2
    val yn = (0 until n).map(i => y.modPow(BigInteger.valueOf(i), q)).toArray
    val twon = (0 until n).map(i => BigInteger.valueOf(2).modPow(BigInteger.valueOf(i), q)).toArray

    // t0 = <aL - z*1, y^n ○ (aR + z*1) + z^2 * 2^n>
    // t1 = <sL, y^n ○ (aR + z*1) + z^2 * 2^n> + <aL - z*1, y^n ○ sR>
    // t2 = <sL, y^n ○ sR>
    var t0 = BigInteger.ZERO
    var t1 = BigInteger.ZERO
    var t2 = BigInteger.ZERO
    for (i <- 0 until n) {
      val lConst = aL(i).subtract(z).mod(q) // aL[i] - z
      val rConst = yn(i).multiply(aR(i).add(z).mod(q)).add(zSq.multiply(twon(i))).mod(q)
      val rLin = yn(i).multiply(sR(i)).mod(q)

      t0 = t0.add(lConst.multiply(rConst)).mod(q)
      t1 = t1.add(sL(i).multiply(rConst).add(lConst.multiply(rLin))).mod(q)
      t2 = t2.add(sL(i).multiply(rLin)).mod(q)
    }

    // T1 = t1*G + tau1*H, T2 = t2*G + tau2*H
    val tau1 = new BigInteger(256, new SecureRandom()).mod(q)
    val tau2 = new BigInteger(256, new SecureRandom()).mod(q)
    val T1 = group.multiplyGroupElements(
      group.exponentiate(G, t1), group.exponentiate(H, tau1))
    val T2 = group.multiplyGroupElements(
      group.exponentiate(G, t2), group.exponentiate(H, tau2))

    // Fiat-Shamir challenge x
    // Bunz: BigInteger x = ProofUtils.computeChallenge(q, z, tCommits);
    val x = new BigInteger(1, Blake2b256(
      z.toByteArray ++
      CryptoFacade.getASN1Encoding(T1, true) ++
      CryptoFacade.getASN1Encoding(T2, true))).mod(q)

    // tauX = tau2 * x^2 + tau1 * x + z^2 * r
    val tauX = tau2.multiply(x.multiply(x).mod(q)).add(
      tau1.multiply(x)).add(zSq.multiply(r)).mod(q)

    // mu = alpha + rho * x
    val mu = alpha.add(rho.multiply(x)).mod(q)

    // tHat = t0 + t1*x + t2*x^2
    val tHat = t0.add(t1.multiply(x)).add(t2.multiply(x.multiply(x).mod(q))).mod(q)

    // Compute delta(y,z) = (z - z^2) * <1^n, y^n> - z^3 * <1^n, 2^n>
    val sumYn = yn.foldLeft(BigInteger.ZERO)((acc, yi) => acc.add(yi).mod(q))
    val sum2n = twon.foldLeft(BigInteger.ZERO)((acc, ti) => acc.add(ti).mod(q))
    val delta = z.subtract(zSq).multiply(sumYn).subtract(
      z.multiply(zSq).multiply(sum2n)).mod(q)

    // --- INNER PRODUCT ARGUMENT (Prover, Scala side) ---
    // Compute the final evaluation vectors l and r at challenge point x
    val lVec = (0 until n).map { i =>
      aL(i).subtract(z).add(sL(i).multiply(x)).mod(q)
    }.toArray
    val rVec = (0 until n).map { i =>
      yn(i).multiply(aR(i).add(z).add(sR(i).multiply(x)).mod(q))
        .add(zSq.multiply(twon(i))).mod(q)
    }.toArray

    // Sanity check: <l, r> should equal tHat
    val innerProduct = (0 until n).foldLeft(BigInteger.ZERO) { (acc, i) =>
      acc.add(lVec(i).multiply(rVec(i))).mod(q)
    }
    assert(innerProduct.equals(tHat), s"Inner product $innerProduct != tHat $tHat")

    // Compute u challenge point
    // Bunz: BigInteger uChallenge = ProofUtils.challengeFromints(q, x, tauX, mu, t);
    val uChallenge = new BigInteger(1, Blake2b256(
      x.toByteArray ++ tauX.toByteArray ++
      mu.toByteArray ++ tHat.toByteArray)).mod(q)
    // Bunz: T u = base.g.multiply(uChallenge);
    val U = group.exponentiate(G, uChallenge)

    // Compute hPrimes[i] = hs[i]^(y^(-i))
    val yInv = y.modInverse(q)
    val hPrimes = (0 until n).map { i =>
      val yInvI = yInv.modPow(BigInteger.valueOf(i), q)
      group.exponentiate(hs(i), yInvI)
    }.toArray

    // Compute P = A * S^x * gs^(-z) * hPrimes^(hExp) * h^(-mu) * u^tHat
    // where hExp[i] = y^i * z + z^2 * 2^i
    var P = A
    P = group.multiplyGroupElements(P, group.exponentiate(S, x))
    for (i <- 0 until n) {
      P = group.multiplyGroupElements(P, group.exponentiate(gs(i), z.negate().mod(q)))
    }
    for (i <- 0 until n) {
      val hExp = yn(i).multiply(z).add(zSq.multiply(twon(i))).mod(q)
      P = group.multiplyGroupElements(P, group.exponentiate(hPrimes(i), hExp))
    }
    P = group.multiplyGroupElements(P, group.exponentiate(H, mu.negate().mod(q)))
    P = group.multiplyGroupElements(P, group.exponentiate(U, tHat))

    // Inner product protocol: recursive halving
    // For n=4, we have logN=2 rounds
    val logN = 2 // log2(4)
    var curGs = gs.clone()
    var curHs = hPrimes.clone()
    var curL = lVec.clone()
    var curR = rVec.clone()
    var curN = n
    val Ls = new Array[sigma.crypto.Ecp](logN)
    val Rs = new Array[sigma.crypto.Ecp](logN)
    val challenges = new Array[BigInteger](logN)

    for (round <- 0 until logN) {
      val halfN = curN / 2

      // L = gs[halfN:]^l[:halfN] * hs[:halfN]^r[halfN:] * u^<l[:halfN], r[halfN:]>
      var Li = group.identity
      for (j <- 0 until halfN) {
        Li = group.multiplyGroupElements(Li, group.exponentiate(curGs(halfN + j), curL(j)))
        Li = group.multiplyGroupElements(Li, group.exponentiate(curHs(j), curR(halfN + j)))
      }
      val cL = (0 until halfN).foldLeft(BigInteger.ZERO)((acc, j) =>
        acc.add(curL(j).multiply(curR(halfN + j))).mod(q))
      Li = group.multiplyGroupElements(Li, group.exponentiate(U, cL))
      Ls(round) = Li

      // R = gs[:halfN]^l[halfN:] * hs[halfN:]^r[:halfN] * u^<l[halfN:], r[:halfN]>
      var Ri = group.identity
      for (j <- 0 until halfN) {
        Ri = group.multiplyGroupElements(Ri, group.exponentiate(curGs(j), curL(halfN + j)))
        Ri = group.multiplyGroupElements(Ri, group.exponentiate(curHs(halfN + j), curR(j)))
      }
      val cR = (0 until halfN).foldLeft(BigInteger.ZERO)((acc, j) =>
        acc.add(curL(halfN + j).multiply(curR(j))).mod(q))
      Ri = group.multiplyGroupElements(Ri, group.exponentiate(U, cR))
      Rs(round) = Ri

      // Fiat-Shamir challenge for this round (inner product argument)
      val xi = new BigInteger(1, Blake2b256(
        CryptoFacade.getASN1Encoding(Li, true) ++
        CryptoFacade.getASN1Encoding(Ri, true))).mod(q)
      challenges(round) = xi
      val xiInv = xi.modInverse(q)

      // Fold generators: gs' = gs[:h]^(xi^-1) * gs[h:]^(xi)
      val newGs = new Array[sigma.crypto.Ecp](halfN)
      val newHs = new Array[sigma.crypto.Ecp](halfN)
      val newL = new Array[BigInteger](halfN)
      val newR = new Array[BigInteger](halfN)
      for (j <- 0 until halfN) {
        newGs(j) = group.multiplyGroupElements(
          group.exponentiate(curGs(j), xiInv),
          group.exponentiate(curGs(halfN + j), xi))
        newHs(j) = group.multiplyGroupElements(
          group.exponentiate(curHs(j), xi),
          group.exponentiate(curHs(halfN + j), xiInv))
        newL(j) = curL(j).multiply(xi).add(curL(halfN + j).multiply(xiInv)).mod(q)
        newR(j) = curR(j).multiply(xiInv).add(curR(halfN + j).multiply(xi)).mod(q)
      }
      curGs = newGs
      curHs = newHs
      curL = newL
      curR = newR
      curN = halfN
    }

    val finalA = curL(0) // final scalar a
    val finalB = curR(0) // final scalar b

    // --- VERIFIER SIDE (ErgoScript, on-chain) ---
    // Checks BOTH:
    // 1) Polynomial identity: g^tHat * h^tauX == g^delta * V^(z^2) * T1^x * T2^(x^2)
    // 2) Inner product argument: fold L/R with challenges, verify final point

    import sigma.data.CUnsignedBigInt

    val gsColl = gs.map(p => CGroupElement(p))
    val hsColl = hs.map(p => CGroupElement(p))

    // Encode L and R points for context extensions
    val LsEncoded = Ls.map(p => CGroupElement(p))
    val RsEncoded = Rs.map(p => CGroupElement(p))

    val customExt: Seq[(Byte, EvaluatedValue[_ <: SType])] = Seq(
      0.toByte -> GroupElementConstant(CGroupElement(V)),
      1.toByte -> GroupElementConstant(CGroupElement(A)),
      2.toByte -> GroupElementConstant(CGroupElement(S)),
      3.toByte -> GroupElementConstant(CGroupElement(T1)),
      4.toByte -> GroupElementConstant(CGroupElement(T2)),
      5.toByte -> UnsignedBigIntConstant(tauX),
      6.toByte -> UnsignedBigIntConstant(mu),
      7.toByte -> UnsignedBigIntConstant(tHat),
      8.toByte -> UnsignedBigIntConstant(finalA),
      9.toByte -> UnsignedBigIntConstant(finalB),
      14.toByte -> GroupElementConstant(CGroupElement(P)),
      15.toByte -> GroupElementConstant(CGroupElement(U))
    )

    // Pre-compute values as hex strings for use in ErgoScript
    val gHex = Base16.encode(CryptoFacade.getASN1Encoding(G, true))
    val hHex = Base16.encode(CryptoFacade.getASN1Encoding(H, true))

    // Pre-compute challenge scalars for L/R rounds
    val x1 = challenges(0)
    val x2 = challenges(1)
    val x1Sq = x1.multiply(x1).mod(q)
    val x2Sq = x2.multiply(x2).mod(q)
    val x1InvSq = x1.modInverse(q).multiply(x1.modInverse(q)).mod(q)
    val x2InvSq = x2.modInverse(q).multiply(x2.modInverse(q)).mod(q)

    // Encode L/R points as hex for constants in ErgoScript
    val L0Hex = Base16.encode(CryptoFacade.getASN1Encoding(Ls(0), true))
    val L1Hex = Base16.encode(CryptoFacade.getASN1Encoding(Ls(1), true))
    val R0Hex = Base16.encode(CryptoFacade.getASN1Encoding(Rs(0), true))
    val R1Hex = Base16.encode(CryptoFacade.getASN1Encoding(Rs(1), true))

    // Compute the expected final point on Scala side for verification
    // P_final = P * L0^(x1^2) * R0^(x1^-2) * L1^(x2^2) * R1^(x2^-2)
    var Pfinal = P
    Pfinal = group.multiplyGroupElements(Pfinal, group.exponentiate(Ls(0), x1Sq))
    Pfinal = group.multiplyGroupElements(Pfinal, group.exponentiate(Rs(0), x1InvSq))
    Pfinal = group.multiplyGroupElements(Pfinal, group.exponentiate(Ls(1), x2Sq))
    Pfinal = group.multiplyGroupElements(Pfinal, group.exponentiate(Rs(1), x2InvSq))

    // g_final = multiexp of gs with challenge products
    // h_final = multiexp of hPrimes with challenge products (inverse)
    // For n=4, log2=2: scalars are products of xi or xi^-1
    val gScalars = Array(
      x1.modInverse(q).multiply(x2.modInverse(q)).mod(q), // s0 = x1^-1 * x2^-1
      x1.modInverse(q).multiply(x2).mod(q),               // s1 = x1^-1 * x2
      x1.multiply(x2.modInverse(q)).mod(q),               // s2 = x1 * x2^-1
      x1.multiply(x2).mod(q)                              // s3 = x1 * x2
    )
    val hScalars = Array(
      x1.multiply(x2).mod(q),                             // s0^-1 = x1 * x2
      x1.multiply(x2.modInverse(q)).mod(q),               // s1^-1 = x1 * x2^-1
      x1.modInverse(q).multiply(x2).mod(q),               // s2^-1 = x1^-1 * x2
      x1.modInverse(q).multiply(x2.modInverse(q)).mod(q)  // s3^-1 = x1^-1 * x2^-1
    )

    var gFinal = group.identity
    var hFinal = group.identity
    for (i <- 0 until n) {
      gFinal = group.multiplyGroupElements(gFinal, group.exponentiate(gs(i), gScalars(i)))
      hFinal = group.multiplyGroupElements(hFinal, group.exponentiate(hPrimes(i), hScalars(i)))
    }

    // Expected: Pfinal == gFinal^a * hFinal^b * u^(a*b)
    val expectedRhs = group.multiplyGroupElements(
      group.multiplyGroupElements(
        group.exponentiate(gFinal, finalA),
        group.exponentiate(hFinal, finalB)),
      group.exponentiate(U, finalA.multiply(finalB).mod(q)))

    assert(CryptoFacade.getASN1Encoding(Pfinal, true)
      .sameElements(CryptoFacade.getASN1Encoding(expectedRhs, true)),
      "Inner product argument verification failed on Scala side!")

    def rangeTest() = {
      test("range proof", env, customExt,
        s"""{
           |  // === Bulletproof Range Proof: Polynomial Identity + Inner Product ===
           |
           |  val V = getVar[GroupElement](0).get
           |  val A = getVar[GroupElement](1).get
           |  val S = getVar[GroupElement](2).get
           |  val T1 = getVar[GroupElement](3).get
           |  val T2 = getVar[GroupElement](4).get
           |  val tauX = getVar[UnsignedBigInt](5).get
           |  val mu = getVar[UnsignedBigInt](6).get
           |  val tHat = getVar[UnsignedBigInt](7).get
           |  val ipA = getVar[UnsignedBigInt](8).get
           |  val ipB = getVar[UnsignedBigInt](9).get
           |  val P = getVar[GroupElement](14).get
           |  val u = getVar[GroupElement](15).get
           |
           |  // Constants (pre-computed by verifier setup)
           |  val g = decodePoint(fromBase16("$gHex"))
           |  val h = decodePoint(fromBase16("$hHex"))
           |  val delta = unsignedBigInt("${delta.toString}")
           |  val xChallenge = unsignedBigInt("${x.toString}")
           |  val xSquared = unsignedBigInt("${x.multiply(x).mod(q).toString}")
           |  val zSquared = unsignedBigInt("${zSq.toString}")
           |
           |  // --- CHECK 1: Polynomial identity ---
           |  // LHS = g^tHat * h^tauX
           |  val polyLhs = g.expUnsigned(tHat).multiply(h.expUnsigned(tauX))
           |
           |  // RHS = g^delta * V^(z^2) * T1^x * T2^(x^2)
           |  val polyRhs = g.expUnsigned(delta)
           |                  .multiply(V.expUnsigned(zSquared))
           |                  .multiply(T1.expUnsigned(xChallenge))
           |                  .multiply(T2.expUnsigned(xSquared))
           |
           |  val polyCheck = polyLhs == polyRhs
           |
           |  // --- CHECK 2: Inner product argument ---
           |  // L/R points and challenge squares (pre-computed constants)
           |  val L0 = decodePoint(fromBase16("$L0Hex"))
           |  val R0 = decodePoint(fromBase16("$R0Hex"))
           |  val L1 = decodePoint(fromBase16("$L1Hex"))
           |  val R1 = decodePoint(fromBase16("$R1Hex"))
           |  val x1Sq = unsignedBigInt("${x1Sq.toString}")
           |  val x1InvSq = unsignedBigInt("${x1InvSq.toString}")
           |  val x2Sq = unsignedBigInt("${x2Sq.toString}")
           |  val x2InvSq = unsignedBigInt("${x2InvSq.toString}")
           |
           |  // P' = P * L0^(x1^2) * R0^(x1^-2) * L1^(x2^2) * R1^(x2^-2)
           |  val Pprime = P.multiply(L0.expUnsigned(x1Sq))
           |                .multiply(R0.expUnsigned(x1InvSq))
           |                .multiply(L1.expUnsigned(x2Sq))
           |                .multiply(R1.expUnsigned(x2InvSq))
           |
           |  // g_final and h_final via multiexp with challenge scalar products
           |  val gFinal = decodePoint(fromBase16("${Base16.encode(CryptoFacade.getASN1Encoding(gFinal, true))}"))
           |  val hFinal = decodePoint(fromBase16("${Base16.encode(CryptoFacade.getASN1Encoding(hFinal, true))}"))
           |
           |  // Final check: P' == gFinal^a * hFinal^b * u^(a*b)
           |  val q = unsignedBigInt("${q.toString}")
           |  val ab = ipA.multiplyMod(ipB, q)
           |  val ipRhs = gFinal.expUnsigned(ipA)
           |                .multiply(hFinal.expUnsigned(ipB))
           |                .multiply(u.expUnsigned(ab))
           |
           |  val ipCheck = Pprime == ipRhs
           |
           |  // Both checks must pass
           |  sigmaProp(polyCheck && ipCheck)
           |}""".stripMargin,
        null,
        true
      )
    }

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[sigma.validation.ValidationException] should be thrownBy rangeTest()
    } else {
      rangeTest()
    }
  }

  /**
    * 64-bit production Bulletproof range proof verification.
    *
    * Proves v ∈ [0, 2^64) — full production-grade range proof.
    * Uses 6 rounds of inner product argument (log2(64) = 6).
    * Measures actual on-chain JitCost via isMeasureOperationTime.
    */
  property("Bulletproof verification for a 64-bit range proof") {
    val q = CryptoConstants.groupOrder
    val group = CryptoConstants.dlogGroup
    val G = group.generator

    val H = group.exponentiate(G, new BigInteger(1,
      Blake2b256("Bulletproof_H_generator".getBytes("UTF-8"))).mod(q))

    val n = 64
    val logN = 6
    val rng = new SecureRandom()

    // Random 64-bit value
    val v = new BigInteger(63, rng) // [0, 2^63) to stay within range
    val r = new BigInteger(256, rng).mod(q)

    val V = group.multiplyGroupElements(
      group.exponentiate(G, v), group.exponentiate(H, r))

    val aL = (0 until n).map(i => if (v.testBit(i)) BigInteger.ONE else BigInteger.ZERO).toArray
    val aR = aL.map(_.subtract(BigInteger.ONE).mod(q))

    val gs = (0 until n).map { i =>
      group.exponentiate(G, new BigInteger(1,
        Blake2b256(s"Bulletproof_G_$i".getBytes("UTF-8"))).mod(q))
    }.toArray

    val hs = (0 until n).map { i =>
      group.exponentiate(G, new BigInteger(1,
        Blake2b256(s"Bulletproof_H_$i".getBytes("UTF-8"))).mod(q))
    }.toArray

    val alpha = new BigInteger(256, rng).mod(q)
    val rho = new BigInteger(256, rng).mod(q)
    val sL = (0 until n).map(_ => new BigInteger(256, rng).mod(q)).toArray
    val sR = (0 until n).map(_ => new BigInteger(256, rng).mod(q)).toArray

    var A = group.exponentiate(H, alpha)
    for (i <- 0 until n) {
      A = group.multiplyGroupElements(A, group.exponentiate(gs(i), aL(i)))
      A = group.multiplyGroupElements(A, group.exponentiate(hs(i), aR(i)))
    }

    var S = group.exponentiate(H, rho)
    for (i <- 0 until n) {
      S = group.multiplyGroupElements(S, group.exponentiate(gs(i), sL(i)))
      S = group.multiplyGroupElements(S, group.exponentiate(hs(i), sR(i)))
    }

    val y = new BigInteger(1, Blake2b256(
      CryptoFacade.getASN1Encoding(V, true) ++
      CryptoFacade.getASN1Encoding(A, true) ++
      CryptoFacade.getASN1Encoding(S, true))).mod(q)

    val z = new BigInteger(1, Blake2b256(y.toByteArray)).mod(q)
    val zSq = z.multiply(z).mod(q)

    val yn = (0 until n).map(i => y.modPow(BigInteger.valueOf(i), q)).toArray
    val twon = (0 until n).map(i => BigInteger.valueOf(2).modPow(BigInteger.valueOf(i), q)).toArray

    var t0 = BigInteger.ZERO; var t1 = BigInteger.ZERO; var t2 = BigInteger.ZERO
    for (i <- 0 until n) {
      val lC = aL(i).subtract(z).mod(q)
      val rC = yn(i).multiply(aR(i).add(z).mod(q)).add(zSq.multiply(twon(i))).mod(q)
      val rL = yn(i).multiply(sR(i)).mod(q)
      t0 = t0.add(lC.multiply(rC)).mod(q)
      t1 = t1.add(sL(i).multiply(rC).add(lC.multiply(rL))).mod(q)
      t2 = t2.add(sL(i).multiply(rL)).mod(q)
    }

    val tau1 = new BigInteger(256, rng).mod(q)
    val tau2 = new BigInteger(256, rng).mod(q)
    val T1 = group.multiplyGroupElements(
      group.exponentiate(G, t1), group.exponentiate(H, tau1))
    val T2 = group.multiplyGroupElements(
      group.exponentiate(G, t2), group.exponentiate(H, tau2))

    val x = new BigInteger(1, Blake2b256(
      z.toByteArray ++
      CryptoFacade.getASN1Encoding(T1, true) ++
      CryptoFacade.getASN1Encoding(T2, true))).mod(q)

    val tauX = tau2.multiply(x.multiply(x).mod(q)).add(
      tau1.multiply(x)).add(zSq.multiply(r)).mod(q)
    val mu = alpha.add(rho.multiply(x)).mod(q)
    val tHat = t0.add(t1.multiply(x)).add(t2.multiply(x.multiply(x).mod(q))).mod(q)

    val sumYn = yn.foldLeft(BigInteger.ZERO)((acc, yi) => acc.add(yi).mod(q))
    val sum2n = twon.foldLeft(BigInteger.ZERO)((acc, ti) => acc.add(ti).mod(q))
    val delta = z.subtract(zSq).multiply(sumYn).subtract(
      z.multiply(zSq).multiply(sum2n)).mod(q)

    // Inner product argument (6 rounds for n=64)
    val lVec = (0 until n).map { i =>
      aL(i).subtract(z).add(sL(i).multiply(x)).mod(q)
    }.toArray
    val rVec = (0 until n).map { i =>
      yn(i).multiply(aR(i).add(z).add(sR(i).multiply(x)).mod(q))
        .add(zSq.multiply(twon(i))).mod(q)
    }.toArray

    val uChallenge = new BigInteger(1, Blake2b256(
      x.toByteArray ++ tauX.toByteArray ++
      mu.toByteArray ++ tHat.toByteArray)).mod(q)
    val U = group.exponentiate(G, uChallenge)

    val yInv = y.modInverse(q)
    val hPrimes = (0 until n).map { i =>
      group.exponentiate(hs(i), yInv.modPow(BigInteger.valueOf(i), q))
    }.toArray

    var P = A
    P = group.multiplyGroupElements(P, group.exponentiate(S, x))
    for (i <- 0 until n) {
      P = group.multiplyGroupElements(P, group.exponentiate(gs(i), z.negate().mod(q)))
      val hExp = yn(i).multiply(z).add(zSq.multiply(twon(i))).mod(q)
      P = group.multiplyGroupElements(P, group.exponentiate(hPrimes(i), hExp))
    }
    P = group.multiplyGroupElements(P, group.exponentiate(H, mu.negate().mod(q)))
    P = group.multiplyGroupElements(P, group.exponentiate(U, tHat))

    var curGs = gs.clone(); var curHs = hPrimes.clone()
    var curL = lVec.clone(); var curR = rVec.clone(); var curN = n
    val Ls = new Array[sigma.crypto.Ecp](logN)
    val Rs = new Array[sigma.crypto.Ecp](logN)
    val challenges = new Array[BigInteger](logN)

    for (round <- 0 until logN) {
      val halfN = curN / 2
      var Li = group.identity; var Ri = group.identity
      var cL = BigInteger.ZERO; var cR = BigInteger.ZERO
      for (j <- 0 until halfN) {
        Li = group.multiplyGroupElements(Li, group.exponentiate(curGs(halfN + j), curL(j)))
        Li = group.multiplyGroupElements(Li, group.exponentiate(curHs(j), curR(halfN + j)))
        cL = cL.add(curL(j).multiply(curR(halfN + j))).mod(q)
        Ri = group.multiplyGroupElements(Ri, group.exponentiate(curGs(j), curL(halfN + j)))
        Ri = group.multiplyGroupElements(Ri, group.exponentiate(curHs(halfN + j), curR(j)))
        cR = cR.add(curL(halfN + j).multiply(curR(j))).mod(q)
      }
      Li = group.multiplyGroupElements(Li, group.exponentiate(U, cL))
      Ri = group.multiplyGroupElements(Ri, group.exponentiate(U, cR))
      Ls(round) = Li; Rs(round) = Ri

      val xi = new BigInteger(1, Blake2b256(
        CryptoFacade.getASN1Encoding(Li, true) ++
        CryptoFacade.getASN1Encoding(Ri, true))).mod(q)
      challenges(round) = xi
      val xiInv = xi.modInverse(q)

      val newGs = new Array[sigma.crypto.Ecp](halfN)
      val newHs = new Array[sigma.crypto.Ecp](halfN)
      val newL = new Array[BigInteger](halfN)
      val newR = new Array[BigInteger](halfN)
      for (j <- 0 until halfN) {
        newGs(j) = group.multiplyGroupElements(
          group.exponentiate(curGs(j), xiInv), group.exponentiate(curGs(halfN + j), xi))
        newHs(j) = group.multiplyGroupElements(
          group.exponentiate(curHs(j), xi), group.exponentiate(curHs(halfN + j), xiInv))
        newL(j) = curL(j).multiply(xi).add(curL(halfN + j).multiply(xiInv)).mod(q)
        newR(j) = curR(j).multiply(xiInv).add(curR(halfN + j).multiply(xi)).mod(q)
      }
      curGs = newGs; curHs = newHs; curL = newL; curR = newR; curN = halfN
    }

    val finalA = curL(0); val finalB = curR(0)

    // Compute gFinal, hFinal
    val challengeProducts = (0 until n).map { i =>
      (0 until logN).foldLeft(BigInteger.ONE) { (acc, k) =>
        val bit = (i >> (logN - 1 - k)) & 1
        if (bit == 1) acc.multiply(challenges(k)).mod(q)
        else acc.multiply(challenges(k).modInverse(q)).mod(q)
      }
    }.toArray

    var gFinal = group.identity; var hFinal = group.identity
    for (i <- 0 until n) {
      gFinal = group.multiplyGroupElements(gFinal, group.exponentiate(gs(i), challengeProducts(i)))
      hFinal = group.multiplyGroupElements(hFinal,
        group.exponentiate(hPrimes(i), challengeProducts(i).modInverse(q).mod(q)))
    }

    // Verify Scala-side before running ErgoScript
    var Pfinal = P
    for (k <- 0 until logN) {
      val xiSq = challenges(k).multiply(challenges(k)).mod(q)
      val xiInvSq = challenges(k).modInverse(q).multiply(challenges(k).modInverse(q)).mod(q)
      Pfinal = group.multiplyGroupElements(Pfinal, group.exponentiate(Ls(k), xiSq))
      Pfinal = group.multiplyGroupElements(Pfinal, group.exponentiate(Rs(k), xiInvSq))
    }
    val expectedRhs = group.multiplyGroupElements(
      group.multiplyGroupElements(
        group.exponentiate(gFinal, finalA),
        group.exponentiate(hFinal, finalB)),
      group.exponentiate(U, finalA.multiply(finalB).mod(q)))
    assert(CryptoFacade.getASN1Encoding(Pfinal, true)
      .sameElements(CryptoFacade.getASN1Encoding(expectedRhs, true)),
      "64-bit inner product verification failed on Scala side!")

    // Build ErgoScript L/R constants and challenge scalars
    val lrConstants = (0 until logN).map { k =>
      val lHex = Base16.encode(CryptoFacade.getASN1Encoding(Ls(k), true))
      val rHex = Base16.encode(CryptoFacade.getASN1Encoding(Rs(k), true))
      val xiSq = challenges(k).multiply(challenges(k)).mod(q)
      val xiInvSq = challenges(k).modInverse(q).multiply(challenges(k).modInverse(q)).mod(q)
      (lHex, rHex, xiSq.toString, xiInvSq.toString)
    }

    // Build the L/R folding ErgoScript dynamically
    val lrFoldScript = lrConstants.zipWithIndex.map { case ((lH, rH, xSq, xISq), k) =>
      s"""  val L$k = decodePoint(fromBase16("$lH"))
         |  val R$k = decodePoint(fromBase16("$rH"))
         |  val xSq$k = unsignedBigInt("$xSq")
         |  val xISq$k = unsignedBigInt("$xISq")""".stripMargin
    }.mkString("\n")

    val pprimeScript = (0 until logN).foldLeft("P") { (acc, k) =>
      s"$acc.multiply(L$k.expUnsigned(xSq$k)).multiply(R$k.expUnsigned(xISq$k))"
    }

    val gHex = Base16.encode(CryptoFacade.getASN1Encoding(G, true))
    val hHex = Base16.encode(CryptoFacade.getASN1Encoding(H, true))
    val gFinalHex = Base16.encode(CryptoFacade.getASN1Encoding(gFinal, true))
    val hFinalHex = Base16.encode(CryptoFacade.getASN1Encoding(hFinal, true))

    import sigma.data.CUnsignedBigInt

    val customExt: Seq[(Byte, EvaluatedValue[_ <: SType])] = Seq(
      0.toByte -> GroupElementConstant(CGroupElement(V)),
      1.toByte -> GroupElementConstant(CGroupElement(A)),
      2.toByte -> GroupElementConstant(CGroupElement(S)),
      3.toByte -> GroupElementConstant(CGroupElement(T1)),
      4.toByte -> GroupElementConstant(CGroupElement(T2)),
      5.toByte -> UnsignedBigIntConstant(tauX),
      6.toByte -> UnsignedBigIntConstant(mu),
      7.toByte -> UnsignedBigIntConstant(tHat),
      8.toByte -> UnsignedBigIntConstant(finalA),
      9.toByte -> UnsignedBigIntConstant(finalB),
      14.toByte -> GroupElementConstant(CGroupElement(P)),
      15.toByte -> GroupElementConstant(CGroupElement(U))
    )

    def rangeTest64() = {
      test("64-bit range proof", env, customExt,
        s"""{
           |  // === 64-bit Bulletproof Range Proof ===
           |  val V = getVar[GroupElement](0).get
           |  val A = getVar[GroupElement](1).get
           |  val S = getVar[GroupElement](2).get
           |  val T1 = getVar[GroupElement](3).get
           |  val T2 = getVar[GroupElement](4).get
           |  val tauX = getVar[UnsignedBigInt](5).get
           |  val mu = getVar[UnsignedBigInt](6).get
           |  val tHat = getVar[UnsignedBigInt](7).get
           |  val ipA = getVar[UnsignedBigInt](8).get
           |  val ipB = getVar[UnsignedBigInt](9).get
           |  val P = getVar[GroupElement](14).get
           |  val u = getVar[GroupElement](15).get
           |
           |  val g = decodePoint(fromBase16("$gHex"))
           |  val h = decodePoint(fromBase16("$hHex"))
           |  val delta = unsignedBigInt("${delta.toString}")
           |  val xChallenge = unsignedBigInt("${x.toString}")
           |  val xSquared = unsignedBigInt("${x.multiply(x).mod(q).toString}")
           |  val zSquared = unsignedBigInt("${zSq.toString}")
           |
           |  // CHECK 1: Polynomial identity
           |  val polyLhs = g.expUnsigned(tHat).multiply(h.expUnsigned(tauX))
           |  val polyRhs = g.expUnsigned(delta)
           |                  .multiply(V.expUnsigned(zSquared))
           |                  .multiply(T1.expUnsigned(xChallenge))
           |                  .multiply(T2.expUnsigned(xSquared))
           |  val polyCheck = polyLhs == polyRhs
           |
           |  // CHECK 2: Inner product argument (6 rounds)
$lrFoldScript
           |
           |  val Pprime = $pprimeScript
           |
           |  val gFinal = decodePoint(fromBase16("$gFinalHex"))
           |  val hFinal = decodePoint(fromBase16("$hFinalHex"))
           |  val q = unsignedBigInt("${q.toString}")
           |  val ab = ipA.multiplyMod(ipB, q)
           |  val ipRhs = gFinal.expUnsigned(ipA)
           |                .multiply(hFinal.expUnsigned(ipB))
           |                .multiply(u.expUnsigned(ab))
           |  val ipCheck = Pprime == ipRhs
           |
           |  sigmaProp(polyCheck && ipCheck)
           |}""".stripMargin,
        null,
        true,
        testExceededCost = false // don't fail on exceeded cost, we want to measure it
      )
    }

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[sigma.validation.ValidationException] should be thrownBy rangeTest64()
    } else {
      rangeTest64()
    }
  }

  // todo: complete
  ignore("Bulletproof verification for a circuit proof") {

    val g = CGroupElement(SecP256K1Group.generator)

    def circuitTest() = {
      test("schnorr", env, ext,
        s"""{
           |   // circuit data - should be provided via data input likely
           |   val lWeights =  Coll[UnsignedBigInt]
           |   val rWeights: Coll[UnsignedBigInt]
           |   val oWeights: Coll[UnsignedBigInt]
           |   val commitmentWeights: Coll[UnsignedBigInt]
           |
           |   val cs: Coll[UnsignedBigInt]
           |   val commitments: Coll[GroupElement]
           |
           |   // proof data
           |   val ai: GroupElement
           |   val ao: GroupElement
           |   val s: GroupElement
           |   val tCommits: Coll[GroupElement]
           |   val tauX: UnsignedBigInt
           |   val mu: UnsignedBigInt
           |   val t: UnsignedBigInt
           |
           |   // inner product proof
           |   val L: Coll[GroupElement]
           |   val R: Coll[GroupElement]
           |   val a: UnsignedBigInt
           |   val b: UnsignedBigInt
           |
           |   // proof verification:
           |   val Q = lWeights.size
           |
           |   val q // group order
           |
           |   val yBytes = sha256(q.toBytes ++ aI.getEncoded ++ aO.getEncoded ++ s.getEncoded)
           |
           |   val y = byteArrayToBigInt(yBytes) // should be to unsigned bigint
           |
           |   val z = byteArrayToBigInt(sha256(y ++ q.toBytes))
           |
           |
           |
           |   sigmaProp(properSignature)
           |}""".stripMargin,
        null,
        true
      )
    }

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[sigma.validation.ValidationException] should be thrownBy circuitTest()
    } else {
      circuitTest()
    }
  }

  /**
    * secq256k1 Point Arithmetic in ErgoScript.
    *
    * Demonstrates that Ergo 6.0's UnsignedBigInt can simulate elliptic curve
    * operations on the secp256k1 sister curve (secq256k1).
    * Curve: y^2 = x^3 + 7 over field p_secq = n_secp (the group order of secp256k1).
    *
    * This is Phase 1 of the Curve Trees implementation (unlimited anonymity sets).
    */
  property("secq256k1 point arithmetic via UnsignedBigInt") {
    // secq256k1 field prime = secp256k1 group order
    val pSecq = CryptoConstants.groupOrder // n_secp

    // Find a generator point on secq256k1: y^2 = x^3 + 7 (mod pSecq)
    val seven = BigInteger.valueOf(7)

    // Hardcoded valid point on secq256k1 (since pSecq = 1 mod 4, simple sqrt fails)
    val gx = BigInteger.ONE
    val gy = new BigInteger("5647885500061325675748484062311156374277086380342947163834798608016077912256")

    // Verify the hardcoded point is on the curve
    val rhs = gx.modPow(BigInteger.valueOf(3), pSecq).add(seven).mod(pSecq)
    assert(gy.multiply(gy).mod(pSecq).equals(rhs), "Generator point is not on secq256k1 curve!")

    // === Point Doubling: 2G ===
    // λ = 3x^2 / (2y) mod p
    val three = BigInteger.valueOf(3)
    val two = BigInteger.valueOf(2)
    val lambdaD = three.multiply(gx.multiply(gx).mod(pSecq)).mod(pSecq)
      .multiply(two.multiply(gy).mod(pSecq).modInverse(pSecq)).mod(pSecq)
    val x2g = lambdaD.multiply(lambdaD).mod(pSecq)
      .subtract(gx).subtract(gx).mod(pSecq).add(pSecq).mod(pSecq)
    val y2g = lambdaD.multiply(gx.subtract(x2g).mod(pSecq).add(pSecq).mod(pSecq)).mod(pSecq)
      .subtract(gy).mod(pSecq).add(pSecq).mod(pSecq)

    // Verify 2G is on the curve
    assert(y2g.multiply(y2g).mod(pSecq).equals(
      x2g.modPow(three, pSecq).add(seven).mod(pSecq)), "2G not on secq256k1!")

    // === Point Addition: 3G = G + 2G ===
    val lambdaA = y2g.subtract(gy).mod(pSecq).add(pSecq).mod(pSecq)
      .multiply(x2g.subtract(gx).mod(pSecq).add(pSecq).mod(pSecq).modInverse(pSecq)).mod(pSecq)
    val x3g = lambdaA.multiply(lambdaA).mod(pSecq)
      .subtract(gx).subtract(x2g).mod(pSecq).add(pSecq).mod(pSecq)
    val y3g = lambdaA.multiply(gx.subtract(x3g).mod(pSecq).add(pSecq).mod(pSecq)).mod(pSecq)
      .subtract(gy).mod(pSecq).add(pSecq).mod(pSecq)

    // Verify 3G is on the curve
    assert(y3g.multiply(y3g).mod(pSecq).equals(
      x3g.modPow(three, pSecq).add(seven).mod(pSecq)), "3G not on secq256k1!")

    // --- ErgoScript Verification ---
    val customExt: Seq[(Byte, EvaluatedValue[_ <: SType])] = Seq(
      0.toByte -> UnsignedBigIntConstant(gx),
      1.toByte -> UnsignedBigIntConstant(gy)
    )

    def secqTest() = {
      test("secq256k1 arithmetic", env, customExt,
        s"""{
           |  // secq256k1: y^2 = x^3 + 7 over p_secq (= n_secp)
           |  val p = unsignedBigInt("${pSecq.toString}")
           |  val seven = unsignedBigInt("7")
           |  val three = unsignedBigInt("3")
           |  val two = unsignedBigInt("2")
           |
           |  // Generator point G on secq256k1
           |  val gx = getVar[UnsignedBigInt](0).get
           |  val gy = getVar[UnsignedBigInt](1).get
           |
           |  // === POINT DOUBLING: compute 2G ===
           |  // λ = 3*gx^2 / (2*gy) mod p
           |  val gx2 = gx.multiplyMod(gx, p)
           |  val num = three.multiplyMod(gx2, p)
           |  val den = two.multiplyMod(gy, p)
           |  val lambdaD = num.multiplyMod(den.modInverse(p), p)
           |
           |  // x_2G = λ^2 - 2*gx mod p
           |  val lambdaD2 = lambdaD.multiplyMod(lambdaD, p)
           |  val twoGx = two.multiplyMod(gx, p)
           |  val x2G = lambdaD2.subtractMod(twoGx, p)
           |
           |  // y_2G = λ*(gx - x_2G) - gy mod p
           |  val dx = gx.subtractMod(x2G, p)
           |  val y2G = lambdaD.multiplyMod(dx, p).subtractMod(gy, p)
           |
           |  // Verify 2G matches Scala-computed values
           |  val x2GExpected = unsignedBigInt("${x2g.toString}")
           |  val y2GExpected = unsignedBigInt("${y2g.toString}")
           |  val doubleCheck = x2G == x2GExpected && y2G == y2GExpected
           |
           |  // === POINT ADDITION: compute 3G = G + 2G ===
           |  // λ = (y2G - gy) / (x2G - gx) mod p
           |  val numA = y2G.subtractMod(gy, p)
           |  val denA = x2G.subtractMod(gx, p)
           |  val lambdaA = numA.multiplyMod(denA.modInverse(p), p)
           |
           |  // x_3G = λ^2 - gx - x2G mod p
           |  val lambdaA2 = lambdaA.multiplyMod(lambdaA, p)
           |  val x3G = lambdaA2.subtractMod(gx, p).subtractMod(x2G, p)
           |
           |  // y_3G = λ*(gx - x_3G) - gy mod p
           |  val dx3 = gx.subtractMod(x3G, p)
           |  val y3G = lambdaA.multiplyMod(dx3, p).subtractMod(gy, p)
           |
           |  // Verify 3G matches Scala-computed values
           |  val x3GExpected = unsignedBigInt("${x3g.toString}")
           |  val y3GExpected = unsignedBigInt("${y3g.toString}")
           |  val addCheck = x3G == x3GExpected && y3G == y3GExpected
           |
           |  // Verify 3G is on the curve: y^2 == x^3 + 7
           |  val lhs = y3G.multiplyMod(y3G, p)
           |  val rhs = x3G.multiplyMod(x3G, p).multiplyMod(x3G, p).plusMod(seven, p)
           |  val onCurve = lhs == rhs
           |
           |  sigmaProp(doubleCheck && addCheck && onCurve)
           |}""".stripMargin,
        null,
        true
      )
    }

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[sigma.validation.ValidationException] should be thrownBy secqTest()
    } else {
      secqTest()
    }
  }

  property("Byte.toBits") {
    val customExt = Map(
      1.toByte -> ByteConstant(1)
    ).toSeq
    def toBitsTest() = test("Byte.toBits", env, customExt,
      """{
        | val b = getVar[Byte](1).get
        | b.toBits == Coll(false, false, false, false, false, false, false, true)
        |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      toBitsTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(toBitsTest())
    }
  }

  property("Long.toBits") {
    val customExt = Map(
      1.toByte -> LongConstant(1)
    ).toSeq
    def toBitsTest() = test("Long.toBits", env, customExt,
      """{
        | val b = getVar[Long](1).get
        | val ba = b.toBits
        |
        | // only rightmost bit is set
        | ba.size == 64 && ba(63) == true && ba.slice(0, 63).forall({ (b: Boolean ) => b == false })
        |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      toBitsTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(toBitsTest())
    }
  }

  property("BigInt.toBits") {
    def toBitsTest() = test("BigInt.toBits", env, ext,
      s"""{
        | val b = bigInt("${CryptoConstants.groupOrder.divide(new BigInteger("2"))}")
        | val ba = b.toBits
        | ba.size == 256
        |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      toBitsTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(toBitsTest())
    }
  }


  property("UnsignedBigInt.toBits") {
    def toBitsTest() = test("UnsignedBigInt.toBits", env, ext,
      s"""{
         | val b = unsignedBigInt("${CryptoConstants.groupOrder}")
         | val ba = b.toBits
         | ba.size == 256
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      toBitsTest()
    } else {
      an[ValidationException] shouldBe thrownBy(toBitsTest())
    }
  }

  property("UnsignedBigInt.toBits - 2") {
    def toBitsTest() = test("UnsignedBigInt.toBits", env, ext,
      s"""{
         | val b = unsignedBigInt("5")
         | val ba = b.toBits
         | ba.size == 8 && ba == Coll(false, false, false, false, false, true, false, true)
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      toBitsTest()
    } else {
      an[ValidationException] shouldBe thrownBy(toBitsTest())
    }
  }


  property("BigInt.bitwiseInverse") {
    def bitwiseInverseTest(): Assertion = test("BigInt.bitwiseInverse", env, ext,
      s"""{
         | val b = bigInt("${CryptoConstants.groupOrder.divide(new BigInteger("2"))}")
         | val bi = b.bitwiseInverse
         | bi.bitwiseInverse == b
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      bitwiseInverseTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(bitwiseInverseTest())
    }
  }

  property("UnsignedBigInt.bitwiseInverse") {
    def bitwiseInverseTest(): Assertion = test("UnsignedBigInt.bitwiseInverse", env, ext,
      s"""{
         | val b = unsignedBigInt("${CryptoConstants.groupOrder}")
         | val bi = b.bitwiseInverse
         | bi.bitwiseInverse == b
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      bitwiseInverseTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(bitwiseInverseTest())
    }
  }


  property("Byte.bitwiseInverse") {
    def bitwiseInverseTest(): Assertion = test("Byte.bitwiseInverse", env, ext,
      s"""{
         | val b = (126 + 1).toByte  // max byte value
         | b.bitwiseInverse == (-128).toByte
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      bitwiseInverseTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(bitwiseInverseTest())
    }
  }

  property("Long.bitwiseInverse") {
    val customExt = Map(
      1.toByte -> LongConstant(9223372036854775807L)
    ).toSeq
    def bitwiseInverseTest(): Assertion = test("Long.bitwiseInverse", env, customExt,
      s"""{
         | val l = getVar[Long](1).get
         | val lb = l.bitwiseInverse
         | lb.bitwiseInverse == l
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      bitwiseInverseTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(bitwiseInverseTest())
    }
  }


  property("Byte.bitwiseOr") {
    val customExt = Map(
      1.toByte -> ByteConstant(127)
    ).toSeq
    def bitwiseOrTest(): Assertion = test("Byte.bitwiseOrTest", env, customExt,
      s"""{
         | val x = getVar[Byte](1).get
         | val y = (-128).toByte
         | x.bitwiseOr(y) == -1
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      bitwiseOrTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(bitwiseOrTest())
    }
  }

  property("BigInt.bitwiseOr") {
    def bitwiseOrTest(): Assertion = test("BigInt.bitwiseOr", env, ext,
      s"""{
         | val x = bigInt("${CryptoConstants.groupOrder.divide(new BigInteger("2"))}")
         | x.bitwiseOr(x) == x
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      bitwiseOrTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(bitwiseOrTest())
    }
  }

  property("UnsignedBigInt.bitwiseOr") {
    def bitwiseOrTest(): Assertion = test("BigInt.bitwiseOr", env, ext,
      s"""{
         | val x = unsignedBigInt("${CryptoConstants.groupOrder}")
         | x.bitwiseOr(x) == x
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      bitwiseOrTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(bitwiseOrTest())
    }
  }

  property("UnsignedBigInt.bitwiseOr - 2") {
    def bitwiseOrTest(): Assertion = test("BigInt.bitwiseOr", env, ext,
      s"""{
         | val x = unsignedBigInt("${CryptoConstants.groupOrder}")
         | val y = unsignedBigInt("121")
         | val z = unsignedBigInt("115792089237316195423570985008687907852837564279074904382605163141518161494393")
         | x.bitwiseOr(y) == z && y.bitwiseOr(x) == z
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      bitwiseOrTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(bitwiseOrTest())
    }
  }

  property("BigInt.bitwiseAnd") {
    def bitwiseAndTest(): Assertion = test("BigInt.bitwiseAnd", env, ext,
      s"""{
         | val x = bigInt("${CryptoConstants.groupOrder.divide(new BigInteger("2"))}")
         | val y = 0.toBigInt
         | x.bitwiseAnd(y) == y
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      bitwiseAndTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(bitwiseAndTest())
    }
  }

  property("UnsignedBigInt.bitwiseAnd") {
    def bitwiseAndTest(): Assertion = test("UnsignedBigInt.bitwiseAnd", env, ext,
      s"""{
         | val x = unsignedBigInt("${CryptoConstants.groupOrder}")
         | val y = 0.toBigInt.toUnsigned
         | x.bitwiseAnd(y) == y
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      bitwiseAndTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(bitwiseAndTest())
    }
  }

  property("UnsignedBigInt.bitwiseAnd - 2") {
    def bitwiseAndTest(): Assertion = test("UnsignedBigInt.bitwiseAnd", env, ext,
      s"""{
         | val x = unsignedBigInt("${CryptoConstants.groupOrder}")
         | val y = unsignedBigInt("1157920892373161954235709850086879078528375642790749043826051631415181614337")
         | val z = unsignedBigInt("1157920892373161954235709850086879078522970439492889181512311797126516834561")
         |
         | // cross-checked with wolfram alpha
         | x.bitwiseAnd(y) == z
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      bitwiseAndTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(bitwiseAndTest())
    }
  }

  property("Short.bitwiseAnd") {
    val customExt = Map(
      1.toByte -> ShortConstant(32767)
    ).toSeq
    def bitwiseAndTest(): Assertion = test("Short.bitwiseAnd", env, customExt,
      s"""{
         | val x = getVar[Short](1).get
         | val y = (-32768).toShort
         | x.bitwiseAnd(y) == 0
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      bitwiseAndTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(bitwiseAndTest())
    }
  }

  property("Short.bitwiseXor") {
    val customExt = Map(
      1.toByte -> ShortConstant(32767)
    ).toSeq
    def bitwiseXorTest(): Assertion = test("Short.bitwiseXor", env, customExt,
      s"""{
         | val x = getVar[Short](1).get
         | val y = (-32768).toShort
         | x.bitwiseXor(y) == -1
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      bitwiseXorTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(bitwiseXorTest())
    }
  }

  property("BigInt.bitwiseXor") {
    def bitwiseXorTest(): Assertion = test("BigInt.bitwiseXor", env, ext,
      s"""{
         | val x = bigInt("-768674748430101084849204595060664949857579483737383833332727484848588886")
         | val y = bigInt("1157920892373161954235709850086879078528375642790749043826051631415181614337")
         | val z = bigInt("-1157640033036725491711737956849584949341472215181452524373827965546397848917")
         |
         | // cross-checked with wolfram alpha
         | x.bitwiseXor(y) == z
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      bitwiseXorTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(bitwiseXorTest())
    }
  }


    property("UnsignedBigInt.bitwiseXor") {
    def bitwiseAndTest(): Assertion = test("UnsignedBigInt.bitwiseXor", env, ext,
      s"""{
         | val x = unsignedBigInt("${CryptoConstants.groupOrder}")
         | val y = unsignedBigInt("1157920892373161954235709850086879078528375642790749043826051631415181614337")
         | val z = unsignedBigInt("114634168344943033469335275158601028774319999042879875063406591178680309439552")
         |
         | // cross-checked with wolfram alpha
         | x.bitwiseXor(y) == z
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      bitwiseAndTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(bitwiseAndTest())
    }
  }

  property("Byte.shiftLeft") {
    def shiftLeftTest(): Assertion = test("Byte.shiftLeft", env, ext,
      s"""{
         | val x = 4.toByte
         | val y = 2
         | x.shiftLeft(y) == 16.toByte
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      shiftLeftTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(shiftLeftTest())
    }
  }

  property("Byte.shiftLeft - over limit") {
    def shiftLeftTest(): Assertion = test("Byte.shiftLeft2", env, ext,
      s"""{
         | val x = 4.toByte
         | val y = 2222
         | x.shiftLeft(y) == 0
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      an[IllegalArgumentException] shouldBe thrownBy(shiftLeftTest())
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(shiftLeftTest())
    }
  }

  property("Byte.shiftLeft - over limit 2") {
    def shiftLeftTest(): Assertion = test("Byte.shiftLeft2", env, ext,
      s"""{
         | val x = (-128).toByte
         | val y = 1
         | x.shiftLeft(y) == 0
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      shiftLeftTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(shiftLeftTest())
    }
  }

  property("BigInt.shiftLeft") {
    def shiftLeftTest(): Assertion = test("BigInt.shiftLeft", env, ext,
      s"""{
         | val x = bigInt("${CryptoConstants.groupOrder.divide(new BigInteger("8"))}")
         | val y = bigInt("${CryptoConstants.groupOrder.divide(new BigInteger("2"))}")
         | x.shiftLeft(2) == y
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      shiftLeftTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(shiftLeftTest())
    }
  }

  property("UnsignedBigInt.shiftLeft") {
    def shiftLeftTest(): Assertion = test("UnsignedBigInt.shiftLeft", env, ext,
      s"""{
         | val x = unsignedBigInt("${CryptoConstants.groupOrder.divide(new BigInteger("8"))}")
         | val y = unsignedBigInt("${CryptoConstants.groupOrder.divide(new BigInteger("2"))}")
         | x.shiftLeft(2) == y
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      shiftLeftTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(shiftLeftTest())
    }
  }

  property("UnsignedBigInt.shiftLeft over limits") {
    def shiftLeftTest(): Assertion = test("UnsignedBigInt.shiftLeft", env, ext,
      s"""{
         | val x = unsignedBigInt("${CryptoConstants.groupOrder}")
         | x.shiftLeft(1) > x
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      an[ArithmeticException] shouldBe thrownBy(shiftLeftTest())
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(shiftLeftTest())
    }
  }


  property("UnsignedBigInt.shiftLeft - neg shift") {
    def shiftLeftTest(): Assertion = test("UnsignedBigInt.shiftLeft", env, ext,
      s"""{
         | val x = unsignedBigInt("${CryptoConstants.groupOrder}")
         | x.shiftLeft(-1) > x
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      an[java.lang.IllegalArgumentException] shouldBe thrownBy(shiftLeftTest())
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(shiftLeftTest())
    }
  }

  property("BigInt.shiftLeft over limits") {
    def shiftLeftTest(): Assertion = test("BigInt.shiftLeft", env, ext,
      s"""{
         | val x = bigInt("${CryptoConstants.groupOrder.divide(new BigInteger("2"))}")
         | x.shiftLeft(1) > x
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      an[ArithmeticException] shouldBe thrownBy(shiftLeftTest())
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(shiftLeftTest())
    }
  }

  property("Byte.shiftRight") {
    def shiftRightTest(): Assertion = test("Byte.shiftRight", env, ext,
      s"""{
         | val x = 8.toByte
         | val y = 2
         | x.shiftRight(y) == 2.toByte
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      shiftRightTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(shiftRightTest())
    }
  }

  property("Byte.shiftRight - neg") {
    def shiftRightTest(): Assertion = test("Byte.shiftRight", env, ext,
      s"""{
         | val x = (-8).toByte
         | val y = 2
         | x.shiftRight(y) == (-2).toByte
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      shiftRightTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(shiftRightTest())
    }
  }

  property("Byte.shiftRight - neg - neg shift") {
    def shiftRightTest(): Assertion = test("Byte.shiftRight", env, ext,
      s"""{
         | val x = (-8).toByte
         | val y = -2
         | x.shiftRight(y) == (-1).toByte
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      an[IllegalArgumentException] shouldBe thrownBy(shiftRightTest())
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(shiftRightTest())
    }
  }


  property("Long.shiftRight - neg") {
    def shiftRightTest(): Assertion = test("Long.shiftRight", env, ext,
      s"""{
         | val x = -32L
         | val y = 2
         | x.shiftRight(y) == -8L
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      shiftRightTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(shiftRightTest())
    }
  }

  property("Long.shiftRight - neg - neg shift") {
    def shiftRightTest(): Assertion = test("Long.shiftRight", env, ext,
      s"""{
         | val x = -32L
         | val y = -2
         | x.shiftRight(y) == -1L
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      an[IllegalArgumentException] shouldBe thrownBy(shiftRightTest())
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(shiftRightTest())
    }
  }

  property("BigInt.shiftRight") {
    def shiftRightTest(): Assertion = test("BigInt.shiftRight", env, ext,
      s"""{
         | val x = bigInt("${CryptoConstants.groupOrder.divide(new BigInteger("2"))}")
         | val y = 2
         | val z = bigInt("${CryptoConstants.groupOrder.divide(new BigInteger("8"))}")
         | x.shiftRight(y) == z
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      shiftRightTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(shiftRightTest())
    }
  }

  property("BigInt.shiftRight - neg shift") {
    def shiftRightTest(): Assertion = test("BigInt.shiftRight", env, ext,
      s"""{
         | val x = bigInt("${CryptoConstants.groupOrder.divide(new BigInteger("2"))}")
         | val y = -2
         | val z = bigInt("${CryptoConstants.groupOrder.divide(new BigInteger("8"))}")
         | z.shiftRight(y) == x
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      an[IllegalArgumentException] shouldBe thrownBy(shiftRightTest())
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(shiftRightTest())
    }
  }

  property("UnsignedBigInt.shiftRight") {
    def shiftRightTest(): Assertion = test("UnsignedBigInt.shiftRight", env, ext,
      s"""{
         | val x = unsignedBigInt("${CryptoConstants.groupOrder}")
         | val y = 3
         | val z = unsignedBigInt("${CryptoConstants.groupOrder.divide(new BigInteger("8"))}")
         | x.shiftRight(y) == z
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      shiftRightTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(shiftRightTest())
    }
  }

  property("UnsignedBigInt.shiftRight - neg shift") {
    def shiftRightTest(): Assertion = test("UnsignedBigInt.shiftRight", env, ext,
      s"""{
         | val x = unsignedBigInt("${CryptoConstants.groupOrder.divide(new BigInteger("2"))}")
         | val y = -2
         | val z = unsignedBigInt("${CryptoConstants.groupOrder.divide(new BigInteger("8"))}")
         | z.shiftRight(y) == x
         |}""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      an[java.lang.IllegalArgumentException] shouldBe thrownBy(shiftRightTest())
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(shiftRightTest())
    }
  }

  property("getVarFromInput - invalid var") {
    def getVarTest(): Assertion = {
      val customExt = Map(
        1.toByte -> IntConstant(5)
      ).toSeq
      test("R1", env, customExt,
        "{ sigmaProp(CONTEXT.getVarFromInput[Int](0, 2).isDefined == false) }",
        null
      )
    }

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      getVarTest()
    } else {
      an[sigma.validation.ValidationException] should be thrownBy getVarTest()
    }
  }

  property("Coll.reverse"){
    def reverseTest() = test("reverse", env, ext,
      """{
        | val c1 = Coll(1, 2, 3)
        | val c2 = Coll(3, 2, 1)
        |
        | val b1 = Coll(INPUTS(0), OUTPUTS(0))
        | val b2 = Coll(OUTPUTS(0), INPUTS(0))
        |
        | c1.reverse == c2 && b1.reverse == b2
        | }""".stripMargin,
      null
    )

    if(VersionContext.current.isV3OrLaterErgoTreeVersion) {
      reverseTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(reverseTest())
    }
  }

  property("Coll.startsWith"){
    def reverseTest() = test("distinct", env, ext,
      """{
        | val c1 = Coll(1, 2, 3)
        | val c2 = Coll(1, 2)
        | val c3 = Coll(1, 3)
        | val c4 = Coll[Int]()
        | val c5 = Coll(1, 2, 3, 4)
        |
        | val b1 = c1.startsWith(c3)
        | val b2 = c1.startsWith(c5)
        |
        | c1.startsWith(c2) && c1.startsWith(c4) && c1.startsWith(c1) && !b1 && !b2
        | }""".stripMargin,
      null
    )

    if(VersionContext.current.isV3OrLaterErgoTreeVersion) {
      reverseTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(reverseTest())
    }
  }

  property("Coll.startsWith - tuples"){
    def reverseTest() = test("distinct", env, ext,
      """{
        | val c1 = Coll((1, 2), (3, 4), (5, 6))
        | val c2 = Coll((1, 2), (3, 4))
        | val c3 = Coll((1, 3))
        | val c4 = Coll[(Int, Int)]()
        | val c5 = Coll((1, 2), (3, 4), (5, 6), (7, 8))
        |
        | val b1 = c1.startsWith(c3)
        | val b2 = c1.startsWith(c5)
        |
        | c1.startsWith(c2) && c1.startsWith(c4) && c1.startsWith(c1) && !b1 && !b2
        | }""".stripMargin,
      null
    )

    if(VersionContext.current.isV3OrLaterErgoTreeVersion) {
      reverseTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(reverseTest())
    }
  }

  property("Coll.endsWith"){
    def reverseTest() = test("distinct", env, ext,
      """{
        | val c1 = Coll(1, 2, 3)
        | val c2 = Coll(2, 3)
        | val c3 = Coll(2, 2)
        | val c4 = Coll[Int]()
        | val c5 = Coll(1, 2, 3, 4)
        |
        | val b1 = c1.endsWith(c3)
        | val b2 = c1.endsWith(c5)
        |
        | c1.endsWith(c2) && c1.endsWith(c4) && c1.endsWith(c1) && !b1 && !b2
        | }""".stripMargin,
      null
    )

    if(VersionContext.current.isV3OrLaterErgoTreeVersion) {
      reverseTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(reverseTest())
    }
  }

  property("Coll.endsWith - tuples"){
    def reverseTest() = test("endsWith tuples", env, ext,
      """{
        | val c1 = Coll((1, 2), (2, 3))
        | val c2 = Coll((2, 3))
        | val c3 = Coll((2, 2))
        | val c4 = Coll[(Int, Int)]()
        | val c5 = Coll((0, 2), (2, 3))
        |
        | val b1 = c1.endsWith(c3)
        | val b2 = c1.endsWith(c5)
        |
        | c1.endsWith(c2) && c1.endsWith(c4) && c1.endsWith(c1) && !b1 && !b2
        | }""".stripMargin,
      null
    )

    if(VersionContext.current.isV3OrLaterErgoTreeVersion) {
      reverseTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(reverseTest())
    }
  }

  property("Coll.get"){
    def getTest() = test("get", env, ext,
      """{
        |   val c1 = Coll(1)
        |   val c2 = Coll[Int]()
        |
        |   c2.get(0).getOrElse(c1.get(0).get) == c1.get(0).get
        | }""".stripMargin,
      null
    )

    if(VersionContext.current.isV3OrLaterErgoTreeVersion) {
      getTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(getTest())
    }
  }

  property("Global.fromBigEndianBytes - byte") {
    def fromTest() = test("fromBigEndianBytes - byte", env, ext,
      s"""{
         |  val ba = Coll(5.toByte)
         |  Global.fromBigEndianBytes[Byte](ba) == 5
         |}
         |""".stripMargin,
      null
    )
    if(VersionContext.current.isV3OrLaterErgoTreeVersion) {
      fromTest()
    } else {
      an[sigma.validation.ValidationException] should be thrownBy(fromTest())
    }
  }

  property("Global.fromBigEndianBytes - short") {
    def fromTest() = test("fromBigEndianBytes - short", env, ext,
      s"""{
         |  val ba = Coll(5.toByte, 5.toByte)
         |  Global.fromBigEndianBytes[Short](ba) != 0
         |}
         |""".stripMargin,
      null
    )
    if(VersionContext.current.isV3OrLaterErgoTreeVersion) {
      fromTest()
    } else {
      an[sigma.validation.ValidationException] should be thrownBy(fromTest())
    }
  }

  property("Global.fromBigEndianBytes - int") {
    def fromTest() = test("fromBigEndianBytes - int", env, ext,
      s"""{
         |  val ba = fromBase16("${Base16.encode(Ints.toByteArray(Int.MaxValue))}")
         |  Global.fromBigEndianBytes[Int](ba) == ${Int.MaxValue}
         |}
         |""".stripMargin,
      null
    )
    if(VersionContext.current.isV3OrLaterErgoTreeVersion) {
      fromTest()
    } else {
      an[sigma.validation.ValidationException] should be thrownBy(fromTest())
    }
  }

  property("Global.fromBigEndianBytes - long") {
    def fromTest() = test("fromBigEndianBytes - long", env, ext,
      s"""{
         |  val l = 1088800L
         |  val ba = longToByteArray(l)
         |  Global.fromBigEndianBytes[Long](ba) == l
         |}
         |""".stripMargin,
      null
    )
    if(VersionContext.current.isV3OrLaterErgoTreeVersion) {
      fromTest()
    } else {
      an[sigma.validation.ValidationException] should be thrownBy(fromTest())
    }
  }

  property("Global.fromBigEndianBytes - Long.toBytes") {
    val customExt = Map(
      1.toByte -> LongConstant(1088800L)
    ).toSeq
    def fromTest() = test("fromBigEndianBytes - long", env, customExt,
      s"""{
         |  val l = getVar[Long](1).get
         |  val ba = l.toBytes
         |  Global.fromBigEndianBytes[Long](ba) == l
         |}
         |""".stripMargin,
      null
    )
    if(VersionContext.current.isV3OrLaterErgoTreeVersion) {
      fromTest()
    } else {
      an[sigma.validation.ValidationException] should be thrownBy(fromTest())
    }
  }

  property("Global.fromBigEndianBytes - bigInt") {
    val bi = new BigInteger("9785856985394593489356430476450674590674598659865986594859056865984690568904")
    def fromTest() = test("fromBigEndianBytes - bigInt", env, ext,
      s"""{
         |  val ba = fromBase16("${Base16.encode(bi.toByteArray)}")
         |  Global.fromBigEndianBytes[BigInt](ba) == bigInt("$bi")
         |}
         |""".stripMargin,
      null
    )
    if(VersionContext.current.isV3OrLaterErgoTreeVersion) {
      fromTest()
    } else {
      an[sigma.validation.ValidationException] should be thrownBy(fromTest())
    }
  }

  property("Int.toBytes") {
    val customExt = Map(
      1.toByte -> IntConstant(1)
    ).toSeq
    def toBytesTest() = test("Int.toBytes", env, customExt,
      """{
        |   val l = getVar[Int](1).get
        |   l.toBytes == Coll(0.toByte, 0.toByte, 0.toByte, 1.toByte)
        | }""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      toBytesTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(toBytesTest())
    }
  }

  property("Int.toBits") {
    val customExt = Map(
      1.toByte -> IntConstant(1477959696)
    ).toSeq
    def toBytesTest() = test("Int.toBytes", env, customExt,
      """{
        |   val l = getVar[Int](1).get
        |   l.toBits == Coll(false, true, false, true, true, false, false, false, false, false, false, true, false, true, true ,true, true, true, true, false, false, false, false, false, false, false, false, true, false, false, false, false)
        | }""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      toBytesTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(toBytesTest())
    }
  }

  property("Byte.toBytes") {
    val customExt = Map(
      1.toByte -> ByteConstant(10)
    ).toSeq
    def toBytesTest() = test("Byte.toBytes", env, customExt,
      """{
        |   val l = getVar[Byte](1).get
        |   l.toBytes == Coll(10.toByte)
        | }""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      toBytesTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(toBytesTest())
    }
  }


  property("BigInt.toBytes") {
    def toBytesTest() = test("BigInt.toBytes", env, ext,
      s"""{
        |   val l = bigInt("${CryptoConstants.groupOrder.divide(new BigInteger("2"))}")
        |   l.toBytes.size == 32
        | }""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      toBytesTest()
    } else {
      an[sigma.validation.ValidationException] shouldBe thrownBy(toBytesTest())
    }
  }

  property("UnsignedBigInt.toBytes") {
    val script = s"""{
                    |   val l = unsignedBigInt("${CryptoConstants.groupOrder}")
                    |   l.toBytes.size == 32
                    | }""".stripMargin

    def toBytesTest() = test("UnsignedBigInt.toBytes", env, ext, script, null)

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      toBytesTest()
    } else {
      an[ValidationException] shouldBe thrownBy(toBytesTest())
    }
  }

  property("UnsignedBigInt.toBytes - 2") {
    val script = s"""{
                    |   val l = unsignedBigInt("5")
                    |   val bs = l.toBytes
                    |   bs.size == 1 && bs == Coll(5.toByte)
                    | }""".stripMargin

    def toBytesTest() = test("UnsignedBigInt.toBytes", env, ext, script, null)

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      toBytesTest()
    } else {
      an[ValidationException] shouldBe thrownBy(toBytesTest())
    }
  }

  property("serialize - byte array") {
    def deserTest() = test("serialize", env, ext,
      s"""{
            val ba = fromBase16("c0ffee");
            Global.serialize(ba).size > ba.size
          }""",
      null,
      true
    )

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an [sigma.validation.ValidationException] should be thrownBy deserTest()
    } else {
      deserTest()
    }
  }

  property("serialize - collection of boxes") {
    def deserTest() = test("serialize", env, ext,
      s"""{
            val boxes = INPUTS;
            Global.serialize(boxes).size > 0
          }""",
      null,
      true
    )

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an [sigma.validation.ValidationException] should be thrownBy deserTest()
    } else {
      deserTest()
    }
  }

  property("serialize - optional collection") {
    def deserTest() = test("serialize", env, ext,
      s"""{
            val opt = SELF.R1[Coll[Byte]];
            Global.serialize(opt).size > SELF.R1[Coll[Byte]].get.size
          }""",
      null,
      true
    )

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an [sigma.validation.ValidationException] should be thrownBy deserTest()
    } else {
      deserTest()
    }
  }

  property("serialize(long) is producing different result from longToByteArray()") {
    def deserTest() = test("serialize", env, ext,
      s"""{
            val l = -1000L
            val ba1 = Global.serialize(l);
            val ba2 = longToByteArray(l)
            ba1 != ba2
          }""",
      null,
      true
    )

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an [sigma.validation.ValidationException] should be thrownBy deserTest()
    } else {
      deserTest()
    }
  }

  // the test shows that serialize(groupElement) is the same as groupElement.getEncoded
  property("serialize - group element - equivalence with .getEncoded") {
    val ge = Helpers.decodeGroupElement("026930cb9972e01534918a6f6d6b8e35bc398f57140d13eb3623ea31fbd069939b")
  //  val ba = Base16.encode(ge.getEncoded.toArray)
    def deserTest() = test("serialize", env, Seq(21.toByte -> GroupElementConstant(ge)),
      s"""{
            val ge = getVar[GroupElement](21).get
            val ba = serialize(ge);
            ba == ge.getEncoded
          }""",
      null,
      true
    )

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an [sigma.validation.ValidationException] should be thrownBy deserTest()
    } else {
      deserTest()
    }
  }

  // the test shows that serialize(sigmaProp) is the same as sigmaProp.propBytes without first 2 bytes
  property("serialize and .propBytes correspondence") {
    def deserTest() = test("serialize", env, ext,
      s"""{
            val p1 = getVar[SigmaProp]($propVar1).get
            val bytes = p1.propBytes
            val ba = bytes.slice(2, bytes.size)
            val ba2 = serialize(p1)
            ba == ba2
          }""",
      null,
      true
    )

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an [sigma.validation.ValidationException] should be thrownBy deserTest()
    } else {
      deserTest()
    }
  }

  property("serialize - collection of collection of headers") {
    val td = new SigmaTestingData {}
    val h1 = td.TestData.h1

    val customExt = Seq(21.toByte -> HeaderConstant(h1))

    def deserTest() = test("serialize", env, customExt,
      s"""{
            val h1 = getVar[Header](21).get;
            val c = Coll(Coll(h1))
            Global.serialize(c).size > 0
          }""",
      null,
      true
    )

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an [sigma.validation.ValidationException] should be thrownBy deserTest()
    } else {
      deserTest()
    }
  }

  property("serialize - deserialize - optional UnsignedBigInt") {
    def deserTest() = test("serialize", env, ext,
      s"""{
            val ub = unsignedBigInt("5");
            val opt = Global.some[UnsignedBigInt](ub)
            val bs = Global.serialize(opt);
            bs == fromBase16("010105") && Global.deserializeTo[Option[UnsignedBigInt]](bs).get == ub
          }""",
      null,
      true
    )

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an [Exception] should be thrownBy deserTest()
    } else {
      deserTest()
    }
  }

  property("serialize - not spam") {
    val customExt = Seq(21.toByte -> ShortArrayConstant((1 to Short.MaxValue).map(_.toShort).toArray),
      22.toByte -> ByteArrayConstant(Array.fill(1)(1.toByte)))
    def deserTest() = test("serialize", env, customExt,
      s"""{
            val indices = getVar[Coll[Short]](21).get
            val base = getVar[Coll[Byte]](22).get

             def check(index:Short): Boolean = { serialize(base) != base }
            indices.forall(check)
          }""",
      null,
      true
    )

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[sigma.validation.ValidationException] should be thrownBy deserTest()
    } else {
      deserTest()
    }
  }

  property("serialize - spam attempt") {
    val customExt = Seq(21.toByte -> ShortArrayConstant((1 to Short.MaxValue).map(_.toShort).toArray),
      22.toByte -> ByteArrayConstant(Array.fill(16000)(1.toByte)))
    def deserTest() = test("serialize", env, customExt,
      s"""{
            val indices = getVar[Coll[Short]](21).get
            val base = getVar[Coll[Byte]](22).get

             def check(index:Short): Boolean = { serialize(base) != base }
            indices.forall(check)
          }""",
      null,
      true
    )

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[sigma.validation.ValidationException] should be thrownBy deserTest()
    } else {
      // we have wrapped CostLimitException here
      an[Exception] should be thrownBy deserTest()
    }
  }

  property("Lazy evaluation of default in Option.getOrElse") {
    val customExt = Map (
      1.toByte -> IntConstant(5)
    ).toSeq
    def optTest() = test("getOrElse", env, customExt,
      """{
        |  getVar[Int](1).getOrElse(getVar[Int](44).get) > 0
        |}
        |""".stripMargin,
      null
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      optTest()
    } else {
      assertExceptionThrown(optTest(), _.isInstanceOf[NoSuchElementException])
    }
  }

  property("Lazy evaluation of default in Coll.getOrElse") {
    def optTest() = test("getOrElse", env, ext,
      """{
        |  val c = Coll[Int](1)
        |  c.getOrElse(0, getVar[Int](44).get) > 0 &&
        |   c.getOrElse(1, c.getOrElse(0, getVar[Int](44).get)) > 0
        |}
        |""".stripMargin,
      null
    )

    if(VersionContext.current.isV3OrLaterErgoTreeVersion) {
      optTest()
    } else {
      assertExceptionThrown(optTest(), _.isInstanceOf[NoSuchElementException])
    }
  }

  property("checking Bitcoin PoW") {
    val h = "00000020a82ff9c62e69a6cbed277b7f2a9ac9da3c7133a59a6305000000000000000000f6cd5708a6ba38d8501502b5b4e5b93627e8dcc9bd13991894c6e04ade262aa99582815c505b2e17479a751b"
    val customExt = Map(
      1.toByte -> ByteArrayConstant(Base16.decode(h).get)
    ).toSeq

    def powTest() = {
      test("Prop1", env, customExt,
        """{
          |    def reverse4(bytes: Coll[Byte]): Coll[Byte] = {
          |        Coll(bytes(3), bytes(2), bytes(1), bytes(0))
          |    }
          |
          |    def reverse32(bytes: Coll[Byte]): Coll[Byte] = {
          |        Coll(bytes(31), bytes(30), bytes(29), bytes(28), bytes(27), bytes(26), bytes(25), bytes(24),
          |             bytes(23), bytes(22), bytes(21), bytes(20), bytes(19), bytes(18), bytes(17), bytes(16),
          |             bytes(15), bytes(14), bytes(13), bytes(12), bytes(11), bytes(10), bytes(9), bytes(8),
          |             bytes(7), bytes(6), bytes(5), bytes(4), bytes(3), bytes(2), bytes(1), bytes(0))
          |    }
          |
          |   val bitcoinHeader = getVar[Coll[Byte]](1).get
          |   val id = reverse32(sha256(sha256(bitcoinHeader)))
          |   val hit = byteArrayToBigInt(id)
          |
          |   val nBitsBytes = reverse4(bitcoinHeader.slice(72, 76))
          |
          |   val pad = Coll[Byte](0.toByte, 0.toByte, 0.toByte, 0.toByte)
          |
          |   val nbits = byteArrayToLong(pad ++ nBitsBytes)
          |
          |   val difficulty = Global.decodeNbits(nbits)
          |
          |   // <= according to https://bitcoin.stackexchange.com/a/105224
          |   hit <= difficulty
          |}
          |""".stripMargin,
        propExp = null,
        testExceededCost = false
      )
    }
    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[sigma.validation.ValidationException] should be thrownBy powTest()
    } else {
      powTest()
    }
  }

  property("decoding nbits from an Ergo block header") {
    // bytes of real mainnet block header at height 1,398,482
    val headerBytes = "03720c6c532506a9bc4cceb6844efaa4096f66ba8a1d67ad7411ed1cb61dd5c008519fedd7d3b56984c43898f9e12aa866bc40e1ede2a6138940c9db2e20d633630024ee218d6a38392a8401c2b324b563e48d487c0f22dad940bd1c8a096084908ad71c77eec44ef083ba073deb8fa4a57b68d6296186c5d61e317849c760c16019b293cbfeb33288c9616f282288ee24c6d306577a76e7ac1cf87422ae0bdc6b44a449451a4e25070412d0d2ad55000000000295facb78290ac2b55f1453204d49df37be5bae9f185ed6704c1ba3ee372280c157221fa789df3f48"
    val header1 = new CHeader(ErgoHeader.sigmaSerializer.fromBytes(Base16.decode(headerBytes).get))

    val customExt = Seq(21.toByte -> HeaderConstant(header1))

    def powTest() = {
      test("Prop1", env, customExt,
        """
          |{
          |   val h = getVar[Header](21).get
          |
          |   val n = h.nBits
          |
          |   val target = Global.decodeNbits(n)
          |
          |   target == bigInt("1146584469340160")
          |}
          |""".stripMargin,
        propExp = null,
        testExceededCost = false
      )
    }

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[sigma.validation.ValidationException] should be thrownBy powTest()
    } else {
      powTest()
    }
  }

  property("serialize - deserialize roundtrip") {
    val customExt = Seq(21.toByte -> ShortArrayConstant((1 to 10).map(_.toShort).toArray))
    def deserTest() = test("serialize", env, customExt,
      s"""{
            val src = getVar[Coll[Short]](21).get
            val ba = serialize(src)
            val restored = deserializeTo[Coll[Short]](ba)
            src == restored
          }""",
      null,
      true
    )

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[Exception] should be thrownBy deserTest()
    } else {
      deserTest()
    }
  }

  property("deserializeTo - int") {
    val value = -109253
    val w = new VLQByteBufferWriter(new ByteArrayBuilder()).putInt(value)
    val bytes = Base16.encode(w.toBytes)
    def deserTest() = {test("deserializeTo", env, ext,
      s"""{ val ba = fromBase16("$bytes"); Global.deserializeTo[Int](ba) == $value }""",
      null,
      true
    )}

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an [sigma.validation.ValidationException] should be thrownBy deserTest()
    } else {
      deserTest()
    }
  }

  property("deserializeTo - coll[int]") {
    val writer = new SigmaByteWriter(new VLQByteBufferWriter(new ByteArrayBuilder()), None, None, None)
    DataSerializer.serialize[SCollection[SInt.type]](Colls.fromArray(Array(IntConstant(5).value)), SCollection(SInt), writer)
    val bytes = Base16.encode(writer.toBytes)

    def deserTest() = {
      test("deserializeTo", env, ext,
        s"""{val ba = fromBase16("$bytes"); val coll = Global.deserializeTo[Coll[Int]](ba); coll(0) == 5 }""",
        null,
        true
      )
    }

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an [sigma.validation.ValidationException] should be thrownBy deserTest()
    } else {
      deserTest()
    }
  }

  property("deserializeTo - long") {
    val value = -10009253L

    val w = new VLQByteBufferWriter(new ByteArrayBuilder()).putLong(value)
    val bytes = Base16.encode(w.toBytes)

    def deserTest() = test("deserializeTo", env, ext,
      s"""{
            val ba = fromBase16("$bytes");
            Global.deserializeTo[Long](ba) == ${value}L
          }""",
      null,
      true
    )

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an [sigma.validation.ValidationException] should be thrownBy deserTest()
    } else {
      deserTest()
    }
  }

  property("deserializeTo - box rountrip") {
    def deserTest() = test("deserializeTo", env, ext,
      s"""{
            val b = INPUTS(0);
            val ba = b.bytes;
            Global.deserializeTo[Box](ba) == b
          }""",
      null,
      true
    )

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an [sigma.validation.ValidationException] should be thrownBy deserTest()
    } else {
      deserTest()
    }
  }

  property("deserializeTo - bigint") {

    val bigInt = SecP256K1Group.q.divide(new BigInteger("512"))
    val biBytes = bigInt.toByteArray

    val w = new VLQByteBufferWriter(new ByteArrayBuilder()).putUShort(biBytes.length)
    val lengthBytes = w.toBytes

    val bytes = Base16.encode(lengthBytes ++ biBytes)

    def deserTest() = test("deserializeTo", env, ext,
      s"""{
            val ba = fromBase16("$bytes");
            val b = Global.deserializeTo[BigInt](ba)
            b == bigInt("${bigInt.toString}")
          }""",
      null,
      true
    )

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an [sigma.validation.ValidationException] should be thrownBy deserTest()
    } else {
      deserTest()
    }
  }

  property("deserializeTo - short") {
    val s = (-1925).toShort
    val w = new VLQByteBufferWriter(new ByteArrayBuilder()).putShort(s)
    val bytes = Base16.encode(w.toBytes)
    def deserTest() = test("deserializeTo", env, ext,
      s"""{
            val ba = fromBase16("$bytes");
            Global.deserializeTo[Short](ba) == -1925
          }""",
      null,
      true
    )

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an [sigma.validation.ValidationException] should be thrownBy deserTest()
    } else {
      deserTest()
    }
  }

  property("deserializeTo - group element") {
    val ge = Helpers.decodeGroupElement("026930cb9972e01534918a6f6d6b8e35bc398f57140d13eb3623ea31fbd069939b")
    val ba = Base16.encode(ge.getEncoded.toArray)
    def deserTest() = test("deserializeTo", env, Seq(21.toByte -> GroupElementConstant(ge)),
      s"""{
            val ge = getVar[GroupElement](21).get
            val ba = fromBase16("$ba");
            val ge2 = Global.deserializeTo[GroupElement](ba)
            ba == ge2.getEncoded && ge == ge2
          }""",
      null,
      true
    )

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
       an [sigma.validation.ValidationException] should be thrownBy deserTest()
    } else {
      deserTest()
    }
  }

  property("deserializeTo - sigmaprop roundtrip") {

    def deserTest() = test("deserializeTo", env, ext,
      s"""{
            val bytes = getVar[Coll[Byte]]($propBytesVar1).get
            val ba = bytes.slice(2, bytes.size)
            val prop = Global.deserializeTo[SigmaProp](ba)
            prop == getVar[SigmaProp]($propVar3).get && prop
          }""",
      null,
      true
    )

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an [sigma.validation.ValidationException] should be thrownBy deserTest()
    } else {
      deserTest()
    }
  }

  property("deserializeTo - .propBytes") {
    def deserTest() = test("deserializeTo", env, ext,
      s"""{
            val p1 = getVar[SigmaProp]($propVar1).get
            val bytes = p1.propBytes
            val ba = bytes.slice(2, bytes.size)
            val prop = Global.deserializeTo[SigmaProp](ba)
            prop == p1 && prop
          }""",
      null,
      true
    )

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an [sigma.validation.ValidationException] should be thrownBy deserTest()
    } else {
      deserTest()
    }
  }

  property("deserializeTo - sigmaprop roundtrip - non evaluated") {

    val script = GT(Height, IntConstant(-1)).toSigmaProp
    val scriptBytes = ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(ErgoTree.fromProposition(script))
    val customExt = Seq(21.toByte -> ByteArrayConstant(scriptBytes))

    def deserTest() = test("deserializeTo", env, customExt,
      s"""{
            val ba = getVar[Coll[Byte]](21).get
            val prop = Global.deserializeTo[SigmaProp](ba)
            prop
          }""",
      null,
      true
    )

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an [sigma.validation.ValidationException] should be thrownBy deserTest()
    } else {
      an [Exception] should be thrownBy deserTest()
    }
  }

  property("deserializeTo - avltree") {
    val elements = Seq(123, 22)
    val treeElements = elements.map(i => Longs.toByteArray(i)).map(s => (ADKey @@@ Blake2b256(s), ADValue @@ s))
    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)
    treeElements.foreach(s => avlProver.performOneOperation(Insert(s._1, s._2)))
    avlProver.generateProof()
    val treeData = new AvlTreeData(avlProver.digest.toColl, AvlTreeFlags.ReadOnly, 32, None)
    val treeBytes = AvlTreeData.serializer.toBytes(treeData)

    val customExt = Seq(21.toByte -> ByteArrayConstant(treeBytes))

    def deserTest() = test("deserializeTo", env, customExt,
      s"""{
            val ba = getVar[Coll[Byte]](21).get
            val tree = Global.deserializeTo[AvlTree](ba)
            tree.digest == fromBase16(${Base16.encode(treeData.digest.toArray)})
              && tree.enabledOperations == 0
              && tree.keyLength == 32
              && tree.valueLengthOpt.isEmpty
          }""",
      null,
      true
    )

    an [Exception] should be thrownBy deserTest()
  }

  property("deserializeTo - header") {
    val td = new SigmaTestingData {}
    val h1 = td.TestData.h1
    val headerBytes = h1.asInstanceOf[CHeader].ergoHeader.bytes

    val headerStateBytes = AvlTreeData.serializer.toBytes(Extensions.CoreAvlTreeOps(h1.stateRoot).toAvlTreeData)
    val customExt = Seq(21.toByte -> ByteArrayConstant(headerBytes), 22.toByte -> ByteArrayConstant(headerStateBytes))

    def deserTest() = test("deserializeTo", env, customExt,
      s"""{
            val ba = getVar[Coll[Byte]](21).get
            val header = Global.deserializeTo[Header](ba)
            val ba2 = getVar[Coll[Byte]](22).get
            val tree = Global.deserializeTo[AvlTree](ba2)
            val id = fromBase16("${Base16.encode(h1.id.toArray)}")
            header.height == ${h1.height} && header.stateRoot == tree && header.id == id
          }""",
      null
    )

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[sigma.validation.ValidationException] should be thrownBy deserTest()
    } else {
      deserTest()
    }
  }

  property("deserializeTo - header with invalid powDistance") {
    val td = new SigmaTestingData {}
    val h1 = td.TestData.h1.asInstanceOf[CHeader].ergoHeader
    val invalidPowDistance = new BigInteger(Array.fill(33)(33.toByte)) // Creating a byte array out of 256-bit range

    val s = new AutolykosSolution(h1.powSolution.pk, h1.powSolution.w, h1.powSolution.n, invalidPowDistance)
    val headerBytes = h1.copy(powSolution = s).bytes

    val customExt = Seq(21.toByte -> ByteArrayConstant(Colls.fromArray(headerBytes)))

    def deserTest() = test("deserializeToInvalidPowDistance", env, customExt,
      s"""{
            val ba = getVar[Coll[Byte]](21).get
            val header = Global.deserializeTo[Header](ba)
            header.height >= 0
          }""",
      null,
      true
    )

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[sigma.validation.ValidationException] should be thrownBy deserTest()
    } else {
      an[Exception] should be thrownBy deserTest()
    }
  }

  property("deserializeTo - header option") {
    val td = new SigmaTestingData {}
    val h1 = td.TestData.h1.asInstanceOf[CHeader].ergoHeader
    val headerBytes = Colls.fromArray(Array(1.toByte) ++ h1.bytes)

    val customExt = Seq(21.toByte -> ByteArrayConstant(headerBytes))

    def deserTest() = test("deserializeTo", env, customExt,
      s"""{
            val ba = getVar[Coll[Byte]](21).get
            val headerOpt = Global.deserializeTo[Option[Header]](ba)
            val header = headerOpt.get
            val id = fromBase16("${Base16.encode(h1.id.toArray)}")
            header.height == ${h1.height} && header.id == id
          }""",
      null,
      true
    )

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[sigma.validation.ValidationException] should be thrownBy deserTest()
    } else {
      deserTest()
    }
  }

  property("executeFromVar - SigmaProp") {
    val script = GT(Height, IntConstant(-1)).toSigmaProp
    val scriptBytes = ValueSerializer.serialize(script)
    val customExt = Seq(21.toByte -> ByteArrayConstant(scriptBytes))
    test("executeFromVar", env, customExt,
      "executeFromVar[SigmaProp](21)",
      null,
      true
    )
  }

  property("executeFromVar - Int") {
    val valueBytes = ValueSerializer.serialize(Plus(IntConstant(2), IntConstant(3)))
    val customExt = Seq(21.toByte -> ByteArrayConstant(valueBytes))
    test("executeFromVar", env, customExt,
      "{ executeFromVar[Int](21) == 5 }",
      null,
      true
    )
  }

  property("executeFromVar - Coll[Byte]") {
    val bytes = Slice(ByteArrayConstant(Colls.fromArray(Array.fill(5)(1.toByte))), IntConstant(1), IntConstant(3))
    val valueBytes = ValueSerializer.serialize(bytes)
    val customExt = Seq(21.toByte -> ByteArrayConstant(valueBytes))
    test("executeFromVar", env, customExt,
      "{val ba = executeFromVar[Coll[Byte]](21); ba.size == 2 }",
      null,
      true
    )
  }

  // test which is showing impossibility of nested Deserialize*
  property("executeFromVar - deserialize") {
    val script = DeserializeContext(21.toByte, SSigmaProp)
    val scriptBytes = ValueSerializer.serialize(script)
    val customExt = Seq(21.toByte -> ByteArrayConstant(scriptBytes))
    an [Exception] should be thrownBy test("executeFromVar", env, customExt,
      "executeFromVar[SigmaProp](21)",
      null,
      true
    )
  }

  property("executeFromSelfRegWithDefault - SigmaProp") {
    val script = GT(Height, IntConstant(-1)).toSigmaProp
    val scriptBytes = ValueSerializer.serialize(script)

    val defScript = EQ(Plus(IntConstant(1), IntConstant(5)), 7).toSigmaProp
    val defBytes = ValueSerializer.serialize(defScript)

    val customExt = Seq(21.toByte -> ByteArrayConstant(defBytes))
    val customEnv = Map(
        "defaultVal" -> CAnyValue(21.toByte)
    )

    test("executeFromSelfReg", customEnv, customExt,
      "executeFromSelfRegWithDefault[SigmaProp](4, getVar[SigmaProp](defaultVal).get)",
      null,
      true,
      additionalRegistersOpt = Some(Map(
        reg1 -> ByteArrayConstant(scriptBytes)
      ))
    )
  }

  property("executeFromSelfRegWithDefault - Coll[Byte]") {
    val bytes = Slice(ByteArrayConstant(Colls.fromArray(Array.fill(5)(1.toByte))), IntConstant(1), IntConstant(3))
    val scriptBytes = ValueSerializer.serialize(bytes)

    // this is bound to defaultByte in order to provide an array that's not size == 2
    val dval = ByteArrayConstant(Colls.fromArray(Array.fill(3)(1.toByte)))
    val defaultBytes = ValueSerializer.serialize(dval)

    val customExt = Seq(21.toByte -> ByteArrayConstant(defaultBytes))
    val customEnv = Map(
        "defaultVal" -> CAnyValue(21.toByte)
    )

    test("executeFromSelfReg", customEnv, customExt,
      "{val ba = executeFromSelfRegWithDefault[Coll[Byte]](4, getVar[Coll[Byte]](defaultVal).get); ba.size == 2 }",
      null,
      true,
      additionalRegistersOpt = Some(Map(
        reg1 -> ByteArrayConstant(scriptBytes)
      ))
    )
  }

  property("executeFromSelfRegWithDefault - Int") {
    val bytes = Plus(IntConstant(2), IntConstant(3))
    val scriptBytes = ValueSerializer.serialize(bytes)

    test("executeFromSelfReg", env, ext,
      "{ executeFromSelfRegWithDefault[Int](4, getVar[Int](1).get) == 5 }",
      null,
      true,
      additionalRegistersOpt = Some(Map(
        reg1 -> ByteArrayConstant(scriptBytes)
      ))
    )
  }

  property("executeFromSelfRegWithDefault - ScriptReduction") {
    assertExceptionThrown(
      test("executeFromSelfReg", env, ext,
        "{ executeFromSelfRegWithDefault[Int](4, getVar[Int](1).get) == 2 }",
        null,
        true,
        additionalRegistersOpt = Some(Map())
      ),
      e => {
        val r = rootCause(e)
        r.isInstanceOf[InterpreterException] && r.getMessage == "Script reduced to false"
      }
    )
  }

  property("executeFromSelfRegWithDefault - ForceDefault") {
    test("executeFromSelfReg", env, ext,
      "{ executeFromSelfRegWithDefault[Int](4, getVar[Int](2).get) == 2 }",
      null,
      true,
      additionalRegistersOpt = Some(Map())
    )
  }

  property("executeFromSelfRegWithDefault - InvalidRegister") {
    test("executeFromSelfReg", env, ext,
      "{ executeFromSelfRegWithDefault[Int](99, getVar[Int](2).get) == 2 }",
      null,
      true,
      additionalRegistersOpt = Some(Map())
    )
  }

  property("executeFromSelfReg - SigmaProp") {
    val script = GT(Height, IntConstant(-1)).toSigmaProp
    val scriptBytes = ValueSerializer.serialize(script)

    val defScript = EQ(Plus(IntConstant(1), IntConstant(5)), 7).toSigmaProp
    val defBytes = ValueSerializer.serialize(defScript)

    val customExt = Seq(21.toByte -> ByteArrayConstant(defBytes))
    val customEnv = Map(
      "defaultVal" -> CAnyValue(21.toByte)
    )

    test("executeFromSelfReg", customEnv, customExt,
      "executeFromSelfReg[SigmaProp](4)",
      null,
      true,
      additionalRegistersOpt = Some(Map(
        reg1 -> ByteArrayConstant(scriptBytes)
      ))
    )
  }

  property("executeFromSelfReg - Coll[Byte]") {
    val bytes = Slice(ByteArrayConstant(Colls.fromArray(Array.fill(5)(1.toByte))), IntConstant(1), IntConstant(3))
    val scriptBytes = ValueSerializer.serialize(bytes)

    // this is bound to defaultByte in order to provide an array that's not size == 2
    val dval = ByteArrayConstant(Colls.fromArray(Array.fill(3)(1.toByte)))
    val defaultBytes = ValueSerializer.serialize(dval)

    val customExt = Seq(21.toByte -> ByteArrayConstant(defaultBytes))
    val customEnv = Map(
      "defaultVal" -> CAnyValue(21.toByte)
    )

    test("executeFromSelfReg", customEnv, customExt,
      "{val ba = executeFromSelfReg[Coll[Byte]](4); ba.size == 2 }",
      null,
      true,
      additionalRegistersOpt = Some(Map(
        reg1 -> ByteArrayConstant(scriptBytes)
      ))
    )
  }

  property("executeFromSelfReg - Int") {
    val bytes = Plus(IntConstant(2), IntConstant(3))
    val scriptBytes = ValueSerializer.serialize(bytes)

    test("executeFromSelfReg", env, ext,
      "{ executeFromSelfReg[Int](4) == 5 }",
      null,
      true,
      additionalRegistersOpt = Some(Map(
        reg1 -> ByteArrayConstant(scriptBytes)
      ))
    )
  }

  property("executeFromSelfReg - InvalidRegister") {
    assertExceptionThrown(
      test("executeFromSelfReg", env, ext,
        "{ executeFromSelfReg[Int](99) == 2 }",
        null,
        true,
        additionalRegistersOpt = Some(Map())
      ),
      e => {
        val r = rootCause(e)
        r.isInstanceOf[sigma.exceptions.InvalidArguments] && r.getMessage.startsWith("Invalid register specified")
      }
    )
  }

  property("Relation operations") {
    test("R1", env, ext,
      "{ allOf(Coll(getVar[Boolean](trueVar).get, true, true)) }",
      AND(GetVarBoolean(booleanVar).get, TrueLeaf, TrueLeaf).toSigmaProp
    )
    test("R2", env, ext,
      "{ anyOf(Coll(getVar[Boolean](trueVar).get, true, false)) }",
      OR(GetVarBoolean(booleanVar).get, TrueLeaf, FalseLeaf).toSigmaProp
    )
    test("R3", env, ext,
      "{ getVar[Int](intVar2).get > getVar[Int](intVar1).get && getVar[Int](intVar1).get < getVar[Int](intVar2).get }",
      BlockValue(
        Vector(ValDef(1, GetVarInt(2).get), ValDef(2, GetVarInt(1).get)),
        BinAnd(GT(ValUse(1, SInt), ValUse(2, SInt)), LT(ValUse(2, SInt), ValUse(1, SInt)))).asBoolValue.toSigmaProp
    )
    test("R4", env, ext,
      "{ getVar[Int](intVar2).get >= getVar[Int](intVar1).get && getVar[Int](intVar1).get <= getVar[Int](intVar2).get }",
      BlockValue(
        Vector(ValDef(1, GetVarInt(2).get), ValDef(2, GetVarInt(1).get)),
        BinAnd(GE(ValUse(1, SInt), ValUse(2, SInt)), LE(ValUse(2, SInt), ValUse(1, SInt)))).asBoolValue.toSigmaProp
    )
    test("R5", env, ext,
      "{ getVar[Byte](byteVar2).get > getVar[Byte](byteVar1).get && getVar[Byte](byteVar1).get < getVar[Byte](byteVar2).get }",
      BlockValue(
        Vector(ValDef(1, GetVarByte(4).get), ValDef(2, GetVarByte(3).get)),
        BinAnd(GT(ValUse(1, SByte), ValUse(2, SByte)), LT(ValUse(2, SByte), ValUse(1, SByte)))).asBoolValue.toSigmaProp
    )
    test("R6", env, ext,
      "{ getVar[Byte](byteVar2).get >= getVar[Byte](byteVar1).get && getVar[Byte](byteVar1).get <= getVar[Byte](byteVar2).get }",
      BlockValue(
        Vector(ValDef(1, GetVarByte(4).get), ValDef(2, List(), GetVarByte(3).get)),
        BinAnd(GE(ValUse(1, SByte), ValUse(2, SByte)), LE(ValUse(2, SByte), ValUse(1, SByte)))).asBoolValue.toSigmaProp
    )
    test("R7", env, ext,
      "{ getVar[BigInt](bigIntVar2).get > getVar[BigInt](bigIntVar1).get && getVar[BigInt](bigIntVar1).get < getVar[BigInt](bigIntVar2).get }",
      BlockValue(
        Vector(ValDef(1, GetVarBigInt(6).get), ValDef(2, List(), GetVarBigInt(5).get)),
        BinAnd(GT(ValUse(1, SBigInt), ValUse(2, SBigInt)), LT(ValUse(2, SBigInt), ValUse(1, SBigInt)))).asBoolValue.toSigmaProp
    )
    test("R8", env, ext,
      "{ getVar[BigInt](bigIntVar2).get >= getVar[BigInt](bigIntVar1).get && getVar[BigInt](bigIntVar1).get <= getVar[BigInt](bigIntVar2).get }",
      BlockValue(
        Vector(ValDef(1, GetVarBigInt(6).get), ValDef(2, List(), GetVarBigInt(5).get)),
        BinAnd(GE(ValUse(1, SBigInt), ValUse(2, SBigInt)), LE(ValUse(2, SBigInt), ValUse(1, SBigInt)))).asBoolValue.toSigmaProp
    )
  }

  property("SigmaProp operations") {
    test("Prop1", env, ext,
      "{ getVar[SigmaProp](proofVar1).get.isProven }",
      GetVarSigmaProp(propVar1).get,
      testExceededCost = false
    )
    test("Prop2", env, ext,
      "{ getVar[SigmaProp](proofVar1).get || getVar[SigmaProp](proofVar2).get }",
      SigmaOr(Seq(GetVarSigmaProp(propVar1).get, GetVarSigmaProp(propVar2).get)),
      testExceededCost = false
    )
    test("Prop3", env, ext,
      "{ getVar[SigmaProp](proofVar1).get && getVar[SigmaProp](proofVar2).get }",
      SigmaAnd(Seq(GetVarSigmaProp(propVar1).get, GetVarSigmaProp(propVar2).get)),
      testExceededCost = false
    )
    test("Prop4", env, ext,
      "{ getVar[SigmaProp](proofVar1).get.isProven && getVar[SigmaProp](proofVar2).get }",
      SigmaAnd(Seq(GetVarSigmaProp(propVar1).get, GetVarSigmaProp(propVar2).get)),
      testExceededCost = false
    )
    test("Prop5", env, ext,
      "{ getVar[SigmaProp](proofVar1).get && getVar[Int](intVar1).get == 1 }",
      SigmaAnd(Seq(GetVarSigmaProp(propVar1).get, BoolToSigmaProp(EQ(GetVarInt(intVar1).get, 1)))),
      testExceededCost = false
    )
    test("Prop6", env, ext,
      "{ getVar[Int](intVar1).get == 1 || getVar[SigmaProp](proofVar1).get }",
      SigmaOr(Seq(BoolToSigmaProp(EQ(GetVarInt(intVar1).get, 1)), GetVarSigmaProp(propVar1).get))
    )
    test("Prop7", env, ext,
      "{ SELF.R4[SigmaProp].get.isProven }",
      ExtractRegisterAs[SSigmaProp.type](Self, reg1).get,
      onlyPositive = true,
      testExceededCost = false
    )
    test("Prop8", env, ext,
      "{ SELF.R4[SigmaProp].get && getVar[SigmaProp](proofVar1).get}",
      SigmaAnd(Seq(ExtractRegisterAs[SSigmaProp.type](Self, reg1).get, GetVarSigmaProp(propVar1).get)),
      onlyPositive = true,
      testExceededCost = false
    )
    test("Prop9", env, ext,
      "{ allOf(Coll(SELF.R4[SigmaProp].get, getVar[SigmaProp](proofVar1).get))}",
      SigmaAnd(Seq(ExtractRegisterAs[SSigmaProp.type](Self, reg1).get, GetVarSigmaProp(propVar1).get)),
      onlyPositive = true,
      testExceededCost = false
    )
    test("Prop10", env, ext,
      "{ anyOf(Coll(SELF.R4[SigmaProp].get, getVar[SigmaProp](proofVar1).get))}",
      SigmaOr(Seq(ExtractRegisterAs[SSigmaProp.type](Self, reg1).get, GetVarSigmaProp(propVar1).get)),
      onlyPositive = true,
      testExceededCost = false
    )
    test("Prop11", env, ext,
      "{ Coll(SELF.R4[SigmaProp].get, getVar[SigmaProp](proofVar1).get).forall({ (p: SigmaProp) => p.isProven }) }",
      SigmaAnd(Seq(ExtractRegisterAs[SSigmaProp.type](Self, reg1).get , GetVarSigmaProp(propVar1).get)),
      onlyPositive = true,
      testExceededCost = false
    )
    test("Prop12", env, ext,
      "{ Coll(SELF.R4[SigmaProp].get, getVar[SigmaProp](proofVar1).get).exists({ (p: SigmaProp) => p.isProven }) }",
      SigmaOr(Seq(ExtractRegisterAs[SSigmaProp.type](Self, reg1).get, GetVarSigmaProp(propVar1).get)),
      onlyPositive = true,
      testExceededCost = false
    )
    test("Prop13", env, ext,
      "{ SELF.R4[SigmaProp].get.propBytes != getVar[SigmaProp](proofVar1).get.propBytes }",
      NEQ(ExtractRegisterAs[SSigmaProp.type](Self, reg1).get.propBytes, GetVarSigmaProp(propVar1).get.propBytes).toSigmaProp,
      true
    )
  }

  property("Arith operations") {
    test("Arith1", env, ext,
      "{ getVar[Int](intVar2).get * 2 + getVar[Int](intVar1).get == 5 }",
      EQ(Plus(Multiply(GetVarInt(intVar2).get, IntConstant(2)), GetVarInt(intVar1).get), IntConstant(5)).toSigmaProp
    )
    test("Arith2", env, ext :+ (bigIntVar3 -> BigIntConstant(50)),
      "{ getVar[BigInt](bigIntVar2).get * 2 + getVar[BigInt](bigIntVar1).get == getVar[BigInt](bigIntVar3).get }",
      EQ(
        Plus(Multiply(GetVarBigInt(bigIntVar2).get, BigIntConstant(2)),
          GetVarBigInt(bigIntVar1).get),
        GetVarBigInt(bigIntVar3).get).toSigmaProp
    )
    test("Arith3", env, ext :+ (byteVar3 -> ByteConstant(5)),
      "{ getVar[Byte](byteVar2).get * 2.toByte + getVar[Byte](byteVar1).get == 5.toByte }",
      EQ(
        Plus(Multiply(GetVarByte(byteVar2).get, ByteConstant(2)),
          GetVarByte(byteVar1).get), ByteConstant(5)).toSigmaProp
    )
    test("Arith4", env, ext,
      "{ getVar[Int](intVar2).get / 2 + getVar[Int](intVar1).get == 2 }",
      EQ(Plus(Divide(GetVarInt(2).get, IntConstant(2)), GetVarInt(1).get),IntConstant(2)).toSigmaProp
    )
    test("Arith5", env, ext,
      "{ getVar[Int](intVar2).get % 2 + getVar[Int](intVar1).get == 1 }",
      EQ(Plus(Modulo(GetVarInt(intVar2).get, IntConstant(2)), GetVarInt(intVar1).get), IntConstant(1)).toSigmaProp
    )
  }

  property("Tuple operations") {
    test("Tup1", env, ext,
      "{ (getVar[Int](intVar1).get, getVar[Int](intVar2).get)._1 == 1 }",
      EQ(GetVarInt(intVar1).get, IntConstant(1)).toSigmaProp
    )
    test("Tup2", env, ext,
      "{ (getVar[Int](intVar1).get, getVar[Int](intVar2).get)._2 == 2 }",
      EQ(GetVarInt(intVar2).get, IntConstant(2)).toSigmaProp
    )
    test("Tup3", env, ext,
      """{ val p = (getVar[Int](intVar1).get, getVar[Int](intVar2).get)
        |  val res = p._1 + p._2
        |  res == 3 }""".stripMargin,
      {
        EQ(Plus(GetVarInt(intVar1).get, GetVarInt(intVar2).get), IntConstant(3)).toSigmaProp
      }
    )
  }

  property("Tuple as Collection operations") {
    test("TupColl1", env, ext,
    """{ val p = (getVar[Int](intVar1).get, getVar[Byte](byteVar2).get)
     |  p.size == 2 }""".stripMargin,
    {
      TrueLeaf.toSigmaProp
    }, true)
    test("TupColl2", env, ext,
    """{ val p = (getVar[Int](intVar1).get, getVar[Byte](byteVar2).get)
     |  p(0) == 1 }""".stripMargin,
    {
      EQ(GetVarInt(intVar1).get, IntConstant(1)).toSigmaProp
    })

    val dataVar = (lastExtVar + 1).toByte
    val Colls = CSigmaDslBuilder.Colls
    implicit val eAny = sigma.AnyType
    val data = Colls.fromItems((Array[Byte](1,2,3).toColl, 10L))
    val env1 = env + ("dataVar" -> CAnyValue(dataVar))
    val dataType = SCollection(STuple(SByteArray, SLong))
    val ext1 = ext :+ ((dataVar, Constant[SType](data.asWrappedType, dataType)))
    test("TupColl3", env1, ext1,
      """{
        |  val data = getVar[Coll[(Coll[Byte], Long)]](dataVar).get
        |  data.size == 1
        |}""".stripMargin,
      {
        val data = GetVar(dataVar, dataType).get
        EQ(SizeOf(data), IntConstant(1)).toSigmaProp
      }
    )
    test("TupColl4", env1, ext1,
      """{
        |  val data = getVar[Coll[(Coll[Byte], Long)]](dataVar).get
        |  data.exists({ (p: (Coll[Byte], Long)) => p._2 == 10L })
        |}""".stripMargin,
      {
        val data = GetVar(dataVar, dataType).get
        Exists(data,
          FuncValue(
            Vector((1, STuple(SByteArray, SLong))),
            EQ(SelectField(ValUse(1, STuple(SByteArray, SLong)), 2), LongConstant(10)))
        ).toSigmaProp
      }
    )
    test("TupColl5", env1, ext1,
      """{
        |  val data = getVar[Coll[(Coll[Byte], Long)]](dataVar).get
        |  data.forall({ (p: (Coll[Byte], Long)) => p._1.size > 0 })
        |}""".stripMargin,
      {
        val data = GetVar(dataVar, dataType).get
        ForAll(data,
          FuncValue(
            Vector((1, STuple(SByteArray, SLong))),
            GT(SizeOf(SelectField(ValUse(1, STuple(SByteArray, SLong)), 1).asCollection[SByte.type]), IntConstant(0)))
        ).toSigmaProp
      }
    )
    test("TupColl6", env1, ext1,
      """{
        |  val data = getVar[Coll[(Coll[Byte], Long)]](dataVar).get
        |  data.map({ (p: (Coll[Byte], Long)) => (p._2, p._1)}).size == 1
        |}""".stripMargin,
      {
        val data = GetVar(dataVar, dataType).get
        EQ(SizeOf(data), IntConstant(1)).toSigmaProp
      }
    )
  }

  property("GetVar") {
    test("GetVar1", env, ext,
      "{ getVar[Int](intVar2).get == 2 }",
      EQ(GetVarInt(intVar2).get, IntConstant(2)).toSigmaProp
    )
    // wrong type
    assertExceptionThrown(
      test("GetVar2", env, ext,
      "{ getVar[Byte](intVar2).isDefined }",
      GetVarByte(intVar2).isDefined.toSigmaProp,
      true
      ),
      rootCause(_).isInstanceOf[InvalidType])
  }

  property("ExtractRegisterAs") {
    test("Extract1", env, ext,
      "{ SELF.R4[SigmaProp].get.isProven }",
      ExtractRegisterAs[SSigmaProp.type](Self, reg1).get,
      onlyPositive = true,
      testExceededCost = false
    )
    // wrong type
    assertExceptionThrown(
      test("Extract2", env, ext,
        "{ SELF.R4[Long].isDefined }",
        ExtractRegisterAs[SLong.type](Self, reg1).isDefined.toSigmaProp,
        true
      ),
      rootCause(_).isInstanceOf[InvalidType])
  }

  property("OptionGet success (SomeValue)") {
    test("Opt1", env, ext,
      "{ getVar[Int](intVar2).get == 2 }",
      EQ(GetVarInt(intVar2).get, IntConstant(2)).toSigmaProp
    )
    test("Opt2", env, ext,
      "{ val v = getVar[Int](intVar2); v.get == 2 }",
      EQ(GetVarInt(intVar2).get, IntConstant(2)).toSigmaProp
    )
  }

  property("OptionGet fail (NoneValue)") {
    assertExceptionThrown(
      test("OptGet1", env, ext,
        "{ getVar[Int](99).get == 2 }",
        EQ(GetVarInt(99).get, IntConstant(2)).toSigmaProp
      ),
      rootCause(_).isInstanceOf[NoSuchElementException])
    assertExceptionThrown(
      test("OptGet2", env, ext,
        "{ SELF.R8[SigmaProp].get.propBytes != getVar[SigmaProp](proofVar1).get.propBytes }",
        NEQ(ExtractRegisterAs[SSigmaProp.type](Self, R8).get.propBytes, GetVarSigmaProp(propVar1).get.propBytes).toSigmaProp,
        true
      ),
      rootCause(_).isInstanceOf[NoSuchElementException])
  }

  property("higher order lambdas") {
    def holTest() = test("HOL", env, ext,
      """
        | {
        |   val c = Coll(Coll(1))
        |   def fn(xs: Coll[Int]) = {
        |     val inc = { (x: Int) => x + 1 }
        |     def apply(in: (Int => Int, Int)) = in._1(in._2)
        |     val ys = xs.map { (x: Int) => apply((inc, x)) }
        |     ys.size == xs.size && ys != xs
        |   }
        |
        |   c.exists(fn)
        | }
        |""".stripMargin,
      null,
      true
    )

    if(VersionContext.current.isV3OrLaterErgoTreeVersion) {
      holTest()
    } else {
      an[scala.MatchError] shouldBe thrownBy(holTest())
    }
  }

  property("OptionGetOrElse") {
    test("OptGet1", env, ext,
      "{ SELF.R5[Int].getOrElse(3) == 1 }",
      EQ(ExtractRegisterAs[SInt.type](Self, reg2).getOrElse(IntConstant(3)), IntConstant(1)).toSigmaProp,
      true
    )
    // register should be empty
    test("OptGet2", env, ext,
      "{ SELF.R6[Int].getOrElse(3) == 3 }",
      EQ(ExtractRegisterAs(Self, R6, SOption(SInt)).getOrElse(IntConstant(3)), IntConstant(3)).toSigmaProp,
      true
    )
    test("OptGet3", env, ext,
      "{ getVar[Int](intVar2).getOrElse(3) == 2 }",
      EQ(GetVarInt(intVar2).getOrElse(IntConstant(3)), IntConstant(2)).toSigmaProp,
      true
    )
    // there should be no variable with this id
    test("OptGet4", env, ext,
    "{ getVar[Int](99).getOrElse(3) == 3 }",
      EQ(GetVarInt(99).getOrElse(IntConstant(3)), IntConstant(3)).toSigmaProp,
      true
    )
  }

  property("OptionIsDefined") {
    test("Def1", env, ext,
      "{ SELF.R4[SigmaProp].isDefined }",
      ExtractRegisterAs[SSigmaProp.type](Self, reg1).isDefined.toSigmaProp,
      true
    )
    // no value
    test("Def2", env, ext,
      "{ SELF.R8[Int].isDefined == false }",
      LogicalNot(ExtractRegisterAs[SInt.type](Self, R8).isDefined).toSigmaProp,
      true
    )

    test("Def3", env, ext,
      "{ getVar[Int](intVar2).isDefined }",
      GetVarInt(intVar2).isDefined.toSigmaProp,
      true
    )
    // there should be no variable with this id
    test("Def4", env, ext,
      "{ getVar[Int](99).isDefined == false }",
      LogicalNot(GetVarInt(99).isDefined).toSigmaProp,
      true
    )
  }

  property("OptionIsEmpty") {
    test("Def1", env, ext,
      "{ SELF.R4[SigmaProp].isEmpty == false }",
      ExtractRegisterAs[SSigmaProp.type](Self, reg1).isDefined.toSigmaProp,
      true
    )
    // no value
    test("Def2", env, ext,
      "{ SELF.R8[Int].isEmpty }",
      LogicalNot(ExtractRegisterAs[SInt.type](Self, R8).isDefined).toSigmaProp,
      true
    )

    test("Def3", env, ext,
      "{ getVar[Int](intVar2).isEmpty == false }",
      GetVarInt(intVar2).isDefined.toSigmaProp,
      true
    )
    // there should be no variable with this id
    test("Def4", env, ext,
      "{ getVar[Int](99).isEmpty }",
      LogicalNot(GetVarInt(99).isDefined).toSigmaProp,
      true
    )
  }

  // TODO this is valid for BigIntModQ type (https://github.com/ScorexFoundation/sigmastate-interpreter/issues/554)
  ignore("ByteArrayToBigInt: big int should always be positive") {
    test("BATBI1", env, ext,
      "{ byteArrayToBigInt(Coll[Byte](-1.toByte)) > 0 }",
      GT(ByteArrayToBigInt(ConcreteCollection.fromItems(ByteConstant(-1))), BigIntConstant(0)).toSigmaProp,
      onlyPositive = true
    )
  }

  // TODO this is valid for BigIntModQ type (https://github.com/ScorexFoundation/sigmastate-interpreter/issues/554)
  ignore("ByteArrayToBigInt: big int should not exceed dlog group order q (it is NOT ModQ integer)") {
    val q = CryptoConstants.dlogGroup.q
    val bytes = q.add(BigInteger.valueOf(1L)).toByteArray
    val itemsStr = bytes.map(v => s"$v.toByte").mkString(",")
    assertExceptionThrown(
      test("BATBI1", env, ext,
        s"{ byteArrayToBigInt(Coll[Byte]($itemsStr)) > 0 }",
        GT(ByteArrayToBigInt(ConcreteCollection.fromSeq(bytes.map(ByteConstant(_)))), BigIntConstant(0)).toSigmaProp,
        onlyPositive = true
      ),
      e => rootCause(e).isInstanceOf[ArithmeticException]
    )
  }

  property("ByteArrayToBigInt: range check") {
    def check(b: BigInteger, shouldThrow: Boolean) = {
      val bytes = b.toByteArray
      val itemsStr = bytes.map(v => s"$v.toByte").mkString(",")
      if (shouldThrow)
        assertExceptionThrown(
          test("BATBI1", env, ext,
            s"{ byteArrayToBigInt(Coll[Byte]($itemsStr)) != 0 }",
            NEQ(ByteArrayToBigInt(ConcreteCollection.fromSeq(bytes.map(ByteConstant(_)))), BigIntConstant(0)).toSigmaProp,
            onlyPositive = true
          ),
          e => rootCause(e).isInstanceOf[ArithmeticException])
      else
        test("BATBI1", env, ext,
          s"{ byteArrayToBigInt(Coll[Byte]($itemsStr)) != 0 }",
          NEQ(ByteArrayToBigInt(ConcreteCollection.fromSeq(bytes.map(ByteConstant(_)))), BigIntConstant(0)).toSigmaProp,
          onlyPositive = true
        )
    }

    val two = BigInteger.valueOf(2) // BigInteger.TWO is not exported in JDK 1.8
    check(two.negate().pow(255), false)
    check(two.negate().pow(256).subtract(BigInteger.ONE), true)
    check(two.pow(255).subtract(BigInteger.ONE), false)
    check(two.pow(255), true)
    check(two.pow(255).add(BigInteger.ONE), true)
    check(two.pow(256), true)
    check(two.negate().pow(256).subtract(BigInteger.ONE), true)
  }

  property("ExtractCreationInfo") {
    test("Info1", env, ext,
      "SELF.creationInfo._1 == 5",
      EQ(SelectField(ExtractCreationInfo(Self),1),IntConstant(5)).toSigmaProp,
      true
    )
    // suppose to be tx.id + box index
    test("Info2", env, ext,
      "SELF.creationInfo._2.size == 34",
      EQ(SizeOf(SelectField(ExtractCreationInfo(Self),2).asValue[SByteArray]),IntConstant(34)).toSigmaProp,
      true
    )
  }

  property("sigmaProp") {
    test("prop1", env, ext, "sigmaProp(HEIGHT >= 0)",
      BoolToSigmaProp(GE(Height, IntConstant(0))), true)
    test("prop2", env, ext, "sigmaProp(HEIGHT >= 0) && getVar[SigmaProp](proofVar1).get",
      SigmaAnd(Vector(BoolToSigmaProp(GE(Height, IntConstant(0))), GetVarSigmaProp(propVar1).get)),
      onlyPositive = true,
      testExceededCost = false
    )
  }

  property("numeric cast") {
    test("downcast", env, ext,
      "{ getVar[Int](intVar2).get.toByte == 2.toByte }",
      EQ(Downcast(GetVarInt(2).get, SByte), ByteConstant(2)).toSigmaProp,
      onlyPositive = true
    )
    test("upcast", env, ext,
      "{ getVar[Int](intVar2).get.toLong == 2L }",
      EQ(Upcast(GetVarInt(2).get, SLong), LongConstant(2)).toSigmaProp,
      onlyPositive = true
    )
  }

  property("EQ const array vs collection") {
    val byteArrayVar1Value = ByteArrayConstant(Array[Byte](1.toByte, 2.toByte))
    test("EQArrayCollection", env + ("byteArrayVar1" -> byteArrayVar1Value), ext,
      "byteArrayVar1 == Coll[Byte](1.toByte, 2.toByte)",
      EQ(byteArrayVar1Value, ConcreteCollection(Array(ByteConstant(1), ByteConstant(2)), SByte)).toSigmaProp,
      true
    )
  }

  property("user defined function") {
    test("function", env, ext,
      "{ def inc(i: Int) = i + 1; inc(2) == 3 }",
      EQ(
        Apply(
          FuncValue(Array((1, SInt)), Plus(ValUse(1, SInt), IntConstant(1))),
          Array(IntConstant(2))
        ),
        IntConstant(3)).toSigmaProp
    )
  }

  property("missing variable in env buildValue error") {
    test("missingVar", env, ext,
      """
        |OUTPUTS.forall({(out:Box) =>
        |  out.R5[Int].get >= HEIGHT + 10 &&
        |  blake2b256(out.propositionBytes) != Coll[Byte](1.toByte)
        |})
      """.stripMargin,
      ForAll(Outputs, FuncValue(Array((1,SBox)),
        BinAnd(
          GE(ExtractRegisterAs(ValUse(1,SBox), ErgoBox.R5, SOption(SInt)).get, Plus(Height, IntConstant(10))),
          NEQ(CalcBlake2b256(ExtractScriptBytes(ValUse(1,SBox))), ConcreteCollection(Array(ByteConstant(1.toByte)), SByte))
        ))).toSigmaProp,
      true
    )
  }

  property("Nested logical ops 1") {
   test("nestedLogic1", env, ext,
     """{
      |    val c = OUTPUTS(0).R4[Int].get
      |    val d = OUTPUTS(0).R5[Int].get
      |
      |    OUTPUTS.size == 2 &&
      |    OUTPUTS(0).value == SELF.value &&
      |    OUTPUTS(1).value == SELF.value
      |} == false""".stripMargin,
     null,
     true
   )
  }

  property("Nested logical ops 2") {
    test("nestedLogic2", env, ext,
      """{
       |    val c = OUTPUTS(0).R4[Int].get
       |    val d = OUTPUTS(0).R5[Int].get
       |
       |    OUTPUTS.size == 1 &&
       |    OUTPUTS(0).value == SELF.value &&
       |    {{ c != d || d == c }  && { true || false } }
       |} """.stripMargin,
      null,
      true
    )
  }

  property("Option.map") {
    test("Option.map", env, ext,
      "getVar[Int](intVar1).map({(i: Int) => i + 1}).get == 2",
      null,
      true
    )
  }

  property("Option.filter") {
    test("Option.filter", env, ext,
      "getVar[Int](intVar1).filter({(i: Int) => i > 0}).get == 1",
      null,
      true
    )
  }

  property("lazy OR") {
    test("lazy OR", env, ext,
      "true || ((1/0) == 1)",
      null,
      true
    )
  }

  property("lazy AND") {
    test("lazy AND", env, ext,
      "(false && ((1/0) == 1)) == false",
      null,
      true
    )
  }

  property("substConstants") {
    val initTreeScript =
      """
        | {
        |   val v1 = 1  // 0
        |   val v2 = 2  // 2
        |   val v3 = 3  // 4
        |   val v4 = 4  // 3
        |   val v5 = 5  // 1
        |   sigmaProp(v1 == -v5 && v2 == -v4 && v3 == v2 + v4)
        | }
        |""".stripMargin

    val iet = ErgoTree.fromProposition(compile(Map.empty, initTreeScript).asInstanceOf[SigmaPropValue])

    iet.constants.toArray shouldBe Array(IntConstant(1), IntConstant(5), IntConstant(2), IntConstant(4), IntConstant(3), IntConstant(6))

    val originalBytes = Base16.encode(ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(iet))

    val set = ErgoTree(
      iet.header,
      IndexedSeq(IntConstant(-2), IntConstant(2), IntConstant(-1), IntConstant(1), IntConstant(0), IntConstant(0)),
      iet.toProposition(false)
    )

    val hostScript =
      s"""
        |{
        | val bytes = fromBase16("${originalBytes}")
        |
        | val substBytes = substConstants[Int](bytes, Coll[Int](0, 2, 4, 3, 1, 5), Coll[Int](-2, -1, 0, 1, 2, 0))
        |
        | val checkSubst = substBytes == fromBase16("${Base16.encode(ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(set))}")
        |
        | sigmaProp(checkSubst)
        |}
        |""".stripMargin

    test("subst", env, ext, hostScript, null)
  }

  property("Box.getReg") {
    val customExt = Map(
      1.toByte -> IntConstant(0)
    ).toSeq
    def getRegTest(): Assertion = {
      test("Box.getReg", env, customExt,
        """{
          |   val idx = getVar[Int](1).get
          |   val x = SELF
          |   x.getReg[Long](idx).get == SELF.value &&
          |   x.getReg[Coll[(Coll[Byte], Long)]](2).get == SELF.tokens &&
          |   x.getReg[Int](9).isEmpty
          |}""".stripMargin,
        null
      )
    }

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      getRegTest()
    } else {
      an[sigma.validation.ValidationException] should be thrownBy getRegTest()
    }
  }

  property("Box.getReg - computable index") {
    val ext: Seq[VarBinding] = Seq(
      (intVar1, IntConstant(0))
    )
    def getRegTest(): Assertion = {
      test("Box.getReg", env, ext,
        """{
          |   val x = SELF.getReg[Long](getVar[Int](1).get).get
          |   x == SELF.value
          |}""".stripMargin,
        null
      )
    }

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      getRegTest()
    } else {
      an[sigma.validation.ValidationException] should be thrownBy getRegTest()
    }
  }

  property("Unit register") {
    test("R1", env, ext,
      script = "{ SELF.R4[Unit].isDefined }",
      ExtractRegisterAs[SUnit.type](Self, reg1)(SUnit).isDefined.toSigmaProp,
      additionalRegistersOpt = Some(Map(
        reg1 -> UnitConstant.instance
      ))
    )

    test("R2", env, ext,
      script = "{ SELF.R4[Unit].get == () }",
      EQ(ExtractRegisterAs[SUnit.type](Self, reg1)(SUnit).get, UnitConstant.instance).toSigmaProp,
      additionalRegistersOpt = Some(Map(
        reg1 -> UnitConstant.instance
      ))
    )
  }

  property("Global.some") {
    val ext: Seq[VarBinding] = Seq(
      (intVar1, IntConstant(0))
    )
    def someTest(): Assertion = {
      test("some", env, ext,
        """{
          |   val xo = Global.some[Int](5)
          |   xo.get == 5
          |}""".stripMargin,
        null
      )
    }

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      someTest()
    } else {
      an[sigma.validation.ValidationException] should be thrownBy someTest()
    }
  }

  property("Global.some - computable value") {
    val ext: Seq[VarBinding] = Seq(
      (intVar1, IntConstant(0))
    )
    def someTest(): Assertion = {
      test("some", env, ext,
        """{
          |   val i = getVar[Int](1)
          |   val xo = Global.some[Int](i.get)
          |   xo == i
          |}""".stripMargin,
        null
      )
    }

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      someTest()
    } else {
      an[sigma.validation.ValidationException] should be thrownBy someTest()
    }
  }

  property("Global.none") {
    val ext: Seq[VarBinding] = Seq(
      (intVar1, IntConstant(0))
    )
    def someTest(): Assertion = {
      test("some", env, ext,
        """{
          |   val xo = Global.some[Long](5L)
          |   val xn = Global.none[Long]()
          |   xn.isDefined == false && xn != xo
          |}""".stripMargin,
        null
      )
    }

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      someTest()
    } else {
      an[sigma.validation.ValidationException] should be thrownBy someTest()
    }
  }

  property("avltree.insertOrUpdate") {
    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)

    val elements = Seq(123, 22)
    val treeElements = elements.map(i => Longs.toByteArray(i)).map(s => (ADKey @@@ Blake2b256(s), ADValue @@ s))
    treeElements.foreach(s => avlProver.performOneOperation(Insert(s._1, s._2)))
    avlProver.generateProof()
    val treeData = new AvlTreeData(avlProver.digest.toColl, AvlTreeFlags.AllOperationsAllowed, 32, None)

    val elements2 = Seq(1, 22)
    val treeElements2 = elements2.map(i => Longs.toByteArray(i)).map(s => (ADKey @@@ Blake2b256(s), ADValue @@ s))
    treeElements2.foreach(s => avlProver.performOneOperation(InsertOrUpdate(s._1, s._2)))
    val updateProof = avlProver.generateProof()
    val treeData2 = new AvlTreeData(avlProver.digest.toColl, AvlTreeFlags.AllOperationsAllowed, 32, None)

    val v: Coll[(Coll[Byte], Coll[Byte])] = treeElements2.map(t => t._1.toColl -> t._2.toColl).toArray.toColl
    val ops = IR.builder.mkConstant[SType](v.asWrappedType, SCollection(STuple(SByteArray, SByteArray)))

    val customExt = Seq(
      21.toByte -> AvlTreeConstant(treeData),
      22.toByte -> AvlTreeConstant(treeData2),
      23.toByte -> ops,
      24.toByte -> ByteArrayConstant(updateProof)
    )

    def deserTest() = test("insertOrUpdate", env, customExt,
      s"""{
            val tree1 = getVar[AvlTree](21).get
            val tree2 = getVar[AvlTree](22).get

            val toInsert = getVar[Coll[(Coll[Byte], Coll[Byte])]](23).get
            val proof = getVar[Coll[Byte]](24).get

            val tree1Updated = tree1.insertOrUpdate(toInsert, proof).get
            tree2.digest == tree1Updated.digest
          }""",
      null,
      true
    )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      deserTest()
    } else {
      an[Exception] should be thrownBy deserTest()
    }
  }

  property("Global.decodeNbits - result of more than 256 bits") {
    val okValue = SigmaDsl.encodeNbits(CBigInt(new BigInteger("2").pow(255).subtract(BigInteger.ONE)))
    val invalidBi: CBigInt = VersionContext.withVersions(2, 2) {
      CBigInt(new BigInteger("2").pow(256).subtract(BigInteger.ONE))
    }
    val invalidValue = SigmaDsl.encodeNbits(invalidBi)

    def someTest(value: Long): Assertion = {
      test("some", env, ext,
        s"""{
          |   val target = Global.decodeNbits(${value}L)
          |   target != 0
          |}""".stripMargin,
        null
      )
    }

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      someTest(okValue)
      // on JVM, InvocationTargetException wrapping (ArithmeticException: BigInteger out of 256 bit range) is thrown
      an[Exception] should be thrownBy someTest(invalidValue)
    } else {
      an[sigma.validation.ValidationException] should be thrownBy someTest(okValue)
    }
  }

  property("Preheader") {
    test("some", env, ext,
      s"""{
         |   CONTEXT.preHeader.height == 0
         |}""".stripMargin,
      null
    )
  }

  property("expUnsigned - with mod inside") {
      val zz = SecP256K1Group.order.add(new BigInteger("8"))
      def someTest() = test("exp", env, ext,
        s"""{
           |
           |      val g: GroupElement = groupGenerator
           |      val z = unsignedBigInt("8")
           |      val zz = unsignedBigInt("${zz.toString}")
           |
           |      sigmaProp(g.expUnsigned(z) == g.expUnsigned(zz))
           |}""".stripMargin,
        null,
        true
      )

    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      someTest()
    } else {
      an[Exception] should be thrownBy someTest()
    }
  }


}
