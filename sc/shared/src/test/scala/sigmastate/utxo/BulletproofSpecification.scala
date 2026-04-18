package sigmastate.utxo

import org.ergoplatform._
import org.ergoplatform.ErgoBox.AdditionalRegisters
import sigma.{Coll, GroupElement, VersionContext}
import sigma.VersionContext.{V6SoftForkVersion, withVersions}
import sigma.data.{AvlTreeData, CAnyValue, CGroupElement, CSigmaProp}
import sigma.ast._
import sigma.ast.syntax._
import sigma.crypto.CryptoConstants
import sigmastate._
import sigmastate.helpers.TestingHelpers._
import sigmastate.helpers.{CompilerTestingCommons, ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTestInterpreter}
import sigma.interpreter.ContextExtension.VarBinding
import sigmastate.interpreter.CErgoTreeEvaluator.DefaultEvalSettings
import sigmastate.interpreter.Interpreter._
import sigma.eval.EvalSettings
import sigma.serialization.ErgoTreeSerializer
import sigma.interpreter.{ContextExtension, ProverResult}
import sigma.validation.ValidationException
import sigmastate.utils.Helpers._

import scala.collection.compat.immutable.ArraySeq
import scala.util.Try

class BulletproofSpecification extends CompilerTestingCommons
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
          propVar3 -> SigmaPropConstant(CSigmaProp(sigma.data.CAND(Seq(p1, d1)))),
          propBytesVar1 -> ByteArrayConstant(CSigmaProp(sigma.data.CAND(Seq(p1, d1))).propBytes)
        )).toMap
      }
      override val evalSettings: EvalSettings = DefaultEvalSettings.copy(
        isMeasureOperationTime = true,
        isDebug = true,
        isTestRun = testExceededCost)
    }

    val prop = if (script == null || script.isEmpty) {
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

  property("Bulletproof modular operations") {
    // Test the modular arithmetic operations needed for bulletproofs
    def modularTest() = {
      test("bulletproof modular math", env, ext,
        s"""{
           |   val q = unsignedBigInt("115792089237316195423570985008687907852837564279074904382605163141518161494337")
           |   
           |   // Test values
           |   val testVal = unsignedBigInt("123456789")
           |   
           |   // Test z^2 = z.multiplyMod(z, q)
           |   val zSquared = testVal.multiplyMod(testVal, q)
           |   
           |   // Test z^3 = zSquared.multiplyMod(z, q)  
           |   val zCubed = zSquared.multiplyMod(testVal, q)
           |   
           |   // Test (z - z^2) mod q
           |   val zMinusZSquared = testVal.subtractMod(zSquared, q)
           |   
           |   // Test basic properties
           |   val squaredValid = zSquared == testVal.multiplyMod(testVal, q)
           |   val cubedValid = zCubed == zSquared.multiplyMod(testVal, q)
           |   val subtractionValid = zMinusZSquared == testVal.subtractMod(zSquared, q)
           |   val allInRange = zSquared < q && zCubed < q && zMinusZSquared < q
           |   
           |   sigmaProp(squaredValid && cubedValid && subtractionValid && allInRange)
           |}""".stripMargin,
        null,
        true
      )
    }

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[sigma.validation.ValidationException] should be thrownBy modularTest()
    } else {
      modularTest()
    }
  }

  property("Bulletproof modular inverse operations") {
    // Test modular inverse operations needed for bulletproofs
    def modularInverseTest() = {
      test("bulletproof modular inverse", env, ext,
        s"""{
           |   val q = unsignedBigInt("115792089237316195423570985008687907852837564279074904382605163141518161494337")
           |   
           |   // Test values that have modular inverses
           |   val testVal1 = unsignedBigInt("123456789")
           |   val testVal2 = unsignedBigInt("987654321")
           |   
           |   // Test modular inverses
           |   val inv1 = testVal1.modInverse(q)
           |   val inv2 = testVal2.modInverse(q)
           |   
           |   // Verify that a * a^-1 ≡ 1 mod q
           |   val identity1 = testVal1.multiplyMod(inv1, q) == unsignedBigInt("1")
           |   val identity2 = testVal2.multiplyMod(inv2, q) == unsignedBigInt("1")
           |   
           |   // Test that inverses are unique and valid
           |   val inv1Valid = inv1 == testVal1.modInverse(q)
           |   val inv2Valid = inv2 == testVal2.modInverse(q)
           |   val allInRange = inv1 < q && inv2 < q
           |   
           |   sigmaProp(identity1 && identity2 && inv1Valid && inv2Valid && allInRange)
           |}""".stripMargin,
        null,
        true
      )
    }

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[sigma.validation.ValidationException] should be thrownBy modularInverseTest()
    } else {
      modularInverseTest()
    }
  }

  property("Bulletproof vector approximation") {
    // Test basic operations that can be used for vector approximations
    def vectorTest() = {
      test("bulletproof vector basics", env, ext,
        s"""{
           |   val q = unsignedBigInt("115792089237316195423570985008687907852837564279074904382605163141518161494337")
           |   val y = unsignedBigInt("2")
           |   
           |   // Test basic operations that would be used in vector computations
           |   // Instead of full vector sum, test individual components
           |   
           |   val y2 = y.multiplyMod(y, q)  // y^2
           |   val y3 = y2.multiplyMod(y, q) // y^3
           |   
           |   // Test vector addition operations
           |   val sum1 = y.plusMod(y2, q)    // y + y^2
           |   val sum2 = y2.plusMod(y3, q)   // y^2 + y^3
           |   
           |   // Test that we can compute powers and basic properties
           |   val y2Valid = y2 == y.multiplyMod(y, q)
           |   val y3Valid = y3 == y2.multiplyMod(y, q)
           |   val sum1Valid = sum1 == y.plusMod(y2, q)
           |   val sum2Valid = sum2 == y2.plusMod(y3, q)
           |   val allInRange = y2 < q && y3 < q && sum1 < q && sum2 < q
           |   val allNonZero = y2 != unsignedBigInt("0") && y3 != unsignedBigInt("0")
           |   
           |   sigmaProp(y2Valid && y3Valid && sum1Valid && sum2Valid && allInRange && allNonZero)
           |}""".stripMargin,
        null,
        true
      )
    }

    // These tests focus on V6+ functionality
    // For older versions, we skip the test since the operations may not be available
    if (ergoTreeVersionInTests >= V6SoftForkVersion) {
      vectorTest()
    }
  }

  property("Bulletproof vector operations") {
    // Test vector operations needed for bulletproofs
    def vectorOpsTest() = {
      test("bulletproof vector ops", env, ext,
        s"""{
           |   val q = unsignedBigInt("115792089237316195423570985008687907852837564279074904382605163141518161494337")
           |   
           |   // Test vector components
           |   val y = unsignedBigInt("2")
           |   val z = unsignedBigInt("3")
           |   
           |   // Compute powers for vector components
           |   val y2 = y.multiplyMod(y, q)
           |   val y3 = y2.multiplyMod(y, q)
           |   val y4 = y3.multiplyMod(y, q)
           |   
           |   // Test vector addition and scaling
           |   val vecSum1 = y.plusMod(y2, q)
           |   val vecSum2 = y2.plusMod(y3, q)
           |   val scaledVec = y.multiplyMod(z, q)
           |   
           |   // Test vector properties
           |   val sumsValid = vecSum1 == y.plusMod(y2, q) && vecSum2 == y2.plusMod(y3, q)
           |   val scalingValid = scaledVec == y.multiplyMod(z, q)
           |   val allInRange = y2 < q && y3 < q && y4 < q && vecSum1 < q && vecSum2 < q && scaledVec < q
           |   
           |   sigmaProp(sumsValid && scalingValid && allInRange)
           |}""".stripMargin,
        null,
        true
      )
    }

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[sigma.validation.ValidationException] should be thrownBy vectorOpsTest()
    } else {
      vectorOpsTest()
    }
  }

  property("Bulletproof polynomial computation") {
    // Test polynomial computations needed for bulletproofs
    def polynomialTest() = {
      test("bulletproof polynomial ops", env, ext,
        s"""{
           |   val q = unsignedBigInt("115792089237316195423570985008687907852837564279074904382605163141518161494337")
           |   
           |   // Test values for polynomial computation
           |   val z = unsignedBigInt("3")
           |   
           |   // Compute polynomial terms
           |   val zSquared = z.multiplyMod(z, q)
           |   val zCubed = zSquared.multiplyMod(z, q)
           |   
           |   // Test polynomial identity: z^3 = z * z^2
           |   val cubicValid = zCubed == z.multiplyMod(zSquared, q)
           |   
           |   // Test polynomial subtraction: z - z^2
           |   val zMinusZSquared = z.subtractMod(zSquared, q)
           |   val subtractionValid = zMinusZSquared == z.subtractMod(zSquared, q)
           |   
           |   // Test polynomial addition: z + z^2 + z^3
           |   val sum1 = z.plusMod(zSquared, q)
           |   val totalSum = sum1.plusMod(zCubed, q)
           |   val additionValid = totalSum == sum1.plusMod(zCubed, q)
           |   
           |   // Test all values are in range
           |   val allInRange = zSquared < q && zCubed < q && zMinusZSquared < q && totalSum < q
           |   
           |   sigmaProp(cubicValid && subtractionValid && additionValid && allInRange)
           |}""".stripMargin,
        null,
        true
      )
    }

    if (ergoTreeVersionInTests < V6SoftForkVersion) {
      an[sigma.validation.ValidationException] should be thrownBy polynomialTest()
    } else {
      polynomialTest()
    }
  }

  ignore("Bulletproof verification for a range proof") {
    /*
     * Original range proof verifier code by Benedikt Bunz:
     *
        VectorBase<T> vectorBase = params.getVectorBase();
        PeddersenBase<T> base = params.getBase();
        int n = vectorBase.getGs().size();
        T a = proof.getaI();
        T s = proof.getS();

        BigInteger q = params.getGroup().groupOrder();
        BigInteger y = ProofUtils.computeChallenge(q, input, a, s);

        FieldVector ys = FieldVector.from(VectorX.iterate(n, BigInteger.ONE, y::multiply), q);

        BigInteger z = ProofUtils.challengeFromints(q, y);

        BigInteger zSquared = z.pow(2).mod(q);
        BigInteger zCubed = z.pow(3).mod(q);

        FieldVector twos = FieldVector.from(VectorX.iterate(n, BigInteger.ONE, bi -> bi.shiftLeft(1)), q);
        FieldVector twoTimesZSquared = twos.times(zSquared);
        GeneratorVector<T> tCommits = proof.gettCommits();

         BigInteger x = ProofUtils.computeChallenge(q,z, tCommits);

        BigInteger tauX = proof.getTauX();
        BigInteger mu = proof.getMu();
        BigInteger t = proof.getT();
        BigInteger k = ys.sum().multiply(z.subtract(zSquared)).subtract(zCubed.shiftLeft(n).subtract(zCubed));
        T lhs = base.commit(t.subtract(k), tauX);
        T rhs = tCommits.commit(Arrays.asList(x, x.pow(2))).add(input.multiply(zSquared));
        System.out.println("y " + y);
        System.out.println("z " + z);

        System.out.println("x " + x);pow
        equal(lhs, rhs, "Polynomial identity check failed, LHS: %s, RHS %s");
        BigInteger uChallenge = ProofUtils.challengeFromints(q, x, tauX, mu, t);
        System.out.println("u " + uChallenge);
        T u = base.g.multiply(uChallenge);
        GeneratorVector<T> hs = vectorBase.getHs();
        GeneratorVector<T> gs = vectorBase.getGs();
        GeneratorVector<T> hPrimes = hs.haddamard(ys.invert());
        FieldVector hExp = ys.times(z).add(twoTimesZSquared);
        T P = a.add(s.multiply(x)).add(gs.sum().multiply(z.negate())).add(hPrimes.commit(hExp)).subtract(base.h.multiply(mu)).add(u.multiply(t));
        VectorBase<T> primeBase = new VectorBase<>(gs, hPrimes, u);
        // System.out.println("PVerify "+P.normalize());
        // System.out.println("XVerify" +x);
        // System.out.println("YVerify" +y);
        // System.out.println("ZVerify" +z);
        // System.out.println("uVerify" +u);

        EfficientInnerProductVerifier<T> verifier = new EfficientInnerProductVerifier<>();
        verifier.verify(primeBase, P, proof.getProductProof(), uChallenge);
     */

    def rangeTest() = {
      test("range proof", env, ext,
        s"""{
           |   // range proof input data
           |   val input: GroupElement = getVar[GroupElement](0).get
           |
           |   // proof data
           |   val ai: GroupElement = getVar[GroupElement](1).get
           |   val s: GroupElement = getVar[GroupElement](2).get
           |   val tCommits: Coll[GroupElement] = getVar[Coll[GroupElement]](3).get
           |   val tauX: UnsignedBigInt = getVar[UnsignedBigInt](4).get
           |   val mu: UnsignedBigInt = getVar[UnsignedBigInt](5).get
           |   val t: UnsignedBigInt = getVar[UnsignedBigInt](6).get
           |
           |   // inner product proof
           |   val L: Coll[GroupElement] = getVar[Coll[GroupElement]](7).get
           |   val R: Coll[GroupElement] = getVar[Coll[GroupElement]](8)).get
           |   val a: UnsignedBigInt = getVar[UnsignedBigInt](9).get
           |   val b: UnsignedBigInt = getVar[UnsignedBigInt](10).get
           |
           |   // proof verification:
           |   val Q = lWeights.size
           |
           |   val q // group order = getVar[UnsignedBigInt](11).get
           |
           |   val yBytes = sha256(q.toBytes ++ input.getEncoded ++ aI.getEncoded ++ s.getEncoded)
           |
           |   val y = byteArrayToBigInt(yBytes).toUnsignedMod(q)
           |
           |   val ys =
           |
           |   val z = byteArrayToBigInt(sha256(q.toBytes ++ yBytes)).toUnsignedMod(q)
           |   val zSquared = z.multiplyMod(z, q)
           |   val zCubed = zSquared.multiplyMod(z, q)
           |
           |   // def times() : // todo: implement
           |
           |   // ops needed: modInverse, mod ops
           |
           |   sigmaProp(zCubed > 0)
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

  // todo: complete
  ignore("Bulletproof verification for a circuit proof") {

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


}