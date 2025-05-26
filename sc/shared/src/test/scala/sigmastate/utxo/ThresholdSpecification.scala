package sigmastate.utxo

import sigma.ast._
import sigma.ast.syntax._
import sigma.data.{AvlTreeData, CAND, COR, ProveDlog, TrivialProp}
import sigma.eval.Extensions.SigmaBooleanOps
import sigma.exceptions.GraphBuildingException
import sigmastate._
import sigmastate.crypto.DLogProtocol.DLogProverInput
import sigmastate.helpers._

class ThresholdSpecification extends CompilerTestingCommons
  with CompilerCrossVersionProps {
  implicit lazy val IR = new TestingIRContext {
  }

  property("basic threshold compilation/execution") {
    val proverA = new ContextEnrichingTestProvingInterpreter
    val proverB = new ContextEnrichingTestProvingInterpreter
    val proverC = new ContextEnrichingTestProvingInterpreter
    val proverD = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val skA = proverA.dlogSecrets.head
    val skB = proverB.dlogSecrets.head
    val skC = proverC.dlogSecrets.head

    val pubkeyA = skA.publicImage
    val pubkeyB = skB.publicImage
    val pubkeyC = skC.publicImage

    val proverABC = proverA.withSecrets(Seq(skB, skC))
    val proverAB = proverA.withSecrets(Seq(skB))
    val proverAC = proverA.withSecrets(Seq(skC))
    val proverBC = proverB.withSecrets(Seq(skC))

    val ctx = ErgoLikeContextTesting(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = ErgoLikeTransactionTesting.dummy,
      self = fakeSelf, activatedVersionInTests)
        .withErgoTreeVersion(ergoTreeVersionInTests)

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC)

    // Basic compilation
    compileAndCheck(env,
      """atLeast(2, Coll(pubkeyA, pubkeyB, pubkeyC))""",
      AtLeast(2, pubkeyA, pubkeyB, pubkeyC))

    // this example is from the white paper
    val (compiledTree2, compiledProp2) = compileAndCheck(env,
      """{
        |    val array = Coll(pubkeyA, pubkeyB, pubkeyC)
        |    atLeast(array.size, array)
        |}""".stripMargin,
      AtLeast(
        IntConstant(3),
        ConcreteCollection(Array[SigmaPropValue](pubkeyA, pubkeyB, pubkeyC), SSigmaProp)))

    val proof = proverABC.prove(compiledTree2, ctx, fakeMessage).get
    verifier.verify(compiledTree2, ctx, proof, fakeMessage).get._1 shouldBe true

    val nonWorkingProvers2 = Seq(proverA, proverB, proverC, proverAB, proverAC, proverBC, proverD)
    for (prover <- nonWorkingProvers2) {
      prover.prove(compiledTree2, ctx, fakeMessage).isFailure shouldBe true
    }

    {
      val prop2And = mkTestErgoTree(CAND(Seq(pubkeyA, pubkeyB, pubkeyC)))
      val res1 = proverA.fullReduction(compiledTree2, ctx).value
      val res2 = proverA.fullReduction(prop2And, ctx).value
      res1 shouldBe res2
    }

    // this example is from the white paper
    val (compiledTree3, compiledProp3) = compileAndCheck(env,
      """{
        |    val array = Coll(pubkeyA, pubkeyB, pubkeyC)
        |    atLeast(1, array)
        |}""".stripMargin,
      AtLeast(1, pubkeyA, pubkeyB, pubkeyC))

    val workingProvers3 = Seq(proverA, proverB, proverC, proverAB, proverBC, proverAC, proverABC)
    for (prover <- workingProvers3) {
      val proof = prover.prove(compiledTree3, ctx, fakeMessage).get
      verifier.verify(compiledTree3, ctx, proof, fakeMessage).get._1 shouldBe true
    }
    proverD.prove(compiledTree3, ctx, fakeMessage).isFailure shouldBe true

    {
      val prop3Or = COR(Seq(pubkeyA, pubkeyB, pubkeyC)).toSigmaPropValue
      val res1 = testReduce(proverA)(ctx, compiledProp3)
      val res2 = testReduce(proverA)(ctx, prop3Or)
      res1 shouldBe res2
    }

    val (compiledTree4, _) = compileAndCheck(env,
      """{
        |    val array = Coll(pubkeyA, pubkeyB, pubkeyC)
        |    atLeast(2, array)
        |}""".stripMargin,
      AtLeast(2, pubkeyA, pubkeyB, pubkeyC))

    val workingProvers4 = Seq(proverAB, proverBC, proverAC, proverABC)
    for (prover <- workingProvers4) {
      val proof = prover.prove(compiledTree4, ctx, fakeMessage).get
      verifier.verify(compiledTree4, ctx, proof, fakeMessage).get._1 shouldBe true
    }
    val nonWorkingProvers4 = Seq(proverA, proverB, proverC, proverD)
    for (prover <- nonWorkingProvers4) {
      prover.prove(compiledTree4, ctx, fakeMessage).isFailure shouldBe true
    }
  }

  property("threshold reduce to crypto") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val ctx = ErgoLikeContextTesting(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = ErgoLikeTransactionTesting.dummy,
      self = fakeSelf, activatedVersionInTests)
        .withErgoTreeVersion(ergoTreeVersionInTests)

    case class TestCase(numTrue: Int, vector: Seq[SigmaPropValue], dlogOnlyVector: DlogOnlyVector)
    case class DlogOnlyVector(v: Seq[SigmaPropValue]) {
      lazy val orVersion = testReduce(prover)(ctx, SigmaOr(v))
      lazy val andVersion = testReduce(prover)(ctx, SigmaAnd(v))
    }

    // Sequence of three secrets, in order to build test cases with 0, 1, 2, or 3 ProveDlogs inputs
    val secrets = Seq[DLogProverInput](DLogProverInput.random(), DLogProverInput.random()/*, DLogProverInput.random()*/)

    val fls = BoolToSigmaProp(FalseLeaf)
    val tr = BoolToSigmaProp(TrueLeaf)
    val emptyDlogOnlyVector = DlogOnlyVector(Seq())

    // build test cases of length 0 to 3 ProveDlogs with all possible inbetweens: nothing, false, true
    var testCaseSeq = Seq[TestCase](
      TestCase(0, Seq(), emptyDlogOnlyVector),
      TestCase(0, Seq(fls), emptyDlogOnlyVector),
      TestCase(1, Seq(tr), emptyDlogOnlyVector))
    for (sk <- secrets) {
      val pk = SigmaPropConstant(sk.publicImage)
      var newTestCaseSeq = Seq[TestCase]()
      for (t <- testCaseSeq) {
        val dlogOnly = DlogOnlyVector(t.dlogOnlyVector.v :+ pk)
        newTestCaseSeq ++=
          Seq(t,
            TestCase(t.numTrue, t.vector :+ pk, dlogOnly),
            TestCase(t.numTrue, t.vector :+ pk :+ fls, dlogOnly),
            TestCase(t.numTrue + 1, t.vector :+ pk :+ tr, dlogOnly))
      }
      testCaseSeq = newTestCaseSeq
    }


    var case0TrueHit = false
    var case0FalseHit = false
    var case1TrueHit = false
    var case1FalseHit = false
    var case1DLogHit = false
    var case2TrueHit = false
    var case2FalseHit = false
    var case2OrHit = false
    var case2AndHit = false
    var case2AtLeastHit = false

    // for each test case, make into atleast and reduce it to crypto with different thresholds
    for (t <- testCaseSeq) {
      for (bound <- 0 to testCaseSeq.length + 1) {
        val res = testReduce(prover)(ctx, AtLeast(bound, t.vector.toArray))
        if (t.dlogOnlyVector.v.isEmpty) { // Case 0: no ProveDlogs in the test vector -- just booleans
          if (t.numTrue >= bound) {
            res shouldBe TrivialProp.TrueProp
            case0TrueHit = true
          }
          else {
            res shouldBe TrivialProp.FalseProp
            case0FalseHit = true
          }
        }
        else if (t.dlogOnlyVector.v.length == 1) { // Case 1: 1 ProveDlog in the test vector
          // Should be just true if numTrue>=bound
          if (t.numTrue >= bound) {
            res shouldBe TrivialProp.TrueProp
            case1TrueHit = true
          }
          // Should be false if bound>numTrue + 1
          else if (bound > t.numTrue + 1) {
            res shouldBe TrivialProp.FalseProp
            case1FalseHit = true
          }
          // if bound is exactly numTrue+1, should be just dlog
          else if (bound == t.numTrue + 1) {
            SigmaPropConstant(res.asInstanceOf[ProveDlog]) shouldBe t.dlogOnlyVector.v.head
            case1DLogHit = true
          }
        }
        else { // Case 2: more than 1 ProveDlogs in the test vector
          // Should be just true if numTrue>=bound
          if (t.numTrue >= bound) {
            res shouldBe TrivialProp.TrueProp
            case2TrueHit = true
          }
          // Should be false if bound>numTrue + dlogOnlyVector.length
          else if (bound > t.numTrue + t.dlogOnlyVector.v.length) {
            res shouldBe TrivialProp.FalseProp
            case2FalseHit = true
          }
          // if bound is exactly numTrue+dlogOnlyVector, should be just AND of all dlogs
          else if (bound == t.numTrue + t.dlogOnlyVector.v.length) {
            res shouldBe t.dlogOnlyVector.andVersion
            case2AndHit = true

          }
          // if bound is exactly numTrue+1, should be just OR of all dlogs
          else if (bound == t.numTrue + 1) {
            res shouldBe t.dlogOnlyVector.orVersion
            case2OrHit = true
          }
          // else should be AtLeast
          else {
            val atLeastReduced = testReduce(prover)(ctx, AtLeast(bound - t.numTrue, t.dlogOnlyVector.v))
            res shouldBe atLeastReduced
            case2AtLeastHit = true
          }
        }
      }
    }
    case0FalseHit && case0TrueHit shouldBe true
    case1FalseHit && case1TrueHit && case1DLogHit shouldBe true
    case2FalseHit && case2TrueHit && case2AndHit && case2OrHit/* && case2AtLeastHit */ shouldBe true
  }

  property("3-out-of-6 threshold") {
    // This example is from the white paper
    val proverA = new ContextEnrichingTestProvingInterpreter
    val proverB = new ContextEnrichingTestProvingInterpreter
    val proverC = new ContextEnrichingTestProvingInterpreter
    val proverD = new ContextEnrichingTestProvingInterpreter
    val proverE = new ContextEnrichingTestProvingInterpreter
    val proverF = new ContextEnrichingTestProvingInterpreter
    val proverG = new ContextEnrichingTestProvingInterpreter
    val proverH = new ContextEnrichingTestProvingInterpreter
    val proverI = new ContextEnrichingTestProvingInterpreter

    val skA = proverA.dlogSecrets.head
    val skB = proverB.dlogSecrets.head
    val skC = proverC.dlogSecrets.head
    val skD = proverD.dlogSecrets.head
    val skE = proverE.dlogSecrets.head
    val skF = proverF.dlogSecrets.head
    val skG = proverG.dlogSecrets.head
    val skH = proverH.dlogSecrets.head
    val skI = proverI.dlogSecrets.head

    val pkA = skA.publicImage
    val pkB = skB.publicImage
    val pkC = skC.publicImage
    val pkD = skD.publicImage
    val pkE = skE.publicImage
    val pkF = skF.publicImage
    val pkG = skG.publicImage
    val pkH = skH.publicImage
    val pkI = skI.publicImage


    val env = Map("pkA" -> pkA, "pkB" -> pkB, "pkC" -> pkC,
      "pkD" -> pkD, "pkE" -> pkE, "pkF" -> pkF,
      "pkG" -> pkG, "pkH" -> pkH, "pkI" -> pkI)
    val (compiledTree, _) = compileAndCheck(env,
      """atLeast(3, Coll (pkA, pkB, pkC, pkD && pkE, pkF && pkG, pkH && pkI))""",
      AtLeast(3, pkA, pkB, pkC, SigmaAnd(pkD, pkE), SigmaAnd(pkF, pkG), SigmaAnd(pkH, pkI)))

    val badProver = proverH.withSecrets(Seq(skB, skC, skE))
    val goodProver1 = badProver.withSecrets(Seq(skD))
    val goodProver2 = badProver.withSecrets(Seq(skA))
    val goodProver3 = badProver.withSecrets(Seq(skF, skG))
    val goodProver4 = badProver.withSecrets(Seq(skF, skG, skA))

    val goodProvers = Seq(goodProver1, goodProver2, goodProver3, goodProver4)

    val ctx = ErgoLikeContextTesting(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = ErgoLikeTransactionTesting.dummy,
      self = fakeSelf, activatedVersionInTests)

    val verifier = new ErgoLikeTestInterpreter


    for (prover <- goodProvers) {
      val proof = prover.prove(compiledTree, ctx, fakeMessage).get
      verifier.verify(compiledTree, ctx, proof, fakeMessage).get._1 shouldBe true
    }

    badProver.prove(compiledTree, ctx, fakeMessage).isFailure shouldBe true
  }

  property("threshold proving of different trees") {
    val secret1 = DLogProverInput.random()
    val subProp1 = secret1.publicImage
    val secret2 = DLogProverInput.random()
    val subProp2 = secret2.publicImage
    val secret31 = DLogProverInput.random()
    val secret32 = DLogProverInput.random()
    val subProp3 = SigmaOr(secret31.publicImage, secret32.publicImage)
    val secret41 = DLogProverInput.random()
    val secret42 = DLogProverInput.random()
    val subProp4 = SigmaAnd(secret41.publicImage, secret42.publicImage)
    val secret51 = DLogProverInput.random()
    val secret52 = DLogProverInput.random()
    val secret53 = DLogProverInput.random()
    val subProp5 = AtLeast(2, secret51.publicImage, secret52.publicImage, secret53.publicImage)
    val secret6 = DLogProverInput.random()

    val propComponents = Seq[SigmaPropValue](subProp1, subProp2, subProp3, subProp4, subProp5)
    val secrets = Seq(Seq(secret1), Seq(secret2), Seq(secret31), Seq(secret41, secret42), Seq(secret51, secret53))

    // the integer indicates how many subpropositions the prover can prove
    var provers = Seq[(Int, ContextEnrichingTestProvingInterpreter)]((0, new ContextEnrichingTestProvingInterpreter))
    // create 32 different provers
    for (i <- secrets.indices) {
      provers = provers ++ provers.map(p => (p._1 + 1, p._2.withSecrets(secrets(i))))
    }
    val ctx = ErgoLikeContextTesting(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = ErgoLikeTransactionTesting.dummy,
      self = fakeSelf, activatedVersionInTests)

    val verifier = new ErgoLikeTestInterpreter

    def canProve(prover: ContextEnrichingTestProvingInterpreter, proposition: SigmaPropValue): Unit = {
      val tree = mkTestErgoTree(proposition)
      val proof = prover.prove(tree, ctx, fakeMessage).get
      verifier.verify(tree, ctx, proof, fakeMessage).get._1 shouldBe true
    }

    def cannotProve(prover: ContextEnrichingTestProvingInterpreter, proposition: SigmaPropValue): Unit = {
      prover.prove(mkTestErgoTree(proposition), ctx, fakeMessage).isFailure shouldBe true
    }

    var twoToi = 1
    for (i <- 0 to secrets.length) {
      for (bound <- 1 to i) {
        // don't go beyond i -- "threshold reduce to crypto" tests that atLeast then becomes false
        // don't test bound 0 -- "threshold reduce to crypto" tests that atLeast then becomes true
        val pureAtLeastProp = AtLeast(bound, propComponents.slice(0, i).toArray)
        val OrPlusAtLeastOnRightProp = SigmaOr(secret6.publicImage, pureAtLeastProp)
        val OrPlusAtLeastOnLeftProp = SigmaOr(pureAtLeastProp, secret6.publicImage)
        val AndPlusAtLeastOnLeftProp = SigmaAnd(pureAtLeastProp, secret6.publicImage)
        val AndPlusAtLeastOnRightProp = SigmaAnd(secret6.publicImage, pureAtLeastProp)

        for (p <- provers.slice(0, twoToi)) {
          val pWithSecret6 = p._2.withSecrets(Seq(secret6))
          // only consider first 2^i provers, because later provers have secrets that are not relevant to this proposition
          if (p._1 >= bound) { // enough secrets for at least
            // prover should be able to prove pure and both ors
            canProve(p._2, pureAtLeastProp)
            canProve(p._2, OrPlusAtLeastOnRightProp)
            canProve(p._2, OrPlusAtLeastOnLeftProp)

            // prover should be unable to prove ands
            cannotProve(p._2, AndPlusAtLeastOnRightProp)
            cannotProve(p._2, AndPlusAtLeastOnLeftProp)

            // prover with more secrets should be able to prove ands
            canProve(pWithSecret6, AndPlusAtLeastOnRightProp)
            canProve(pWithSecret6, AndPlusAtLeastOnLeftProp)

          } else { // not enough secrets for atLeast
            // prover should be unable to prove pure and both ors and both ands
            cannotProve(p._2, pureAtLeastProp)
            cannotProve(p._2, OrPlusAtLeastOnRightProp)
            cannotProve(p._2, OrPlusAtLeastOnLeftProp)
            cannotProve(p._2, AndPlusAtLeastOnRightProp)
            cannotProve(p._2, AndPlusAtLeastOnLeftProp)

            // prover with more secrets should be able to prove both ors
            canProve(pWithSecret6, OrPlusAtLeastOnRightProp)
            canProve(pWithSecret6, OrPlusAtLeastOnLeftProp)

            // prover with more secrets should still be unable to prove pure and both ands
            cannotProve(pWithSecret6, pureAtLeastProp)
            cannotProve(pWithSecret6, AndPlusAtLeastOnRightProp)
            cannotProve(pWithSecret6, AndPlusAtLeastOnLeftProp)
          }
        }

      }
      twoToi *= 2
    }
  }

  property("fail compilation when input limit exceeded") {
    val proverA = new ContextEnrichingTestProvingInterpreter
    val skA = proverA.dlogSecrets.head
    val pubkeyA = skA.publicImage
    val keyName = "pubkeyA"
    val env = Map(keyName -> pubkeyA)
    val pubKeysStrExceeding = Array.fill[String](AtLeast.MaxChildrenCount + 1)(keyName).mkString(",")
    an[GraphBuildingException] should be thrownBy compile(env, s"""atLeast(2, Coll($pubKeysStrExceeding))""")
    an[GraphBuildingException] should be thrownBy
      compile(env, s"""{ val arr = Coll($pubKeysStrExceeding); atLeast(2, arr) }""")

    // max children should work fine
    val pubKeysStrMax = Array.fill[String](AtLeast.MaxChildrenCount)(keyName).mkString(",")
    compile(env, s"""atLeast(2, Coll($pubKeysStrMax))""")
    compile(env, s"""{ val arr = Coll($pubKeysStrMax); atLeast(2, arr) }""")

    // collection with unknown items should pass
    compile(env, s"""atLeast(2, getVar[Coll[SigmaProp]](1).get)""")
  }
}
