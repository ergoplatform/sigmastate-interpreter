package org.ergoplatform

import org.ergoplatform.ErgoAddressEncoder.TestnetNetworkPrefix
import org.ergoplatform.ErgoBox.R4
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.settings.MonetarySettings
import org.scalacheck.Gen
import scorex.crypto.hash.Blake2b256
import scorex.util.Random
import sigma.Colls
import sigma.ast._
import sigma.ast.syntax.CollectionConstant
import sigma.data.{AvlTreeData, Digest32Coll, ProveDlog, TrivialProp}
import sigma.util.BenchmarkUtil.measure
import sigmastate._
import sigmastate.crypto.DLogProtocol.DLogProverInput
import sigmastate.helpers.{CompilerTestingCommons, ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTestInterpreter}
import sigmastate.helpers.TestingHelpers._
import sigmastate.interpreter.Interpreter.{ScriptNameProp, emptyEnv}
import sigma.ast.syntax.ValueOps
import sigma.interpreter.{ContextExtension, ProverResult}
import sigma.serialization.ValueSerializer
import sigmastate.utils.Helpers._

import scala.util.Try

class ErgoTreePredefSpec extends CompilerTestingCommons with CompilerCrossVersionProps {
  private implicit lazy val IR: TestingIRContext = new TestingIRContext {
  }

  private val emptyProverResult: ProverResult = ProverResult(Array.emptyByteArray, ContextExtension.empty)
  private val settings = MonetarySettings(30 * 2 * 24 * 365, 90 * 24 * 30, 75L * EmissionRules.CoinsInOneErgo,
    3L * EmissionRules.CoinsInOneErgo, 720, 75L * EmissionRules.CoinsInOneErgo / 10)
  private val emission = new EmissionRules(settings)

  property("boxCreationHeight") {
    val verifier = new ErgoLikeTestInterpreter
    val prover = new ContextEnrichingTestProvingInterpreter
    val minerProp = prover.dlogSecrets.head.publicImage
    val pk = minerProp.pkBytes

    val nextHeight = 1
    val prop = EQ(Height, ErgoTreePredef.boxCreationHeight(ByIndex(Outputs, IntConstant(0)))).toSigmaProp
    val propInlined = EQ(Height, SelectField(ExtractCreationInfo(ByIndex(Outputs, IntConstant(0))), 1).asIntValue).toSigmaProp
    prop shouldBe propInlined
    val propTree = mkTestErgoTree(prop)
    val inputBox = testBox(1, propTree, nextHeight, Seq(), Map())
    val inputBoxes = IndexedSeq(inputBox)
    val inputs = inputBoxes.map(b => Input(b.id, emptyProverResult))
    val minerBox = new ErgoBoxCandidate(1, mkTestErgoTree(SigmaPropConstant(minerProp)), nextHeight)

    val spendingTransaction = ErgoLikeTransaction(inputs, IndexedSeq(minerBox))

    val ctx = ErgoLikeContextTesting(
      currentHeight = nextHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = pk,
      boxesToSpend = inputBoxes,
      spendingTransaction,
      self = inputBox, activatedVersionInTests)
    val pr = prover.prove(emptyEnv + (ScriptNameProp -> "boxCreationHeight_prove"), propTree, ctx, fakeMessage).get
    verifier.verify(emptyEnv + (ScriptNameProp -> "boxCreationHeight_verify"), propTree, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("collect coins from the founders' box") {
    def remaining(h: Int) = emission.remainingFoundationRewardAtHeight(h)

    val prover = new ContextEnrichingTestProvingInterpreter
    val prop = ErgoTreePredef.foundationScript(settings)

    def R4Prop(ableToProve: Boolean): CollectionConstant[SByte.type] = if (ableToProve) {
      val pks = (DLogProverInput.random() +: prover.dlogSecrets.take(2)).map(s => SigmaPropConstant(s.publicImage))
      ByteArrayConstant(ValueSerializer.serialize(AtLeast(IntConstant(2), pks.toArray)))
    } else {
      val pk = (new ContextEnrichingTestProvingInterpreter).dlogSecrets.head.publicImage
      ByteArrayConstant(ValueSerializer.serialize(SigmaPropConstant(pk)))
    }

    val verifier = new ErgoLikeTestInterpreter

    checkAtHeight(1)
    checkAtHeight(settings.fixedRatePeriod)
    checkAtHeight(settings.fixedRatePeriod + 1)
    checkAtHeight(settings.fixedRatePeriod + settings.epochLength)
    checkAtHeight(settings.fixedRatePeriod + settings.epochLength + 1)
    checkAtHeight(settings.fixedRatePeriod + 2 * settings.epochLength)
    checkAtHeight(settings.fixedRatePeriod + 2 * settings.epochLength + 1)

    def checkAtHeight(height: Int) = {
      // collect correct amount of coins, correct new script, able to satisfy R4 conditions
      checkSpending(remaining(height), height, prop, R4Prop(true)).isSuccess shouldBe true
      // unable to satisfy R4 conditions
      checkSpending(remaining(height), height, prop, R4Prop(false)).isFailure shouldBe true
      // incorrect new script
      checkSpending(remaining(height), height,
        ErgoTree.fromProposition(TrivialProp.TrueProp),
        R4Prop(true)).isFailure shouldBe true
      // collect less coins then possible
      checkSpending(remaining(height) + 1, height, prop, R4Prop(true)).isSuccess shouldBe true
      // collect more coins then possible
      checkSpending(remaining(height) - 1, height, prop, R4Prop(true)).isFailure shouldBe true
    }

    def checkSpending(remainingAmount: Long,
                      height: Int,
                      newProp: ErgoTree,
                      inputR4Val: CollectionConstant[SByte.type]): Try[Unit] = Try {
      val outputR4Val: CollectionConstant[SByte.type] = ByteArrayConstant(Random.randomBytes())
      val inputBoxes = IndexedSeq(testBox(emission.foundersCoinsTotal, prop, 0, Seq(), Map(R4 -> inputR4Val)))
      val inputs = inputBoxes.map(b => Input(b.id, emptyProverResult))
      val newFoundersBox = testBox(remainingAmount, newProp, 0, Seq(), Map(R4 -> outputR4Val))
      val collectedBox = testBox(inputBoxes.head.value - remainingAmount, TrueTree, 0)
      val spendingTransaction = ErgoLikeTransaction(inputs, IndexedSeq(newFoundersBox, collectedBox))
      val ctx = ErgoLikeContextTesting(
        currentHeight = height,
        lastBlockUtxoRoot = AvlTreeData.dummy,
        minerPubkey = ErgoLikeContextTesting.dummyPubkey,
        boxesToSpend = inputBoxes,
        spendingTransaction,
        self = inputBoxes.head, activatedVersionInTests)
      val pr = prover.prove(emptyEnv + (ScriptNameProp -> "checkSpending_prove"), prop, ctx, fakeMessage).get
      verifier.verify(emptyEnv + (ScriptNameProp -> "checkSpending_verify"), prop, ctx, pr, fakeMessage).get._1 shouldBe true
    }
  }

  property("collect coins from rewardOutputScript") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val minerPk = prover.dlogSecrets.head.publicImage
    val prop = ErgoTreePredef.rewardOutputScript(settings.minerRewardDelay, minerPk)
    val verifier = new ErgoLikeTestInterpreter
    val inputBoxes = IndexedSeq(testBox(20, prop, 0, Seq(), Map()))
    val inputs = inputBoxes.map(b => Input(b.id, emptyProverResult))
    val spendingTransaction = ErgoLikeTransaction(inputs, IndexedSeq(testBox(inputBoxes.head.value, TrueTree, 0)))

    val ctx = ErgoLikeContextTesting(
      currentHeight = inputBoxes.head.creationHeight + settings.minerRewardDelay,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = inputBoxes,
      spendingTransaction,
      self = inputBoxes.head, activatedVersionInTests)
    val prevBlockCtx = ErgoLikeContextTesting(
      currentHeight = inputBoxes.head.creationHeight + settings.minerRewardDelay - 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = inputBoxes,
      spendingTransaction,
      self = inputBoxes.head, activatedVersionInTests)

    // should not be able to collect before minerRewardDelay
    val prove = prover.prove(emptyEnv + (ScriptNameProp -> "rewardOutputScript_prove"), prop, ctx, fakeMessage).get
    verifier.verify(emptyEnv + (ScriptNameProp -> "rewardOutputScript_verify"), prop, prevBlockCtx, prove, fakeMessage)
      .getOrThrow should matchPattern { case (false,_) => }

    // should be able to collect after minerRewardDelay
    val pr = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), prop, ctx, fakeMessage).getOrThrow
    verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), prop, ctx, pr, fakeMessage).getOrThrow._1 shouldBe true
  }

  property("create transaction collecting the emission box") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val minerPk = prover.dlogSecrets.head.publicImage
    val prop = ErgoTreePredef.emissionBoxProp(settings)
    val emissionBox = testBox(emission.coinsTotal, prop, 0, Seq(), Map())
    val minerProp = ErgoTreePredef.rewardOutputScript(settings.minerRewardDelay, minerPk)

    // collect coins during the fixed rate period
    forAll(Gen.choose(1, settings.fixedRatePeriod)) { height =>
      val currentRate = emission.minersRewardAtHeight(height)
      createRewardTx(currentRate, height, minerProp).getOrThrow
      createRewardTx(currentRate + 1, height, minerProp).isFailure shouldBe true
      createRewardTx(currentRate - 1, height, minerProp).isFailure shouldBe true
    }

    // collect coins after the fixed rate period
    forAll(Gen.choose(1, emission.blocksTotal - 1)) { height =>
      val currentRate = emission.minersRewardAtHeight(height)
      createRewardTx(currentRate, height, minerProp).isSuccess shouldBe true
      createRewardTx(currentRate + 1, height, minerProp).isFailure shouldBe true
      createRewardTx(currentRate - 1, height, minerProp).isFailure shouldBe true
    }

    // collect coins to incorrect proposition
    forAll(Gen.choose(1, emission.blocksTotal - 1)) { height =>
      val currentRate = emission.minersRewardAtHeight(height)
      val pk2 = prover.dlogSecrets(1).publicImage
      val correctProp = ErgoTreePredef.rewardOutputScript(settings.minerRewardDelay, minerPk)
      val incorrectDelay = ErgoTreePredef.rewardOutputScript(settings.minerRewardDelay + 1, minerPk)
      val incorrectPk = ErgoTreePredef.rewardOutputScript(settings.minerRewardDelay, pk2)
      createRewardTx(currentRate, height, correctProp).isSuccess shouldBe true
      createRewardTx(currentRate, height, incorrectDelay).isFailure shouldBe true
      createRewardTx(currentRate, height, incorrectPk).isFailure shouldBe true
      createRewardTx(currentRate, height,
        ErgoTree.fromSigmaBoolean(minerPk)).isFailure shouldBe true
    }

    def createRewardTx(emissionAmount: Long, nextHeight: Int, minerProp: ErgoTree): Try[ErgoLikeTransaction] = {
      checkRewardTx(minerPk,
        minerProp,
        emissionBox,
        emissionAmount,
        nextHeight)(prover)
    }

  }

  property("tokenThreshold") {
    val prover = new ContextEnrichingTestProvingInterpreter()
    val verifier = new ErgoLikeTestInterpreter()

    val pubkey = prover.dlogSecrets.head.publicImage
    val pubkeyTree = mkTestErgoTree(pubkey)

    val tokenId = Digest32Coll @@@ Colls.fromArray(Blake2b256("id"))
    val wrongId = Digest32Coll @@@ Colls.fromArray(Blake2b256(tokenId.toArray))
    val wrongId2 = Digest32Coll @@@ Colls.fromArray(Blake2b256(wrongId.toArray))
    val tokenAmount: Int = 50

    val prop = mkTestErgoTree(ErgoScriptPredef.tokenThresholdScript(tokenId.toArray, tokenAmount, TestnetNetworkPrefix))

    def check(inputBoxes: IndexedSeq[ErgoBox]): Try[Unit] = Try {
      val inputs = inputBoxes.map(b => Input(b.id, emptyProverResult))
      val amount = inputBoxes.map(_.value).sum
      val spendingTransaction = ErgoLikeTransaction(inputs, IndexedSeq(testBox(amount, pubkeyTree, 0)))

      val ctx = ErgoLikeContextTesting(
        currentHeight = 50,
        lastBlockUtxoRoot = AvlTreeData.dummy,
        minerPubkey = ErgoLikeContextTesting.dummyPubkey,
        boxesToSpend = inputBoxes,
        spendingTransaction,
        self = inputBoxes.head,
        activatedVersionInTests).withCostLimit(scriptCostLimitInTests * 10)

      val pr = prover.prove(emptyEnv + (ScriptNameProp -> "tokenThresholdScript_prove"), prop, ctx, fakeMessage).getOrThrow
      verifier.verify(emptyEnv + (ScriptNameProp -> "tokenThresholdScript_verify"), prop, ctx, pr, fakeMessage).getOrThrow._1 shouldBe true
    }


    measure(10) { _ =>
      // transaction with the only input with enough token should pass
      val inputs0 = IndexedSeq(
        testBox(20, prop, 0, Seq((wrongId, tokenAmount), (tokenId, tokenAmount), (wrongId2, tokenAmount)), Map())
      )
      check(inputs0).get shouldBe (())

      // transaction with the only input with insufficient token should fail
      val inputs1 = IndexedSeq(
        testBox(20, prop, 0, Seq((wrongId, tokenAmount), (tokenId, tokenAmount - 1)), Map())
      )
      check(inputs1).isFailure shouldBe true

      // transaction with multiple inputs with insufficient token should fail
      val inputs2 = IndexedSeq(
        testBox(20, prop, 0, Seq((wrongId, tokenAmount), (tokenId, tokenAmount - 2)), Map()),
        testBox(20, prop, 0, Seq((wrongId, tokenAmount)), Map()),
        testBox(20, prop, 0, Seq((tokenId, 1), (wrongId2, tokenAmount)), Map())
      )
      check(inputs2).isFailure shouldBe true

      // transaction with multiple inputs with enough token should pass
      val inputs3 = IndexedSeq(
        testBox(20, prop, 0, Seq((wrongId, 1), (tokenId, tokenAmount / 2)), Map()),
        testBox(20, prop, 0, Seq((wrongId, 1)), Map()),
        testBox(20, prop, 0, Seq((tokenId, tokenAmount / 2 + 1), (wrongId2, 1)), Map())
      )
      check(inputs3).getOrThrow

      // A transaction which contains input with no tokens
      val inputs4 = IndexedSeq(
        testBox(20, prop, 0, Seq((wrongId, 1), (tokenId, tokenAmount / 2)), Map()),
        testBox(20, prop, 0, Seq(), Map()),
        testBox(20, prop, 0, Seq((tokenId, tokenAmount / 2 + 1), (wrongId2, 1)), Map())
      )
      check(inputs4).isSuccess shouldBe true
    }
    /*
    Iter 0: 777 ms
    Iter 1: 353 ms
    Iter 2: 304 ms
    Iter 10: 110 ms
    Iter 30: 80 ms
    Iter 40: 72 ms
    Iter 60: 68 ms
    */
  }

  def checkRewardTx(minerPk: ProveDlog,
                    minerProp: ErgoTree,
                    emissionBox: ErgoBox,
                    emissionAmount: Long,
                    nextHeight: Int)(prover: ContextEnrichingTestProvingInterpreter): Try[ErgoLikeTransaction] = Try {
    val verifier = new ErgoLikeTestInterpreter
    val prop = emissionBox.ergoTree
    val inputBoxes = IndexedSeq(emissionBox)
    val inputs = inputBoxes.map(b => Input(b.id, emptyProverResult))
    val pkBytes = minerPk.pkBytes

    val newEmissionBox = new ErgoBoxCandidate(emissionBox.value - emissionAmount, prop, nextHeight)
    val minerBox = new ErgoBoxCandidate(emissionAmount, minerProp, nextHeight)

    val spendingTransaction = ErgoLikeTransaction(inputs, IndexedSeq(newEmissionBox, minerBox))

    val ctx = ErgoLikeContextTesting(
      currentHeight = nextHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = pkBytes,
      boxesToSpend = inputBoxes,
      spendingTransaction,
      self = inputBoxes.head, activatedVersionInTests)
    val pr = prover.prove(emptyEnv + (ScriptNameProp -> "checkRewardTx_prove"), prop, ctx, fakeMessage).getOrThrow
    verifier.verify(emptyEnv + (ScriptNameProp -> "checkRewardTx_verify"), prop, ctx, pr, fakeMessage).getOrThrow._1 shouldBe true
    spendingTransaction
  }

}
