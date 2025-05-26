package sigmastate.utxo.examples

import org.ergoplatform._
import org.ergoplatform.settings.ErgoAlgos
import sigma.Colls
import sigma.ast.syntax.OptionValueOps
import sigma.ast._
import sigmastate._
import sigmastate.helpers.TestingHelpers._
import sigmastate.helpers.{CompilerTestingCommons, ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting}
import sigma.interpreter.ContextExtension
import sigmastate.interpreter.Interpreter.{ScriptNameProp, emptyEnv}
import sigma.ast.syntax._
import sigmastate.utxo.blockchain.BlockchainSimulationTestingCommons._

/**
  * An example of currency emission contract.
  * Instead of having implicit emission via coinbase transaction, we put 1 coin into genesis state with a script
  * that controls emission.
  * This script is corresponding to the whitepaper. Please note that Ergo has different contract
  * defined in ErgoScriptPredef.
  */
class CoinEmissionSpecification extends CompilerTestingCommons
  with CompilerCrossVersionProps {
  // don't use TestingIRContext, this suite also serves the purpose of testing the RuntimeIRContext
  implicit lazy val IR: TestingIRContext = new TestingIRContext {
    // uncomment if you want to log script evaluation
    // override val okPrintEvaluatedEntries = true
    saveGraphsInFile = false
  }

  protected def logMessage(msg: String) = {
    println(msg)
  }

  private val reg1 = ErgoBox.nonMandatoryRegisters.head

  private val coinsInOneErgo: Long = 100000000
  private val blocksPerHour: Int = 30

  case class MonetarySettings(fixedRatePeriod: Int,
                              epochLength: Int,
                              fixedRate: Long,
                              oneEpochReduction: Long)

  val s = MonetarySettings(blocksPerHour * 24 * 7, 24 * blocksPerHour, 15 * coinsInOneErgo, 3 * coinsInOneErgo)

  val (coinsTotal, blocksTotal) = {
    def loop(height: Int, acc: Long): (Long, Int) = {
      val currentRate = emissionAtHeight(height)
      if (currentRate > 0) {
        loop(height + 1, acc + currentRate)
      } else {
        (acc, height - 1)
      }
    }

    loop(0, 0)
  }

  def emissionAtHeight(h: Int): Long = {
    if (h < s.fixedRatePeriod) {
      s.fixedRate
    } else {
      val epoch = 1 + (h - s.fixedRatePeriod) / s.epochLength
      Math.max(s.fixedRate - s.oneEpochReduction * epoch, 0)
    }
  }.ensuring(_ >= 0, s"Negative at $h")


  /*
block 0 in 0 ms, 2430000000000 coins remain, defs: 0
block 0 in 41 ms, 2430000000000 coins remain
block 100 in 1292 ms, 2280000000000 coins remain, defs: 61661
block 200 in 965 ms, 2130000000000 coins remain, defs: 61661
block 300 in 991 ms, 1980000000000 coins remain, defs: 61661
block 400 in 842 ms, 1830000000000 coins remain, defs: 61661
block 500 in 833 ms, 1680000000000 coins remain, defs: 61661
block 600 in 788 ms, 1530000000000 coins remain, defs: 61661
block 700 in 903 ms, 1380000000000 coins remain, defs: 61661
block 800 in 789 ms, 1230000000000 coins remain, defs: 61661
block 900 in 774 ms, 1080000000000 coins remain, defs: 61661
block 1000 in 753 ms, 930000000000 coins remain, defs: 61661
block 1000 in 8889 ms, 930000000000 coins remain
block 1100 in 764 ms, 780000000000 coins remain, defs: 61661
block 1200 in 886 ms, 630000000000 coins remain, defs: 61661
block 1300 in 1371 ms, 480000000000 coins remain, defs: 61661
block 1400 in 1908 ms, 330000000000 coins remain, defs: 61661
block 1500 in 1626 ms, 180000000000 coins remain, defs: 61661
block 1600 in 1622 ms, 30000000000 coins remain, defs: 61661
  */
  property("emission specification") {
    val register = reg1
    val prover = new ContextEnrichingTestProvingInterpreter()

    val rewardOut = ValUse(1, SBox)

    val epoch =
      Upcast(
        Plus(IntConstant(1), Divide(Minus(Height, IntConstant(s.fixedRatePeriod)), IntConstant(s.epochLength))),
        SLong)

    val coinsToIssue = If(LT(Height, IntConstant(s.fixedRatePeriod)),
      s.fixedRate,
      Minus(s.fixedRate, Multiply(s.oneEpochReduction, epoch))
    )
    val sameScriptRule = EQ(ExtractScriptBytes(Self), ExtractScriptBytes(rewardOut))
    val heightCorrect = EQ(ExtractRegisterAs[SInt.type](rewardOut, register).get, Height)
    val correctCoinsConsumed = EQ(
      coinsToIssue,
      Minus(ValUse(3, SLong), ExtractAmount(rewardOut))
    )

    val prop = BlockValue(
      Array(
        ValDef(1, List(), ByIndex(Outputs, IntConstant(0), None)),
        ValDef(2, List(), GT(Height, OptionGet(ExtractRegisterAs(Self, ErgoBox.R4, SOption(SInt))))),
        ValDef(3, List(), ExtractAmount(Self))
      ),
      BoolToSigmaProp(BinOr(
        AND(heightCorrect, ValUse(2, SBoolean), sameScriptRule, correctCoinsConsumed),
        BinAnd(
          ValUse(2, SBoolean),
          LE(ValUse(3, SLong), LongConstant(s.oneEpochReduction)))
      ))
    ).asSigmaProp
    val tree = mkTestErgoTree(prop)

    val env = Map("fixedRatePeriod" -> s.fixedRatePeriod,
      "epochLength" -> s.epochLength,
      "fixedRate" -> s.fixedRate,
      "oneEpochReduction" -> s.oneEpochReduction)

    val prop1 = compile(env,
      """{
        |    val epoch = 1 + ((HEIGHT - fixedRatePeriod) / epochLength)
        |    val out = OUTPUTS(0)
        |    val coinsToIssue = if(HEIGHT < fixedRatePeriod) fixedRate else fixedRate - (oneEpochReduction * epoch)
        |    val correctCoinsConsumed = coinsToIssue == (SELF.value - out.value)
        |    val sameScriptRule = SELF.propositionBytes == out.propositionBytes
        |    val heightIncreased = HEIGHT > SELF.R4[Int].get
        |    val heightCorrect = out.R4[Int].get == HEIGHT
        |    val lastCoins = SELF.value <= oneEpochReduction
        |    sigmaProp(allOf(Coll(heightCorrect, heightIncreased, sameScriptRule, correctCoinsConsumed)) || (heightIncreased && lastCoins))
        |}""".stripMargin).asSigmaProp

    prop1 shouldEqual prop

    val minerImage = prover.dlogSecrets.head.publicImage
    val minerPubkey = minerImage.pkBytes
    val minerProp = minerImage

    val initialBoxCandidate: ErgoBox = testBox(coinsTotal / 4,
      ErgoTree.fromProposition(ergoTreeHeaderInTests, prop),
      0, Seq(), Map(register -> IntConstant(-1)))
    val initBlock = FullBlock(IndexedSeq(createTransaction(initialBoxCandidate)), minerPubkey)
    val genesisState = ValidationState.initialState(activatedVersionInTests, initBlock)
    val fromState = genesisState.boxesReader.byId(genesisState.boxesReader.allIds.head).get
    val initialBox = new ErgoBox(initialBoxCandidate.value, initialBoxCandidate.ergoTree,
      initialBoxCandidate.additionalTokens, initialBoxCandidate.additionalRegisters,
      initBlock.txs.head.id, 0, 0)
    initialBox shouldBe fromState

    def genCoinbaseLikeTransaction(state: ValidationState,
                                   emissionBox: ErgoBox,
                                   height: Int): ErgoLikeTransaction = {
      assert(state.state.currentHeight == height - 1)
      val ut = if (emissionBox.value > s.oneEpochReduction) {
        val minerBox = new ErgoBoxCandidate(emissionAtHeight(height), ErgoTree.fromSigmaBoolean(minerProp), height, Colls.emptyColl, Map())
        val newEmissionBox: ErgoBoxCandidate =
          new ErgoBoxCandidate(emissionBox.value - minerBox.value, tree, height, Colls.emptyColl, Map(register -> IntConstant(height)))

        UnsignedErgoLikeTransaction(
          IndexedSeq(new UnsignedInput(emissionBox.id)),
          IndexedSeq(newEmissionBox, minerBox)
        )
      } else {
        val minerBox1 = new ErgoBoxCandidate(emissionBox.value - 1, ErgoTree.fromSigmaBoolean(minerProp), height, Colls.emptyColl, Map(register -> IntConstant(height)))
        val minerBox2 = new ErgoBoxCandidate(1, ErgoTree.fromSigmaBoolean(minerProp), height, Colls.emptyColl, Map(register -> IntConstant(height)))
        UnsignedErgoLikeTransaction(
          IndexedSeq(new UnsignedInput(emissionBox.id)),
          IndexedSeq(minerBox1, minerBox2)
        )
      }

      val context = ErgoLikeContextTesting(height,
        state.state.lastBlockUtxoRoot,
        minerPubkey,
        IndexedSeq(emissionBox),
        ut,
        emissionBox,
        activatedVersionInTests,
        ContextExtension.empty)
      val proverResult = prover.prove(
        emptyEnv + (ScriptNameProp -> "prove"), tree, context, ut.messageToSign).get
      ut.toSigned(IndexedSeq(proverResult))
    }

    var st = System.currentTimeMillis()
    var thousandTime = System.currentTimeMillis()

    def chainGen(state: ValidationState,
                 emissionBox: ErgoBox,
                 height: Int,
                 hLimit: Int): Unit = if (height < hLimit) {
      if (height % 100 == 0) {
        val t = System.currentTimeMillis()
        if (printDebugInfo)
          println(s"block $height in ${t - st} ms, ${emissionBox.value} coins remain, defs: ${IR.defCount}")
        st = t
        IR.resetContext()
      }
      if (height % 1000 == 0) {
        val t = System.currentTimeMillis()
        if (printDebugInfo)
          println(s"block $height in ${t - thousandTime} ms, ${emissionBox.value} coins remain")
        thousandTime = t
      }
      val tx = genCoinbaseLikeTransaction(state, emissionBox, height)
      val block = FullBlock(IndexedSeq(tx), minerPubkey)
      val newState = state.applyBlock(block).get
      if (tx.outputs.last.value > 1) {
        val newEmissionBox = newState.boxesReader.byId(tx.outputs.head.id).get
        chainGen(newState, newEmissionBox, height + 1, hLimit)
      } else {
        logMessage(s"Emission box is consumed at height $height")
      }
    }

    chainGen(genesisState, initialBox, 0, 1000000)

    if (printDebugInfo)
      println(s"Emission Tree: ${ErgoAlgos.encode(tree.bytes)}")
  }
}
