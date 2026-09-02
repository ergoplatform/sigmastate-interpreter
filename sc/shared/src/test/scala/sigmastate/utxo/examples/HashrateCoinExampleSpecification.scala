package sigmastate.utxo.examples

import org.ergoplatform._
import org.ergoplatform.validation.ValidationRules
import sigma.ast._
import sigma.util.NBitsUtils
import sigma.data.AvlTreeData
import sigma.interpreter.ContextExtension
import sigma.{Colls, VersionContext}
import sigma.serialization.{GroupElementSerializer, SigmaSerializer}
import sigma.util.Extensions.EcpOps
import sigmastate.eval.CPreHeader
import sigmastate.CompilerCrossVersionProps
import sigmastate.helpers._
import sigmastate.helpers.TestingHelpers._
import sigmastate.interpreter.CErgoTreeEvaluator

import java.math.BigInteger
import scala.math.BigInt

/**
 * HashrateCoin Example:
 * A trustless oracle-based derivative token whose price adjusts based on mining
 * difficulty changes.
 *
 * Based on the ErgoForum post: https://www.ergoforum.org/t/blockchain-based-trustless-derivatives-hashratecoin-and-randomcoin/4999
 *
 * Concept (from the post):
 * - Set initial price and initial difficulty in a trustless oracle box
 * - On every difficulty epoch (128 blocks), readjust price according to difficulty change
 * - E.g. if price is 10 ERG per Hashrate coin, and difficulty raised up by 2% in an
 *   epoch, next price is 10.2 ERG per Hashrate coin
 * - On top of the oracle, derivative instruments can be built (dexyHashrateCoin,
 *   GluonHashrateCoin, hodlHashrateCoin etc.), allowing miners to hedge against
 *   difficulty growth
 *
 * The oracle is trustless because the difficulty is read from the blockchain itself:
 * the upcoming block's difficulty is encoded in the pre-header's nBits field and
 * decoded with Global.decodeNbits. No trusted party signs off the price update.
 *
 * Uses ErgoTree v3 / V6 features (Global.decodeNbits), so every property runs with
 * the V6 soft-fork activated.
 */
class HashrateCoinExampleSpecification extends CompilerTestingCommons with CompilerCrossVersionProps {
  private implicit lazy val IR: TestingIRContext = new TestingIRContext

  // Global.decodeNbits is a v6.0/ErgoTree-v3 feature: run all properties with
  // the V6 soft-fork activated and ErgoTree v3 only
  override protected val activatedVersions: Seq[Byte] = Seq(VersionContext.V6SoftForkVersion)
  override val ergoTreeVersions: Seq[Byte] = Seq(VersionContext.V6SoftForkVersion)

  /**
    * Trustless oracle: reads the difficulty of the upcoming block from
    * CONTEXT.preHeader.nBits and forces the output box to carry the new difficulty
    * and the price adjusted by new_price = old_price * new_difficulty / old_difficulty.
    */
  private val trustlessOracleScript = """
    |{
    |  // Difficulty of the upcoming block, decoded from the pre-header's nBits field.
    |  // This makes the oracle trustless: the price is derived from blockchain state.
    |  val currentDifficulty = Global.decodeNbits(CONTEXT.preHeader.nBits)
    |
    |  val previousDifficulty = SELF.R4[BigInt].get
    |  val previousPrice = SELF.R5[BigInt].get
    |
    |  // new_price = old_price * new_difficulty / old_difficulty
    |  // (BigInt arithmetic: no Long overflow on real difficulty/price magnitudes)
    |  val currentPrice = (previousPrice * currentDifficulty) / previousDifficulty
    |
    |  val outputBox = OUTPUTS(0)
    |  val hasCorrectDifficulty = outputBox.R4[BigInt].get == currentDifficulty
    |  val hasCorrectPrice = outputBox.R5[BigInt].get == currentPrice
    |
    |  sigmaProp(hasCorrectDifficulty && hasCorrectPrice)
    |}
    |""".stripMargin

  /** Same as the trustless oracle, but updates are only accepted on difficulty epoch
    * boundaries (every 128 blocks), matching Ergo's difficulty adjustment schedule. */
  private val epochOracleScript = """
    |{
    |  val epochLength = 128
    |  val isEpochBoundary = (HEIGHT % epochLength) == 0
    |
    |  if (isEpochBoundary) {
    |    val currentDifficulty = Global.decodeNbits(CONTEXT.preHeader.nBits)
    |
    |    val previousDifficulty = SELF.R4[BigInt].get
    |    val previousPrice = SELF.R5[BigInt].get
    |    val currentPrice = (previousPrice * currentDifficulty) / previousDifficulty
    |
    |    val outputBox = OUTPUTS(0)
    |    val hasCorrectDifficulty = outputBox.R4[BigInt].get == currentDifficulty
    |    val hasCorrectPrice = outputBox.R5[BigInt].get == currentPrice
    |
    |    sigmaProp(hasCorrectDifficulty && hasCorrectPrice)
    |  } else {
    |    // no updates between difficulty adjustment epochs
    |    sigmaProp(false)
    |  }
    |}
    |""".stripMargin

  /** Example derivative instrument: pays out iff the oracle-reported difficulty grew
    * by more than 5% over the reference difficulty. */
  private val derivativeScript = """
    |{
    |  val oracleBox = CONTEXT.dataInputs(0)
    |  val currentDifficulty = oracleBox.R4[BigInt].get
    |  val difficultyThreshold = 1050000.toBigInt // 5% increase over the initial 1000000
    |
    |  sigmaProp(currentDifficulty >= difficultyThreshold)
    |}
    |""".stripMargin

  private val initialDifficulty: Long = 1000000L
  private val raisedDifficulty: Long = 1100000L  // +10%, above the 5% derivative threshold
  private val smallRaiseDifficulty: Long = 1040000L // +4%, below the threshold
  private val initialPrice: BigInt = BigInt(10000000000L) // 10 ERG in nanoERG
  // +10% difficulty => +10% price (exact: BigInt arithmetic, no rounding)
  private val raisedPrice: BigInt = BigInt(11000000000L)  // 11 ERG
  private val wrongPrice: BigInt = BigInt(12000000000L)   // 12 ERG

  private def compileTree(script: String): ErgoTree = {
    // compile with ErgoTree v3 explicitly: compile() defaults to version 0, under
    // which v6-only methods (Global.decodeNbits) are not visible to the typer
    val prop = compile(Map.empty, script, VersionContext.V6SoftForkVersion).toSigmaProp
    mkTestErgoTree(prop)
  }

  /** nBits encoding of a difficulty value, as it would appear in a block header. */
  private def nBitsOf(difficulty: Long): Long = NBitsUtils.encodeCompactBits(BigInteger.valueOf(difficulty))

  /** Context whose pre-header carries the given nBits (i.e. blockchain difficulty). */
  private def ctxWithDifficulty(selfBox: ErgoBox,
                                inputs: IndexedSeq[ErgoBox],
                                tx: UnsignedErgoLikeTransaction,
                                height: Int,
                                nBits: Long,
                                dataBoxes: IndexedSeq[ErgoBox] = IndexedSeq.empty): ErgoLikeContext = {
    val preHeader = CPreHeader(
      version = 0,
      parentId = Colls.emptyColl[Byte],
      timestamp = 3L,
      nBits = nBits,
      height = height,
      minerPk = GroupElementSerializer.parse(SigmaSerializer.startReader(ErgoLikeContextTesting.dummyPubkey)).toGroupElement,
      votes = Colls.emptyColl[Byte]
    )
    new ErgoLikeContext(
      AvlTreeData.dummy,
      ErgoLikeContextTesting.noHeaders,
      preHeader,
      dataBoxes,
      inputs,
      tx,
      inputs.indexOf(selfBox),
      ContextExtension.empty,
      ValidationRules.currentSettings,
      CErgoTreeEvaluator.DefaultEvalSettings.scriptCostLimitInEvaluator,
      initCost = 0L,
      activatedVersionInTests
    )
  }

  private def oracleBox(tree: ErgoTree, difficulty: BigInt, price: BigInt, height: Int = 100000): ErgoBox =
    testBox(
      1000000L,
      tree,
      height,
      additionalRegisters = Map(
        ErgoBox.R4 -> BigIntConstant(difficulty.bigInteger),
        ErgoBox.R5 -> BigIntConstant(price.bigInteger)
      )
    )

  property("trustless oracle updates price from blockchain difficulty") {
    val prover = new ErgoLikeTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter
    val tree = compileTree(trustlessOracleScript)

    val initialOracleBox = oracleBox(tree, BigInt(initialDifficulty), initialPrice)
    val updatedOracleBox = oracleBox(tree, BigInt(raisedDifficulty), raisedPrice)

    val tx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(initialOracleBox.id)),
      IndexedSeq(updatedOracleBox)
    )

    // the pre-header reports a 10% difficulty increase
    val ctx = ctxWithDifficulty(initialOracleBox, IndexedSeq(initialOracleBox), tx, 100000, nBitsOf(raisedDifficulty))

    val pr = prover.prove(tree, ctx, fakeMessage).get
    verifier.verify(tree, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("trustless oracle fails with incorrectly adjusted price") {
    val prover = new ErgoLikeTestProvingInterpreter
    val tree = compileTree(trustlessOracleScript)

    val initialOracleBox = oracleBox(tree, BigInt(initialDifficulty), initialPrice)
    // price not matching the +10% difficulty change
    val updatedOracleBox = oracleBox(tree, BigInt(raisedDifficulty), wrongPrice)

    val tx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(initialOracleBox.id)),
      IndexedSeq(updatedOracleBox)
    )

    val ctx = ctxWithDifficulty(initialOracleBox, IndexedSeq(initialOracleBox), tx, 100000, nBitsOf(raisedDifficulty))

    prover.prove(tree, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("epoch oracle updates at difficulty epoch boundary") {
    val prover = new ErgoLikeTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter
    val tree = compileTree(epochOracleScript)

    val epochHeight = 128000 // divisible by 128
    val initialOracleBox = oracleBox(tree, BigInt(initialDifficulty), initialPrice, epochHeight - 1)
    val updatedOracleBox = oracleBox(tree, BigInt(raisedDifficulty), raisedPrice, epochHeight)

    val tx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(initialOracleBox.id)),
      IndexedSeq(updatedOracleBox)
    )

    val ctx = ctxWithDifficulty(initialOracleBox, IndexedSeq(initialOracleBox), tx, epochHeight, nBitsOf(raisedDifficulty))

    val pr = prover.prove(tree, ctx, fakeMessage).get
    verifier.verify(tree, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("epoch oracle rejects update off epoch boundary") {
    val prover = new ErgoLikeTestProvingInterpreter
    val tree = compileTree(epochOracleScript)

    val nonEpochHeight = 128001 // not divisible by 128
    val initialOracleBox = oracleBox(tree, BigInt(initialDifficulty), initialPrice, nonEpochHeight - 1)
    val updatedOracleBox = oracleBox(tree, BigInt(raisedDifficulty), raisedPrice, nonEpochHeight)

    val tx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(initialOracleBox.id)),
      IndexedSeq(updatedOracleBox)
    )

    val ctx = ctxWithDifficulty(initialOracleBox, IndexedSeq(initialOracleBox), tx, nonEpochHeight, nBitsOf(raisedDifficulty))

    prover.prove(tree, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("derivative pays out when difficulty is above threshold") {
    val userProver = new ErgoLikeTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter
    val oracleTree = compileTree(trustlessOracleScript)
    val derivativeTree = compileTree(derivativeScript)

    // oracle box already updated with the +10% difficulty
    val oracle = oracleBox(oracleTree, BigInt(raisedDifficulty), raisedPrice)
    val derivativeBox = testBox(5000000L, derivativeTree, 100000)
    val payoutBox = testBox(5000000L, mkTestErgoTree(userProver.dlogSecrets.head.publicImage), 100000)

    // the oracle is referenced as a data input (read without being spent)
    val tx = createTransaction(IndexedSeq(oracle), IndexedSeq(payoutBox))

    val ctx = ErgoLikeContextTesting(
      currentHeight = 100000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      dataBoxes = IndexedSeq(oracle),
      boxesToSpend = IndexedSeq(derivativeBox),
      spendingTransaction = tx,
      selfIndex = 0,
      activatedVersionInTests
    )

    val pr = userProver.prove(derivativeTree, ctx, fakeMessage).get
    verifier.verify(derivativeTree, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("derivative fails when difficulty is below threshold") {
    val userProver = new ErgoLikeTestProvingInterpreter
    val oracleTree = compileTree(trustlessOracleScript)
    val derivativeTree = compileTree(derivativeScript)

    // oracle reports only a +4% difficulty increase
    val oracle = oracleBox(oracleTree, BigInt(smallRaiseDifficulty), BigInt(10400000000L))
    val derivativeBox = testBox(5000000L, derivativeTree, 100000)
    val payoutBox = testBox(5000000L, mkTestErgoTree(userProver.dlogSecrets.head.publicImage), 100000)

    // the oracle is referenced as a data input (read without being spent)
    val tx = createTransaction(IndexedSeq(oracle), IndexedSeq(payoutBox))

    val ctx = ErgoLikeContextTesting(
      currentHeight = 100000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      dataBoxes = IndexedSeq(oracle),
      boxesToSpend = IndexedSeq(derivativeBox),
      spendingTransaction = tx,
      selfIndex = 0,
      activatedVersionInTests
    )

    userProver.prove(derivativeTree, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("derivative fails with missing oracle data") {
    val userProver = new ErgoLikeTestProvingInterpreter
    val derivativeTree = compileTree(derivativeScript)

    // oracle box without the difficulty register
    val brokenOracleBox = testBox(1000000L, compileTree("{ sigmaProp(true) }"), 100000)
    val derivativeBox = testBox(5000000L, derivativeTree, 100000)
    val payoutBox = testBox(5000000L, mkTestErgoTree(userProver.dlogSecrets.head.publicImage), 100000)

    // the oracle is referenced as a data input (read without being spent)
    val tx = createTransaction(IndexedSeq(brokenOracleBox), IndexedSeq(payoutBox))

    val ctx = ErgoLikeContextTesting(
      currentHeight = 100000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      dataBoxes = IndexedSeq(brokenOracleBox),
      boxesToSpend = IndexedSeq(derivativeBox),
      spendingTransaction = tx,
      selfIndex = 0,
      activatedVersionInTests
    )

    userProver.prove(derivativeTree, ctx, fakeMessage).isSuccess shouldBe false
  }
}
