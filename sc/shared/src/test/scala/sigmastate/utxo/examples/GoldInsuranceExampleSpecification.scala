package sigmastate.utxo.examples

import org.ergoplatform.ErgoBox
import sigma.Extensions.ArrayOps
import sigma.ast.{ByteArrayConstant, IntConstant}
import sigma.ast.syntax._
import sigma.data.AvlTreeData
import sigmastate.CompilerCrossVersionProps
import sigmastate.eval.Extensions.ArrayByteOps
import sigmastate.helpers.TestingHelpers._
import sigmastate.helpers.{CompilerTestingCommons, ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTestInterpreter}

/** Example of a simple insurance contract for a physical gold purchase
  * (see https://github.com/ergoplatform/sigmastate-interpreter/issues/1181 and
  * https://www.ergoforum.org/t/physical-or-digital-gold-simple-insurance-on-ergo/4715).
  *
  * A client buys a physical gold brick from a vault, to be delivered within some period,
  * and pays an insurance premium on top. The vault then:
  *   - issues a unique insurance NFT to the client, and
  *   - locks an equivalent amount of digital gold (gold-pegged tokens) in a box guarded
  *     by the contract below.
  *
  * The insurance box can be spent in two ways:
  *   - redemption, before (and at) the insurance deadline: any transaction which presents
  *     the insurance NFT in one of its inputs may send the digital gold to any address.
  *     Normally that is the client claiming compensation for a failed delivery, but since
  *     the NFT itself is the claim ticket, the same path serves the vault when the client
  *     surrenders the NFT on successful delivery ("either party can present the NFT");
  *   - reclaim, strictly after the deadline: the vault takes the digital gold back with
  *     its own key, no NFT required.
  *
  * The redemption window closes exactly at the deadline, so the two spending paths never
  * overlap: an expired insurance box cannot be raced by the NFT holder, and before the
  * expiry the vault cannot pull the collateral out from under the client.
  *
  * The contract constrains only the inputs of the spending transaction; where the digital
  * gold goes is deliberately left free.
  */
class GoldInsuranceExampleSpecification extends CompilerTestingCommons
  with CompilerCrossVersionProps {
  private implicit lazy val IR: TestingIRContext = new TestingIRContext

  private val vault = new ContextEnrichingTestProvingInterpreter
  private val vaultPubKey = vault.dlogSecrets.head.publicImage

  /** The client holds the insurance NFT, not any secret the contract knows about. */
  private val client = new ContextEnrichingTestProvingInterpreter

  private val env = Map("vault" -> vaultPubKey)

  private val insuranceScript =
    """{
      |  val deadline = SELF.R4[Int].get
      |  val insuranceNftId = SELF.R5[Coll[Byte]].get
      |
      |  // the insurance NFT is presented if any input of the spending transaction
      |  // carries the token issued for this insurance deal
      |  val nftPresented = INPUTS.exists({ (b: Box) =>
      |    b.tokens.exists({ (t: (Coll[Byte], Long)) => t._1 == insuranceNftId })
      |  })
      |
      |  sigmaProp(HEIGHT <= deadline && nftPresented) || (sigmaProp(HEIGHT > deadline) && vault)
      |}""".stripMargin

  private val insuranceDeadline = 5000
  private val insuranceNftId = Array.tabulate(32)(i => (i + 1).toByte)
  private val digitalGoldTokenId = Array.tabulate(32)(i => (i + 101).toByte)
  private val digitalGoldAmount = 1000000L

  /** The box locking the vault's digital gold under the insurance contract:
    * R4 - insurance deadline (height), R5 - id of the insurance NFT. */
  private def insuranceBox(tree: sigma.ast.ErgoTree): ErgoBox =
    testBox(1000000, tree, creationHeight = 100,
      additionalTokens = Seq(digitalGoldTokenId.toTokenId -> digitalGoldAmount),
      additionalRegisters = Map(
        ErgoBox.R4 -> IntConstant(insuranceDeadline),
        ErgoBox.R5 -> ByteArrayConstant(insuranceNftId.toColl)))

  /** A box carrying the given token, spent alongside the insurance box to present it.
    * In a real deal it is guarded by its owner's key; `TrueTree` keeps the example
    * focused on the insurance contract. */
  private def tokenBox(tokenId: Array[Byte]): ErgoBox =
    testBox(1000000, TrueTree, creationHeight = 100,
      additionalTokens = Seq(tokenId.toTokenId -> 1L))

  /** Tries to spend the insurance box at the given height, optionally presenting
    * another input, and checks the expected prove/verify outcome. */
  private def trySpend(
      prover: ContextEnrichingTestProvingInterpreter,
      height: Int,
      presentedBox: Option[ErgoBox],
      expectSuccess: Boolean): Unit = {
    val tree = mkTestErgoTree(compile(env, insuranceScript).asSigmaProp)
    val inputs = IndexedSeq(insuranceBox(tree)) ++ presentedBox

    // the contract does not constrain outputs: the digital gold may go to any address
    val anyOutput = testBox(1000000, TrueTree, creationHeight = height)
    val ctx = ErgoLikeContextTesting(
      currentHeight = height,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = inputs,
      spendingTransaction = createTransaction(IndexedSeq(anyOutput)),
      self = inputs.head,
      activatedVersionInTests)

    val res = prover.prove(env, tree, ctx, fakeMessage)
    if (expectSuccess) {
      val proof = res.get
      val verifier = new ErgoLikeTestInterpreter
      verifier.verify(env, tree, ctx, proof, fakeMessage).get._1 shouldBe true
    } else {
      res.isFailure shouldBe true
    }
  }

  property("insurance NFT unlocks the digital gold before the deadline") {
    trySpend(client, insuranceDeadline - 1, Some(tokenBox(insuranceNftId)), expectSuccess = true)
    // the redemption window includes the deadline itself
    trySpend(client, insuranceDeadline, Some(tokenBox(insuranceNftId)), expectSuccess = true)
  }

  property("vault reclaims the digital gold after the deadline") {
    trySpend(vault, insuranceDeadline + 1, None, expectSuccess = true)
  }

  property("without the NFT nobody can spend before the deadline, not even the vault") {
    trySpend(client, insuranceDeadline - 1, None, expectSuccess = false)
    trySpend(vault, insuranceDeadline - 1, None, expectSuccess = false)
    // reclaim requires the deadline to have passed, not just arrived
    trySpend(vault, insuranceDeadline, None, expectSuccess = false)
  }

  property("the NFT no longer unlocks after the deadline - the claim window is closed") {
    trySpend(client, insuranceDeadline + 1, Some(tokenBox(insuranceNftId)), expectSuccess = false)
    // while the vault reclaims regardless of the NFT being presented
    trySpend(vault, insuranceDeadline + 1, Some(tokenBox(insuranceNftId)), expectSuccess = true)
  }

  property("a different token does not pass as the insurance NFT") {
    val otherTokenId = Array.tabulate(32)(i => (i + 201).toByte)
    trySpend(client, insuranceDeadline - 1, Some(tokenBox(otherTokenId)), expectSuccess = false)
  }

  property("a stranger cannot take the gold after the deadline") {
    trySpend(client, insuranceDeadline + 1, None, expectSuccess = false)
  }
}
