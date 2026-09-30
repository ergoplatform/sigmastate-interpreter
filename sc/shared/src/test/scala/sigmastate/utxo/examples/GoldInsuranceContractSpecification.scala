package sigmastate.utxo.examples

import org.ergoplatform._
import sigma.data.{AvlTreeData, Digest32Coll}
import sigma.ast._
import sigmastate.helpers._
import sigmastate.helpers.TestingHelpers._
import sigmastate.CompilerCrossVersionProps
import sigma._
import scorex.crypto.hash.Blake2b256

/**
 * Tests for Gold Insurance Contract Pattern
 *
 * Based on the ErgoForum post: https://www.ergoforum.org/t/physical-or-digital-gold-simple-insurance-on-ergo/4715
 *
 * Use case (from the post): a client buying a right to physical gold delivery within
 * an insurance period also buys an insurance option. The vault locks digital gold
 * (gold-pegged tokens) in this contract's box and sends a unique policy NFT to the client:
 *
 *  - at any moment before the insurance period end, the holder of the policy NFT can
 *    present it (in inputs) and order the digital gold transferred to any address;
 *    the vault can do the same when the client visits the vault to get the physical gold;
 *  - after the insurance period end, the vault reclaims the unused digital gold.
 *
 * Note: unlike SchnorrSignatureVerificationSpecification, this contract does not verify
 * Schnorr signatures via ErgoScript; the vault path uses the native proveDlog sigma
 * protocol, and the client path relies on possession of the policy NFT (bearer instrument).
 *
 * Registers of the insurance box:
 *  - R4 - insurance period end (block height)
 *  - R5 - vault public key (GroupElement)
 *  - R6 - policy NFT id (Coll[Byte])
 */
class GoldInsuranceContractSpecification extends CompilerTestingCommons
  with CompilerCrossVersionProps {

  private implicit lazy val IR: TestingIRContext = new TestingIRContext

  private val goldInsuranceScript = """
    |{
    |  // Physical/Digital Gold Insurance Contract
    |  // Based on ErgoForum post: https://www.ergoforum.org/t/physical-or-digital-gold-simple-insurance-on-ergo/4715
    |  //
    |  // The insurance box holds digital gold (gold-pegged tokens).
    |  // The policy NFT itself is held by the client (sent by the vault when the
    |  // insurance is purchased) and must be presented in inputs to redeem.
    |  val insurancePeriodEnd = SELF.R4[Long].get
    |  val vaultPublicKey = SELF.R5[GroupElement].get
    |  val policyNftId = SELF.R6[Coll[Byte]].get
    |
    |  val periodEnded = HEIGHT >= insurancePeriodEnd
    |
    |  val nftInInputs = INPUTS.exists { (input: Box) =>
    |    input.tokens.exists { (token: (Coll[Byte], Long)) =>
    |      token._1 == policyNftId
    |    }
    |  }
    |
    |  val vaultRedemption = periodEnded && proveDlog(vaultPublicKey)
    |  val clientRedemption = !periodEnded && nftInInputs
    |
    |  sigmaProp(vaultRedemption || clientRedemption)
    |}
    |""".stripMargin

  // A stand-in "gold-pegged token" held in the insurance box as collateral
  private val digitalGoldTokenId: Digest32Coll = Digest32Coll @@ Colls.fromArray(Blake2b256("digital-gold"))
  private val policyNftId: Digest32Coll = Digest32Coll @@ Colls.fromArray(Blake2b256("gold-insurance-policy"))

  private val currentHeight = 100000
  private val periodEndBefore = currentHeight + 100000L   // period still active
  private val periodEndAfter = currentHeight - 100000L    // period already ended

  private def compileScript: ErgoTree = {
    val scriptProp = compile(Map.empty, goldInsuranceScript).toSigmaProp
    mkTestErgoTree(scriptProp)
  }

  /** Insurance box holding digital gold and the contract registers (R4, R5, R6 set densely). */
  private def insuranceBox(tree: ErgoTree, periodEnd: Long, vaultPubKey: Constant[SGroupElement.type]): ErgoBox =
    testBox(
      1000000L,
      tree,
      creationHeight = 100000,
      additionalTokens = Seq((digitalGoldTokenId, 100L)),
      additionalRegisters = Map(
        ErgoBox.R4 -> LongConstant(periodEnd),
        ErgoBox.R5 -> vaultPubKey,
        ErgoBox.R6 -> ByteArrayConstant(policyNftId.toArray)
      )
    )

  private def spendingCtx(selfBox: ErgoBox, inputs: IndexedSeq[ErgoBox], tx: UnsignedErgoLikeTransaction): ErgoLikeContext =
    ErgoLikeContextTesting(
      currentHeight = currentHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = inputs,
      tx,
      self = selfBox,
      activatedVersionInTests
    )

  private def unsignedTx(tree: ErgoTree, inputs: IndexedSeq[ErgoBox]): UnsignedErgoLikeTransaction =
    UnsignedErgoLikeTransaction(
      inputs.map(b => new UnsignedInput(b.id)),
      IndexedSeq(testBox(1000000L, tree, 100000))
    )

  property("client redemption before period end with policy NFT") {
    val vault = new ErgoLikeTestProvingInterpreter
    val client = new ErgoLikeTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter
    val tree = compileScript
    val vaultPubKey = GroupElementConstant(vault.dlogSecrets.head.publicImage.value)

    val insurance = insuranceBox(tree, periodEndBefore, vaultPubKey)
    // the client holds the policy NFT (sent by the vault when purchasing insurance)
    val clientBox = testBox(
      1000000L,
      tree,
      100000,
      additionalTokens = Seq((policyNftId, 1L))
    )

    val inputs = IndexedSeq(insurance, clientBox)
    val ctx = spendingCtx(insurance, inputs, unsignedTx(tree, inputs))

    // anyone holding the policy NFT can order the digital gold transfer before period end
    val pr = client.prove(tree, ctx, fakeMessage).get
    verifier.verify(tree, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("vault redemption after period end") {
    val vault = new ErgoLikeTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter
    val tree = compileScript
    val vaultPubKey = GroupElementConstant(vault.dlogSecrets.head.publicImage.value)

    val insurance = insuranceBox(tree, periodEndAfter, vaultPubKey)

    val inputs = IndexedSeq(insurance)
    val ctx = spendingCtx(insurance, inputs, unsignedTx(tree, inputs))

    // after the insurance period end the vault reclaims the unused digital gold
    val pr = vault.prove(tree, ctx, fakeMessage).get
    verifier.verify(tree, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("vault redemption before period end must fail") {
    val vault = new ErgoLikeTestProvingInterpreter
    val tree = compileScript
    val vaultPubKey = GroupElementConstant(vault.dlogSecrets.head.publicImage.value)

    val insurance = insuranceBox(tree, periodEndBefore, vaultPubKey)

    val inputs = IndexedSeq(insurance)
    val ctx = spendingCtx(insurance, inputs, unsignedTx(tree, inputs))

    // vault cannot reclaim while the insurance period is still active,
    // even though it knows the secret (client's NFT right takes precedence)
    vault.prove(tree, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("client redemption without policy NFT must fail") {
    val client = new ErgoLikeTestProvingInterpreter
    val tree = compileScript
    val vaultPubKey = GroupElementConstant((new ErgoLikeTestProvingInterpreter).dlogSecrets.head.publicImage.value)

    val insurance = insuranceBox(tree, periodEndBefore, vaultPubKey)
    // some unrelated input, no policy NFT anywhere
    val otherBox = testBox(1000000L, tree, 100000)

    val inputs = IndexedSeq(insurance, otherBox)
    val ctx = spendingCtx(insurance, inputs, unsignedTx(tree, inputs))

    client.prove(tree, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("redemption with wrong vault secret after period end must fail") {
    val vault = new ErgoLikeTestProvingInterpreter
    val wrongVault = new ErgoLikeTestProvingInterpreter
    val tree = compileScript
    val vaultPubKey = GroupElementConstant(vault.dlogSecrets.head.publicImage.value)

    val insurance = insuranceBox(tree, periodEndAfter, vaultPubKey)

    val inputs = IndexedSeq(insurance)
    val ctx = spendingCtx(insurance, inputs, unsignedTx(tree, inputs))

    wrongVault.prove(tree, ctx, fakeMessage).isSuccess shouldBe false
  }
}
