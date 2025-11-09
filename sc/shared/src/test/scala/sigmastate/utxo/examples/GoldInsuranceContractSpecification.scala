package sigmastate.utxo.examples

import org.ergoplatform._
import sigma.data.{AvlTreeData, Digest32Coll}
import sigma.ast._
import sigmastate.helpers._
import sigmastate.helpers.TestingHelpers._
import sigmastate.CompilerCrossVersionProps
import sigma._
import sigma.interpreter.ContextExtension

/**
 * Tests for Gold Insurance Contract Pattern
 * 
 * Based on the ErgoForum post: https://www.ergoforum.org/t/physical-or-digital-gold-simple-insurance-on-ergo/4715
 * 
 * Demonstrates real-world gold insurance use case on Ergo blockchain.
 */
class GoldInsuranceContractSpecification extends CompilerTestingCommons 
  with CompilerCrossVersionProps {
  
  private implicit lazy val IR: TestingIRContext = new TestingIRContext

  /**
   * Gold Insurance Contract ErgoScript template
   * Based on the ErgoForum post requirements
   */
  private val goldInsuranceScript = """
    |{
    |  // Physical/Digital Gold Insurance Contract
    |  // Based on ErgoForum post: https://www.ergoforum.org/t/physical-or-digital-gold-simple-insurance-on-ergo/4715
    |  // 
    |  // Use Case:
    |  // - Client buys physical gold in Dubai with remote delivery
    |  // - Insurance provides digital gold if physical delivery fails
    |  // - NFT represents insurance policy
    |  // - Digital gold tokens are held as insurance
    |  //
    |  // Redemption paths:
    |  // 1. Client redemption: Present NFT anytime before period end
    |  // 2. Vault redemption: After period end, vault reclaims unused insurance
    |  
    |  val policyNft = SELF.tokens(0)
    |  val insurancePeriodEnd = SELF.R4[Long].get
    |  val vaultPublicKey = SELF.R5[GroupElement].get
    |  val currentTime = CONTEXT.headers(0).timestamp
    |  
    |  val periodEnded = currentTime >= insurancePeriodEnd
    |  
    |  val nftInInputs = INPUTS.exists { (input: Box) =>
    |    input.tokens.exists { (token: (Coll[Byte], Long)) =>
    |      token._1 == policyNft._1
    |    }
    |  }
    |  
    |  val vaultRedemption = periodEnded && proveDlog(vaultPublicKey)
    |  val clientRedemption = !periodEnded && nftInInputs
    |  
    |  sigmaProp(vaultRedemption || clientRedemption)
    |}
    |""".stripMargin

  /**
   * Creates test environment for gold insurance contract
   */
  private def createGoldInsuranceEnv(
    policyNftId: Array[Byte], 
    periodEnd: Long, 
    vaultPubKey: Value[SGroupElement.type]
  ): Map[String, Value[SType]] = {
    Map(
      "policyNftId" -> ByteArrayConstant(policyNftId),
      "periodEnd" -> LongConstant(periodEnd),
      "vaultPubKey" -> vaultPubKey
    )
  }

  /**
   * Creates standard test parameters for gold insurance
   */
  private def createStandardTestParams(vault: ContextEnrichingTestProvingInterpreter): (Array[Byte], Long, Value[SGroupElement.type]) = {
    val vaultPubKey = vault.dlogSecrets.head.publicImage
    val policyNftId = Array[Byte](0x67, 0x6f, 0x6c, 0x64, 0x69, 0x6e, 0x73, 0x75, 0x72, 0x61, 0x6e, 0x63, 0x65) // "goldinsurance" in hex
    val purchaseTime = 1000L
    val insurancePeriod = 6 * 30 * 24 * 60 * 60L // 6 months in seconds
    val periodEnd = purchaseTime + insurancePeriod
    
    (policyNftId, periodEnd, GroupElementConstant(vaultPubKey.value))
  }

  property("gold insurance vault redemption with proveDlog") {
    // Test vault redemption using proveDlog (simplified verification)
    
    val vault = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter
    
    val scriptProp = compile(Map.empty, goldInsuranceScript).toSigmaProp
    val scriptTree = mkTestErgoTree(scriptProp)

    // Create insurance box with vault public key
    val insuranceBox = testBox(
      1000000L,
      scriptTree,
      100000,
      additionalRegisters = Map(
        ErgoBox.R5 -> GroupElementConstant(vault.dlogSecrets.head.publicImage.value)
      )
    )
    
    val outputBox = testBox(1000000L, scriptTree, 100000)
    
    val tx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(insuranceBox.id)),
      IndexedSeq(outputBox)
    )

    val ctx = ErgoLikeContextTesting(
      currentHeight = 100000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(insuranceBox),
      tx,
      self = insuranceBox,
      activatedVersionInTests
    )

    // Vault should succeed in redeeming (proveDlog verification)
    val pr = vault.prove(scriptTree, ctx, fakeMessage).get
    verifier.verify(scriptTree, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("gold insurance client redemption with NFT") {
    // Test client redemption with NFT
    
    val vault = new ContextEnrichingTestProvingInterpreter
    val client = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter
    val (policyNftId, periodEnd, vaultPubKey) = createStandardTestParams(vault)
    
    val scriptProp = compile(Map.empty, goldInsuranceScript).toSigmaProp
    val scriptTree = mkTestErgoTree(scriptProp)

    // Convert policyNftId to Digest32Coll
    val policyNftIdColl: Digest32Coll = Digest32Coll @@ Colls.fromArray(policyNftId)

    // Create insurance box with NFT
    val insuranceBox = testBox(
      1000000L,
      scriptTree,
      100000,
      additionalTokens = Seq((policyNftIdColl, 1L)),
      additionalRegisters = Map(
        ErgoBox.R5 -> GroupElementConstant(vault.dlogSecrets.head.publicImage.value)
      )
    )
    
    // Create client box WITH the NFT
    val clientBox = testBox(
      1000000L,
      scriptTree,
      100000,
      additionalTokens = Seq((policyNftIdColl, 1L))
    )
    
    val outputBox = testBox(1000000L, scriptTree, 100000)
    
    val tx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(insuranceBox.id), new UnsignedInput(clientBox.id)),
      IndexedSeq(outputBox)
    )

    val ctx = ErgoLikeContextTesting(
      currentHeight = 100000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(insuranceBox, clientBox),
      tx,
      self = insuranceBox,
      activatedVersionInTests
    )

    // Client should succeed in redeeming with NFT
    val pr = client.prove(scriptTree, ctx, fakeMessage).get
    verifier.verify(scriptTree, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("gold insurance vault redemption fails without secret") {
    // Test that wrong vault cannot redeem
    
    val vault = new ContextEnrichingTestProvingInterpreter
    val wrongVault = new ContextEnrichingTestProvingInterpreter
    
    val scriptProp = compile(Map.empty, goldInsuranceScript).toSigmaProp
    val scriptTree = mkTestErgoTree(scriptProp)

    // Create insurance box with vault public key
    val insuranceBox = testBox(
      1000000L,
      scriptTree,
      100000,
      additionalRegisters = Map(
        ErgoBox.R5 -> GroupElementConstant(vault.dlogSecrets.head.publicImage.value)
      )
    )
    
    val outputBox = testBox(1000000L, scriptTree, 100000)
    
    val tx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(insuranceBox.id)),
      IndexedSeq(outputBox)
    )

    val ctx = ErgoLikeContextTesting(
      currentHeight = 100000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(insuranceBox),
      tx,
      self = insuranceBox,
      activatedVersionInTests
    )

    // Wrong vault should fail to redeem
    wrongVault.prove(scriptTree, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("gold insurance client redemption fails without NFT") {
    // Test that client cannot redeem without the NFT
    
    val vault = new ContextEnrichingTestProvingInterpreter
    val client = new ContextEnrichingTestProvingInterpreter
    val (policyNftId, periodEnd, vaultPubKey) = createStandardTestParams(vault)
    
    val scriptProp = compile(Map.empty, goldInsuranceScript).toSigmaProp
    val scriptTree = mkTestErgoTree(scriptProp)

    // Convert policyNftId to Digest32Coll
    val policyNftIdColl: Digest32Coll = Digest32Coll @@ Colls.fromArray(policyNftId)

    // Create insurance box with NFT
    val insuranceBox = testBox(
      1000000L,
      scriptTree,
      100000,
      additionalTokens = Seq((policyNftIdColl, 1L)),
      additionalRegisters = Map(
        ErgoBox.R5 -> GroupElementConstant(vault.dlogSecrets.head.publicImage.value)
      )
    )
    
    // Create client box WITHOUT the NFT
    val clientBox = testBox(
      1000000L,
      scriptTree,
      100000
    )
    
    val outputBox = testBox(1000000L, scriptTree, 100000)
    
    val tx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(insuranceBox.id), new UnsignedInput(clientBox.id)),
      IndexedSeq(outputBox)
    )

    val ctx = ErgoLikeContextTesting(
      currentHeight = 100000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(insuranceBox, clientBox),
      tx,
      self = insuranceBox,
      activatedVersionInTests
    )

    // Client should fail to redeem without NFT
    client.prove(scriptTree, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("gold insurance simplified contract verification") {
    // Create a simplified version of the contract for testing without timestamp dependency
    val simplifiedGoldInsuranceScript = """
      |{
      |  // Simplified Gold Insurance Contract (without timestamp checks)
      |  // For testing core functionality
      |  
      |  val policyNft = SELF.tokens(0)
      |  val vaultPublicKey = SELF.R5[GroupElement].get
      |  
      |  val nftInInputs = INPUTS.exists { (input: Box) =>
      |    input.tokens.exists { (token: (Coll[Byte], Long)) =>
      |      token._1 == policyNft._1
      |    }
      |  }
      |  
      |  val vaultRedemption = proveDlog(vaultPublicKey)
      |  val clientRedemption = nftInInputs
      |  
      |  sigmaProp(vaultRedemption || clientRedemption)
      |}
      |""".stripMargin

    val vault = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter
    
    val (policyNftId, periodEnd, vaultPubKey) = createStandardTestParams(vault)
    
    val scriptProp = compile(Map.empty, simplifiedGoldInsuranceScript).toSigmaProp
    val scriptTree = mkTestErgoTree(scriptProp)

    // Convert policyNftId to Digest32Coll
    val policyNftIdColl: Digest32Coll = Digest32Coll @@ Colls.fromArray(policyNftId)

    // Create insurance box with NFT and vault public key
    val insuranceBox = testBox(
      1000000L,
      scriptTree,
      100000,
      additionalTokens = Seq((policyNftIdColl, 1L)),
      additionalRegisters = Map(
        ErgoBox.R4 -> LongConstant(0L), // Dummy value for R4
        ErgoBox.R5 -> GroupElementConstant(vault.dlogSecrets.head.publicImage.value)
      )
    )
    
    val outputBox = testBox(1000000L, scriptTree, 100000)
    
    val tx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(insuranceBox.id)),
      IndexedSeq(outputBox)
    )

    val ctx = ErgoLikeContextTesting(
      currentHeight = 100000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(insuranceBox),
      tx,
      self = insuranceBox,
      activatedVersionInTests
    )

    // Vault should succeed in redeeming (proveDlog verification)
    val pr = vault.prove(scriptTree, ctx, fakeMessage).get
    verifier.verify(scriptTree, ctx, pr, fakeMessage).get._1 shouldBe true
  }


}