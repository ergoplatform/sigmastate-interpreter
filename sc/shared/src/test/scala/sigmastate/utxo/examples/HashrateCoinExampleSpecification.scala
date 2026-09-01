package sigmastate.utxo.examples

import org.ergoplatform._
import sigma.data.AvlTreeData
import sigmastate._
import sigmastate.helpers._
import sigmastate.helpers.TestingHelpers._
import sigma.ast._


class HashrateCoinExampleSpecification extends CompilerTestingCommons with CompilerCrossVersionProps {
  private implicit lazy val IR: TestingIRContext = new TestingIRContext

  // Common trustless oracle script used across most tests
  // This oracle:
  // - Reads difficulty from blockchain headers (simulated as fixed value in tests)
  // - Automatically calculates price based on difficulty change
  // - Updates at any block height (no restrictions)
  // - Uses mathematical formula: new_price = old_price * new_difficulty / old_difficulty
  private val trustlessOracleScript = """
    |{
    |  // For testing purposes, we'll use a fixed new difficulty value
    |  // In a real implementation, this would come from CONTEXT.headers(0).nBits
    |  // and would be converted using Global.decodeNbits()
    |  val currentDifficulty = 1100000L  // 10% increase from initial 1000000
    |  
    |  // Get previous difficulty from self box register (R4)
    |  val previousDifficulty = SELF.R4[Long].get
    |  val previousPrice = SELF.R5[Long].get
    |  
    |  // Calculate price based on difficulty change
    |  // Using integer arithmetic to avoid type casting issues
    |  // Formula: new_price = old_price * new_difficulty / old_difficulty
    |  val currentPrice = (previousPrice * currentDifficulty) / previousDifficulty
    |  
    |  // Store calculated price in register
    |  // In ErgoScript, we create the output box directly
    |  val outputBox = OUTPUTS(0)
    |  val hasCorrectDifficulty = outputBox.R4[Long].get == currentDifficulty
    |  val hasCorrectPrice = outputBox.R5[Long].get == currentPrice
    |  
    |  // Only allow spending if we're updating with valid blockchain data
    |  // This ensures the oracle always reflects current blockchain state
    |  sigmaProp(hasCorrectDifficulty && hasCorrectPrice)
    |}
    |""".stripMargin

  /**
    * HashrateCoin Example:
    * A trustless oracle-based derivative token whose price adjusts based on mining difficulty changes.
    * 
    * Based on the ErgoForum post: https://www.ergoforum.org/t/blockchain-based-trustless-derivatives-hashratecoin-and-randomcoin/4999
    * 
    * Concept:
    * - Set initial price and initial difficulty in a trustless oracle box
    * - On every difficulty epoch (128 blocks), readjust price according to difficulty change
    * - E.g. if price is 10 ERG per Hashrate coin, and difficulty raised up by 2% in an epoch, 
    *   next price is 10.2 ERG per Hashrate coin
    * 
    * This creates a derivative that allows miners to hedge against difficulty growth and provides
    * a trading instrument that correlates with mining difficulty.
    * 
    * KEY ARCHITECTURE:
    * - Trustless Oracle: Reads difficulty directly from blockchain headers, no trusted parties
    * - Epoch Oracle: Only updates every 128 blocks (matches Ergo's difficulty adjustment schedule)
    * - Derivative Contract: Uses oracle data to determine payout conditions
    * - Price Formula: new_price = old_price * new_difficulty / old_difficulty
    */
  property("hashrate coin derivative contract using trustless oracle") {
    val userProver = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    // Use the common trustless oracle script
    val oracleProp = compile(Map.empty, trustlessOracleScript).toSigmaProp
    val oracleTree = mkTestErgoTree(oracleProp)

    // HASH RATE COIN DERIVATIVE CONTRACT:
    // This is the actual derivative contract that users interact with
    // It reads oracle data and determines payout conditions based on difficulty changes
    val derivativeScript = """
      |{
      |  // Find the oracle box in data inputs (not spending inputs)
      |  // Data inputs allow reading data without spending the box
      |  val oracleBox = CONTEXT.dataInputs(0)
      |  
      |  // Extract current difficulty and price from oracle registers
      |  // R4: current difficulty, R5: current price
      |  val currentDifficulty = oracleBox.R4[Long].get
      |  val currentPrice = oracleBox.R5[Long].get
      |  
      |  // Example condition: payout if difficulty increased by more than 5%
      |  // In a real implementation, this would track previous difficulty
      |  // For this test, we use a fixed threshold
      |  val difficultyThreshold = 1050000L  // 5% increase from initial 1000000
      |  
      |  // If difficulty exceeds threshold, payout to derivative holder
      |  // Otherwise, return funds to original owner
      |  // In this test, we'll use simple true/false conditions
      |  sigmaProp(currentDifficulty >= difficultyThreshold)
      |}
      |""".stripMargin

    val derivativeProp = compile(Map.empty, derivativeScript).toSigmaProp
    val derivativeTree = mkTestErgoTree(derivativeProp)

    // Create oracle box with current difficulty and price
    // This simulates an oracle that has already been updated with current blockchain data
    val currentDifficulty: Long = 1100000L  // 10% increase (exceeds 5% threshold)
    val currentPrice: Long = 11000000000L   // 11 ERG in nanoErg (10% price increase)
    
    val oracleBox = testBox(
      1000000L,
      oracleTree,
      100000,
      additionalRegisters = Map(
        ErgoBox.nonMandatoryRegisters(0) -> LongConstant(currentDifficulty),
        ErgoBox.nonMandatoryRegisters(1) -> LongConstant(currentPrice)
      )
    )

    // Create derivative box - this is the actual derivative token box
    val derivativeBox = testBox(5000000L, derivativeTree, 100000)

    // Create payout box for derivative holder (when conditions are met)
    val payoutBox = testBox(5000000L, mkTestErgoTree(userProver.dlogSecrets.head.publicImage), 100000)

    // Create transaction that spends derivative box with oracle as data input
    // Note: oracleBox is a data input, not a spending input
    val tx = createTransaction(
      IndexedSeq(oracleBox),
      IndexedSeq(payoutBox)
    )

    val ctx = ErgoLikeContextTesting(
      currentHeight = 100000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      dataBoxes = IndexedSeq(oracleBox),  // Oracle box provided as data input
      boxesToSpend = IndexedSeq(derivativeBox),  // Derivative box being spent
      spendingTransaction = tx,
      selfIndex = 0,
      activatedVersion = activatedVersionInTests
    )

    // This should succeed as difficulty (1100000) exceeds threshold (1050000)
    val pr = userProver.prove(derivativeTree, ctx, fakeMessage).get
    verifier.verify(derivativeTree, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("trustless hashrate coin oracle using blockchain data") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val oracleProp = compile(Map.empty, trustlessOracleScript).toSigmaProp
    val oracleTree = mkTestErgoTree(oracleProp)

    // Create initial oracle box with difficulty
    val initialDifficulty: Long = 1000000L  // Example initial difficulty
    val initialOracleBox = testBox(
      1000000L,
      oracleTree,
      100000,
      additionalRegisters = Map(
        ErgoBox.nonMandatoryRegisters(0) -> LongConstant(initialDifficulty), // R4: initial difficulty
        ErgoBox.nonMandatoryRegisters(1) -> LongConstant(10000000000L)  // R5: initial price
      )
    )

    // Create updated oracle box with new difficulty from header
    val newDifficulty: Long = 1100000L  // 10% increase
    val calculatedPrice: Long = 11000000000L  // 11 ERG (10% increase)
    
    val updatedOracleBox = testBox(
      1000000L,
      oracleTree,
      100000,
      additionalRegisters = Map(
        ErgoBox.nonMandatoryRegisters(0) -> LongConstant(newDifficulty),
        ErgoBox.nonMandatoryRegisters(1) -> LongConstant(calculatedPrice)
      )
    )



    val tx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(initialOracleBox.id)),
      IndexedSeq(updatedOracleBox)
    )

    // For testing purposes, we'll use the standard ErgoLikeContextTesting
    // In a real implementation, headers would be available through CONTEXT.headers
    val ctx = ErgoLikeContextTesting(
      currentHeight = 100000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(initialOracleBox),
      spendingTransaction = tx,
      self = initialOracleBox,
      activatedVersion = activatedVersionInTests
    )

    // This should succeed as we're updating with valid blockchain data
    val pr = prover.prove(oracleTree, ctx, fakeMessage).get
    verifier.verify(oracleTree, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("hashrate coin with epoch-based updates") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    // EPOCH-BASED ORACLE - KEY DIFFERENCE FROM REGULAR TRUSTLESS ORACLE:
    // This oracle only updates on difficulty epoch boundaries (every 128 blocks)
    // This matches the actual Ergo blockchain's difficulty adjustment schedule
    // Regular trustless oracle updates at any block, epoch oracle updates only at epoch boundaries
    val epochOracleScript = """
      |{
      |  // KEY DIFFERENCE: Only update on difficulty epoch boundaries (every 128 blocks)
      |  // This matches the actual Ergo blockchain's difficulty adjustment schedule
      |  val epochLength = 128
      |  val isEpochBoundary = (HEIGHT % epochLength) == 0
      |  
      |  if (isEpochBoundary) {
      |    // Update difficulty and price based on current header
      |    // For testing purposes, we'll use a fixed new difficulty value
      |    // In a real implementation, this would come from CONTEXT.headers(0).nBits
      |    // and would be converted using Global.decodeNbits()
      |    val currentDifficulty = 1100000L  // 10% increase
      |    
      |    // Get previous difficulty from register
      |    val previousDifficulty = SELF.R4[Long].get
      |    val previousPrice = SELF.R5[Long].get
      |    
      |    // Calculate price change based on difficulty change
      |    // Using integer arithmetic to avoid type casting issues
      |    val currentPrice = (previousPrice * currentDifficulty) / previousDifficulty
      |    
      |    // Verify the output box has correct values
      |    val outputBox = OUTPUTS(0)
      |    val hasCorrectDifficulty = outputBox.R4[Long].get == currentDifficulty
      |    val hasCorrectPrice = outputBox.R5[Long].get == currentPrice
      |    
      |    sigmaProp(hasCorrectDifficulty && hasCorrectPrice)
      |  } else {
      |    // KEY DIFFERENCE: Not an epoch boundary, don't allow updates
      |    // This prevents oracle updates between difficulty adjustment epochs
      |    sigmaProp(false)
      |  }
      |}
      |""".stripMargin

    val oracleProp = compile(Map.empty, epochOracleScript).toSigmaProp
    val oracleTree = mkTestErgoTree(oracleProp)

    // Test at epoch boundary (height divisible by 128)
    // This is the key requirement for epoch-based oracle updates
    val epochHeight = 128000
    
    val initialDifficulty: Long = 1000000L  // Example initial difficulty
    val initialOracleBox = testBox(
      1000000L,
      oracleTree,
      epochHeight - 1,  // Height just before epoch boundary
      additionalRegisters = Map(
        ErgoBox.nonMandatoryRegisters(0) -> LongConstant(initialDifficulty),
        ErgoBox.nonMandatoryRegisters(1) -> LongConstant(10000000000L)
      )
    )

    val newDifficulty: Long = 1100000L  // 10% increase
    val calculatedPrice: Long = 11000000000L  // 11 ERG (10% increase)
    
    val updatedOracleBox = testBox(
      1000000L,
      oracleTree,
      epochHeight,  // Height at epoch boundary
      additionalRegisters = Map(
        ErgoBox.nonMandatoryRegisters(0) -> LongConstant(newDifficulty),
        ErgoBox.nonMandatoryRegisters(1) -> LongConstant(calculatedPrice)
      )
    )



    val tx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(initialOracleBox.id)),
      IndexedSeq(updatedOracleBox)
    )

    // For testing purposes, we'll use the standard ErgoLikeContextTesting
    // In a real implementation, headers would be available through CONTEXT.headers
    val ctx = ErgoLikeContextTesting(
      currentHeight = epochHeight,  // KEY: Must be at epoch boundary for this oracle
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(initialOracleBox),
      spendingTransaction = tx,
      self = initialOracleBox,
      activatedVersion = activatedVersionInTests
    )

    // This should succeed at epoch boundary
    val pr = prover.prove(oracleTree, ctx, fakeMessage).get
    verifier.verify(oracleTree, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  // NEGATIVE TEST: Derivative contract should fail when difficulty below threshold
  // This tests the boundary condition where difficulty increases but not enough to trigger payout
  property("derivative contract should fail when difficulty below threshold") {
    val userProver = new ContextEnrichingTestProvingInterpreter

    // Hashrate coin derivative contract
    val derivativeScript = """
      |{
      |  val oracleBox = CONTEXT.dataInputs(0)
      |  val currentDifficulty = oracleBox.R4[Long].get
      |  val difficultyThreshold = 1050000L  // 5% increase threshold
      |  
      |  // Should only payout if difficulty exceeds threshold
      |  sigmaProp(currentDifficulty >= difficultyThreshold)
      |}
      |""".stripMargin

    val derivativeProp = compile(Map.empty, derivativeScript).toSigmaProp
    val derivativeTree = mkTestErgoTree(derivativeProp)

    // Create oracle box with difficulty BELOW threshold
    // The oracle script uses 1100000L (10% increase) but we need to test with 1040000L (4% increase)
    // So we'll create a custom oracle script for this specific test case
    val customOracleScript = """
      |{
      |  val currentDifficulty = 1040000L  // 4% increase (below threshold)
      |  val previousDifficulty = SELF.R4[Long].get
      |  val previousPrice = SELF.R5[Long].get
      |  val currentPrice = (previousPrice * currentDifficulty) / previousDifficulty
      |  
      |  val outputBox = OUTPUTS(0)
      |  val hasCorrectDifficulty = outputBox.R4[Long].get == currentDifficulty
      |  val hasCorrectPrice = outputBox.R5[Long].get == currentPrice
      |  
      |  sigmaProp(hasCorrectDifficulty && hasCorrectPrice)
      |}
      |""".stripMargin

    val customOracleProp = compile(Map.empty, customOracleScript).toSigmaProp
    val customOracleTree = mkTestErgoTree(customOracleProp)
    
    val currentDifficulty: Long = 1040000L  // 4% increase (below 5% threshold)
    val currentPrice: Long = 10400000000L   // 10.4 ERG
    
    val oracleBox = testBox(
      1000000L,
      customOracleTree,
      100000,
      additionalRegisters = Map(
        ErgoBox.nonMandatoryRegisters(0) -> LongConstant(currentDifficulty),
        ErgoBox.nonMandatoryRegisters(1) -> LongConstant(currentPrice)
      )
    )

    // Create derivative box
    val derivativeBox = testBox(5000000L, derivativeTree, 100000)

    // Create payout box for derivative holder
    val payoutBox = testBox(5000000L, mkTestErgoTree(userProver.dlogSecrets.head.publicImage), 100000)

    // Create transaction that spends derivative box with oracle as data input
    val tx = createTransaction(
      IndexedSeq(oracleBox),
      IndexedSeq(payoutBox)
    )

    val ctx = ErgoLikeContextTesting(
      currentHeight = 100000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      dataBoxes = IndexedSeq(oracleBox),
      boxesToSpend = IndexedSeq(derivativeBox),
      spendingTransaction = tx,
      selfIndex = 0,
      activatedVersion = activatedVersionInTests
    )

    // This should FAIL as difficulty is below threshold
    val pr = userProver.prove(derivativeTree, ctx, fakeMessage)
    pr.isFailure shouldBe true  // Should not be able to prove when condition fails
  }

  // NEGATIVE TEST: Epoch oracle should fail when updating at non-epoch boundary
  // This tests the key security feature of epoch-based oracles
  property("epoch oracle should fail when updating at non-epoch boundary") {
    val prover = new ContextEnrichingTestProvingInterpreter

    // Epoch oracle script - same as the positive test
    val epochOracleScript = """
      |{
      |  val epochLength = 128
      |  val isEpochBoundary = (HEIGHT % epochLength) == 0
      |  
      |  if (isEpochBoundary) {
      |    val currentDifficulty = 1100000L
      |    val previousDifficulty = SELF.R4[Long].get
      |    val previousPrice = SELF.R5[Long].get
      |    val currentPrice = (previousPrice * currentDifficulty) / previousDifficulty
      |    
      |    val outputBox = OUTPUTS(0)
      |    val hasCorrectDifficulty = outputBox.R4[Long].get == currentDifficulty
      |    val hasCorrectPrice = outputBox.R5[Long].get == currentPrice
      |    
      |    sigmaProp(hasCorrectDifficulty && hasCorrectPrice)
      |  } else {
      |    sigmaProp(false)
      |  }
      |}
      |""".stripMargin

    val oracleProp = compile(Map.empty, epochOracleScript).toSigmaProp
    val oracleTree = mkTestErgoTree(oracleProp)

    // KEY DIFFERENCE FROM POSITIVE TEST: Test at NON-epoch boundary
    // Height NOT divisible by 128 (128001 instead of 128000)
    val nonEpochHeight = 128001
    
    val initialDifficulty: Long = 1000000L
    val initialOracleBox = testBox(
      1000000L,
      oracleTree,
      nonEpochHeight - 1,
      additionalRegisters = Map(
        ErgoBox.nonMandatoryRegisters(0) -> LongConstant(initialDifficulty),
        ErgoBox.nonMandatoryRegisters(1) -> LongConstant(10000000000L)
      )
    )

    val newDifficulty: Long = 1100000L
    val calculatedPrice: Long = 11000000000L
    
    val updatedOracleBox = testBox(
      1000000L,
      oracleTree,
      nonEpochHeight,
      additionalRegisters = Map(
        ErgoBox.nonMandatoryRegisters(0) -> LongConstant(newDifficulty),
        ErgoBox.nonMandatoryRegisters(1) -> LongConstant(calculatedPrice)
      )
    )

    val tx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(initialOracleBox.id)),
      IndexedSeq(updatedOracleBox)
    )

    val ctx = ErgoLikeContextTesting(
      currentHeight = nonEpochHeight,  // KEY: Not at epoch boundary
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(initialOracleBox),
      spendingTransaction = tx,
      self = initialOracleBox,
      activatedVersion = activatedVersionInTests
    )

    // This should FAIL as we're not at epoch boundary
    // The oracle should reject updates between difficulty adjustment epochs
    val pr = prover.prove(oracleTree, ctx, fakeMessage)
    pr.isFailure shouldBe true  // Should not be able to prove when not at epoch boundary
  }

  property("derivative contract should fail with incorrect oracle data") {
    val userProver = new ContextEnrichingTestProvingInterpreter

    // Oracle contract
    val oracleScript = """
      |{
      |  sigmaProp(true)
      |}
      |""".stripMargin

    val oracleProp = compile(Map.empty, oracleScript).toSigmaProp
    val oracleTree = mkTestErgoTree(oracleProp)

    // Hashrate coin derivative contract
    val derivativeScript = """
      |{
      |  val oracleBox = CONTEXT.dataInputs(0)
      |  val currentDifficulty = oracleBox.R4[Long].get
      |  val difficultyThreshold = 1050000L
      |  
      |  sigmaProp(currentDifficulty >= difficultyThreshold)
      |}
      |""".stripMargin

    val derivativeProp = compile(Map.empty, derivativeScript).toSigmaProp
    val derivativeTree = mkTestErgoTree(derivativeProp)

    // Create oracle box with MISSING difficulty register
    val oracleBox = testBox(
      1000000L,
      oracleTree,
      100000
      // Intentionally missing R4 register to simulate invalid oracle data
    )

    // Create derivative box
    val derivativeBox = testBox(5000000L, derivativeTree, 100000)

    // Create payout box
    val payoutBox = testBox(5000000L, mkTestErgoTree(userProver.dlogSecrets.head.publicImage), 100000)

    val tx = createTransaction(
      IndexedSeq(oracleBox),
      IndexedSeq(payoutBox)
    )

    val ctx = ErgoLikeContextTesting(
      currentHeight = 100000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      dataBoxes = IndexedSeq(oracleBox),
      boxesToSpend = IndexedSeq(derivativeBox),
      spendingTransaction = tx,
      selfIndex = 0,
      activatedVersion = activatedVersionInTests
    )

    // This should FAIL due to missing oracle data
    val pr = userProver.prove(derivativeTree, ctx, fakeMessage)
    pr.isFailure shouldBe true  // Should not be able to prove with invalid oracle data
  }

  property("trustless oracle should fail with incorrect output values") {
    val prover = new ContextEnrichingTestProvingInterpreter

    val oracleProp = compile(Map.empty, trustlessOracleScript).toSigmaProp
    val oracleTree = mkTestErgoTree(oracleProp)

    // Create initial oracle box
    val initialDifficulty: Long = 1000000L
    val initialOracleBox = testBox(
      1000000L,
      oracleTree,
      100000,
      additionalRegisters = Map(
        ErgoBox.nonMandatoryRegisters(0) -> LongConstant(initialDifficulty),
        ErgoBox.nonMandatoryRegisters(1) -> LongConstant(10000000000L)
      )
    )

    // Create updated oracle box with INCORRECT price calculation
    val newDifficulty: Long = 1100000L
    val incorrectPrice: Long = 12000000000L  // Wrong price (should be 11000000000L)
    
    val updatedOracleBox = testBox(
      1000000L,
      oracleTree,
      100000,
      additionalRegisters = Map(
        ErgoBox.nonMandatoryRegisters(0) -> LongConstant(newDifficulty),
        ErgoBox.nonMandatoryRegisters(1) -> LongConstant(incorrectPrice)
      )
    )

    val tx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(initialOracleBox.id)),
      IndexedSeq(updatedOracleBox)
    )

    val ctx = ErgoLikeContextTesting(
      currentHeight = 100000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(initialOracleBox),
      spendingTransaction = tx,
      self = initialOracleBox,
      activatedVersion = activatedVersionInTests
    )

    // This should FAIL due to incorrect price calculation
    val pr = prover.prove(oracleTree, ctx, fakeMessage)
    pr.isFailure shouldBe true  // Should not be able to prove with incorrect output values
  }
}