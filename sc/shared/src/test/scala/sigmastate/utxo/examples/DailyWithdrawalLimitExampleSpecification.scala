package sigmastate.utxo.examples

import org.ergoplatform._
import sigma.data.AvlTreeData
import sigmastate._
import sigmastate.helpers._
import sigmastate.helpers.TestingHelpers._
import sigma.ast._

class DailyWithdrawalLimitExampleSpecification extends CompilerTestingCommons with CompilerCrossVersionProps {
  private implicit lazy val IR: TestingIRContext = new TestingIRContext
  
  // Register definitions for daily withdrawal limit tracking
  private val R4 = ErgoBox.nonMandatoryRegisters(0) // R4: remaining limit (Long)
  private val R5 = ErgoBox.nonMandatoryRegisters(1) // R5: period start block (Int)

  // Common daily withdrawal limit contract script
  private val dailyLimitScript = """
    |{
    |  val dailyLimit = 1000000L  // 1 ERG daily limit
    |  val blocksPerDay = 720     // 720 blocks per day
    |  
    |  // Get current period info from registers
    |  val currentRemainingLimit = SELF.R4[Long].getOrElse(dailyLimit)
    |  val periodStartBlock = SELF.R5[Int].getOrElse(HEIGHT)
    |  
    |  // Check if we're in a new period
    |  val isNewPeriod = HEIGHT >= periodStartBlock + blocksPerDay
    |  
    |  // Assume remaining output is always at index 0
    |  val remainingOutput = OUTPUTS(0)
    |  
    |  // Calculate withdrawal amount (sum of all outputs except the remaining one)
    |  val withdrawalAmount = OUTPUTS.fold(0L, { (acc: Long, box: Box) => 
    |    if (box.id == remainingOutput.id) acc else acc + box.value 
    |  })
    |  
    |  // Calculate remaining limit for this transaction
    |  val newRemainingLimit = if (isNewPeriod) {
    |    // New period, reset limit
    |    dailyLimit - withdrawalAmount
    |  } else {
    |    // Same period, subtract withdrawal
    |    currentRemainingLimit - withdrawalAmount
    |  }
    |  
    |  // Calculate new period start block
    |  val newPeriodStartBlock = if (isNewPeriod) {
    |    // Start of current period
    |    HEIGHT
    |  } else {
    |    // Keep existing period
    |    periodStartBlock
    |  }
    |  
    |  // Conditions:
    |  // 1. Withdrawal doesn't exceed the appropriate limit (current or daily)
    |  // 2. New remaining limit is non-negative
    |  // 3. Remaining output has correct value and updated registers
    |  val withdrawalValid = if (isNewPeriod) {
    |    // In new period, check against daily limit
    |    withdrawalAmount <= dailyLimit && newRemainingLimit >= 0
    |  } else {
    |    // In same period, check against current remaining limit
    |    withdrawalAmount <= currentRemainingLimit && newRemainingLimit >= 0
    |  }
    |  
    |  // Check if the remaining output has correct value and updated registers
    |  val remainingOutputValid = 
    |    remainingOutput.propositionBytes == SELF.propositionBytes &&
    |    remainingOutput.value == SELF.value - withdrawalAmount &&
    |    remainingOutput.R4[Long].get == newRemainingLimit &&
    |    remainingOutput.R5[Int].get == newPeriodStartBlock
    |  
    |  sigmaProp(withdrawalValid && remainingOutputValid)
    |}
    |""".stripMargin

  // Compiled contract properties
  private lazy val dailyLimitProp = compile(Map.empty, dailyLimitScript).toSigmaProp
  private lazy val dailyLimitTree = mkTestErgoTree(dailyLimitProp)

  /**
    * Daily Withdrawal Limit Example:
    * A contract that enforces a daily withdrawal limit based on block height.
    * 
    * The script tracks withdrawals within 24-hour periods (720 blocks) and ensures:
    * - Withdrawals cannot exceed a daily limit
    * - The limit resets every 720 blocks
    * - Previous period's remaining limit is tracked in R4 register
    * - Current period start block is tracked in R5 register
    * 
    * Based on the ErgoForum post: https://www.ergoforum.org/t/ergoscript-design-patterns/222/11
    */
  property("basic daily withdrawal limit contract") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    // Create initial box with full daily limit
    val initialBox = testBox(
      value = 5000000L, 
      ergoTree = dailyLimitTree, 
      creationHeight = 100000,
      additionalRegisters = Map(
        R4 -> LongConstant(1000000L), // R4: remaining limit = 1 ERG
        R5 -> IntConstant(100000)      // R5: period start block = 100000
      )
    )

    // Create a dummy input to make the transaction valid
    val dummyInput = testBox(1000000L, TrueTree, 100000)

    // Create output box with updated registers after withdrawal (must be at index 0)
    val withdrawalAmount = 500000L // 0.5 ERG withdrawal
    val remainingBox = testBox(
      value = initialBox.value - withdrawalAmount, 
      ergoTree = dailyLimitTree, 
      creationHeight = 100000,
      additionalRegisters = Map(
        R4 -> LongConstant(500000L),  // R4: new remaining limit = 0.5 ERG
        R5 -> IntConstant(100000)      // R5: same period start block
      )
    )
    
    // Create a withdrawal output (index 1) to make withdrawal amount non-zero
    val withdrawalBox = testBox(
      value = withdrawalAmount, 
      ergoTree = TrueTree, 
      creationHeight = 100000
    )
    
    val tx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(initialBox.id), new UnsignedInput(dummyInput.id)),
      IndexedSeq(remainingBox, withdrawalBox)
    )

    val ctx = ErgoLikeContextTesting(
      currentHeight = 100100, // Within same period (100000 + 100 < 720)
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(initialBox, dummyInput),
      tx,
      self = initialBox,
      activatedVersionInTests
    )

    // This should succeed as withdrawal is within daily limit
    val pr = prover.prove(dailyLimitTree, ctx, fakeMessage).get
    verifier.verify(dailyLimitTree, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("daily withdrawal limit with period reset") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    // Create box at end of period with small remaining limit
    val initialBox = testBox(
      value = 5000000L, 
      ergoTree = dailyLimitTree, 
      creationHeight = 100000,
      additionalRegisters = Map(
        R4 -> LongConstant(100000L), // R4: remaining limit = 0.1 ERG (small amount)
        R5 -> IntConstant(100000)     // R5: period start block = 100000
      )
    )

    // Create a dummy input to make the transaction valid
    val dummyInput = testBox(1000000L, TrueTree, 100000)

    // New period starts at block 100721 - we can withdraw up to 1.0 ERG now
    val withdrawalAmount = 500000L // 0.5 ERG withdrawal (within new period limit of 1.0 ERG)
    val remainingBox = testBox(
      value = initialBox.value - withdrawalAmount, 
      ergoTree = dailyLimitTree, 
      creationHeight = 100721,
      additionalRegisters = Map(
        R4 -> LongConstant(500000L),  // R4: new remaining limit = 0.5 ERG (1.0 - 0.5)
        R5 -> IntConstant(100721)      // R5: new period start block
      )
    )
    
    // Create a withdrawal output (index 1) to make withdrawal amount non-zero
    val withdrawalBox = testBox(
      value = withdrawalAmount, 
      ergoTree = TrueTree, 
      creationHeight = 100721
    )
    
    val tx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(initialBox.id), new UnsignedInput(dummyInput.id)),
      IndexedSeq(remainingBox, withdrawalBox)
    )

    val ctx = ErgoLikeContextTesting(
      currentHeight = 100721, // New period (100000 + 721 >= 720)
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(initialBox, dummyInput),
      tx,
      self = initialBox,
      activatedVersionInTests
    )

    // This should succeed as we're in a new period with reset limit
    val pr = prover.prove(dailyLimitTree, ctx, fakeMessage).get
    verifier.verify(dailyLimitTree, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("daily withdrawal limit fails when exceeded") {
    val prover = new ContextEnrichingTestProvingInterpreter

    // Create box with small remaining limit
    val initialBox = testBox(
      value = 5000000L, 
      ergoTree = dailyLimitTree, 
      creationHeight = 100000,
      additionalRegisters = Map(
        R4 -> LongConstant(100000L), // R4: remaining limit = 0.1 ERG
        R5 -> IntConstant(100000)     // R5: period start block = 100000
      )
    )

    // Try to withdraw more than remaining limit
    val withdrawalAmount = 200000L // 0.2 ERG withdrawal (exceeds 0.1 ERG limit)
    val remainingBox = testBox(
      value = initialBox.value - withdrawalAmount, 
      ergoTree = dailyLimitTree, 
      creationHeight = 100100,
      additionalRegisters = Map(
        R4 -> LongConstant(-100000L), // R4: would be negative (invalid)
        R5 -> IntConstant(100000)      // R5: same period
      )
    )
    
    val tx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(initialBox.id)),
      IndexedSeq(remainingBox)
    )

    val ctx = ErgoLikeContextTesting(
      currentHeight = 100100, // Within same period
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(initialBox),
      tx,
      self = initialBox,
      activatedVersionInTests
    )

    // This should fail as withdrawal exceeds daily limit
    prover.prove(dailyLimitTree, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("daily withdrawal limit with multiple outputs") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    // Create box with full limit
    val initialBox = testBox(
      value = 5000000L, 
      ergoTree = dailyLimitTree, 
      creationHeight = 100000,
      additionalRegisters = Map(
        R4 -> LongConstant(1000000L), // R4: full limit
        R5 -> IntConstant(100000)      // R5: period start
      )
    )

    // Create multiple output boxes with tracking box at index 0
    val withdrawalAmount = 700000L // Total withdrawal = 0.7 ERG
    val trackingBox = testBox(
      value = initialBox.value - withdrawalAmount, 
      ergoTree = dailyLimitTree, 
      creationHeight = 100100,
      additionalRegisters = Map(
        R4 -> LongConstant(300000L),  // R4: new remaining limit = 0.3 ERG
        R5 -> IntConstant(100000)      // R5: same period
      )
    )
    val outputBox1 = testBox(
      value = 300000L, 
      ergoTree = dailyLimitTree, 
      creationHeight = 100100
    )
    val outputBox2 = testBox(
      value = 400000L, 
      ergoTree = dailyLimitTree, 
      creationHeight = 100100
    )
    
    val tx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(initialBox.id)),
      IndexedSeq(trackingBox, outputBox1, outputBox2)
    )

    val ctx = ErgoLikeContextTesting(
      currentHeight = 100100, // Within same period
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(initialBox),
      tx,
      self = initialBox,
      activatedVersionInTests
    )

    // This should succeed as total withdrawal is within limit and tracking box exists
    val pr = prover.prove(dailyLimitTree, ctx, fakeMessage).get
    verifier.verify(dailyLimitTree, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("daily withdrawal limit fails when output value incorrect") {
    val prover = new ContextEnrichingTestProvingInterpreter

    // Create initial box with full daily limit
    val initialBox = testBox(
      value = 5000000L, 
      ergoTree = dailyLimitTree, 
      creationHeight = 100000,
      additionalRegisters = Map(
        R4 -> LongConstant(1000000L), // R4: remaining limit = 1 ERG
        R5 -> IntConstant(100000)      // R5: period start block = 100000
      )
    )

    // Create output box with WRONG value (should be 4500000L but is 4600000L)
    val withdrawalAmount = 500000L // 0.5 ERG withdrawal
    val remainingBox = testBox(
      value = initialBox.value - withdrawalAmount + 100000L, // WRONG: 100000 extra
      ergoTree = dailyLimitTree, 
      creationHeight = 100000,
      additionalRegisters = Map(
        R4 -> LongConstant(500000L),  // R4: new remaining limit = 0.5 ERG
        R5 -> IntConstant(100000)      // R5: same period start block
      )
    )
    
    val tx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(initialBox.id)),
      IndexedSeq(remainingBox)
    )

    val ctx = ErgoLikeContextTesting(
      currentHeight = 100100, // Within same period (100000 + 100 < 720)
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(initialBox),
      tx,
      self = initialBox,
      activatedVersionInTests
    )

    // This should fail as output value doesn't match expected value
    prover.prove(dailyLimitTree, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("daily withdrawal limit prevents double-counting bug") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    // Create initial box with full daily limit
    val initialBox = testBox(
      value = 5000000L, 
      ergoTree = dailyLimitTree, 
      creationHeight = 100000,
      additionalRegisters = Map(
        R4 -> LongConstant(1000000L), // R4: remaining limit = 1 ERG
        R5 -> IntConstant(100000)      // R5: period start block = 100000
      )
    )

    // This test demonstrates that our implementation correctly handles the case
    // where there are multiple outputs including the remaining box
    // With the buggy implementation (summing ALL outputs), this would fail
    // With our correct implementation (excluding remaining box), this should succeed
    
    val actualWithdrawal = 500000L // 0.5 ERG actual withdrawal
    val remainingBoxValue = initialBox.value - actualWithdrawal // 4.5 ERG
    
    // Create remaining box at index 0
    val remainingBox = testBox(
      value = remainingBoxValue, 
      ergoTree = dailyLimitTree, 
      creationHeight = 100000,
      additionalRegisters = Map(
        R4 -> LongConstant(500000L),  // R4: new remaining limit = 0.5 ERG
        R5 -> IntConstant(100000)      // R5: same period start block
      )
    )
    
    // Create additional withdrawal output
    val withdrawalBox = testBox(
      value = actualWithdrawal, 
      ergoTree = dailyLimitTree, 
      creationHeight = 100000
    )
    
    val tx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(initialBox.id)),
      IndexedSeq(remainingBox, withdrawalBox)
    )

    val ctx = ErgoLikeContextTesting(
      currentHeight = 100100, // Within same period
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(initialBox),
      tx,
      self = initialBox,
      activatedVersionInTests
    )

    // This should SUCCEED with our correct implementation
    // With the buggy implementation, this would fail because:
    // withdrawalAmount = remainingBox.value (4.5 ERG) + withdrawalBox.value (0.5 ERG) = 5.0 ERG
    // But actual withdrawal is only 0.5 ERG
    // So the contract would incorrectly think we're withdrawing 5.0 ERG
    val pr = prover.prove(dailyLimitTree, ctx, fakeMessage).get
    verifier.verify(dailyLimitTree, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("daily withdrawal limit across multiple epochs") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    // Test scenario: Simulate multiple epochs by testing different period start blocks
    // This tests that the contract correctly handles period transitions

    // Test case 1: Same period withdrawal
    val samePeriodBox = testBox(
      value = 5000000L,
      ergoTree = dailyLimitTree,
      creationHeight = 100000,
      additionalRegisters = Map(
        R4 -> LongConstant(800000L), // 0.8 ERG remaining
        R5 -> IntConstant(100000)     // Period start block
      )
    )

    val samePeriodWithdrawal = 200000L
    val samePeriodRemainingBox = testBox(
      value = samePeriodBox.value - samePeriodWithdrawal,
      ergoTree = dailyLimitTree,
      creationHeight = 100100,
      additionalRegisters = Map(
        R4 -> LongConstant(600000L), // 0.6 ERG remaining (0.8 - 0.2)
        R5 -> IntConstant(100000)     // Same period
      )
    )
    val samePeriodWithdrawalBox = testBox(
      value = samePeriodWithdrawal,
      ergoTree = TrueTree,
      creationHeight = 100100
    )

    val samePeriodDummyInput = testBox(1000000L, TrueTree, 100000)

    val samePeriodTx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(samePeriodBox.id), new UnsignedInput(samePeriodDummyInput.id)),
      IndexedSeq(samePeriodRemainingBox, samePeriodWithdrawalBox)
    )

    val samePeriodCtx = ErgoLikeContextTesting(
      currentHeight = 100100, // Within same period (100000 + 100 < 720)
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(samePeriodBox, samePeriodDummyInput),
      samePeriodTx,
      self = samePeriodBox,
      activatedVersionInTests
    )

    // Same period should succeed
    val samePeriodPr = prover.prove(dailyLimitTree, samePeriodCtx, fakeMessage).get
    verifier.verify(dailyLimitTree, samePeriodCtx, samePeriodPr, fakeMessage).get._1 shouldBe true

    // Test case 2: New period withdrawal with reset limit
    val newPeriodBox = testBox(
      value = 5000000L,
      ergoTree = dailyLimitTree,
      creationHeight = 100000,
      additionalRegisters = Map(
        R4 -> LongConstant(100000L), // 0.1 ERG remaining (will reset to 1.0 in new period)
        R5 -> IntConstant(100000)     // Old period start block
      )
    )

    val newPeriodWithdrawal = 800000L // 0.8 ERG withdrawal (within new period limit of 1.0 ERG)
    val newPeriodRemainingBox = testBox(
      value = newPeriodBox.value - newPeriodWithdrawal,
      ergoTree = dailyLimitTree,
      creationHeight = 100721,
      additionalRegisters = Map(
        R4 -> LongConstant(200000L), // 0.2 ERG remaining (1.0 - 0.8)
        R5 -> IntConstant(100721)     // New period start block
      )
    )
    val newPeriodWithdrawalBox = testBox(
      value = newPeriodWithdrawal,
      ergoTree = TrueTree,
      creationHeight = 100721
    )

    val newPeriodDummyInput = testBox(1000000L, TrueTree, 100721)

    val newPeriodTx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(newPeriodBox.id), new UnsignedInput(newPeriodDummyInput.id)),
      IndexedSeq(newPeriodRemainingBox, newPeriodWithdrawalBox)
    )

    val newPeriodCtx = ErgoLikeContextTesting(
      currentHeight = 100721, // New period (100000 + 721 >= 720)
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(newPeriodBox, newPeriodDummyInput),
      newPeriodTx,
      self = newPeriodBox,
      activatedVersionInTests
    )

    // New period should succeed (limit reset to 1.0 ERG)
    val newPeriodPr = prover.prove(dailyLimitTree, newPeriodCtx, fakeMessage).get
    verifier.verify(dailyLimitTree, newPeriodCtx, newPeriodPr, fakeMessage).get._1 shouldBe true
  }
}