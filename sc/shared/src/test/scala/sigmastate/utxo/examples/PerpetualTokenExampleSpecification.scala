package sigmastate.utxo.examples

import org.ergoplatform._
import sigma.data.AvlTreeData
import sigmastate._
import sigmastate.helpers._
import sigmastate.helpers.TestingHelpers._

class PerpetualTokenExampleSpecification extends CompilerTestingCommons with CompilerCrossVersionProps {
  private implicit lazy val IR: TestingIRContext = new TestingIRContext

  /**
    * Perpetual Token Example:
    * A token which is guaranteed to exist forever, unless it gets garbage-collected.
    * 
    * The script ensures that when the box is spent, at least one output box must have:
    * - The same proposition bytes (same script)
    * - The same tokens (same token collection)
    * 
    * This creates a "perpetual" token that cannot be destroyed through normal spending,
    * only through garbage collection (when the box value becomes too small).
    * 
    * Based on the ErgoForum post: https://www.ergoforum.org/t/a-perpetual-token/205
    */
  property("basic perpetual token contract") {
    // Demonstrates the original perpetual token concept from the ErgoForum post
    // A token that must be preserved in at least one output box with the same script and tokens
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    // Compile the perpetual token contract
    val perpetualScript = """
      |{
      |  val isPerpetual = {(b: Box) => 
      |    b.propositionBytes == SELF.propositionBytes && 
      |    b.tokens == SELF.tokens 
      |  }
      |  sigmaProp(OUTPUTS.exists(isPerpetual))
      |}
      |""".stripMargin

    val perpetualProp = compile(Map.empty, perpetualScript).toSigmaProp
    val perpetualTree = mkTestErgoTree(perpetualProp)

    // Create test boxes with the perpetual token contract
    val inputBox = testBox(1000000L, perpetualTree, 100000)
    val outputBox = testBox(1000000L, perpetualTree, 100000)
    
    // Create a transaction that preserves the perpetual token
    val tx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(inputBox.id)),
      IndexedSeq(outputBox)
    )

    val ctx = ErgoLikeContextTesting(
      currentHeight = 100000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(inputBox),
      tx,
      self = inputBox,
      activatedVersionInTests
    )

    // This should succeed as the transaction preserves the perpetual token
    val pr = prover.prove(perpetualTree, ctx, fakeMessage).get
    verifier.verify(perpetualTree, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("enhanced perpetual token with minimum value") {
    // Demonstrates an enhanced version that prevents garbage collection
    // by requiring output boxes to have a minimum value
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    // Enhanced version with minimum value protection to prevent garbage collection
    val enhancedScript = """
      |{
      |  val isPerpetual = {(b: Box) => 
      |    b.propositionBytes == SELF.propositionBytes && 
      |    b.tokens == SELF.tokens &&
      |    b.value >= 1000000 // minimum value to prevent garbage collection
      |  }
      |  sigmaProp(OUTPUTS.exists(isPerpetual))
      |}
      |""".stripMargin

    val enhancedProp = compile(Map.empty, enhancedScript).toSigmaProp
    val enhancedTree = mkTestErgoTree(enhancedProp)

    // Create test boxes with sufficient value
    val inputBox = testBox(2000000L, enhancedTree, 100000)
    val outputBox = testBox(2000000L, enhancedTree, 100000)
    
    val tx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(inputBox.id)),
      IndexedSeq(outputBox)
    )

    val ctx = ErgoLikeContextTesting(
      currentHeight = 100000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(inputBox),
      tx,
      self = inputBox,
      activatedVersionInTests
    )

    val pr = prover.prove(enhancedTree, ctx, fakeMessage).get
    verifier.verify(enhancedTree, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("perpetual token with multiple outputs") {
    // Demonstrates that the contract works when there are multiple outputs
    // and only one needs to preserve the perpetual token
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    // Simple perpetual token contract
    val simpleScript = """
      |{
      |  val isPerpetual = {(b: Box) => 
      |    b.propositionBytes == SELF.propositionBytes
      |  }
      |  sigmaProp(OUTPUTS.exists(isPerpetual))
      |}
      |""".stripMargin

    val simpleProp = compile(Map.empty, simpleScript).toSigmaProp
    val simpleTree = mkTestErgoTree(simpleProp)

    // Create test boxes with multiple outputs
    val inputBox = testBox(3000000L, simpleTree, 100000)
    val outputBox1 = testBox(1000000L, simpleTree, 100000)
    val outputBox2 = testBox(1000000L, simpleTree, 100000)
    val outputBox3 = testBox(1000000L, simpleTree, 100000)
    
    val tx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(inputBox.id)),
      IndexedSeq(outputBox1, outputBox2, outputBox3)
    )

    val ctx = ErgoLikeContextTesting(
      currentHeight = 100000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(inputBox),
      tx,
      self = inputBox,
      activatedVersionInTests
    )

    val pr = prover.prove(simpleTree, ctx, fakeMessage).get
    verifier.verify(simpleTree, ctx, pr, fakeMessage).get._1 shouldBe true
  }



  property("perpetual token contract fails when token not preserved") {
    // Demonstrates the security property: the contract correctly fails
    // when no output preserves the perpetual token
    val prover = new ContextEnrichingTestProvingInterpreter

    // Compile the perpetual token contract
    val perpetualScript = """
      |{
      |  val isPerpetual = {(b: Box) => 
      |    b.propositionBytes == SELF.propositionBytes && 
      |    b.tokens == SELF.tokens 
      |  }
      |  sigmaProp(OUTPUTS.exists(isPerpetual))
      |}
      |""".stripMargin

    val perpetualProp = compile(Map.empty, perpetualScript).toSigmaProp
    val perpetualTree = mkTestErgoTree(perpetualProp)

    // Create test boxes - input has perpetual contract, output has different contract
    val inputBox = testBox(1000000L, perpetualTree, 100000)
    
    // Create a different contract for the output (not perpetual)
    val differentScript = """
      |{
      |  sigmaProp(true)
      |}
      |""".stripMargin
    val differentProp = compile(Map.empty, differentScript).toSigmaProp
    val differentTree = mkTestErgoTree(differentProp)
    
    val outputBox = testBox(1000000L, differentTree, 100000)
    
    val tx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(inputBox.id)),
      IndexedSeq(outputBox)
    )

    val ctx = ErgoLikeContextTesting(
      currentHeight = 100000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(inputBox),
      tx,
      self = inputBox,
      activatedVersionInTests
    )

    // This should fail as the output doesn't preserve the perpetual token
    prover.prove(perpetualTree, ctx, fakeMessage).isSuccess shouldBe false
  }
}