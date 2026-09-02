package sigmastate.utxo.examples

import org.ergoplatform._
import sigma.data.{AvlTreeData, Digest32Coll}
import sigma.ast.ErgoTree
import sigma.Colls
import sigmastate._
import sigmastate.helpers._
import sigmastate.helpers.TestingHelpers._
import scorex.crypto.hash.Blake2b256

/**
 * Tests for the Perpetual Token design pattern.
 *
 * Based on the ErgoForum post: https://www.ergoforum.org/t/a-perpetual-token/205
 * (script by jasondavies; the minimum-value enhancement against garbage collection
 * was suggested by scalahub in reply #4 of the same thread)
 *
 * A token protected by this script is guaranteed to exist forever, unless the box
 * gets garbage-collected: every spending transaction must send the whole token
 * collection (and the same script) into at least one output box.
 *
 * Note: this pattern involves no signature verification at all - neither Schnorr
 * in ErgoScript nor proveDlog; possession of the box is the spending right.
 */
class PerpetualTokenExampleSpecification extends CompilerTestingCommons with CompilerCrossVersionProps {
  private implicit lazy val IR: TestingIRContext = new TestingIRContext

  /** A singleton token standing in for the "perpetual" token protected by the script. */
  private val perpetualTokenId: Digest32Coll = Digest32Coll @@ Colls.fromArray(Blake2b256("perpetual-token"))

  /** The original contract from the forum post, verbatim. */
  private val perpetualScript = """
    |{
    |  val isPerpetual = {(b: Box) =>
    |    b.propositionBytes == SELF.propositionBytes &&
    |    b.tokens == SELF.tokens
    |  }
    |  sigmaProp(OUTPUTS.exists(isPerpetual))
    |}
    |""".stripMargin

  /** scalahub's enhancement from reply #4: require a minimum value to prevent garbage collection. */
  private val enhancedScript = """
    |{
    |  val isPerpetual = {(b: Box) =>
    |    b.propositionBytes == SELF.propositionBytes &&
    |    b.tokens == SELF.tokens &&
    |    b.value >= 1000000 // minimum value to prevent garbage collection
    |  }
    |  sigmaProp(OUTPUTS.exists(isPerpetual))
    |}
    |""".stripMargin

  private def compileTree(script: String): ErgoTree = {
    val prop = compile(Map.empty, script).toSigmaProp
    mkTestErgoTree(prop)
  }

  private def spendingCtx(selfBox: ErgoBox, tx: UnsignedErgoLikeTransaction): ErgoLikeContext =
    ErgoLikeContextTesting(
      currentHeight = 100000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(selfBox),
      tx,
      self = selfBox,
      activatedVersionInTests
    )

  property("basic perpetual token contract preserves a singleton token") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter
    val tree = compileTree(perpetualScript)

    // input box holds the singleton perpetual token
    val inputBox = testBox(1000000L, tree, 100000, additionalTokens = Seq((perpetualTokenId, 1L)))
    // the token (and the script) is carried over to the output
    val outputBox = testBox(1000000L, tree, 100000, additionalTokens = Seq((perpetualTokenId, 1L)))

    val tx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(inputBox.id)),
      IndexedSeq(outputBox)
    )

    val ctx = spendingCtx(inputBox, tx)

    val pr = prover.prove(tree, ctx, fakeMessage).get
    verifier.verify(tree, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("enhanced perpetual token with minimum value") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter
    val tree = compileTree(enhancedScript)

    val inputBox = testBox(2000000L, tree, 100000, additionalTokens = Seq((perpetualTokenId, 1L)))
    val outputBox = testBox(2000000L, tree, 100000, additionalTokens = Seq((perpetualTokenId, 1L)))

    val tx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(inputBox.id)),
      IndexedSeq(outputBox)
    )

    val ctx = spendingCtx(inputBox, tx)

    val pr = prover.prove(tree, ctx, fakeMessage).get
    verifier.verify(tree, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("enhanced perpetual token fails when output value drops below minimum") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val tree = compileTree(enhancedScript)

    val inputBox = testBox(2000000L, tree, 100000, additionalTokens = Seq((perpetualTokenId, 1L)))
    // token and script preserved, but value below the 1000000 minimum: box would be
    // garbage-collectable, which the enhanced contract must prevent
    val outputBox = testBox(500000L, tree, 100000, additionalTokens = Seq((perpetualTokenId, 1L)))

    val tx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(inputBox.id)),
      IndexedSeq(outputBox)
    )

    val ctx = spendingCtx(inputBox, tx)

    prover.prove(tree, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("perpetual token with multiple outputs, one preserving") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter
    val tree = compileTree(perpetualScript)

    val inputBox = testBox(3000000L, tree, 100000, additionalTokens = Seq((perpetualTokenId, 1L)))
    // only the first output carries the perpetual token forward; the others may do anything
    val outputBox1 = testBox(1000000L, tree, 100000, additionalTokens = Seq((perpetualTokenId, 1L)))
    val outputBox2 = testBox(1000000L, tree, 100000)
    val outputBox3 = testBox(1000000L, tree, 100000)

    val tx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(inputBox.id)),
      IndexedSeq(outputBox1, outputBox2, outputBox3)
    )

    val ctx = spendingCtx(inputBox, tx)

    val pr = prover.prove(tree, ctx, fakeMessage).get
    verifier.verify(tree, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("perpetual token contract fails when script is not preserved") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val tree = compileTree(perpetualScript)

    val inputBox = testBox(1000000L, tree, 100000, additionalTokens = Seq((perpetualTokenId, 1L)))

    // output keeps the token but has a different proposition: not a perpetual box
    val differentTree = compileTree("{ sigmaProp(true) }")
    val outputBox = testBox(1000000L, differentTree, 100000, additionalTokens = Seq((perpetualTokenId, 1L)))

    val tx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(inputBox.id)),
      IndexedSeq(outputBox)
    )

    val ctx = spendingCtx(inputBox, tx)

    prover.prove(tree, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("perpetual token contract fails when tokens are not preserved") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val tree = compileTree(perpetualScript)

    val inputBox = testBox(1000000L, tree, 100000, additionalTokens = Seq((perpetualTokenId, 1L)))

    // output keeps the script but drops the perpetual token: must be rejected,
    // this is the core token-preservation property of the pattern
    val outputBox = testBox(1000000L, tree, 100000)

    val tx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(inputBox.id)),
      IndexedSeq(outputBox)
    )

    val ctx = spendingCtx(inputBox, tx)

    prover.prove(tree, ctx, fakeMessage).isSuccess shouldBe false
  }
}
