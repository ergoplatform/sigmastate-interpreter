package sigmastate.utxo

import org.ergoplatform.ErgoBox.{AdditionalRegisters, R4, R5, Token}
import org.ergoplatform.{ErgoLikeTransaction, Input}
import scorex.util.encode.Base16
import sigma.Colls
import sigma.ast._
import sigma.ast.syntax._
import sigma.data.{AvlTreeData, Digest32Coll}
import sigma.exceptions.TyperException
import sigma.interpreter.{ContextExtension, ProverResult}
import sigmastate.CompilerCrossVersionProps
import sigmastate.helpers.TestingHelpers._
import sigmastate.helpers.{CompilerTestingCommons, ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTestInterpreter}
import sigmastate.interpreter.Interpreter.emptyEnv
import sigmastate.utils.Helpers._

import scala.collection.compat.immutable.ArraySeq

/** Specification of the global utility functions for common box verification patterns
  * (see https://github.com/ergoplatform/sigmastate-interpreter/issues/1037):
  * verifySameForBasicRequiredRegisters, verifySameForRequiredRegisters,
  * verifyUsedAdditionalRegisters, verifyBoxHasMarkerToken, verifyBoxHasNoMarkerToken,
  * verifySpentToken.
  *
  * These functions are compiler-level (frontend) utilities, i.e. they are expanded
  * during compilation into ErgoTree nodes already supported by the interpreter, so no
  * changes of the consensus-critical ErgoTree level are involved.
  */
class UtilFunctionsSpecification extends CompilerTestingCommons with CompilerCrossVersionProps {
  override val printVersions: Boolean = false
  implicit lazy val IR: TestingIRContext = new TestingIRContext

  private def tokenBytes(b: Byte): Array[Byte] = Array.fill(32)(b)
  private def mkToken(b: Byte): Digest32Coll = Digest32Coll @@ Colls.fromArray(tokenBytes(b))
  private def tokenHex(b: Byte): String = Base16.encode(tokenBytes(b))

  private val token1: Digest32Coll = mkToken(1)
  private val token2: Digest32Coll = mkToken(2)

  /** Compiles `script` to an ErgoTree guarding the spent box (`SELF`), builds a spending
    * transaction with a single output box (`OUTPUTS(0)`) and checks the result:
    * if `expectSuccess` the proposition must prove and verify, otherwise proving must fail
    * (which happens both when the proposition evaluates to `false` and when its evaluation
    * throws an exception).
    */
  private def testEval(
      script: String,
      selfValue: Long = 10,
      selfTokens: Seq[Token] = ArraySeq.empty,
      selfRegisters: AdditionalRegisters = Map.empty,
      outValue: Long = 10,
      outTokens: Seq[Token] = ArraySeq.empty,
      outRegisters: AdditionalRegisters = Map.empty,
      outSameScript: Boolean = true,
      expectSuccess: Boolean = true): Unit = {
    val prop = compile(emptyEnv, script).asBoolValue.toSigmaProp
    val tree = mkTestErgoTree(prop)
    val outTree = if (outSameScript) tree else TrueTree

    val boxToSpend = testBox(selfValue, tree, creationHeight = 5,
      additionalTokens = selfTokens, additionalRegisters = selfRegisters)
    val newBox = testBox(outValue, outTree, creationHeight = 0,
      additionalTokens = outTokens, additionalRegisters = outRegisters)
    val tx = new ErgoLikeTransaction(
      IndexedSeq(Input(boxToSpend.id, ProverResult(Array.emptyByteArray, ContextExtension.empty))),
      ArraySeq.empty,
      IndexedSeq(newBox))
    val ctx = ErgoLikeContextTesting(currentHeight = 0,
      lastBlockUtxoRoot = AvlTreeData.dummy, ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(boxToSpend),
      spendingTransaction = tx, self = boxToSpend, ergoTreeVersionInTests)

    val prover = new ContextEnrichingTestProvingInterpreter()
    val res = prover.prove(emptyEnv, tree, ctx, fakeMessage)
    if (expectSuccess) {
      val pr = res.getOrThrow
      val verifier = new ErgoLikeTestInterpreter
      verifier.verify(emptyEnv, tree, ctx.withExtension(pr.extension), pr.proof, fakeMessage)
        .getOrThrow._1 shouldBe true
    } else {
      res.isFailure shouldBe true
    }
  }

  // verifySameForBasicRequiredRegisters

  property("verifySameForBasicRequiredRegisters - same value and script") {
    testEval("sigmaProp(verifySameForBasicRequiredRegisters(SELF, OUTPUTS(0)))")
  }

  property("verifySameForBasicRequiredRegisters - different value") {
    testEval("sigmaProp(verifySameForBasicRequiredRegisters(SELF, OUTPUTS(0)) == false)",
      outValue = 20)
  }

  property("verifySameForBasicRequiredRegisters - different script") {
    testEval("sigmaProp(verifySameForBasicRequiredRegisters(SELF, OUTPUTS(0)) == false)",
      outSameScript = false)
  }

  // verifySameForRequiredRegisters

  property("verifySameForRequiredRegisters - same value, script and tokens") {
    testEval("sigmaProp(verifySameForRequiredRegisters(SELF, OUTPUTS(0)))",
      selfTokens = Seq(token1 -> 100L, token2 -> 1L),
      outTokens = Seq(token1 -> 100L, token2 -> 1L))
  }

  property("verifySameForRequiredRegisters - different token amount") {
    testEval("sigmaProp(verifySameForRequiredRegisters(SELF, OUTPUTS(0)) == false)",
      selfTokens = Seq(token1 -> 100L),
      outTokens = Seq(token1 -> 99L))
  }

  property("verifySameForRequiredRegisters - missing token") {
    testEval("sigmaProp(verifySameForRequiredRegisters(SELF, OUTPUTS(0)) == false)",
      selfTokens = Seq(token1 -> 100L))
  }

  // verifyUsedAdditionalRegisters

  property("verifyUsedAdditionalRegisters - exactly the used registers") {
    testEval("sigmaProp(verifyUsedAdditionalRegisters(SELF, 2))",
      selfRegisters = Map(R4 -> IntConstant(7), R5 -> LongConstant(1L)))
  }

  property("verifyUsedAdditionalRegisters - no additional registers") {
    testEval("sigmaProp(verifyUsedAdditionalRegisters(SELF, 0))")
  }

  property("verifyUsedAdditionalRegisters - fewer registers than allowed") {
    testEval("sigmaProp(verifyUsedAdditionalRegisters(SELF, 6))",
      selfRegisters = Map(R4 -> IntConstant(7)))
  }

  property("verifyUsedAdditionalRegisters - register beyond the allowed ones") {
    // NOTE: when a register beyond the first `used` ones is defined, checking that the
    // register is empty (`isEmpty` of the register read as `Option[Any]`) throws rather
    // than returning false, so the spending attempt fails, which is the desired
    // guarding behavior.
    testEval("sigmaProp(verifyUsedAdditionalRegisters(SELF, 1))",
      selfRegisters = Map(R4 -> IntConstant(7), R5 -> LongConstant(1L)),
      expectSuccess = false)
  }

  // verifyBoxHasMarkerToken

  property("verifyBoxHasMarkerToken - box with the marker token") {
    testEval(s"""sigmaProp(verifyBoxHasMarkerToken(SELF, fromBase16("${tokenHex(1)}")))""",
      selfTokens = Seq(token1 -> 5L))
  }

  property("verifyBoxHasMarkerToken - box without the marker token") {
    testEval(s"""sigmaProp(verifyBoxHasMarkerToken(SELF, fromBase16("${tokenHex(2)}")) == false)""",
      selfTokens = Seq(token1 -> 5L))
  }

  property("verifyBoxHasMarkerToken - box without tokens at all") {
    testEval(s"""sigmaProp(verifyBoxHasMarkerToken(SELF, fromBase16("${tokenHex(1)}")) == false)""")
  }

  // verifyBoxHasNoMarkerToken

  property("verifyBoxHasNoMarkerToken - box without the marker token") {
    testEval(s"""sigmaProp(verifyBoxHasNoMarkerToken(SELF, fromBase16("${tokenHex(2)}")))""",
      selfTokens = Seq(token1 -> 5L))
  }

  property("verifyBoxHasNoMarkerToken - box without tokens at all") {
    testEval(s"""sigmaProp(verifyBoxHasNoMarkerToken(SELF, fromBase16("${tokenHex(1)}")))""")
  }

  property("verifyBoxHasNoMarkerToken - box with the marker token") {
    testEval(s"""sigmaProp(verifyBoxHasNoMarkerToken(SELF, fromBase16("${tokenHex(1)}")) == false)""",
      selfTokens = Seq(token1 -> 5L))
  }

  // verifySpentToken

  property("verifySpentToken - token spent by exact amount, others preserved") {
    testEval(s"""sigmaProp(verifySpentToken(SELF, OUTPUTS(0), fromBase16("${tokenHex(1)}"), 40L))""",
      selfTokens = Seq(token1 -> 100L, token2 -> 7L),
      outTokens = Seq(token1 -> 60L, token2 -> 7L))
  }

  property("verifySpentToken - wrong spent amount") {
    testEval(s"""sigmaProp(verifySpentToken(SELF, OUTPUTS(0), fromBase16("${tokenHex(1)}"), 30L) == false)""",
      selfTokens = Seq(token1 -> 100L),
      outTokens = Seq(token1 -> 60L))
  }

  property("verifySpentToken - another token not preserved") {
    testEval(s"""sigmaProp(verifySpentToken(SELF, OUTPUTS(0), fromBase16("${tokenHex(1)}"), 40L) == false)""",
      selfTokens = Seq(token1 -> 100L, token2 -> 7L),
      outTokens = Seq(token1 -> 60L, token2 -> 6L))
  }

  property("verifySpentToken - token fully spent is not supported") {
    // when the token is completely absent in the output box, `exists` returns false,
    // so full spending (down to zero) makes the function return false by design
    testEval(s"""sigmaProp(verifySpentToken(SELF, OUTPUTS(0), fromBase16("${tokenHex(1)}"), 100L) == false)""",
      selfTokens = Seq(token1 -> 100L),
      outTokens = ArraySeq.empty)
  }

  property("verifySpentToken - inBox without the token is vacuously true") {
    // the check is a `forall` over the tokens of `inBox`: when `inBox` does not hold
    // `token` at all there is nothing to spend and the function returns true (pinned
    // here so that the semantics is asserted rather than implicit)
    testEval(s"""sigmaProp(verifySpentToken(SELF, OUTPUTS(0), fromBase16("${tokenHex(1)}"), 40L))""",
      selfTokens = Seq(token2 -> 7L),
      outTokens = Seq(token2 -> 7L))
  }

  property("verifySpentToken - inBox without any token is vacuously true") {
    testEval(s"""sigmaProp(verifySpentToken(SELF, OUTPUTS(0), fromBase16("${tokenHex(1)}"), 40L))""")
  }

  // name reservation

  private val utilFunctionNames = Seq(
    "verifySameForBasicRequiredRegisters", "verifySameForRequiredRegisters",
    "verifyUsedAdditionalRegisters", "verifyBoxHasMarkerToken",
    "verifyBoxHasNoMarkerToken", "verifySpentToken")

  property("util function names cannot be redefined by user scripts (same signature)") {
    // the typer seeds its environment with the predefined function names, so a
    // user-defined function with the same name is rejected before any expansion can
    // silently replace it
    val script =
      s"""{
        |  def verifyBoxHasMarkerToken(box: Box, token: Coll[Byte]): Boolean = false
        |  sigmaProp(verifyBoxHasMarkerToken(SELF, fromBase16("${tokenHex(1)}")))
        |}""".stripMargin
    val e = the[TyperException] thrownBy compile(emptyEnv, script)
    e.getMessage should include("already defined")
  }

  property("util function names cannot be redefined by user scripts (any signature)") {
    utilFunctionNames.foreach { name =>
      val script = s"{ def $name(x: Int): Int = x; sigmaProp($name(1) == 1) }"
      val e = the[TyperException] thrownBy compile(emptyEnv, script)
      e.getMessage should include(s"Variable $name already defined")
    }
  }

  property("util function names cannot be rebound by val in user scripts") {
    utilFunctionNames.foreach { name =>
      val script = s"{ val $name = 1; sigmaProp($name == 1) }"
      val e = the[TyperException] thrownBy compile(emptyEnv, script)
      e.getMessage should include(s"Variable $name already defined")
    }
  }
}
