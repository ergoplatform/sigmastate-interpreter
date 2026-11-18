package sigmastate.eval

import org.ergoplatform.{ErgoBoxCandidate, UnsignedErgoLikeTransaction, UnsignedInput}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scorex.crypto.authds.ADKey
import sigma.ast.syntax.TrueSigmaProp
import sigma.ast.{BooleanConstant, ErgoTree, IntConstant}
import sigma.interpreter.{ContextExtension, SigmaMap}
import sigma.{ContextVarsMap, ContractsTestkit, VersionContext}

class CContextSpecification extends AnyFunSuite with ContractsTestkit with Matchers {

  private def intConst(i: Int) = IntConstant(i)
  private def boolConst(b: Boolean) = BooleanConstant(b)

  private def ctxWithVars(vars: ContextVarsMap): CContext =
    newContext(0, newAliceBox(100), VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion, vars)

  test("getVar returns Some for present ids") {
    val vars = SigmaMap(Map[Byte, sigma.ast.EvaluatedValue[sigma.ast.SType]](
      5.toByte -> intConst(5),
      10.toByte -> intConst(10)
    ))
    val ctx = ctxWithVars(vars)

    ctx.getVar[Int](5.toByte) shouldBe Some(5)
    ctx.getVar[Int](10.toByte) shouldBe Some(10)
  }

  test("getVar returns None for negative, above-maxKey and absent ids") {
    val vars = SigmaMap(Map[Byte, sigma.ast.EvaluatedValue[sigma.ast.SType]](
      5.toByte -> intConst(5)
    ))
    val ctx = ctxWithVars(vars)

    ctx.getVar[Int]((-1).toByte) shouldBe None
    ctx.getVar[Int](127.toByte) shouldBe None
    ctx.getVar[Int](0.toByte) shouldBe None
  }

  test("getVar throws InvalidType when requested type does not match stored type") {
    val vars = SigmaMap(Map[Byte, sigma.ast.EvaluatedValue[sigma.ast.SType]](
      5.toByte -> intConst(5)
    ))
    val ctx = ctxWithVars(vars)

    ctx.getVar[Int](5.toByte) shouldBe Some(5)
    an[sigma.exceptions.InvalidType] should be thrownBy ctx.getVar[Boolean](5.toByte)
  }

  test("getVarFromInput reads variables from input extensions") {
    val extension = ContextExtension(SigmaMap(Map[Byte, sigma.ast.EvaluatedValue[sigma.ast.SType]](
      11.toByte -> boolConst(true)
    )))
    val input = new UnsignedInput(ADKey @@ newAliceBox(1).id.toArray, extension)
    val tx = UnsignedErgoLikeTransaction(
      inputs = IndexedSeq(input),
      outputCandidates = IndexedSeq(new ErgoBoxCandidate(1, ErgoTree.fromProposition(TrueSigmaProp), 0))
    )

    val ctx = ctxWithVars(SigmaMap.empty).copy(spendingTransaction = tx)

    ctx.getVarFromInput[Boolean](0.toShort, 11.toByte) shouldBe Some(true)
    ctx.getVarFromInput[Boolean](0.toShort, 12.toByte) shouldBe None
    ctx.getVarFromInput[Boolean](1.toShort, 11.toByte) shouldBe None
    ctx.getVarFromInput[Int](0.toShort, 11.toByte) shouldBe None
  }

  test("getVarFromInput returns None when input extension uses SigmaMap with sparse keys") {
    val extension = ContextExtension(SigmaMap(Map[Byte, sigma.ast.EvaluatedValue[sigma.ast.SType]](
      100.toByte -> intConst(42)
    )))
    val input = new UnsignedInput(ADKey @@ newAliceBox(1).id.toArray, extension)
    val tx = UnsignedErgoLikeTransaction(
      inputs = IndexedSeq(input),
      outputCandidates = IndexedSeq(new ErgoBoxCandidate(1, ErgoTree.fromProposition(TrueSigmaProp), 0))
    )

    val ctx = ctxWithVars(SigmaMap.empty).copy(spendingTransaction = tx)

    ctx.getVarFromInput[Int](0.toShort, 100.toByte) shouldBe Some(42)
    ctx.getVarFromInput[Int](0.toShort, 99.toByte) shouldBe None
  }
}
