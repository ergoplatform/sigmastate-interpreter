package sigma.eval

import sigma.BaseTests
import sigma.eval.EvalSettings.{AotEvaluationMode, JitEvaluationMode}
import sigma.eval.EvalSettings.EvaluationMode.EvaluationModeOps

class EvaluationModeValueClassTests extends BaseTests {
  test("evaluation mode wrappers retain value equality and hashing") {
    val first = new EvaluationModeOps(AotEvaluationMode)
    val same = new EvaluationModeOps(AotEvaluationMode)
    val different = new EvaluationModeOps(JitEvaluationMode)
    first shouldBe same
    first.hashCode() shouldBe same.hashCode()
    first.hashCode() shouldBe 1.hashCode()
    (first == different) shouldBe false
    first.equals("AotEvaluationMode") shouldBe false
  }
}
