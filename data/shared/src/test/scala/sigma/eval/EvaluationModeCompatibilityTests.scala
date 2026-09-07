package sigma.eval

import sigma.BaseTests
import sigma.eval.EvalSettings.{AotEvaluationMode, EvaluationMode, JitEvaluationMode}
import sigma.eval.EvalSettings.EvaluationMode.EvaluationModeOps

class EvaluationModeCompatibilityTests extends BaseTests {
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

  test("evaluation mode names") {
    AotEvaluationMode.name shouldBe "AotEvaluationMode"
    JitEvaluationMode.name shouldBe "JitEvaluationMode"
  }

  test("undefined evaluation modes throw MatchError") {
    Seq(Int.MinValue, -1, 0, 3, Int.MaxValue).foreach { value =>
      withClue(s"evaluation mode $value: ") {
        intercept[MatchError] {
          (EvaluationMode @@ value).name
        }
      }
    }
  }
}
