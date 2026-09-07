package sigma.eval

import sigma.BaseTests
import sigma.eval.EvalSettings.{AotEvaluationMode, EvaluationMode, JitEvaluationMode}

class EvaluationModeCompatibilityTests extends BaseTests {
  test("evaluation mode names") {
    AotEvaluationMode.name shouldBe "AotEvaluationMode"
    JitEvaluationMode.name shouldBe "JitEvaluationMode"
  }

  test("evaluation modes select only their matching interpreter") {
    AotEvaluationMode.okEvaluateAot shouldBe true
    AotEvaluationMode.okEvaluateJit shouldBe false
    JitEvaluationMode.okEvaluateAot shouldBe false
    JitEvaluationMode.okEvaluateJit shouldBe true
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
