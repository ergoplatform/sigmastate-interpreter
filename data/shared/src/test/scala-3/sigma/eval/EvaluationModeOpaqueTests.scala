package sigma.eval

import scala.compiletime.testing.typeCheckErrors
import sigma.BaseTests
import sigma.eval.EvalSettings.{AotEvaluationMode, EvaluationMode, JitEvaluationMode}

class EvaluationModeOpaqueTests extends BaseTests {
  test("evaluation modes retain their integer representation") {
    AotEvaluationMode.value shouldBe 1
    JitEvaluationMode.value shouldBe 2
    EvaluationMode(1) shouldBe AotEvaluationMode
    EvaluationMode(2) shouldBe JitEvaluationMode
  }

  test("evaluation modes require explicit conversion to and from Int") {
    assert(typeCheckErrors("""
      import sigma.eval.EvalSettings.EvaluationMode
      val mode: EvaluationMode = 1
    """).nonEmpty)
    assert(typeCheckErrors("""
      import sigma.eval.EvalSettings.AotEvaluationMode
      val value: Int = AotEvaluationMode
    """).nonEmpty)
    assert(typeCheckErrors("""
      import sigma.eval.EvalSettings.EvaluationMode
      val mode: EvaluationMode = EvaluationMode(1)
      val value: Int = mode.value
    """).isEmpty)
  }
}
