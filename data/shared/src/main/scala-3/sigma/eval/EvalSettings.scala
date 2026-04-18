package sigma.eval

import sigma.eval.EvalSettings.EvaluationMode

/** Configuration parameters of the evaluation run. */
case class EvalSettings(
    /** Used together with [[ErgoTreeEvaluator.profiler]] to measure individual operations timings. */
    isMeasureOperationTime: Boolean,

    /** Used together with [[ErgoTreeEvaluator.profiler]] to measure script timings. */
    isMeasureScriptTime: Boolean,

    /** Used by [[ErgoTreeEvaluator]] to conditionally perform debug mode operations. */
    isDebug: Boolean = false,

    /** Used by [[ErgoTreeEvaluator]] to conditionally emit log messages. */
    isLogEnabled: Boolean = false,

    /** Used by [[ErgoTreeEvaluator]] to conditionally build a trace of added costs.
      *
      * @see Value.addCost
      */
    costTracingEnabled: Boolean = false,

    /** Profiler which, when defined, should be used in [[ErgoTreeEvaluator]] constructor. */
    profilerOpt: Option[Profiler] = None,

    /** Should be set to true, if evaluation is performed as part of test suite.
      * In such a case, additional operations may be performed (such as sanity checks). */
    isTestRun: Boolean = false,

    /** If true, then expected test vectors are pretty-printed. */
    printTestVectors: Boolean = false,

    /** When Some(mode) is specified then it defines which version of the Interpreter.verify
      * and Interpreter.prove methods should use.
      * The default value is None, which means the version is defined by ErgoTree.version
      * and Context.activatedScriptVersion.
      */
    evaluationMode: Option[EvaluationMode] = None,

    /** Maximum execution cost of a script used by profiler.
      *
      * @see ErgoTreeEvaluator
      */
    scriptCostLimitInEvaluator: Int = 1000000
)

object EvalSettings {
  /** Enumeration type of evaluation modes of [[Interpreter]].
    * This type can be removed in v5.x releases together with AOT implementation once v5.0
    * protocol is activated.
    */
  opaque type EvaluationMode = Int
  
  object EvaluationMode {
    def apply(i: Int): EvaluationMode = i
  }
  
  extension (x: EvaluationMode) {
    def name: String = x match {
      case 1 => "AotEvaluationMode"
      case 2 => "JitEvaluationMode"
      case _ => "UnknownEvaluationMode"
    }

    /** Returns true if AOT interpreter should be evaluated. */
    def okEvaluateAot: Boolean = {
      x == AotEvaluationMode
    }

    /** Returns true if JIT interpreter should be evaluated. */
    def okEvaluateJit: Boolean = {
      x == JitEvaluationMode
    }
  }

  /** Evaluation mode when the interpreter is executing using AOT costing implementation
    * of v4.x protocol. */
  val AotEvaluationMode: EvaluationMode = 1 // first bit

  /** Evaluation mode when the interpreter is executing using JIT costing implementation
    * of v5.x protocol. */
  val JitEvaluationMode: EvaluationMode = 2 // second bit
}
