package sigmastate.interpreter

import sigma.interpreter.{ContextExtension, SigmaMap}
import sigma.interpreter.ContextExtension.VarBinding
import sigma.validation.SigmaValidationSettings

/** Base class of the context passed to verifier and prover.
  * @see [[sigmastate.interpreter.Interpreter]]
  */
trait InterpreterContext {
  /** Prover-defined key-value pairs, that may be used inside a script. */
  val extension: ContextExtension

  /** Validation parameters passed to Interpreter.verify to detect soft-fork conditions. */
  val validationSettings: SigmaValidationSettings

  /** Hard limit on accumulated execution cost. Exceeding it leads to CostLimitException
    * to be thrown.
    */
  val costLimit: Long

  /** Initial value of execution cost already accumulated before `Interpreter.verify`(or
    * `prove`) is called.
    */
  val initCost: Long

  /** Maximum version of ErgoTree currently activated on the network. The activation is
    * performed via miners voting.
    * The maximum version supported by the interpreter is defined by
    * `Interpreter.MaxSupportedScriptVersion`. As a result, the execution of the
    * `Interpreter.verify` method depends on the relation between
    * max supported and activated version. (see docs/aot-jit-switch.md).
    */
  def activatedScriptVersion: Byte

  /** Creates a new instance with currErgoTreeVersion updated with the given value. */
  def withErgoTreeVersion(newVersion: Byte): InterpreterContext

  /** Creates a new instance with costLimit updated with given value. */
  def withCostLimit(newCostLimit: Long): InterpreterContext

  /** Creates a new instance with initCost updated with given value. */
  def withInitCost(newCost: Long): InterpreterContext

  /** Creates a new instance with extension updated with given value. */
  def withExtension(newExtension: ContextExtension): InterpreterContext

  /**
    * Creates a new instance with given bindings added to extension.
    * USED IN TESTS ONLY! thus not optimized for efficiency
    */
  def withBindings(bindings: VarBinding*): InterpreterContext = {
    val ext = extension.values.iterator.toMap ++ bindings
    withExtension(ContextExtension(SigmaMap(ext)))
  }

  /** Creates a new instance with given validation settings. */
  def withValidationSettings(newVs: SigmaValidationSettings): InterpreterContext

  /** Creates `sigma.Context` instance based on this context. The created instance
    * contains all data represented using types form [[sigma]] package.
    * These types are used internally by ErgoTree interpreter.
    * Thus, this method performs transformation from Ergo to internal Sigma representation
    * of all context data.
    *
    * @see sigmastate.eval.Evaluation
    */
  def toSigmaContext(): sigma.Context
}

