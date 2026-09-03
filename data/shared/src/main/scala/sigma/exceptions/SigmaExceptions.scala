package sigma.exceptions

import sigma.SigmaException
import sigma.ast.JitCost

/** Exception thrown by [[sigmastate.interpreter.Interpreter]].
  *
  * @param message the error message
  * @param cause an optional cause for the exception
  */
class InterpreterException(message: String, cause: Option[Throwable] = None)
    extends SigmaException(message, cause)

/** Deterministic language-version failure raised when an ErgoTree structurally
  * contains the EIP-0045 opcode before its first admissible tree version.
  */
final class StarkOpcodeErgoTreeVersionException(
  val actualVersion: Int,
  val requiredVersion: Int = sigma.VersionContext.StarkVerificationVersion.toInt,
  message: String
) extends InterpreterException(message)

/** Deterministic failure raised when a parsed opcode has no active execution
  * capability. It is deliberately not a ValidationException: ordinary
  * soft-fork handling must not turn an executed unavailable opcode into a
  * Boolean value.
  *
  * @param opCode unsigned raw opcode byte in the range 0..255
  * @param message the error message
  */
final class OpcodeUnavailableException(
  val opCode: Int,
  message: String
) extends InterpreterException(message)

/** Deterministic, non-Boolean failure for a STARK profile marked quarantined
  * by the authenticated capability snapshot. Snapshot-transition validation
  * makes quarantine irreversible; ordinary soft-fork handling must not turn
  * it into authorization.
  */
final class StarkProfileQuarantinedException(
  val profileIdHex: String,
  message: String
) extends InterpreterException(message)

/** Fatal implementation/startup invariant failure in trusted profile runtime
  * code. It is never a proof-controlled invalidity result.
  */
final class StarkProfileRuntimeException(message: String)
    extends InterpreterException(message)

/** Exception thrown when the estimated cost exceeds the allowed cost limit.
  *
  * @param estimatedCost the estimated cost of execution
  * @param message the error message
  * @param cause an optional cause for the exception
  */
class CostLimitException(
  val estimatedCost: Long,
  message: String
) extends SigmaException(message, None)

object CostLimitException {
  /** Generates a cost limit error message.
    *
    * @param cost  the estimated cost of execution
    * @param limit the allowed cost limit
    */
  def msgCostLimitError(
      cost: JitCost,
      limit: JitCost) = s"Estimated execution cost $cost exceeds the limit $limit"
}
