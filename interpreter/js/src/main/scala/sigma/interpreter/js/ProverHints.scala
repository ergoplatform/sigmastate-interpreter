package sigma.interpreter.js

import sigma.js.JsWrapper
import sigmastate.interpreter.HintsBag

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Represents hints used by [[SigmaPropProver]] to perform operations as part of
  * multi-signature scheme. See [EIP-11](https://github.com/ergoplatform/eips/pull/8).
  */
@JSExportTopLevel("ProverHints")
class ProverHints(override val wrappedValue: HintsBag)
    extends JsWrapper[HintsBag] {
}

@JSExportTopLevel("ProverHints$")
object ProverHints extends js.Object {
  private lazy val _empty = new ProverHints(HintsBag.empty)

  /** Empty bag of hints. Immutable value can be reused where necessary. */
  def empty(): ProverHints = _empty
}
