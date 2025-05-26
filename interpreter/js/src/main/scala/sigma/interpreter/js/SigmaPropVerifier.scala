package sigma.interpreter.js

import sigma.js.{JsWrapper, SigmaProp}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.typedarray.Int8Array

/** Verifier which can verify signature (proof) for arbitrary sigma propositions
  * represented by [[SigmaProp]] values.
  *
  * See [EIP-11](https://github.com/ergoplatform/eips/pull/8) for details of multi-signature scheme.
  *
  * @see SigmaPropProver
  */
@JSExportTopLevel("SigmaPropVerifier")
class SigmaPropVerifier(override val wrappedValue: org.ergoplatform.SigmaPropVerifier)
    extends JsWrapper[org.ergoplatform.SigmaPropVerifier] {
  /**
    * Verify a signature on given (arbitrary) message for a given sigma proposition (public key).
    *
    * @param sigmaProp public key (represented as a sigma proposition)
    * @param message   message
    * @param signature signature for the message
    * @return whether signature is valid or not (valid signature contains proofs for the sigma proposition)
    */
  def verifySignature(
      sigmaProp: SigmaProp,
      message: Int8Array,
      signature: Int8Array): Boolean = {
    val ok = wrappedValue.verifySignature(sigmaProp.sigmaBoolean, message.toArray, signature.toArray)(null)
    ok
  }
}

@JSExportTopLevel("SigmaPropVerifier$")
object SigmaPropVerifier extends js.Object {
  /** Create a new instance of SigmaPropVerifier. */
  def create(): SigmaPropVerifier = {
    new SigmaPropVerifier(new org.ergoplatform.SigmaPropVerifier())
  }
}

