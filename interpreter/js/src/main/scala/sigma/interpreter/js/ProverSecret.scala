package sigma.interpreter.js

import sigma.data.{CBigInt, ProveDHTuple}
import sigma.js.{Isos, JsWrapper, SigmaProp}
import sigma.util.Extensions.BigIntOps
import sigmastate.crypto.DLogProtocol.DLogProverInput
import sigmastate.crypto.{DiffieHellmanTupleProverInput, SigmaProtocolPrivateInput}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Represents one secret (aka SigmaProtocolPrivateInput) used by [[SigmaPropProver]]. */
@JSExportTopLevel("ProverSecret")
class ProverSecret(
  override val wrappedValue: SigmaProtocolPrivateInput[sigma.data.SigmaLeaf]
) extends JsWrapper[SigmaProtocolPrivateInput[sigma.data.SigmaLeaf]] {
  /** Public key generated from the secret.
    * Represents proof of knowledge sigma proposition.
    */
  def publicKey(): SigmaProp = new SigmaProp(wrappedValue.publicImage)

  /** Secret random number stored in this instance. */
  def secret(): js.BigInt = Isos.isoBigInt.from(CBigInt(wrappedValue.w))
}

@JSExportTopLevel("ProverSecret$")
object ProverSecret extends js.Object {
  /** Creates a new [[ProverSecret]] instance for the given secret of descrete logarithm
    * sigma protocol.
    * @param w secret exponent value
    */
  def dlog(w: js.BigInt): ProverSecret = {
    val input = DLogProverInput(Isos.isoBigInt.to(w).toBigInteger)
    new ProverSecret(input)
  }

  /** Creates a new [[ProverSecret]] instance for the given secret of Diffie Hellman tuple
    * sigma protocol.
    * @param w secret exponent value used to compute `u = g^w` and `v = h^w`, where `g` and `h` are generators
    * @param dhtProp a [[SigmaProp]] representing public key of Diffie Hellman tuple sigma protocol, should be created using `w`
    */
  def dht(w: js.BigInt, dhtProp: SigmaProp): ProverSecret = {
    dhtProp.sigmaBoolean match {
      case dht: ProveDHTuple =>
        val input = DiffieHellmanTupleProverInput(Isos.isoBigInt.to(w).toBigInteger, dht)
        new ProverSecret(input)
      case _ => throw new Exception("Expected ProveDHTuple sigma proposition")
    }
  }
}