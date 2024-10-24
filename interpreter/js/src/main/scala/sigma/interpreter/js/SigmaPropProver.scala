package sigma.interpreter.js

import sigma.data.{Iso, SigmaBoolean}
import sigma.exceptions.InterpreterException
import sigma.js.{Isos, JsWrapper, SigmaProp}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.typedarray.Int8Array
import scala.util.{Failure, Success}

/** Prover which can sign messages (generate proofs) for arbitrary sigma propositions
  * represented by [[SigmaProp]] values.
  *
  * See [EIP-11](https://github.com/ergoplatform/eips/pull/8) for details of multi-signature scheme.
  *
  * @see SigmaPropVerifier
  */
@JSExportTopLevel("SigmaPropProver")
class SigmaPropProver(override val wrappedValue: org.ergoplatform.SigmaPropProver)
    extends JsWrapper[org.ergoplatform.SigmaPropProver] {
  private val isoSP = Isos.isoArrayToIndexed(Iso.identityIso[SigmaProp])

  private def toSigmaBooleanSeq(sps: js.Array[SigmaProp]): Seq[SigmaBoolean] = {
    isoSP.to(sps).map(_.sigmaBoolean)
  }

  /**
    * A method which is generating commitments for all the public keys provided.
    * This is used as part of multi-signature scheme.
    *
    * Currently only keys in form of ProveDlog and ProveDiffieHellman are supported, not more complex subtrees.
    *
    * @param sigmaTree   - crypto-tree which is being signed
    * @param generateFor - public keys for which commitments should be generated
    * @return generated commitments in a form of prover hints
    *         - private, containing secret randomness
    *         - public, containing only commitments
    */
  def generateCommitmentsFor(
      sigmaTree: SigmaProp,
      generateFor: js.Array[SigmaProp]): ProverHints = {
    val pks = toSigmaBooleanSeq(generateFor)
    val reduced = wrappedValue.generateCommitmentsFor(sigmaTree.sigmaBoolean, pks)
    new ProverHints(reduced)
  }

  /**
    * A method which is extracting partial proofs of secret knowledge for particular secrets with their
    * respective public images given. Useful for distributed signature applications.
    *
    * See DistributedSigSpecification for examples of usage.
    *
    * @param sigmaTree                 - public key (in form of a sigma-tree)
    * @param proof                     - signature for the key
    * @param realSecretsToExtract      - public keys of secrets with real proofs
    * @param simulatedSecretsToExtract - public keys of secrets with simulated proofs
    * @return - bag of OtherSecretProven and OtherCommitment hints
    */
  def hintsForMultisig(
      sigmaTree: SigmaProp,
      proof: Int8Array,
      realSecretsToExtract: js.Array[SigmaProp],
      simulatedSecretsToExtract: js.Array[SigmaProp]): ProverHints = {
    val realsToExtract = toSigmaBooleanSeq(realSecretsToExtract)
    val simsToExtract  = toSigmaBooleanSeq(simulatedSecretsToExtract)
    val hints        = wrappedValue.bagForMultisig(
      sigmaTree = sigmaTree.sigmaBoolean, proof.toArray,
      realSecretsToExtract = realsToExtract,
      simulatedSecretsToExtract = simsToExtract)
    new ProverHints(hints)
  }

  /**
    * Generate commitments for given crypto-tree (sigma-tree) for prover's secrets.
    */
  def generateCommitments(sigmaTree: SigmaProp): ProverHints = {
    val hints = wrappedValue.generateCommitments(sigmaTree.sigmaBoolean)
    new ProverHints(hints)
  }

  /** Sign arbitrary message under a key representing a statement provable via a sigma-protocol.
    *
    * @param sigmaProp - public key
    * @param message   - message to sign
    * @param hintsBag  - additional hints for a signer (useful for distributed signing)
    * @return - signature or error
    */
  def signMessage(
      sigmaProp: SigmaProp,
      message: Int8Array,
      hintsBag: ProverHints): Int8Array = {
    wrappedValue.signMessage(sigmaProp.sigmaBoolean, message.toArray, hintsBag.wrappedValue) match {
      case Success(signature) => Int8Array.of(signature:_*)
      case Failure(t) => throw new InterpreterException("Failed to sign message", Some(t))
    }
  }
}

@JSExportTopLevel("SigmaPropProver$")
object SigmaPropProver extends js.Object {
  /** Creates a new [[SigmaPropProver]] with the given secrets. */
  def withSecrets(secrets: js.Array[ProverSecret]): SigmaPropProver = {
    val privateInputs = Isos.isoArrayToIndexed(Iso.identityIso[ProverSecret]).to(secrets).map(_.wrappedValue)
    new SigmaPropProver(new org.ergoplatform.SigmaPropProver(privateInputs))
  }
}