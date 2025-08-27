package org.ergoplatform.sdk.js

import org.ergoplatform.sdk
import org.ergoplatform.sdk.SecretString

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import Isos._
import sigma.data.js.{Isos => DataIsos}
import sigma.eval.SigmaDsl

/** Equivalent of [[sdk.ProverBuilder]] available from JS.
  *
  * @param parameters Blockchain parameters re-adjustable via miners voting and
  *                   voting-related data. All of them are included into extension
  *                   section of a first block of a voting epoch.
  * @param network    Network prefix to use for addresses.
  */
@JSExportTopLevel("ProverBuilder")
class ProverBuilder(parameters: BlockchainParameters, network: Byte) extends js.Object {
  val _builder = new sdk.ProverBuilder(Isos.isoBlockchainParameters.to(parameters), network)

  /** Configure this builder to use the given seed when building a new prover.
    *
    * @param mnemonicPhrase          secret seed phrase to be used in prover for generating proofs.
    * @param mnemonicPass            password to protect secret seed phrase.
    */
  def withMnemonic(mnemonicPhrase: String, mnemonicPass: String): ProverBuilder = {
    _builder.withMnemonic(
      SecretString.create(mnemonicPhrase),
      SecretString.create(mnemonicPass),
      usePre1627KeyDerivation = false
    )
    this
  }

  /** Configure this builder to derive the new EIP-3 secret key with the given index.
    * The derivation uses master key derived from the mnemonic configured using
    * [[ErgoProverBuilder.withMnemonic]].
    *
    * @param index last index in the EIP-3 derivation path.
    */
  def withEip3Secret(index: Int): ProverBuilder = {
    _builder.withEip3Secret(index)
    this
  }

  /** Configures this builder to use group elements (g, h, u, v) and secret x for a
    * ProveDHTuple statement when building a new prover.
    *
    * ProveDHTuple is a statement consisting of 4 group elements (g, h, u, v) and
    * requires the prover to prove knowledge of secret integer x such that.
    *
    * u = g^x
    * and
    * y = h^x
    *
    * @param g [[GroupElement]] instance defining g
    * @param h [[GroupElement]] instance defining h
    * @param u [[GroupElement]] instance defining u
    * @param v [[GroupElement]] instance defining v
    * @param x [[BigInteger]] instance defining x
    * @see
    * <a href="https://github.com/ScorexFoundation/sigmastate-interpreter/blob/b3695bdb785c9b3a94545ffea506358ee3f8ed3d/sigmastate/src/test/scala/sigmastate/utxo/examples/DHTupleExampleSpecification.scala#L28">example</a>
    * @see
    * <a href="https://github.com/ScorexFoundation/sigmastate-interpreter/blob/b54a173865a532de09bbcbf10da32ee2a491c8f9/sigmastate/src/main/scala/sigmastate/basics/DiffieHellmanTupleProtocol.scala#L58">implementation</a>
    */
  def withDHTSecret(g: String, h: String, u: String, v: String, x: js.BigInt): ProverBuilder = {
    _builder.withDHTData(
      DataIsos.isoStringToGroupElement.to(g),
      DataIsos.isoStringToGroupElement.to(h),
      DataIsos.isoStringToGroupElement.to(u),
      DataIsos.isoStringToGroupElement.to(v),
      SigmaDsl.toBigInteger(sigma.js.Isos.isoBigInt.to(x))
    )
    this
  }

  /** This allows adding additional secret for use in proveDlog, when the secret is not
    * part of the wallet.
    *
    * Multiple secrets can be added by calling this method multiple times.
    *
    * Multiple secrets are necessary for statements that need multiple proveDlogs, such
    * as proveDlog(a) && proveDlog(b), where a and b are two group elements.
    */
  def withDLogSecret(x: js.BigInt): ProverBuilder = {
    _builder.withDLogSecret(SigmaDsl.toBigInteger(sigma.js.Isos.isoBigInt.to(x)))
    this
  }

  /** Builds a new prover using provided configuration. */
  def build(): SigmaProver = {
    val p =_builder.build()
    new SigmaProver(p)
  }
}

@JSExportTopLevel("ProverBuilder$")
object ProverBuilder extends js.Object {
  /** Creates a new [[ProverBuilder]] instance with the given blockchain parameters
    * and network prefix.
    *
    * @param parameters Blockchain parameters re-adjustable via miners voting and
    *                   voting-related data. All of them are included into extension
    *                   section of a first block of a voting epoch.
    * @param network    Network prefix to use for addresses.
    */
  def create(parameters: BlockchainParameters, network: Byte): ProverBuilder =
    new ProverBuilder(parameters, network)
}