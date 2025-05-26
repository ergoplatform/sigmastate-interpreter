package org.ergoplatform.sdk

import org.ergoplatform.ErgoAddressEncoder.{MainnetNetworkPrefix, NetworkPrefix}
import org.ergoplatform.sdk.wallet.secrets.ExtendedSecretKey
import sigmastate.crypto.DLogProtocol.DLogProverInput
import sigmastate.crypto.{DLogProtocol, DiffieHellmanTupleProverInput}
import sigma.GroupElement

import java.math.BigInteger
import scala.collection.mutable.ArrayBuffer

/** A builder class for constructing a `Prover` with specified secrets.
  *
  * @param parameters    Blockchain parameters re-adjustable via miners voting and
  *                      voting-related data. All of them are included into extension
  *                      section of a first block of a voting epoch.
  * @param networkPrefix Network prefix to use for addresses.
  */
class ProverBuilder(parameters: BlockchainParameters, networkPrefix: NetworkPrefix) {
  private var _masterKey: Option[ExtendedSecretKey] = None

  /** Generated EIP-3 secret keys paired with their derivation path index. */
  private val _eip2Keys =  ArrayBuffer.empty[(Int, ExtendedSecretKey)]

  private val _dhtSecrets = ArrayBuffer.empty[DiffieHellmanTupleProverInput]
  private val _dLogSecrets = ArrayBuffer.empty[DLogProverInput]

  /** Sets the mnemonic phrase and password for the prover.
    *
    * @param usePre1627KeyDerivation whether to use pre-1627 key derivation
    */
  def withMnemonic(
      mnemonicPhrase: SecretString,
      mnemonicPass: SecretString,
      usePre1627KeyDerivation: Boolean = false
  ): ProverBuilder = {
    _masterKey = Some(JavaHelpers.seedToMasterKey(mnemonicPhrase, mnemonicPass, usePre1627KeyDerivation))
    this
  }

  /** Adds an EIP-3 secret key for the specified index to the prover.
    *
    * @param index the derivation path index
    * @return an updated ProverBuilder
    */
  def withEip3Secret(index: Int): ProverBuilder = {
    require(_masterKey.isDefined, s"Mnemonic is not specified, use withMnemonic method.")
    require(!_eip2Keys.exists(_._1 == index),
      s"Secret key for derivation index $index has already been added.")
    val path = JavaHelpers.eip3DerivationWithLastIndex(index)
    val secretKey = _masterKey.get.derive(path)
    _eip2Keys += (index -> secretKey)
    this
  }

  /** Adds a Diffie-Hellman Tuple secret to the prover. */
  def withDHTData(
      g: GroupElement,
      h: GroupElement,
      u: GroupElement,
      v: GroupElement,
      x: BigInteger): ProverBuilder = {
    val dht = org.ergoplatform.sdk.JavaHelpers.createDiffieHellmanTupleProverInput(g, h, u, v, x)
    if (_dhtSecrets.contains(dht))
      throw new IllegalStateException("DHTuple secret already exists")
    _dhtSecrets += dht
    this
  }

  /** Adds a DLog secret to the prover.
    *
    * @param x the x value of the DLog secret
    * @return an updated ProverBuilder
    */
  def withDLogSecret(x: BigInteger): ProverBuilder = {
    val dLog = new DLogProtocol.DLogProverInput(x)
    if (_dLogSecrets.contains(dLog))
      throw new IllegalStateException("Dlog secret already exists")
    _dLogSecrets += dLog
    this
  }

  /** Constructs a `Prover` with the specified secrets.
    *
    * @return a new Prover instance
    */
  def build(): SigmaProver = {
    val secretKeys = _masterKey.toIndexedSeq ++ _eip2Keys.map(_._2)
    val interpreter = new AppkitProvingInterpreter(
      secretKeys,
      _dLogSecrets.toIndexedSeq,
      _dhtSecrets.toIndexedSeq, parameters)
    new SigmaProver(interpreter, networkPrefix)
  }
}

object ProverBuilder {
  def forMainnet(parameters: BlockchainParameters): ProverBuilder =
    new ProverBuilder(parameters, MainnetNetworkPrefix)
}

