package org.ergoplatform.sdk.wallet.secrets

import sigma.crypto.{BigIntegers, CryptoConstants, CryptoFacade}
import sigma.data.ProveDlog
import java.math.BigInteger
import sigmastate.crypto.DLogProtocol.DLogProverInput
import sigma.serialization.{SigmaByteReader, SigmaByteWriter, SigmaSerializer}

/**
  * Secret, its chain code and path in key tree.
  * (see: https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki)
  */
final class ExtendedSecretKey(/*private[secrets]*/ val keyBytes: Array[Byte],
                              /*private[secrets]*/ val chainCode: Array[Byte],
                              val usePre1627KeyDerivation: Boolean,
                              val path: DerivationPath)
  extends ExtendedKey[ExtendedSecretKey] with SecretKey {

  def selfReflection: ExtendedSecretKey = this

  override def privateInput: DLogProverInput = DLogProverInput(BigIntegers.fromUnsignedByteArray(keyBytes))

  def publicImage: ProveDlog = privateInput.publicImage

  def child(idx: Int): ExtendedSecretKey = ExtendedSecretKey.deriveChildSecretKey(this, idx)

  def publicKey: ExtendedPublicKey =
    new ExtendedPublicKey(CryptoFacade.getASN1Encoding(privateInput.publicImage.value, true), chainCode, path.toPublicBranch)

  def isErased: Boolean = keyBytes.forall(_ == 0x00)

  def zeroSecret(): Unit = java.util.Arrays.fill(keyBytes, 0: Byte)

  override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) || (obj match {
    case that: ExtendedSecretKey =>
      java.util.Arrays.equals(that.keyBytes, this.keyBytes) &&
        java.util.Arrays.equals(that.chainCode, this.chainCode) &&
        that.path == this.path
    case _ => false
  })

  override def hashCode(): Int = {
    var h = java.util.Arrays.hashCode(keyBytes)
    h = 31 * h + java.util.Arrays.hashCode(chainCode)
    h = 31 * h + path.hashCode()
    h
  }

}

object ExtendedSecretKey {

  @scala.annotation.tailrec
  def deriveChildSecretKey(parentKey: ExtendedSecretKey, idx: Int): ExtendedSecretKey = {
    val keyCoded: Array[Byte] =
      if (Index.isHardened(idx)) (0x00: Byte) +: parentKey.keyBytes
      else CryptoFacade.getASN1Encoding(parentKey.privateInput.publicImage.value, true)
    val (childKeyProto, childChainCode) = CryptoFacade
        .hashHmacSHA512(parentKey.chainCode, keyCoded ++ Index.serializeIndex(idx))
        .splitAt(CryptoFacade.SecretKeyLength)
    val childKeyProtoDecoded = BigIntegers.fromUnsignedByteArray(childKeyProto)
    val childKey = childKeyProtoDecoded
      .add(BigIntegers.fromUnsignedByteArray(parentKey.keyBytes))
      .mod(CryptoConstants.groupOrder)
    if (childKeyProtoDecoded.compareTo(CryptoConstants.groupOrder) >= 0 || childKey.equals(BigInteger.ZERO))
      deriveChildSecretKey(parentKey, idx + 1)
    else {
      val keyBytes = if (parentKey.usePre1627KeyDerivation) {
        // maybe less than 32 bytes if childKey is small enough while BIP32 requires 32 bytes.
        // see https://github.com/ergoplatform/ergo/issues/1627 for details
        BigIntegers.asUnsignedByteArray(childKey)
      } else {
        // padded with leading zeroes to 32 bytes
        BigIntegers.asUnsignedByteArray(CryptoFacade.SecretKeyLength, childKey)
      }
      new ExtendedSecretKey(keyBytes, childChainCode, parentKey.usePre1627KeyDerivation, parentKey.path.extended(idx))
      }
  }

  def deriveChildPublicKey(parentKey: ExtendedSecretKey, idx: Int): ExtendedPublicKey = {
    val derivedSecret = deriveChildSecretKey(parentKey, idx)
    val derivedPk = CryptoFacade.getASN1Encoding(derivedSecret.privateInput.publicImage.value, true)
    val derivedPath = derivedSecret.path.copy(publicBranch = true)
    new ExtendedPublicKey(derivedPk, derivedSecret.chainCode, derivedPath)
  }


  /**
   * Derives master secret key from the seed 
   * @param seed - seed bytes
   * @param usePre1627KeyDerivation - use incorrect(previous) BIP32 derivation, expected to be false for new wallets, and true for old pre-1627 wallets (see https://github.com/ergoplatform/ergo/issues/1627 for details)
   */
  def deriveMasterKey(seed: Array[Byte], usePre1627KeyDerivation: Boolean): ExtendedSecretKey = {
    val (masterKey, chainCode) = CryptoFacade
        .hashHmacSHA512(CryptoFacade.BitcoinSeed, seed)
        .splitAt(CryptoFacade.SecretKeyLength)
    new ExtendedSecretKey(masterKey, chainCode, usePre1627KeyDerivation, DerivationPath.MasterPath)
  }

}

object ExtendedSecretKeySerializer extends SigmaSerializer[ExtendedSecretKey, ExtendedSecretKey] {

  import scorex.util.Extensions._

  override def serialize(obj: ExtendedSecretKey, w: SigmaByteWriter): Unit = {
    w.putBytes(obj.keyBytes)
    w.putBytes(obj.chainCode)
    val pathBytes = DerivationPathSerializer.toBytes(obj.path)
    w.putUInt(pathBytes.length)
    w.putBytes(pathBytes)
  }

  override def parse(r: SigmaByteReader): ExtendedSecretKey = {
    val keyBytes = r.getBytes(CryptoFacade.SecretKeyLength)
    val chainCode = r.getBytes(CryptoFacade.SecretKeyLength)
    val pathLen = r.getUInt().toIntExact
    val path = DerivationPathSerializer.fromBytes(r.getBytes(pathLen))
    new ExtendedSecretKey(keyBytes, chainCode, false, path)
  }

}
