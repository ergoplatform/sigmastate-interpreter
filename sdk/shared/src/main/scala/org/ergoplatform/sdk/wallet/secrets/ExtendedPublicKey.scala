package org.ergoplatform.sdk.wallet.secrets

import sigma.crypto.{BigIntegers, CryptoConstants, CryptoFacade}
import sigma.data.ProveDlog
import sigmastate.crypto.DLogProtocol.DLogProverInput
import sigma.serialization.{SigmaByteReader, SigmaByteWriter, SigmaSerializer}
import scala.annotation.tailrec

/**
  * Public key, its chain code and path in key tree.
  * (see: https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki)
  */
final class ExtendedPublicKey(/*private[secrets] */val keyBytes: Array[Byte],
                              /*private[secrets] */val chainCode: Array[Byte],
                              val path: DerivationPath) 
  extends ExtendedKey[ExtendedPublicKey] {

  def selfReflection: ExtendedPublicKey = this

  def key: ProveDlog = ProveDlog(
    CryptoConstants.dlogGroup.ctx.decodePoint(keyBytes)
  )

  def child(idx: Int): ExtendedPublicKey = ExtendedPublicKey.deriveChildPublicKey(this, idx)

  override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) || (obj match {
    case that: ExtendedPublicKey =>
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

  override def toString: String = s"ExtendedPublicKey($path : $key)"
}

object ExtendedPublicKey {

  @tailrec
  def deriveChildPublicKey(parentKey: ExtendedPublicKey, idx: Int): ExtendedPublicKey = {
    require(!Index.isHardened(idx), "Hardened public keys derivation is not supported")
    val (childKeyProto, childChainCode) = CryptoFacade
        .hashHmacSHA512(parentKey.chainCode, parentKey.keyBytes ++ Index.serializeIndex(idx))
        .splitAt(CryptoFacade.SecretKeyLength)
    val childKeyProtoDecoded = BigIntegers.fromUnsignedByteArray(childKeyProto)
    val childKey = CryptoFacade.multiplyPoints(DLogProverInput(childKeyProtoDecoded).publicImage.value, parentKey.key.value)
    if (childKeyProtoDecoded.compareTo(CryptoConstants.groupOrder) >= 0 || CryptoFacade.isInfinityPoint(childKey)) {
      deriveChildPublicKey(parentKey, idx + 1)
    } else {
      new ExtendedPublicKey(CryptoFacade.getASN1Encoding(childKey, true), childChainCode, parentKey.path.extended(idx))
    }
  }

}

object ExtendedPublicKeySerializer extends SigmaSerializer[ExtendedPublicKey, ExtendedPublicKey] {

  import scorex.util.Extensions._

  //ASN.1 encoding for secp256k1 points - 1 byte for sign + 32 bytes for x-coordinate of the point
  val PublicKeyBytesSize: Int = CryptoFacade.SecretKeyLength + 1

  override def serialize(obj: ExtendedPublicKey, w: SigmaByteWriter): Unit = {
    w.putBytes(obj.keyBytes)
    w.putBytes(obj.chainCode)
    val pathBytes = DerivationPathSerializer.toBytes(obj.path)
    w.putUInt(pathBytes.length)
    w.putBytes(pathBytes)
  }

  override def parse(r: SigmaByteReader): ExtendedPublicKey = {
    val keyBytes = r.getBytes(PublicKeyBytesSize)
    val chainCode = r.getBytes(CryptoFacade.SecretKeyLength)
    val pathLen = r.getUInt().toIntExact
    val path = DerivationPathSerializer.fromBytes(r.getBytes(pathLen))
    new ExtendedPublicKey(keyBytes, chainCode, path)
  }

}
