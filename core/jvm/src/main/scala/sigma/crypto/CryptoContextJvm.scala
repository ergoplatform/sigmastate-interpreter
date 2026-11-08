package sigma.crypto

import org.bitcoin.NativeSecp256k1
import org.bouncycastle.asn1.x9.X9ECParameters

import java.math.BigInteger

/** JVM implementation of context for cryptographic operations using secp256k1-jni
  * with Bouncycastle as a fallback.
  */
class CryptoContextJvm(x9params: X9ECParameters) extends CryptoContext {
  private lazy val _curve = x9params.getCurve

  override def curve: Curve = Platform.Curve(_curve)

  override def fieldCharacteristic: BigInteger = _curve.getField.getCharacteristic

  override def order: BigInteger = x9params.getN

  override def generator: Ecp = {
    Platform.Ecp(x9params.getG)
  }

  override def validatePoint(x: BigInteger, y: BigInteger): Ecp = {
    Platform.Ecp(_curve.validatePoint(x, y))
  }

  override def infinity(): Ecp = {
    Platform.Ecp(_curve.getInfinity)
  }

  override def decodePoint(encoded: Array[Byte]): Ecp = {
    if (
      Platform.isSecp256k1Enabled &&
      (_curve eq Platform.secp256k1Curve) &&
      (encoded.length == 33 || encoded.length == 65) &&
      encoded(0) != 0
    ) {
      try {
        val uncompressed = NativeSecp256k1.decompress(encoded)
        if (uncompressed != null && uncompressed.length == 65 && uncompressed(0) == 0x04) {
          return Platform.decodeUncompressedSecp256k1Point(uncompressed)
        }
      } catch {
        case _: Throwable => // fall back to BouncyCastle below
      }
    }
    Platform.Ecp(_curve.decodePoint(encoded))
  }
}
