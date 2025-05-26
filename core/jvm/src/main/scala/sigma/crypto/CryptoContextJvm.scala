package sigma.crypto

import org.bouncycastle.asn1.x9.X9ECParameters

import java.math.BigInteger

/** JVM implementation of context for cryptographic operations using Bouncycastle. */
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
    Platform.Ecp(_curve.decodePoint(encoded))
  }
}
