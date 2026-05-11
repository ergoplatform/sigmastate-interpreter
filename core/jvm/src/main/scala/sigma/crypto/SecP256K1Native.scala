package sigma.crypto

import org.bitcoin.{NativeSecp256k1, Secp256k1Context}
import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.bouncycastle.math.ec.ECPoint

import java.math.BigInteger

/** Native secp256k1 backend via bitcoin-s JNI wrapper.
  * Falls back transparently when native lib is not available.
  */
private[crypto] object SecP256K1Native {

  /** True if the native libsecp256k1 loaded successfully on this platform. */
  val isEnabled: Boolean = Secp256k1Context.isEnabled

  private lazy val x9params  = CustomNamedCurves.getByName("secp256k1")
  private lazy val curve      = x9params.getCurve
  private lazy val groupOrder = x9params.getN

  /** EC scalar multiplication using the native library.
    * Returns None when the effective scalar is 0 (result is the point at infinity) or on any
    * error, so the caller can fall back to BouncyCastle.
    */
  def multiplyPointByScalar(point: ECPoint, n: BigInteger): Option[ECPoint] = {
    // secp256k1-jni requires a positive scalar in [1, order-1]; normalise negative inputs.
    val scalar = if (n.signum() < 0) n.mod(groupOrder) else n
    if (scalar.signum() == 0) return None
    try {
      val pointBytes  = point.getEncoded(true)     // compressed, 33 bytes
      val scalarBytes = toScalarBytes(scalar)       // unsigned, exactly 32 bytes
      val result      = NativeSecp256k1.pubKeyTweakMul(pointBytes, scalarBytes, true)
      Some(curve.decodePoint(result))
    } catch {
      case _: Exception => None
    }
  }

  // BigInteger.toByteArray() is signed and variable-length; secp256k1 needs 32 unsigned bytes.
  private def toScalarBytes(n: BigInteger): Array[Byte] = {
    val raw = n.toByteArray
    raw.length match {
      case 33  => raw.tail  // drop leading 0x00 sign byte
      case 32  => raw
      case len =>
        val padded = new Array[Byte](32)
        System.arraycopy(raw, 0, padded, 32 - len, len)
        padded
    }
  }
}
