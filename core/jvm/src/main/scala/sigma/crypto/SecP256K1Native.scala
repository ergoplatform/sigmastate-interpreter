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
    * Returns None when the scalar is congruent to 0 modulo the group order (result is the point
    * at infinity) or on any error, so the caller can fall back to BouncyCastle.
    */
  def multiplyPointByScalar(point: ECPoint, n: BigInteger): Option[ECPoint] = {
    // secp256k1-jni accepts only a 32-byte scalar in [1, order-1]. Every non-infinity point of
    // secp256k1 has exactly `groupOrder` as its order, so p * n == p * (n mod order) for any
    // integer n (negative, or wider than 256 bits), which is what BouncyCastle computes.
    val scalar = n.mod(groupOrder)
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

  /** Big-endian unsigned 32-byte encoding of `n`, which must be in [0, 2^256).
    * BigInteger.toByteArray() is signed and variable-length: it may carry a leading 0x00 sign
    * byte (33 bytes for values with bit 255 set) or be shorter than 32 bytes.
    */
  private def toScalarBytes(n: BigInteger): Array[Byte] = {
    require(n.signum() >= 0 && n.bitLength() <= 256, s"scalar out of range: $n")
    val raw = n.toByteArray
    val padded = new Array[Byte](32)
    val len = math.min(raw.length, 32)
    System.arraycopy(raw, raw.length - len, padded, 32 - len, len)
    padded
  }
}
