package sigma.crypto

import java.math.BigInteger

/** A context for cryptographic operations over elliptic curve group. */
abstract class CryptoContext {
  /** The underlying elliptic curve descriptor. */
  def curve: Curve

  /** The characteristics of the underlying finite field. */
  def fieldCharacteristic: BigInteger

  /** The order of the underlying group. */
  def order: BigInteger

  /** Validates a point.
    * @param x the x-coordinate of the point
    * @param y the y-coordinate of the point
    * @return the point if it is valid
    * @throws IllegalArgumentException if the coordinates do not represent a valid point
    */
  def validatePoint(x: BigInteger, y: BigInteger): Ecp

  /** The point at infinity. */
  def infinity(): Ecp

  /** Decodes a point from its byte representation. */
  def decodePoint(encoded: Array[Byte]): Ecp

  /** The generator of the underlying group. */
  def generator: Ecp
}

object CryptoContext {
  /** Default context for cryptographic operations. */
  val default: CryptoContext = Platform.createContext()
}

