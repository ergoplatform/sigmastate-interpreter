package sigmastate.crypto

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Uint8Array

/** Represents imported CryptoContext class from `sigmajs-crypto-facade` JS libarary. */
@js.native
@JSImport("sigmajs-crypto-facade", "CryptoContext")
class CryptoContextJs() extends js.Object {
  /** The characteristics (modulus) of the underlying finite field. */
  def getModulus(): js.BigInt = js.native

  /** The order of the underlying group. */
  def getOrder(): js.BigInt = js.native

  /** Validates a point.
    *
    * @param x the x-coordinate of the point
    * @param y the y-coordinate of the point
    * @return the point if it is valid
    */
  def validatePoint(x: js.BigInt, y: js.BigInt): Platform.Point = js.native

  /** The point at infinity. */
  def getInfinity(): Platform.Point = js.native

  /** Decodes a point from its hex string representation. */
  def decodePoint(encoded: String): Platform.Point = js.native

  /** The generator of the underlying group. */
  def getGenerator(): Platform.Point = js.native
}

/** Represents imported CryptoFacade object from `sigmajs-crypto-facade` JS libarary. */
@js.native
@JSImport("sigmajs-crypto-facade", "CryptoFacade")
object CryptoFacadeJs extends js.Object {
  /** Normalization ensures that any projective coordinate is 1, and therefore that the x, y
    * coordinates reflect those of the equivalent point in an affine coordinate system.
    *
    * @return a new ECPoint instance representing the same point, but with normalized coordinates
    */
  def normalizePoint(point: Platform.Point): Platform.Point = js.native

  /** Creates a new context for cryptographic operations. */
  def createCryptoContext(): CryptoContextJs = js.native

  /** Negates this point by negating the y coordinate. */
  def negatePoint(point: Platform.Point): Platform.Point = js.native

  /** Check if a point is infinity. */
  def isInfinityPoint(point: Platform.Point): Boolean = js.native

  /** Multiplies the [[ECPoint]] `p`` by `k`, i.e. `p` is added `k` times to itself.
    *
    * @param p The [[ECPoint]] to be multiplied.
    * @param k The factor by which `p` is multiplied.
    */
  def multiplyPoint(point: Platform.Point, scalar: js.BigInt): Platform.Point = js.native

  /** Adds two EC points. */
  def addPoint(point1: Platform.Point, point2: Platform.Point): Platform.Point = js.native

  /** Return simplified string representation of the point (used only for debugging) */
  def showPoint(point: Platform.Point): String = js.native

  // TODO refactor: raname to signOf to be consistent with CryptoFacade.signOf
  /** Returns the sign of the field element. */
  def testBitZeroOfFieldElem(element: js.BigInt): Boolean = js.native

  // TODO refactor: raname to encodeFieldElem to be consistent with CryptoFacade.encodeFieldElem
  /** Returns byte representation of the given field element. */
  def getEncodedOfFieldElem(element: js.BigInt): Uint8Array = js.native

  /** Returns the x-coordinate.
    *
    * Caution: depending on the curve's coordinate system, this may not be the same value as in an
    * affine coordinate system; use normalizePoint() to get a point where the coordinates have their
    * affine values, or use getAffineXCoord() if you expect the point to already have been
    * normalized.
    *
    * @return the x-coordinate of this point
    */
  def getXCoord(point: Platform.Point): js.BigInt = js.native

  /** Returns the y-coordinate.
    *
    * Caution: depending on the curve's coordinate system, this may not be the same value as in an
    * affine coordinate system; use normalizePoint() to get a point where the coordinates have their
    * affine values, or use getAffineYCoord() if you expect the point to already have been
    * normalized.
    *
    * @return the y-coordinate of this point
    */
  def getYCoord(point: Platform.Point): js.BigInt = js.native

  /** Returns the affine x-coordinate after checking that this point is normalized.
    *
    * @return The affine x-coordinate of this point
    */
  def getAffineXCoord(point: Platform.Point): js.BigInt = js.native

  /** Returns the affine y-coordinate after checking that this point is normalized
    *
    * @return The affine y-coordinate of this point
    */
  def getAffineYCoord(point: Platform.Point): js.BigInt = js.native
}

/** Represents imported Point class from `sigmajs-crypto-facade` JS libarary. */
@js.native
@JSImport("sigmajs-crypto-facade", "Point")
object Point extends js.Any {
  def fromHex(hex: String): Platform.Point = js.native
  def ZERO: Platform.Point = js.native
}

/** Represents imported `utils` module from `sigmajs-crypto-facade` JS libarary. */
@js.native
@JSImport("sigmajs-crypto-facade", "utils")
object utils extends js.Any {
  def bytesToHex(bytes: Uint8Array): String = js.native
  def hexToBytes(hex: String): Uint8Array = js.native
}