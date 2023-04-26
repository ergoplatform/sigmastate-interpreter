package sigmastate.crypto

import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.bouncycastle.math.ec.{ECCurve, ECFieldElement, ECPoint}
import scalan.RType

import java.math.BigInteger
import sigmastate._
import sigmastate.basics.BcDlogGroup
import special.collection.Coll
import special.sigma._

/** JVM specific implementation of crypto methods*/
object Platform {
  /** Description of elliptic curve of point `p` which belongs to the curve.
    * @param p the elliptic curve point
    */
  def getCurve(p: Ecp): Curve = Curve(p.value.getCurve)

  /** Returns the x-coordinate.
    *
    * Caution: depending on the curve's coordinate system, this may not be the same value as in an
    * affine coordinate system; use normalize() to get a point where the coordinates have their
    * affine values, or use getAffineXCoord() if you expect the point to already have been
    * normalized.
    *
    * @return the x-coordinate of this point
    */
  def getXCoord(p: Ecp): ECFieldElem = ECFieldElem(p.value.getXCoord)

  /** Returns the y-coordinate.
    *
    * Caution: depending on the curve's coordinate system, this may not be the same value as in an
    * affine coordinate system; use normalize() to get a point where the coordinates have their
    * affine values, or use getAffineYCoord() if you expect the point to already have been
    * normalized.
    *
    * @return the y-coordinate of this point
    */
  def getYCoord(p: Ecp): ECFieldElem = ECFieldElem(p.value.getYCoord)

  /** Returns the affine x-coordinate after checking that this point is normalized.
    *
    * @return The affine x-coordinate of this point
    * @throws IllegalStateException if the point is not normalized
    */
  def getAffineXCoord(p: Ecp): ECFieldElem = ECFieldElem(p.value.getAffineXCoord)

  /** Returns the affine y-coordinate after checking that this point is normalized
    *
    * @return The affine y-coordinate of this point
    * @throws IllegalStateException if the point is not normalized
    */
  def getAffineYCoord(p: Ecp): ECFieldElem = ECFieldElem(p.value.getAffineYCoord)

  /** Returns byte representation of the given field element. */
  def encodeFieldElem(p: ECFieldElem): Array[Byte] = p.value.getEncoded

  /** Byte representation of the given point.
    * @param p point to encode
    * @param compressed if true, generates a compressed point encoding
    */
  def encodePoint(p: Ecp, compressed: Boolean): Array[Byte] = p.value.getEncoded(compressed)

  /** Returns the value of bit 0 in BigInteger representation of this point. */
  def signOf(p: ECFieldElem): Boolean = p.value.testBitZero()

  /** * Normalization ensures that any projective coordinate is 1, and therefore that the x, y
    * coordinates reflect those of the equivalent point in an affine coordinate system.
    *
    * @return a new ECPoint instance representing the same point, but with normalized coordinates
    */
  def normalizePoint(p: Ecp): Ecp = Ecp(p.value.normalize())

  /** Return simplified string representation of the point (used only for debugging) */
  def showPoint(p: Ecp): String = {
    val rawX = p.value.getRawXCoord.toString.substring(0, 6)
    val rawY = p.value.getRawYCoord.toString.substring(0, 6)
    s"ECPoint($rawX,$rawY,...)"
  }

  /** Multiply two points.
    * @param p1 first point
    * @param p2 second point
    * @return group multiplication (p1 * p2)
    */
  def multiplyPoints(p1: Ecp, p2: Ecp): Ecp = {
    /*
     * BC treats EC as additive group while we treat that as multiplicative group.
     */
    Ecp(p1.value.add(p2.value))
  }

  /** Exponentiate a point.
    * @param p point to exponentiate
    * @param n exponent
    * @return p to the power of n (p^n)
    */
  def exponentiatePoint(p: Ecp, n: BigInteger): Ecp = {
    /*
     * BC treats EC as additive group while we treat that as multiplicative group.
     * Therefore, exponentiate point is multiply.
     */
    Ecp(p.value.multiply(n))
  }

  /** Check if a point is infinity. */
  def isInfinityPoint(p: Ecp): Boolean = p.value.isInfinity

  /** Negate a point. */
  def negatePoint(p: Ecp): Ecp = Ecp(p.value.negate())

  /** Wrapper for curve descriptor. Serves as the concrete implementation of the
    * [[sigmastate.crypto.Curve]] type in JVM.
    */
  case class Curve(private[crypto] val value: ECCurve)

  /** Wrapper for point type. */
  case class Ecp(private[crypto] val value: ECPoint)

  /** Wrapper for field element type. */
  case class ECFieldElem(value: ECFieldElement)

  /** Secure source of randomness on JVM. */
  type SecureRandom = java.security.SecureRandom
  
  /** Create a new context for cryptographic operations. */
  def createContext(): CryptoContext = new CryptoContextJvm(CustomNamedCurves.getByName("secp256k1"))

  /** Create JVM specific source of secure randomness. */
  def createSecureRandom(): SecureRandom = new SecureRandom()

  /** Checks that the type of the value corresponds to the descriptor `tpe`.
    * If the value has complex structure only root type constructor is checked.
    * NOTE, this is surface check with possible false positives, but it is ok
    * when used in assertions, like `assert(isCorrestType(...))`, see `ConstantNode`.
    */
  def isCorrectType[T <: SType](value: Any, tpe: T): Boolean = value match {
    case c: Coll[_] => tpe match {
      case STuple(items) => c.tItem == RType.AnyType && c.length == items.length
      case tpeColl: SCollection[_] => true
      case _ => sys.error(s"Collection value $c has unexpected type $tpe")
    }
    case _: Option[_] => tpe.isOption
    case _: Tuple2[_, _] => tpe.isTuple && tpe.asTuple.items.length == 2
    case _: Boolean => tpe == SBoolean
    case _: Byte => tpe == SByte
    case _: Short => tpe == SShort
    case _: Int => tpe == SInt
    case _: Long => tpe == SLong
    case _: BigInt => tpe == SBigInt
    case _: String => tpe == SString
    case _: GroupElement => tpe.isGroupElement
    case _: SigmaProp => tpe.isSigmaProp
    case _: AvlTree => tpe.isAvlTree
    case _: Box => tpe.isBox
    case _: PreHeader => tpe == SPreHeader
    case _: Header => tpe == SHeader
    case _: Context => tpe == SContext
    case _: Function1[_, _] => tpe.isFunc
    case _: Unit => tpe == SUnit
    case _ => false
  }

  /** This JVM specific methods are used in Ergo node which won't be JS cross-compiled. */
  implicit class EcpOps(val p: Ecp) extends AnyVal {
    def getCurve: ECCurve = p.value.getCurve
    def isInfinity: Boolean = CryptoFacade.isInfinityPoint(p)
    def add(p2: Ecp): Ecp = CryptoFacade.multiplyPoints(p, p2)
    def multiply(n: BigInteger): Ecp = CryptoFacade.exponentiatePoint(p, n)
  }

  /** This JVM specific methods are used in Ergo node which won't be JS cross-compiled. */
  implicit class BcDlogGroupOps(val group: BcDlogGroup) extends AnyVal {
    def curve: Curve = group.ctx.asInstanceOf[CryptoContextJvm].curve
  }
}