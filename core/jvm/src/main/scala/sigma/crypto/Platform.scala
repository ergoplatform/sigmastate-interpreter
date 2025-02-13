package sigma.crypto

import org.bouncycastle.crypto.digests.SHA512Digest
import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.bouncycastle.crypto.generators.PKCS5S2ParametersGenerator
import org.bouncycastle.crypto.params.KeyParameter
import org.bouncycastle.math.ec.{ECCurve, ECFieldElement, ECPoint}

import java.math.BigInteger
import sigma.Coll
import sigma._
import sigma.ast._

import java.text.Normalizer.Form.NFKD
import java.text.Normalizer

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
    * @see [[CryptoFacade.getASN1Encoding]]
    */
  def getASN1Encoding(p: Ecp, compressed: Boolean): Array[Byte] = p.value.getEncoded(compressed)

  /** Returns the value of bit 0 in BigInteger representation of this point. */
  def signOf(p: ECFieldElem): Boolean = p.value.testBitZero()

  /** Normalization ensures that any projective coordinate is 1, and therefore that the x, y
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
    *
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
    * @return p to the power of n (`p^n`) i.e. `p + p + ... + p` (n times)
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

  /** Negates the given point by negating its y coordinate. */
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

  /** Computes HMAC-SHA512 hash of the given data using the specified key.
    *
    * @param key  the secret key used for hashing
    * @param data the input data to be hashed
    * @return a HMAC-SHA512 hash of the input data
    */
  def hashHmacSHA512(key: Array[Byte], data: Array[Byte]): Array[Byte] = HmacSHA512.hash(key, data)

  /** Generates PBKDF2 key from a mnemonic and passphrase using SHA512 digest.
    * Seed generation based on bouncycastle implementation.
    * See https://github.com/ergoplatform/ergo-appkit/issues/82
    */
  def generatePbkdf2Key(normalizedMnemonic: String, normalizedPass: String): Array[Byte] = {
    val gen = new PKCS5S2ParametersGenerator(new SHA512Digest)
    gen.init(
      normalizedMnemonic.getBytes(CryptoFacade.Encoding),
      normalizedPass.getBytes(CryptoFacade.Encoding),
      CryptoFacade.Pbkdf2Iterations)
    val dk = gen.generateDerivedParameters(CryptoFacade.Pbkdf2KeyLength).asInstanceOf[KeyParameter].getKey
    dk
  }

  /** Normalize a sequence of char values.
    * The sequence will be normalized according to the NFKD normalization form.
    * Implementation that uses [[java.text.Normalizer]].
    * See https://www.unicode.org/reports/tr15/  */
  def normalizeChars(chars: Array[Char]): String = {
    Normalizer.normalize(ArrayCharSequence(chars), NFKD)
  }

  /** Checks that the type of the value corresponds to the descriptor `tpe`.
    * If the value has complex structure only root type constructor is checked.
    * NOTE, this is surface check with possible false positives, but it is ok
    * when used in assertions, like `assert(isCorrestType(...))`, see `ConstantNode`.
    */
  def isCorrectType[T <: SType](value: Any, tpe: T): Boolean = value match {
    case c: Coll[_] => tpe match {
      case STuple(items) => c.tItem == sigma.AnyType && c.length == items.length
      case _: SCollection[_] => true
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
    case _: UnsignedBigInt => tpe == SUnsignedBigInt
    case _: String => tpe == SString  // TODO v6.0: remove this case (see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/905)
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

}
