package sigma.crypto

import sigma.data.RType
import scorex.util.encode.Base16
import sigma.Coll
import sigma._
import sigma.ast._

import java.math.BigInteger
import scala.scalajs.js
import scala.scalajs.js.UnicodeNormalizationForm
import scala.scalajs.js.typedarray.Uint8Array

/** JVM specific implementation of crypto methods (NOT yet implemented). */
object Platform {
  /** Description of elliptic curve of point `p` which belongs to the curve.
    *
    * @param p the elliptic curve point
    */
  def getCurve(p: Ecp): Curve = ???

  /** Returns the x-coordinate.
    *
    * Caution: depending on the curve's coordinate system, this may not be the same value as in an
    * affine coordinate system; use normalizePoint() to get a point where the coordinates have their
    * affine values, or use getAffineXCoord() if you expect the point to already have been
    * normalized.
    */
  def getXCoord(p: Ecp): ECFieldElem = new ECFieldElem(CryptoFacadeJs.getXCoord(p.point))

  /** Returns the y-coordinate.
    *
    * Caution: depending on the curve's coordinate system, this may not be the same value as in an
    * affine coordinate system; use normalizePoint() to get a point where the coordinates have their
    * affine values, or use getAffineYCoord() if you expect the point to already have been
    * normalized.
    */
  def getYCoord(p: Ecp): ECFieldElem = new ECFieldElem(CryptoFacadeJs.getYCoord(p.point))

  /** Returns the affine x-coordinate after checking that this point is normalized. */
  def getAffineXCoord(p: Ecp): ECFieldElem = new ECFieldElem(CryptoFacadeJs.getAffineXCoord(p.point))

  /** Returns the affine y-coordinate after checking that this point is normalized. */
  def getAffineYCoord(p: Ecp): ECFieldElem = new ECFieldElem(CryptoFacadeJs.getAffineYCoord(p.point))

  /** Converts JS representation of bytes array to Scala's equivalent. */
  def Uint8ArrayToBytes(jsShorts: Uint8Array): Array[Byte] = {
    jsShorts.toArray[Short].map(x => x.toByte)
  }

  /** Converts Scala's representation of bytes array to JS array of Shorts. */
  def bytesToJsShorts(bytes: Array[Byte]): js.Array[Short] = {
    js.Array(bytes.map(x => (x & 0xFF).toShort): _*)
  }

  /** Converts JS array of Short values to Scala's bytes array by dropping most
    * significant byte of each Short.
    */
  def jsShortsToBytes(jsShorts: js.Array[Short]): Array[Byte] = {
    jsShorts.toArray[Short].map(x => x.toByte)
  }

  /** Returns byte representation of the given field element. */
  def encodeFieldElem(p: ECFieldElem): Array[Byte] = {
    Uint8ArrayToBytes(CryptoFacadeJs.getEncodedOfFieldElem(p.elem))
  }

  /** Byte representation of the given point.
    *
    * @param p point to encode
    * @param compressed if true, generates a compressed point encoding
    * @see [[CryptoFacade.getASN1Encoding]]
    */
  def getASN1Encoding(p: Ecp, compressed: Boolean): Array[Byte] = {
    val hex = if (isInfinityPoint(p)) "00"  // to ensure equality with Java implementation
              else p.point.toHex(compressed)
    Base16.decode(hex).get
  }

  /** Returns the sign of the field element. */
  def signOf(p: ECFieldElem): Boolean = CryptoFacadeJs.testBitZeroOfFieldElem(p.elem)

  /** Normalization ensures that any projective coordinate is 1, and therefore that the x, y
    * coordinates reflect those of the equivalent point in an affine coordinate system.
    *
    * @return a new ECPoint instance representing the same point, but with normalized coordinates
    */
  def normalizePoint(p: Ecp): Ecp = new Ecp(CryptoFacadeJs.normalizePoint(p.point))

  /** Return simplified string representation of the point (used only for debugging) */
  def showPoint(p: Ecp): String = CryptoFacadeJs.showPoint(p.point)

  /** Multiply two points.
    *
    * @param p1 first point
    * @param p2 second point
    * @return group multiplication (p1 * p2)
    */
  def multiplyPoints(p1: Ecp, p2: Ecp): Ecp = new Ecp(CryptoFacadeJs.addPoint(p1.point, p2.point))

  /** Exponentiate a point p.
    * The implementation mimics the Java implementation of `AbstractECMultiplier.multiply`
    * from BouncyCastle library.
    *
    * @param p point to exponentiate
    * @param n exponent
    * @return p to the power of n (`p^n`) i.e. `p + p + ... + p` (n times)
    */
  def exponentiatePoint(p: Ecp, n: BigInteger): Ecp = {
    val sign = n.signum()
    if (sign == 0 || isInfinityPoint(p)) {
      val ctx = new CryptoContextJs()
      return new Ecp(ctx.getInfinity())
    }
    val scalar = Convert.bigIntegerToBigInt(n.abs())
    val positive = CryptoFacadeJs.multiplyPoint(p.point, scalar)
    val result = if (sign > 0) positive else CryptoFacadeJs.negatePoint(positive)
    new Ecp(result)
  }

  /** Check if a point is infinity. */
  def isInfinityPoint(p: Ecp): Boolean = CryptoFacadeJs.isInfinityPoint(p.point)

  /** Negates the given point by negating its y coordinate. */
  def negatePoint(p: Ecp): Ecp = new Ecp(CryptoFacadeJs.negatePoint(p.point))

  /** JS implementation of Elliptic Curve. */
  class Curve

  // TODO JS: Use JS library for secure source of randomness
  type SecureRandom = sigma.crypto.SecureRandomJS

  /** Opaque point type. */
  @js.native
  trait Point extends js.Object {
    /** coordinate x of this point. */
    def x: js.BigInt = js.native

    /** coordinate y of this point. */
    def y: js.BigInt = js.native

    /** Returns hex prepresentation of this point. */
    def toHex(b: Boolean): String = js.native
  }

  /** JS implementation of EC point. */
  class Ecp(val point: Point) {
    lazy val hex = point.toHex(true)
    override def hashCode(): Int = hex.hashCode

    override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) || (obj match {
      case that: Ecp => this.hex == that.hex
      case _ => false
    })
  }

  /** JS implementation of field element. */
  class ECFieldElem(val elem: js.BigInt) {
    private lazy val digits: String = elem.toString(10)

    override def hashCode(): Int = digits.hashCode

    override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) || (obj match {
      case that: ECFieldElem => this.digits == that.digits
      case _ => false
    })
  }

  /** Helper converters. */
  object Convert {
    /** Converts a JavaScript BigInt to a Java BigInteger. */
    def bigIntToBigInteger(jsValue: js.BigInt): BigInteger = {
      new BigInteger(jsValue.toString(10), 10)
    }

    /** Converts a Java BigInteger to a JavaScript BigInt. */
    def bigIntegerToBigInt(value: BigInteger): js.BigInt = {
      js.BigInt(value.toString(10))
    }
  }

  /** Create a new context for cryptographic operations. */
  def createContext(): CryptoContext = new CryptoContext {
    private val ctx = new CryptoContextJs

    /** The underlying elliptic curve descriptor. */
    override def curve: Curve = ???

    override def fieldCharacteristic: BigInteger = Convert.bigIntToBigInteger(ctx.getModulus())

    override def order: BigInteger = Convert.bigIntToBigInteger(ctx.getOrder())

    override def validatePoint(x: BigInteger, y: BigInteger): Ecp = {
      val point = ctx.validatePoint(Convert.bigIntegerToBigInt(x), Convert.bigIntegerToBigInt(y))
      new Ecp(point)
    }

    override def infinity(): Ecp =
      new Ecp(ctx.getInfinity())

    override def decodePoint(encoded: Array[Byte]): Ecp = {
      if (encoded(0) == 0) {
        return infinity()
      }
      new Ecp(ctx.decodePoint(Base16.encode(encoded)))
    }

    override def generator: Ecp =
      new Ecp(ctx.getGenerator())
  }

  /** Create JS specific source of secure randomness. */
  def createSecureRandom(): SecureRandom = new SecureRandomJS

  /** Computes HMAC-SHA512 hash of the given data using the specified key.
    *
    * @param key  the secret key used for hashing
    * @param data the input data to be hashed
    * @return a HMAC-SHA512 hash of the input data
    */
  def hashHmacSHA512(key: Array[Byte], data: Array[Byte]): Array[Byte] = {
    val keyArg = Uint8Array.from(bytesToJsShorts(key))
    val dataArg = Uint8Array.from(bytesToJsShorts(data))
    val hash = CryptoFacadeJs.hashHmacSHA512(keyArg, dataArg)
    Uint8ArrayToBytes(hash)
  }

  /** Generates PBKDF2 key from a mnemonic and passphrase using SHA512 digest. */
  def generatePbkdf2Key(normalizedMnemonic: String, normalizedPass: String): Array[Byte] = {
    val res = CryptoFacadeJs.generatePbkdf2Key(normalizedMnemonic, normalizedPass)
    Uint8ArrayToBytes(res)
  }

  /** Normalize a sequence of char values using NFKD normalization form. */
  def normalizeChars(chars: Array[Char]): String = {
    import js.JSStringOps._
    String.valueOf(chars).normalize(UnicodeNormalizationForm.NFKD)
  }

  /** Checks that the type of the value corresponds to the descriptor `tpe`.
    * If the value has complex structure only root type constructor is checked.
    * NOTE, this is surface check with possible false positives, but it is ok
    * when used in assertions, like `assert(isCorrestType(...))`, see `ConstantNode`.
    */
  def isCorrectType[T <: SType](value: Any, tpe: T): Boolean = value match {
    case c: Coll[_] => tpe match {
      case STuple(items) => c.tItem == sigma.AnyType && c.length == items.length
      case tpeColl: SCollection[_] => true
      case _ => sys.error(s"The value in the collection $c is not the correct type: $tpe. " +
        "Please make sure the value is of the correct type. " +
        "If the issue keeps happening, contact <a href=\"#\">Customer care</a>.")
    }
    case _: Option[_] => tpe.isOption
    case _: Tuple2[_, _] => tpe.isTuple && tpe.asTuple.items.length == 2
    case _: Boolean => tpe == SBoolean
    case _: Byte | _: Short | _: Int | _: Long => tpe.isInstanceOf[SNumericType]
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
}
