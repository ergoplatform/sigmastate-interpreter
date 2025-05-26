package sigma.crypto

import debox.cfor
import java.math.BigInteger
import scala.collection.mutable


/** Base class for EC-based groups where DLOG problem is hard (with bouncycastle-like interface).
  * @param ctx context which abstracts basic operations with curve and elements.
  */
abstract class BcDlogGroup(val ctx: CryptoContext) extends DlogGroup {
  /** Characteristic of the finite field of the underlying curve. */
  lazy val p: BigInteger = ctx.fieldCharacteristic

  /** Order of the group as defined in ASN.1 def for Elliptic-Curve ECParameters structure.
    * See X9.62, for further details.
    * For reference implementation see `org.bouncycastle.asn1.x9.X9ECParameters.getN`.
    */
  lazy val q: BigInteger = ctx.order

  /** Now that we have p, we can calculate k which is the maximum length in bytes
    * of a string to be converted to a Group Element of this group.
    */
  lazy val k = calcK(p)

  /**
    * The class GroupElementExponentiations is a nested class of DlogGroupAbs.<p>
    * It performs the actual work of pre-computation of the exponentiations for one base.
    * It is composed of two main elements. The group element for which the optimized computations
    * are built for, called the base and a vector of group elements that are the result of
    * exponentiations of order 1,2,4,8,
    *
    * The constructor creates a map structure in memory.
    * Then calculates the exponentiations of order 1,2,4,8 for the given base and save them in the map.
    *
    * @param base group element for which the optimized computations are built for
    * @throws IllegalArgumentException
    *
    */
  private class GroupElementsExponentiations(base: ElemType) {

    private val exponentiations = new mutable.ListBuffer[ElemType]()

    exponentiations += this.base // add the base - base^1
    val two = new BigInteger("2")
    cfor(1)(_ < 4, _ + 1) { i =>
      exponentiations += exponentiate(exponentiations(i - 1), two)
    }

    /**
      * Calculates the necessary additional exponentiations and fills the exponentiations vector with them.
      *
      * @param size - the required exponent
      * @throws IllegalArgumentException
      */
    private def prepareExponentiations(size: BigInteger): Unit = { //find log of the number - this is the index of the size-exponent in the exponentiation array
      val index = size.bitLength - 1
      /* calculates the necessary exponentiations and put them in the exponentiations vector */
      cfor(exponentiations.size)(_ <= index, _ + 1) { i =>
        exponentiations += exponentiate(exponentiations(i - 1), two)
      }
    }

    /**
      * Checks if the exponentiations had already been calculated for the required size.
      * If so, returns them, else it calls the private function prepareExponentiations with the given size.
      *
      * @param size - the required exponent
      * @return groupElement - the exponentiate result
      */
    def getExponentiation(size: BigInteger): ElemType = {
      /**
        * The exponents in the exponents vector are all power of 2.
        * In order to achieve the exponent size, we calculate its closest power 2 in the exponents vector
        * and continue the calculations from there.
        */
      // find the the closest power 2 exponent
      val index = size.bitLength - 1

      /* if the requested index out of the vector bounds, the exponents have not been calculated yet, so calculates them.*/
      if (exponentiations.lengthCompare(index) <= 0) prepareExponentiations(size)

      var exponent = exponentiations(index) //get the closest exponent in the exponentiations vector

      /* if size is not power 2, calculates the additional multiplications */
      val lastExp = two.pow(index)
      val difference = size.subtract(lastExp)
      if (difference.compareTo(BigInteger.ZERO) > 0) {
        val diff = getExponentiation(size.subtract(lastExp))
        exponent = multiplyGroupElements(diff, exponent)
      }
      exponent
    }
  }

  //map for multExponentiationsWithSameBase calculations
  private val exponentiationsCache = mutable.Map[ElemType, GroupElementsExponentiations]()


  /** Creates the generator.
    * Assume that (x,y) are the coordinates of a point that is indeed a generator but
    * check that (x,y) are the coordinates of a point.
    */
  override lazy val generator: ElemType = ctx.generator

  /**
    * This function calculates k, the maximum length in bytes of a string to be converted to a Group Element of this group.
    *
    * @param p
    * @return k
    */
  def calcK(p: BigInteger): Int = {
    val bitsInp = p.bitLength
    var k = Math.floor((0.4 * bitsInp) / 8).toInt - 1
    //For technical reasons of how we chose to do the padding for encoding and decoding (the least significant byte of the encoded string contains the size of the
    //the original binary string sent for encoding, which is used to remove the padding when decoding) k has to be <= 255 bytes so that the size can be encoded in the padding.
    if (k > 255) k = 255
    k
  }

  /**
    * @return the order of this Dlog group
    */
  override lazy val order: BigInteger = ctx.order

  /**
    * @return the identity of this Dlog group
    */
  override lazy val identity: ElemType = ctx.infinity.asInstanceOf[ElemType]

  /**
    * Calculates the inverse of the given GroupElement.
    *
    * @param groupElement to invert
    * @return the inverse element of the given GroupElement
    * @throws IllegalArgumentException
    **/
  override def inverseOf(groupElement: ElemType): ElemType =
    CryptoFacade.negatePoint(groupElement)

  /**
    * Raises the base GroupElement to the exponent. The result is another GroupElement.
    *
    * @param exponent
    * @param base
    * @return the result of the exponentiation
    * @throws IllegalArgumentException
    */
  override def exponentiate(base: ElemType, exponent: BigInteger): ElemType = {
    //infinity remains the same after any exponentiate
    if (CryptoFacade.isInfinityPoint(base)) return base

    //If the exponent is negative, convert it to be the exponent modulus q.
    val exp = if (exponent.compareTo(BigInteger.ZERO) < 0) exponent.mod(order) else exponent

    CryptoFacade.exponentiatePoint(base, exp)
  }


  /**
    * Creates a random member of this Dlog group
    *
    * @return the random element
    */
  override def createRandomElement(): ElemType = {
    //This is a default implementation that is valid for all the Dlog Groups and relies on mathematical properties of the generators.
    //However, if a specific Dlog Group has a more efficient implementation then is it advised to override this function in that concrete
    //Dlog group. For example we do so in CryptoPpDlogZpSafePrime.
    val one = BigInteger.ONE
    val qMinusOne = ctx.order.subtract(one)
    // choose a random number x in Zq*
    val randNum = BigIntegers.createRandomInRange(one, qMinusOne, secureRandom)
    // compute g^x to get a new element
    exponentiate(generator, randNum)
  }


  /**
    *
    * Checks if the order of this group is greater than `2^numBits`
    * @param numBits
    * @return <code>true</code> if the order is greater than `2^numBits`;<p>
    *         <code>false</code> otherwise.
    **/
  override def orderGreaterThan(numBits: Int): Boolean =
    if (order.compareTo(new BigInteger("2").pow(numBits)) > 0) true else false

  /**
    * Multiplies two GroupElements
    *
    * @param groupElement1
    * @param groupElement2
    * @return the multiplication result
    * @throws IllegalArgumentException
    */
  override def multiplyGroupElements(groupElement1: ElemType, groupElement2: ElemType): ElemType =
    CryptoFacade.multiplyPoints(groupElement1, groupElement2)

  /**
    * Computes the product of several exponentiations of the same base
    * and distinct exponents.
    * An optimization is used to compute it more quickly by keeping in memory
    * the result of h1, h2, h4,h8,... and using it in the calculation.<p>
    * Note that if we want a one-time exponentiation of h it is preferable to use the basic exponentiation function
    * since there is no point to keep anything in memory if we have no intention to use it.
    *
    * @param base
    * @param exponent
    * @return the exponentiation result
    */
  override def exponentiateWithPreComputedValues(base: ElemType, exponent: BigInteger): ElemType = {
    //extracts from the map the GroupElementsExponentiations object corresponding to the accepted base
    val exponentiations = exponentiationsCache.getOrElse(key = base, {
      // if there is no object that matches this base - create it and add it to the map
      val exps = new GroupElementsExponentiations(base)
      exponentiationsCache.put(base, exps)
      exps
    })

    // calculates the required exponent
    exponentiations.getExponentiation(exponent)
  }

  /**
    * This function cleans up any resources used by exponentiateWithPreComputedValues for the requested base.
    * It is recommended to call it whenever an application does not need to continue calculating exponentiations for this specific base.
    *
    * @param base
    */
  override def endExponentiateWithPreComputedValues(base: ElemType): Unit = exponentiationsCache -= base

  /**
    * This function returns the value <I>k</I> which is the maximum length of a string to be encoded to a Group Element of this group.<p>
    * Any string of length <I>k</I> has a numeric value that is less than (p-1)/2 - 1.
    * <I>k</I> is the maximum length a binary string is allowed to be in order to encode the said binary string to a group element and vice-versa.<p>
    * If a string exceeds the <I>k</I> length it cannot be encoded.
    *
    * @return k the maximum length of a string to be encoded to a Group Element of this group. k can be zero if there is no maximum.
    */
  override lazy val maxLengthOfByteArrayForEncoding: Int = k

}

/** Implementation of [[BcDlogGroup]] using SecP256K1 curve. */
object SecP256K1Group extends BcDlogGroup(CryptoFacade.createCryptoContext())