/*
 By Leonid Reyzin

 This is free and unencumbered software released into the public domain.

 Anyone is free to copy, modify, publish, use, compile, sell, or
 distribute this software, either in source code form or as a compiled
 binary, for any purpose, commercial or non-commercial, and by any
 means.

 In jurisdictions that recognize copyright laws, the author or authors
 of this software dedicate any and all copyright interest in the
 software to the public domain. We make this dedication for the benefit
 of the public at large and to the detriment of our heirs and
 successors. We intend this dedication to be an overt act of
 relinquishment in perpetuity of all present and future rights to this
 software under copyright law.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
 OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 OTHER DEALINGS IN THE SOFTWARE.

 For more information, please refer to <http://unlicense.org>
 */

package sigmastate.crypto

import debox.cfor

class GF2_192_Poly {
  final private var c: Array[GF2_192] = null // must be not null and of length at least 1

  private var deg: Int = 0 // must be >=0. actual degree is <= deg. c[deg+1]...c[c.length-1] must be 0 or null
  // deg of the 0 polynomial is 0

  /**
    * Constructs the polynomial given the byte array representation of the coefficients.
    * Coefficient of degree zero is given separately. Each coefficient should be given
    * as a 24-byte representation of a GF2_192 value. Coefficient of degree 1 should
    * start at moreCoeffs[0].
    *
    * @param coeff0     byte array representing lowest coefficient (24 bytes)
    * @param moreCoeffs byte array with concatenation of byte-converted coefficients
    *                   (24 bytes each) from degree 1 to the highest
    */
  def this(coeff0: Array[Byte], moreCoeffs: Array[Byte]) {
    this()
    deg = moreCoeffs.length / 24
    c = new Array[GF2_192](deg + 1)
    c(0) = new GF2_192(coeff0)
    cfor(1)(_ <= deg, _ + 1) { i =>
      c(i) = new GF2_192(moreCoeffs, (i - 1) * 24)
    }
  }

  /**
    * Evaluates the polynomial at a given point
    *
    * @param x the last byte of a field element (all other bits are assumed to be 0)
    * @return the value of this polynomial evaluated at the field element
    */
  def evaluate(x: Byte): GF2_192 = {
    val res = new GF2_192(c(deg))
    cfor(deg - 1)(_ >= 0, _ - 1) { d =>
      GF2_192.mul(res, res, x)
      GF2_192.add(res, res, c(d))
    }
    res
  }

  /**
    * adds r*p to this; assumes p is monic, c.length>p.deg, and (p.deg == this.deg+1, or this==0 and p==1)
    *
    * @param p the monic polynomial being added to this
    * @param r the constant by which p is multiplied before being added
    */
  private def addMonicTimesConstantTo(p: GF2_192_Poly, r: GF2_192): Unit = {
    val t: GF2_192 = new GF2_192
    cfor(0)(_ < p.deg, _ + 1) { i =>
      GF2_192.mul(t, p.c(i), r)
      GF2_192.add(c(i), c(i), t)
    }
    deg = p.deg
    c(deg) = new GF2_192(r)
  }

  /**
    * multiplies this by (x+r), assuming this is monic of degree deg (i.e. assumed c[deg]==1)
    *
    * @param r the constant term of the monomial
    */
  private def monicTimesMonomial(r: Byte): Unit = {
    deg += 1
    c(deg) = new GF2_192(1)
    cfor(deg - 1)(_ > 0, _ - 1) { i =>
      // c[i] = c[i-1]+r*c[i]
      GF2_192.mul(c(i), c(i), r)
      GF2_192.add(c(i), c(i), c(i - 1))
    }
    GF2_192.mul(c(0), c(0), r)
  }

  /**
    * Constructs a constant polynomial
    *
    * @param maxDeg       the maximum degree this polynomial could possibly have (to allocate space)
    * @param constantTerm the polynomial is initially created with degree 0 and given constantTerm
    */
  def this(maxDeg: Int, constantTerm: Int) {
    this()
    c = new Array[GF2_192](maxDeg + 1)
    c(0) = new GF2_192(constantTerm)
    deg = 0
  }

  /**
    *
    * @return this represented in usual polynomial notation (but possibly leading 0s), with X as the free variable
    */
  override def toString: String = {
    var ret: String = ""
    if (deg >= 2) {
      ret += c(deg).toString + "*X^" + deg
      cfor(deg - 1)(_ >= 2, _ - 1) { i =>
        ret += " + " + c(i) + "*X^" + i
      }
      ret += " + "
    }
    if (deg >= 1) {
      ret += c(1) + "*X" + " + "
    }
    ret += c(0)
    ret
  }

  /**
    * Returns a byte array that contains the concatenation of all the coefficients
    * (except possibly the degree-0 coefficient, which is omitted if coeff0 is false).
    * Lowest-degree coefficient (0 or 1 depending on coeff0) starts at index 0 of the returned array.
    * Each coefficient takes 24 bytes, for a total of degree*24 bytes if coeff0 is false,
    * or (degree+1)*24 bytes if coeff0 is true
    *
    * @param coeff0 whether to include coeff0
    * @return array of all coefficients (except possibly 0th depending on coeff0)
    */
  def toByteArray(coeff0: Boolean): Array[Byte] = {
    val c0 = if (coeff0) 0 else 1
    val ret: Array[Byte] = new Array[Byte]((deg + 1 - c0) * 24)
    cfor(c0)(_ <= deg, _ + 1) { i =>
      c(i).toByteArray(ret, (i - c0) * 24)
    }
    ret
  }

  /**
    * @return The degree-0 coefficient, converted to an array of 24 bytes
    */
  def coeff0Bytes: Array[Byte] = c(0).toByteArray

  override def hashCode: Int = {
    31 * java.util.Arrays.deepHashCode(c.asInstanceOf[Array[AnyRef]]) + deg
  }

  override def equals(obj: Any): Boolean = {
    if (this eq obj.asInstanceOf[AnyRef]) return true
    obj match {
      case that: GF2_192_Poly =>
        java.util.Arrays.deepEquals(c.asInstanceOf[Array[AnyRef]], that.c.asInstanceOf[Array[AnyRef]]) &&
          deg == that.deg
      case _ => false
    }
  }
}

object GF2_192_Poly {
  /**
    * Factory constructor -- same as GF2_192_Poly(coeff0, moreCoeffs)
    *
    * @param coeff0     byte array representing lowest coefficient (24 bytes)
    * @param moreCoeffs byte array with concatenation of byte-converted coefficients
    *                   (24 bytes each) from degree 1 to the highest
    * @return new polynomial with the given coefficients
    */
  // deg of the 0 polynomial is 0
  def fromByteArray(coeff0: Array[Byte], moreCoeffs: Array[Byte]): GF2_192_Poly = {
    val res = new GF2_192_Poly(coeff0, moreCoeffs)
    res
  }

  /**
    * Interpolates the polynomial at given points (and at point 0, if valueAt0!=null).
    * If points are not all distinct, or if 0 is in the points array and valueAt0!=null, behavior is undefined.
    * valueAt0 is separated only for efficiency reason; the caller can treat 0 like any other point instead
    * (i.e., the points array can include 0 if valueAt0==null, but computation will be slightly less efficient).
    * If points is null, or values is null, or if lengths of points and values arrays differ,
    * or if the arrays are 0 length and valueAt0 is null, returns null.
    *
    * @param points   the set of distinct inputs to the returned polynomial
    *                 (last byte of the field element only; all other bits are assumed to be 0)
    * @param values   values[i] will be the result evaluating the returned polynomial at points[i]. values[i] must not be null.
    * @param valueAt0 if not null, then valueAt0 will be the result of evaluating the returned polynomial at 0
    * @return the unique lowest-degree polynomial p such that for every i, p(points[i]) = values[i] and p(0)=valueAt0
    *         (if valueAt0!=null)
    */
  def interpolate(points: Array[Byte],
                  values: Array[GF2_192],
                  valueAt0: GF2_192): GF2_192_Poly = {
    if (points == null || values == null ||
        (values.length == 0 && valueAt0 == null) ||
        values.length != points.length) return null

    var resultDegree = values.length - 1
    if (valueAt0 != null) resultDegree += 1

    val result = new GF2_192_Poly(resultDegree, 0)
    val vanishingPoly = new GF2_192_Poly(resultDegree, 1)

    cfor (0)(_ < points.length, _ + 1) { i =>
      val t = result.evaluate(points(i))
      val s = vanishingPoly.evaluate(points(i))

      // need to find r such that currentValue+r*valueOfVanishingPoly = values[i]
      GF2_192.add(t, t, values(i))
      GF2_192.invert(s, s)
      GF2_192.mul(t, t, s)

      result.addMonicTimesConstantTo(vanishingPoly, t)

      if (i < points.length - 1 || valueAt0 != null) {
        vanishingPoly.monicTimesMonomial(points(i))
      }
    }

    if (valueAt0 != null) {
      // the last point is 0
      val t = new GF2_192(result.c(0)) // evaluating at 0 is easy
      val s = new GF2_192(vanishingPoly.c(0))

      // need to find r such that currentValue+r*valueOfVanishingPoly = valueAt0]
      GF2_192.add(t, t, valueAt0)
      GF2_192.invert(s, s)
      GF2_192.mul(t, t, s)
      result.addMonicTimesConstantTo(vanishingPoly, t)
    }

    result
  }

}

