package sigma.data

import debox.cfor
import scorex.util.encode.Base16
import sigma._
import sigma.crypto.BigIntegers
import sigma.data.UnsignedBigIntOrderingOps.UnsignedBigIntOrdering
import sigma.eval.Extensions.IntExt

import scala.math.{Integral, Ordering}

object UnsignedBigIntOrderingOps {
  def apply[T](implicit ord: Ordering[T]) = ord

  trait UnsignedBigIntOrdering extends Ordering[UnsignedBigInt] {
    def compare(x: UnsignedBigInt, y: UnsignedBigInt) = x.compareTo(y)
  }
  implicit object UnsignedBigIntOrdering extends UnsignedBigIntOrdering
}

object UnsignedBigIntNumericOps {

  /** Base implementation of Integral methods for UnsignedBigInt. */
  trait UnsignedBigIntIsIntegral extends Integral[UnsignedBigInt] {
    /** This method should not be used in v4.x */
    def quot(x: UnsignedBigInt, y: UnsignedBigInt): UnsignedBigInt = x.divide(y)

    /** This method is used in ErgoTreeEvaluator based interpreter, to implement
      * '%' operation of ErgoTree (i.e. `%: (T, T) => T` operation) for all
      * numeric types T including BigInt.
      *
      * In the v4.x interpreter, however, the `%` operation is implemented using
      * [[CBigInt]].mod method , which delegates to [[java.math.BigInteger]].mod method.
      *
      * Even though this method is called `rem`, the semantics of ErgoTree
      * language requires it to correspond to [[java.math.BigInteger]].mod
      * method.
      *
      * For this reason we define implementation of this `rem` method using
      * [[BigInt]].mod.
      *
      * NOTE: This method should not be used in v4.x
      */
    def rem(x: UnsignedBigInt, y: UnsignedBigInt): UnsignedBigInt = x.mod(y)

    def plus(x: UnsignedBigInt, y: UnsignedBigInt): UnsignedBigInt = x.add(y)
    def minus(x: UnsignedBigInt, y: UnsignedBigInt): UnsignedBigInt = x.subtract(y)
    def times(x: UnsignedBigInt, y: UnsignedBigInt): UnsignedBigInt = x.multiply(y)
    def negate(x: UnsignedBigInt): UnsignedBigInt = ???
    def fromInt(x: Int): UnsignedBigInt = x.toUnsignedBigInt
    def toInt(x: UnsignedBigInt): Int = x.toInt
    def toLong(x: UnsignedBigInt): Long = x.toLong
    def toFloat(x: UnsignedBigInt): Float = x.toFloat
    def toDouble(x: UnsignedBigInt): Double = x.toDouble
  }

  /**
    * The instance of Integral for UnsignedBigInt.
    * Done similarly to BigIntIsIntegral.
    */
  object UnsignedBigIntIsIntegral extends UnsignedBigIntIsIntegral with UnsignedBigIntOrdering {
    def parseString(str: String): Option[UnsignedBigInt] = ???
  }

  /** The instance of [[ExactIntegral]] typeclass for [[BigInt]]. */
  implicit object UnsignedBigIntIsExactIntegral extends ExactIntegral[UnsignedBigInt] {
    val n = UnsignedBigIntIsIntegral
    override def plus(x: UnsignedBigInt, y: UnsignedBigInt): UnsignedBigInt = n.plus(x, y)
    override def minus(x: UnsignedBigInt, y: UnsignedBigInt): UnsignedBigInt = n.minus(x, y)
    override def times(x: UnsignedBigInt, y: UnsignedBigInt): UnsignedBigInt = n.times(x, y)

    override def quot(x: UnsignedBigInt, y: UnsignedBigInt): UnsignedBigInt = x.divide(y)

    /** This method is used in ErgoTreeEvaluator based interpreter, to implement
      * '%' operation of ErgoTree (i.e. `%: (T, T) => T` operation) for all
      * numeric types T including BigInt.
      *
      * In the v4.x interpreter, however, the `%` operation is implemented using
      * [[CBigInt]].mod method, which delegates to [[java.math.BigInteger]].mod method.
      *
      * Even though this method is called `divisionRemainder`, the semantics of ErgoTree
      * language requires it to correspond to [[java.math.BigInteger]].mod method.
      *
      * For this reason we define implementation of this method using [[BigInt]].mod.
      *
      * NOTE: This method should not be used in v4.x
      */
    override def divisionRemainder(x: UnsignedBigInt, y: UnsignedBigInt): UnsignedBigInt = x.mod(y)

    /** Returns a big-endian representation of this value in a collection of bytes.
      * For example, the `Int` value `0x12131415` would yield the
      * collection of bytes [0x12, 0x13, 0x14, 0x15]
      */
    override def toBigEndianBytes(x: UnsignedBigInt): Coll[Byte] = x.toBytes

    /**
      * @return a numeric value which is inverse of `x` (every bit is flipped)
      */
    override def bitwiseInverse(x: UnsignedBigInt): UnsignedBigInt = x.bitwiseInverse()

    /**
      * @return a numeric value which is `this | that`
      */
    override def bitwiseOr(x: UnsignedBigInt, y: UnsignedBigInt): UnsignedBigInt = {
      val vx = x.asInstanceOf[CUnsignedBigInt].wrappedValue
      val vy = y.asInstanceOf[CUnsignedBigInt].wrappedValue
      CUnsignedBigInt(vx.or(vy))
    }

    /**
      * @return a numeric value which is `this && that`
      */
    override def bitwiseAnd(x: UnsignedBigInt, y: UnsignedBigInt): UnsignedBigInt = {
      val vx = x.asInstanceOf[CUnsignedBigInt].wrappedValue
      val vy = y.asInstanceOf[CUnsignedBigInt].wrappedValue
      CUnsignedBigInt(vx.and(vy))
    }

    /**
      * @return a numeric value which is `this xor that`
      */
    override def bitwiseXor(x: UnsignedBigInt, y: UnsignedBigInt): UnsignedBigInt = {
      val vx = x.asInstanceOf[CUnsignedBigInt].wrappedValue
      val vy = y.asInstanceOf[CUnsignedBigInt].wrappedValue
      CUnsignedBigInt(vx.xor(vy))
    }

    /**
      * @return a value which is (this << n). The shift distance, n, may be negative,
      *         in which case this method performs a right shift. (Computes floor(this * 2n).)
      */
    override def shiftLeft(x: UnsignedBigInt, bits: Int): UnsignedBigInt = {
      if (bits < 0 || bits >= 256) {
        throw new IllegalArgumentException(s"Wrong argument in UnsignedBigInt.shiftLeft: bits < 0 || bits >= 256 ($bits)")
      } else {
        x.shiftLeft(bits)
      }
    }

    /**
      * @return a value which is (this >> n). Sign extension is performed. The shift distance, n,
      *         may be negative, in which case this method performs a left shift. (Computes floor(this / 2n).)
      */
    override def shiftRight(x: UnsignedBigInt, bits: Int): UnsignedBigInt = {
      if (bits < 0 || bits >= 256) {
        throw new IllegalArgumentException(s"Wrong argument in UnsignedBigInt.shiftLeft: bits < 0 || bits >= 256 ($bits)")
      } else {
        x.shiftRight(bits)
      }
    }
  }

  /** The instance of [[scalan.ExactOrdering]] typeclass for [[BigInt]]. */
  implicit object UnsignedBigIntIsExactOrdering extends ExactOrderingImpl[UnsignedBigInt](UnsignedBigIntIsIntegral)
}

