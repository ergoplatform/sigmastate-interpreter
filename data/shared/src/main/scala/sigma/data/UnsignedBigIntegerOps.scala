package sigma.data

import sigma._
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
    def toFloat(x: UnsignedBigInt): Float = ???
    def toDouble(x: UnsignedBigInt): Double = ???
  }

  /** The instance of Integral for BigInt.
    *
    * Note: ExactIntegral was not defined for [[sigma.BigInt]] in v4.x.
    * This is because arithmetic BigInt operations were handled in a special way
    * (see `case op: ArithOp[t] if op.tpe == SBigInt =>` in RuntimeCosting.scala).
    * As result [[scalan.primitives.UnBinOps.ApplyBinOp]] nodes were not created for
    * BigInt operations in v4.x., and hence operation descriptors such as
    * [[scalan.primitives.NumericOps.IntegralDivide]] and
    * [[scalan.primitives.NumericOps.IntegralMod]] were not used for BigInt.
    * NOTE: this instance is used in the new v5.0 interpreter.
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
    override def toBigEndianBytes(x: UnsignedBigInt): Coll[Byte] = ???

    /**
      * @return a numeric value which is inverse of `x` (every bit, including sign, is flipped)
      */
    override def bitwiseInverse(x: UnsignedBigInt): UnsignedBigInt = ???

    /**
      * @return a numeric value which is `this | that`
      */
    override def bitwiseOr(x: UnsignedBigInt, y: UnsignedBigInt): UnsignedBigInt = ???

    /**
      * @return a numeric value which is `this && that`
      */
    override def bitwiseAnd(x: UnsignedBigInt, y: UnsignedBigInt): UnsignedBigInt = ???

    /**
      * @return a numeric value which is `this xor that`
      */
    override def bitwiseXor(x: UnsignedBigInt, y: UnsignedBigInt): UnsignedBigInt = ???

    /**
      * @return a value which is (this << n). The shift distance, n, may be negative,
      *         in which case this method performs a right shift. (Computes floor(this * 2n).)
      */
    override def shiftLeft(x: UnsignedBigInt, bits: Int): UnsignedBigInt = ???

    /**
      * @return a value which is (this >> n). Sign extension is performed. The shift distance, n,
      *         may be negative, in which case this method performs a left shift. (Computes floor(this / 2n).)
      */
    override def shiftRight(x: UnsignedBigInt, bits: Int): UnsignedBigInt = ???
  }

  /** The instance of [[scalan.ExactOrdering]] typeclass for [[BigInt]]. */
  implicit object UnsignedBigIntIsExactOrdering extends ExactOrderingImpl[UnsignedBigInt](UnsignedBigIntIsIntegral)
}

