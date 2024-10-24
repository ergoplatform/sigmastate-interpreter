package sigma.data

import sigma._
import sigma.eval.Extensions.IntExt
import sigma.util.Extensions.BigIntOps

import scala.math.{Integral, Ordering}

object OrderingOps {
  def apply[T](implicit ord: Ordering[T]) = ord

  trait BigIntOrdering extends Ordering[BigInt] {
    def compare(x: BigInt, y: BigInt) = x.compareTo(y)
  }
  implicit object BigIntOrdering extends BigIntOrdering
}

object NumericOps {

  /** Base implementation of Integral methods for BigInt. */
  trait BigIntIsIntegral extends Integral[BigInt] {
    /** This method should not be used in v4.x */
    def quot(x: BigInt, y: BigInt): BigInt = x.divide(y)

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
    def rem(x: BigInt, y: BigInt): BigInt = x.mod(y)

    def plus(x: BigInt, y: BigInt): BigInt = x.add(y)
    def minus(x: BigInt, y: BigInt): BigInt = x.subtract(y)
    def times(x: BigInt, y: BigInt): BigInt = x.multiply(y)
    def negate(x: BigInt): BigInt = x.negate()
    def fromInt(x: Int): BigInt = x.toBigInt
    def toInt(x: BigInt): Int = x.toInt
    def toLong(x: BigInt): Long = x.toLong
    def toFloat(x: BigInt): Float = CSigmaDslBuilder.toBigInteger(x).floatValue()
    def toDouble(x: BigInt): Double = CSigmaDslBuilder.toBigInteger(x).doubleValue()
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
  object BigIntIsIntegral extends BigIntIsIntegral with OrderingOps.BigIntOrdering {
    def parseString(str: String): Option[BigInt] = ???
  }

  /** The instance of [[ExactIntegral]] typeclass for [[BigInt]]. */
  implicit object BigIntIsExactIntegral extends ExactIntegral[BigInt] {
    val n = BigIntIsIntegral
    override def plus(x: BigInt, y: BigInt): BigInt = n.plus(x, y)
    override def minus(x: BigInt, y: BigInt): BigInt = n.minus(x, y)
    override def times(x: BigInt, y: BigInt): BigInt = n.times(x, y)

    override def quot(x: BigInt, y: BigInt): BigInt = x.divide(y)

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
    override def divisionRemainder(x: BigInt, y: BigInt): BigInt = x.mod(y)

    override def toBigEndianBytes(x: BigInt): Coll[Byte] = Colls.fromArray(x.toBigInteger.toByteArray)

    override def bitwiseInverse(x: BigInt): BigInt = CBigInt(x.toBigInteger.not())

    override def bitwiseOr(x: BigInt, y: BigInt): BigInt = x.or(y)

    override def bitwiseAnd(x: BigInt, y: BigInt): BigInt = x.and(y)

    override def bitwiseXor(x: BigInt, y: BigInt): BigInt = x.xor(y)

    override def shiftLeft(x: BigInt, bits: Int): BigInt = {
      if (bits < 0 || bits >= 256) {
        throw new IllegalArgumentException(s"Wrong argument in BigInt.shiftRight: bits < 0 || bits >= 256 ($bits)")
      } else {
        x.shiftLeft(bits)
      }
    }

    override def shiftRight(x: BigInt, bits: Int): BigInt = {
      if (bits < 0 || bits >= 256) {
        throw new IllegalArgumentException(s"Wrong argument in BigInt.shiftRight: bits < 0 || bits >= 256 ($bits)")
      } else {
        x.shiftRight(bits)
      }
    }
  }

  /** The instance of [[scalan.ExactOrdering]] typeclass for [[BigInt]]. */
  implicit object BigIntIsExactOrdering extends ExactOrderingImpl[BigInt](BigIntIsIntegral)
}

