package sigma.data

import sigma.{Coll, Colls}
import sigma.util.Extensions.{ByteOps, ShortOps}

/** Type-class which defines the operations on Integral types (Byte, Short, Int, Long, BigInt, UnsignedBigInt)
  * with overflow checks.
  *
  * An exception is raised when an overflow is detected.
  * Each concrete instance of this type-class overrides three methods `plus`, `minus`,
  * `times`.
  *
  * By default all the methods are implemented by delegating to the corresponding Integral
  * instance from the standard Scala library.
  *
  * This trait is used in core IR to avoid implicitly using standard scala implementations.
  */
trait ExactIntegral[T] extends ExactNumeric[T] {
  protected val n: scala.math.Integral[T]

  /** Integer division operation `x / y`. */
  def quot(x: T, y: T): T = n.quot(x, y)

  /** Operation which returns remainder from dividing x by y.
    * The exact rules are defined in the concrete instance of the type T.
    * A default implementation delegates to Integral[T].rem method for the corresponding
    * type T.
    * The default implementation can be overridden for any concrete type T.
    */
  def divisionRemainder(x: T, y: T): T = n.rem(x, y)
}

/** ExactIntegral instances for all types. */
object ExactIntegral {

  implicit object ByteIsExactIntegral extends ExactIntegral[Byte] {
    val n = scala.math.Numeric.ByteIsIntegral
    override def plus(x: Byte, y: Byte): Byte = x.addExact(y)
    override def minus(x: Byte, y: Byte): Byte = x.subtractExact(y)
    override def times(x: Byte, y: Byte): Byte = x.multiplyExact(y)
    override def toBigEndianBytes(x: Byte): Coll[Byte] = Colls.fromItems(x)
    override def bitwiseInverse(x: Byte): Byte = (~x).toByte
    override def bitwiseOr(x: Byte, y: Byte): Byte = (x | y).toByte
    override def bitwiseAnd(x: Byte, y: Byte): Byte = (x & y).toByte
    override def bitwiseXor(x: Byte, y: Byte): Byte = (x ^ y).toByte
    override def shiftLeft(x: Byte, bits: Int): Byte = {
      if (bits < 0 || bits >= 8) {
        throw new IllegalArgumentException(s"Wrong argument in Byte.shiftRight: bits < 0 || bits >= 8 ($bits)")
      } else {
        (x << bits).toByte
      }
    }
    override def shiftRight(x: Byte, bits: Int): Byte = {
      if (bits < 0 || bits >= 8) {
        throw new IllegalArgumentException(s"Wrong argument in Byte.shiftRight: bits < 0 || bits >= 8 ($bits)")
      } else {
        (x >> bits).toByte
      }
    }
  }

  implicit object ShortIsExactIntegral extends ExactIntegral[Short] {
    val n = scala.math.Numeric.ShortIsIntegral
    override def plus(x: Short, y: Short): Short = x.addExact(y)
    override def minus(x: Short, y: Short): Short = x.subtractExact(y)
    override def times(x: Short, y: Short): Short = x.multiplyExact(y)
    override def toBigEndianBytes(x: Short): Coll[Byte] = Colls.fromItems((x >> 8).toByte, x.toByte)
    override def bitwiseInverse(x: Short): Short = (~x).toShort
    override def bitwiseOr(x: Short, y: Short): Short = (x | y).toShort
    override def bitwiseAnd(x: Short, y: Short): Short = (x & y).toShort
    override def bitwiseXor(x: Short, y: Short): Short = (x ^ y).toShort
    override def shiftLeft(x: Short, bits: Int): Short = {
      if (bits < 0 || bits >= 16) {
        throw new IllegalArgumentException(s"Wrong argument in Short.shiftRight: bits < 0 || bits >= 16 ($bits)")
      } else {
        (x << bits).toShort
      }
    }
    override def shiftRight(x: Short, bits: Int): Short = {
      if (bits < 0 || bits >= 16){
        throw new IllegalArgumentException(s"Wrong argument in Short.shiftRight: bits < 0 || bits >= 16 ($bits)")
      } else {
        (x >> bits).toShort
      }
    }
  }

  implicit object IntIsExactIntegral extends ExactIntegral[Int] {
    val n = scala.math.Numeric.IntIsIntegral
    override def plus(x: Int, y: Int): Int = java7.compat.Math.addExact(x, y)
    override def minus(x: Int, y: Int): Int = java7.compat.Math.subtractExact(x, y)
    override def times(x: Int, y: Int): Int = java7.compat.Math.multiplyExact(x, y)
    override def toBigEndianBytes(x: Int): Coll[Byte] =
      Colls.fromItems((x >> 24).toByte, (x >> 16).toByte, (x >> 8).toByte, x.toByte)
    override def bitwiseInverse(x: Int): Int = ~x
    override def bitwiseOr(x: Int, y: Int): Int = x | y
    override def bitwiseAnd(x: Int, y: Int): Int = x & y
    override def bitwiseXor(x: Int, y: Int): Int = x ^ y

    override def shiftLeft(x: Int, bits: Int): Int = {
      if (bits < 0 || bits >= 32) {
        throw new IllegalArgumentException(s"Wrong argument in Byte.shiftRight: bits < 0 || bits >= 32 ($bits)")
      } else {
        x << bits
      }
    }

    override def shiftRight(x: Int, bits: Int): Int = {
      if (bits < 0 || bits >= 32) {
        throw new IllegalArgumentException(s"Wrong argument in Int.shiftRight: bits < 0 || bits >= 32 ($bits)")
      } else {
        x >> bits
      }
    }
  }

  implicit object LongIsExactIntegral extends ExactIntegral[Long] {
    val n = scala.math.Numeric.LongIsIntegral
    override def plus(x: Long, y: Long): Long = java7.compat.Math.addExact(x, y)
    override def minus(x: Long, y: Long): Long = java7.compat.Math.subtractExact(x, y)
    override def times(x: Long, y: Long): Long = java7.compat.Math.multiplyExact(x, y)
    override def toBigEndianBytes(x: Long): Coll[Byte] =
      Colls.fromItems((x >> 56).toByte, (x >> 48).toByte, (x >> 40).toByte, (x >> 32).toByte, (x >> 24).toByte, (x >> 16).toByte, (x >> 8).toByte, x.toByte)
    override def bitwiseInverse(x: Long): Long = ~x
    override def bitwiseOr(x: Long, y: Long): Long = x | y
    override def bitwiseAnd(x: Long, y: Long): Long = x & y
    override def bitwiseXor(x: Long, y: Long): Long = x ^ y

    override def shiftLeft(x: Long, bits: Int): Long = {
      if (bits < 0 || bits >= 64) {
        throw new IllegalArgumentException(s"Wrong argument in Long.shiftRight: bits < 0 || bits >= 64 ($bits)")
      } else {
        x << bits
      }
    }

    override def shiftRight(x: Long, bits: Int): Long = {
      if (bits < 0 || bits >= 64) {
        throw new IllegalArgumentException(s"Wrong argument in Long.shiftRight: bits < 0 || bits >= 64 ($bits)")
      } else {
        x >> bits
      }
    }
  }
}
