package sigma.data

import sigma.util.Extensions.{ByteOps, ShortOps}

/** Type-class which defines the operations on Integral types (Byte, Short, Int, Long, BigInt)
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
  }

  implicit object ShortIsExactIntegral extends ExactIntegral[Short] {
    val n = scala.math.Numeric.ShortIsIntegral
    override def plus(x: Short, y: Short): Short = x.addExact(y)
    override def minus(x: Short, y: Short): Short = x.subtractExact(y)
    override def times(x: Short, y: Short): Short = x.multiplyExact(y)
  }

  implicit object IntIsExactIntegral extends ExactIntegral[Int] {
    val n = scala.math.Numeric.IntIsIntegral
    override def plus(x: Int, y: Int): Int = java7.compat.Math.addExact(x, y)
    override def minus(x: Int, y: Int): Int = java7.compat.Math.subtractExact(x, y)
    override def times(x: Int, y: Int): Int = java7.compat.Math.multiplyExact(x, y)
  }

  implicit object LongIsExactIntegral extends ExactIntegral[Long] {
    val n = scala.math.Numeric.LongIsIntegral
    override def plus(x: Long, y: Long): Long = java7.compat.Math.addExact(x, y)
    override def minus(x: Long, y: Long): Long = java7.compat.Math.subtractExact(x, y)
    override def times(x: Long, y: Long): Long = java7.compat.Math.multiplyExact(x, y)
  }
}
