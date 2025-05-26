package sigma.data

import sigma.data.ExactIntegral._

/** Numeric operations with overflow checks.
  * Raise exception when overflow is detected.
  * Each instance of this typeclass overrides three methods `plus`, `minus`, `times`.
  * All other methods are implemented by delegating to the corresponding Numeric instance from
  * standard Scala library.
  * This trait is used in core IR to avoid implicitly using standard scala implementations
  */
trait ExactNumeric[T] {
  protected val n: Numeric[T]

  /** Addition operation `x + y`. */
  def plus(x: T, y: T): T

  /** Subtraction operation `x - y`. */
  def minus(x: T, y: T): T

  /** Multiplication operation `x * y`. */
  def times(x: T, y: T): T

  /** Returns negative value `-x`. */
  def negate(x: T): T = n.negate(x)

  /** Returns a value of type T, which corresponds to the given integer value `x`. */
  def fromInt(x: Int): T = n.fromInt(x)

  def toInt(x: T): Int = n.toInt(x)
  def toLong(x: T): Long = n.toLong(x)

  /** A value of type T which corresponds to integer 0. */
  lazy val zero: T = fromInt(0)

  /** A value of type T which corresponds to integer 1. */
  lazy val one: T = fromInt(1)
}

object ExactNumeric {
  /** ExactNumeric instances are the same as ExactIntegral.
    * This values are implicitly used wherever ExactNumeric is needed (see usages).
    */
  implicit def IntIsExactNumeric: ExactNumeric[Int] = IntIsExactIntegral
  implicit def LongIsExactNumeric: ExactNumeric[Long] = LongIsExactIntegral
}
