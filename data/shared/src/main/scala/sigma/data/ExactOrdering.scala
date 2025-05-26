package sigma.data

import scala.math.Numeric.{ByteIsIntegral, IntIsIntegral, LongIsIntegral, ShortIsIntegral}

/** Ordering operations to be used with other Exact traits.
  * All methods are implemented by delegating to the corresponding Ordering instance from
  * standard Scala library.
  * This trait is used in core IR to avoid implicitly using standard scala implementations.
  */
trait ExactOrdering[T] extends Ordering[T] {
  val n: Ordering[T]
  override def compare(x: T, y: T): Int = n.compare(x, y)
}

class ExactOrderingImpl[T](val n: Ordering[T]) extends ExactOrdering[T]

/** ExactOrdering instances for all types. */
object ExactOrdering {

  implicit object ByteIsExactOrdering extends ExactOrderingImpl[Byte](ByteIsIntegral)

  implicit object ShortIsExactOrdering extends ExactOrderingImpl[Short](ShortIsIntegral)

  implicit object IntIsExactOrdering extends ExactOrderingImpl[Int](IntIsIntegral)

  implicit object LongIsExactOrdering extends ExactOrderingImpl[Long](LongIsIntegral)
}
