package sigma.ast

/** Version-specific alias for the wrapped Scala type of an `SType` subtype.
  *
  * Scala 3 dropped support for type projection `T#A` on an abstract type parameter `T`,
  * so `T#WrappedType` is illegal here. We widen to the (legal) projection on the concrete
  * `SType` trait. Within `core` the wrapped values are handled through casts, so the loss
  * of static precision is recovered at runtime; see the scala-2 variant for the exact type.
  */
object Wrapped {
  type Of[T <: SType] = SType#WrappedType
}
