package sigma.ast

/** Version-specific alias for the wrapped Scala type of an `SType` subtype.
  *
  * On Scala 2.x this is the exact type projection `T#WrappedType`. Keeping it exact is
  * important: the `data`/`sc` modules project `SCollection[T]#WrappedType`,
  * `RType[V#WrappedType]`, etc., and rely on these being precisely `T#WrappedType`
  * (consensus-relevant typing in invariant positions like `Array`, `Coll`, `RType`).
  *
  * The Scala 3 variant keeps the same projection using migration mode.
  */
object Wrapped {
  type Of[T <: SType] = T#WrappedType
}
