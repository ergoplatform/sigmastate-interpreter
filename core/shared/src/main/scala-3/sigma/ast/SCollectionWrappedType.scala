package sigma.ast

import sigma.Coll

/** Version-specific definition of `SCollection#WrappedType` mixed into [[SCollection]].
  *
  * Scala 3 dropped projection on an abstract type parameter (`T#WrappedType`), so this widens to
  * `Coll[SType#WrappedType]`. See the scala-2 variant for the exact `Coll[T#WrappedType]` form and
  * the rationale (avoiding the `Wrapped.Of` alias over-normalization for concrete element types).
  */
trait SCollectionWrappedType[T <: SType] {
  type WrappedType = Coll[SType#WrappedType]
}
