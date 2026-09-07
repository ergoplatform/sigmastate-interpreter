package sigma.ast

import sigma.Coll

/** Version-specific definition of `SCollection#WrappedType` mixed into [[SCollection]].
  *
  * On Scala 2 this is the exact `Coll[T#WrappedType]` projection. Keeping it as a direct
  * projection (rather than routing through the `Wrapped.Of` alias) is important: for a concrete
  * element type such as `STuple`'s `SAny.type`, `Coll[SAny.type#WrappedType]` stays a projection
  * that conforms to `Coll[SType#WrappedType]` by subtyping, whereas the alias `Wrapped.Of[SAny.type]`
  * over-normalizes to `Coll[Any]` and would force casts at use sites (e.g. DataJsonEncoder).
  * The Scala 3 variant keeps the same projection using migration mode.
  */
trait SCollectionWrappedType[T <: SType] {
  type WrappedType = Coll[T#WrappedType]
}
