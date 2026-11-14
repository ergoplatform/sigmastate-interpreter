package sigma.ast

import sigma.Coll

/** Retains collection element types while compiling in Scala 3 migration mode. */
trait SCollectionWrappedType[T <: SType] {
  type WrappedType = Coll[T#WrappedType]
}
