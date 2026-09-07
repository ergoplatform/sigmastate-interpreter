package sigma.compat

/** A distinct tag whose values retain the operations of their underlying type.
  * The factories tag a single value, without changing its representation.
  */
trait TaggedType[T] {
  // Preserve nullable reference tags without admitting null for value-backed tags.
  opaque type Type >: Null & T <: T = T

  @inline def apply(value: T): Type = value

  @inline def @@(value: T): Type = value

  @inline def @@@(value: T): Type = value

  @inline def raw(value: Type): T = value

  @inline def untag(value: Type): T = value

  @inline def unapply(value: Type): Option[T] = Some(value)
}
