package sigma.ast

/** Retains the wrapped-type relationship while compiling in Scala 3 migration mode. */
object Wrapped {
  type Of[T <: SType] = T#WrappedType
}
