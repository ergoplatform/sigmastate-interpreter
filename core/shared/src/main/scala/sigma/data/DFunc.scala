package sigma.data

/** Function interface which support specialization and thus unboxed invocations. */
abstract class DFunc[@specialized(Int) A, B] {
  def apply(x: A): B
}
