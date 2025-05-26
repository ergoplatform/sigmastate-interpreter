package sigma.data

/** Interface implmented by wrappers to provide access to the underlying wrapped value. */
trait WrapperOf[T] {
  /** The data value wrapped by this wrapper. */
  def wrappedValue: T
}
