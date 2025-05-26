package sigma.data

/** Non-thread safe (but efficient on single thread) immutable lazy value.
  * The `block` is executed only once. */
class Lazy[A] private (block: => A) {
  @volatile private[this] var _isSet: Boolean = false
  private[this] var _value: A = _

  def value: A = {
    if (!_isSet) {
      _value = block
      _isSet = true
    }
    _value
  }
  
  @inline def isSet = _isSet
  
  override def toString = {
    if (!_isSet)
      "<lazy>"
    else
      value.toString
  }
}

object Lazy {
  def apply[A](block: => A): Lazy[A] = new Lazy(block)
}




