package sigma.util

import sigma.data.AVHashMap

/** Transforms a given function into memoized equivalent function.
  * Memoization is implemented by computing function f only once for each
  * argument value and storing computed result in a hash table, so that
  * it can be later retrieved on repeated invocations with the same argument.
  * The cache of computed results can be cleared by calling `reset`.
  */
class MemoizedFunc(f: AnyRef => AnyRef) {
  private var _table: AVHashMap[AnyRef, AnyRef] = AVHashMap(100)
  /** Apply the function to the given argument using memoized result if available. */
  def apply[T <: AnyRef](x: T): AnyRef = {
    var v = _table(x)
    if (v == null) {
      v = f(x)
      _table.put(x, v)
    }
    v
  }
  /** Clears the cache of memoized results. */
  def reset() = {
    _table = AVHashMap(100)
  }
}


