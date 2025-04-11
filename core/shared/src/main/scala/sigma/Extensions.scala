package sigma

import debox.cfor
import scorex.util.encode.Base16
import scorex.util.{ModifierId, bytesToId}
import sigma.data.RType

/** Declaration of extension methods introduced in `sigma-core` module.
  * See `implicit class ...` wrappers below.
  */
object Extensions {

  /** Extension methods for `Array[T]` where implicit descriptor `RType[T]` is also
    * required.
    */
  implicit class ArrayOps[T: RType](arr: Array[T]) {
    /** Wraps array into Coll instance. The source array in not cloned. */
    @inline def toColl: Coll[T] = Colls.fromArray(arr)
  }

  /** Extension methods for `Coll[T]`. */
  implicit class CollOps[T](val source: Coll[T]) extends AnyVal {
    /** Applies a function `f` to each element of the `source` collection. */
    def foreach(f: T => Unit) = {
      val limit = source.length
      cfor(0)(_ < limit, _ + 1) { i =>
        f(source(i))
      }
    }
  }

  /** Extension methods for `Coll[Byte]` not available for generic `Coll[T]`. */
  implicit class CollBytesOps(val source: Coll[Byte]) extends AnyVal {
    /** Returns a representation of this collection of bytes as a modifier id. */
    def toModifierId: ModifierId = {
      val bytes = source.toArray
      bytesToId(bytes)
    }

    /** Encodes this collection of bytes as a hex string. */
    def toHex: String = Base16.encode(source.toArray)
  }

  /** Extension methods for `Coll[(A,B)]`.
    * Has priority over the extensions in [[CollOps]].
    */
  implicit class PairCollOps[A,B](val source: Coll[(A,B)]) extends AnyVal {
    /** Applies a function `f` to each pair of elements in the `source` collection. */
    def foreach(f: (A, B) => Unit) = {
      val b = source.builder
      val (as, bs) = b.unzip(source)
      val limit = source.length
      cfor(0)(_ < limit, _ + 1) { i =>
        f(as(i), bs(i))
      }
    }
  }

}
