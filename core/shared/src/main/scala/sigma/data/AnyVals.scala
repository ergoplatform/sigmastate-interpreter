package sigma.data

import java.util.HashMap

/** Allocation free alternative to scala.Option with similar interface.
  * Using this in recognizers allows:
  * 1) to avoid allocation of Some(x)
  * 2) reading random memory location (where Some is stored) to access x
  **/
class Nullable[+T](val x: T) extends AnyVal {
  @inline final def isEmpty = x == null
  @inline final def get: T = x
  @inline final def isDefined = x != null
  @inline final def getOrElse[B >: T](default: =>B): B = if (x != null) x else default
  @inline final def toList: List[T] = if (x == null) Nil else x :: Nil
  @inline final def toOption: Option[T] = if (x == null) None else Some(x)
  override def toString = if (x == null) "None" else s"Nullable($x)"
}
object Nullable {
  final val None: Nullable[Null] = new Nullable(null)
  final def apply[T](x: T): Nullable[T] = new Nullable(x)
  final def unapply[T](opt: Nullable[T]): Nullable[T] = opt
}

/** Allocation free alternative to scala.collection.mutable.Map with similar interface.
  * This simplifies optimization of performance critical code. */
class AVHashMap[K,V](val hashMap: HashMap[K,V]) extends AnyVal {
  @inline final def isEmpty: Boolean = hashMap.isEmpty
  @inline final def get(key: K): Nullable[V] = Nullable(hashMap.get(key))
  @inline final def apply(key: K): V = hashMap.get(key)
  @inline final def containsKey(key: K): Boolean = hashMap.containsKey(key)
  @inline final def put(key: K, value: V): V = hashMap.put(key, value)
  @inline final def clear(): Unit = {
    hashMap.clear()
  }
  @inline final def keySet: java.util.Set[K] = hashMap.keySet()
}
object AVHashMap {
  /** Helper method to create a new map with the given capacity. */
  def apply[K,V](initialCapacity: Int) = new AVHashMap[K,V](new HashMap[K,V](initialCapacity))

  /** Helper method to create a new map from the sequence of K, V pairs. */
  def fromSeq[K,V](items: Seq[(K, V)]): AVHashMap[K,V] = {
    val map = new AVHashMap[K,V](new HashMap[K,V](items.length))
    items.foreach { case (k, v) =>
      map.put(k, v)
    }
    map
  }
}

