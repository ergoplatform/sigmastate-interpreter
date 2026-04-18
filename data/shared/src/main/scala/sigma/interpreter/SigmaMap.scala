package sigma.interpreter

import scala.collection.Iterator

/**
  * Consensus-critical deterministic map.
  *
  * IMPORTANT: do not replace this structure with scala.collection.Map.
  *
  * Scala `Map` implementations do not guarantee iteration order. Relying on their
  * traversal order (e.g. in serializers) is consensus-breaking because the produced
  * bytes can differ across Scala/JVM versions and environments.
  *
  * `SigmaMap` is deterministic by construction:
  * - Explicitly stores entries in insertion order.
  * - Explicitly defines iteration over that stored order.
  * - Uses a fixed-size index (256) for O(1) key lookup over full Byte key space.
  */
final class SigmaMap[+V] private (
  private val entries: Vector[(Byte, V)],
  private val index: Array[Int]
) {

  /** Number of key-value pairs. */
  def size: Int = entries.size

  /** Returns true if the key is present. */
  def contains(key: Byte): Boolean = index(byteToIndex(key)) >= 0

  /** Returns value for key if present. */
  def get[V1 >: V](key: Byte): Option[V1] = {
    val i = index(byteToIndex(key))
    if (i >= 0) Some(entries(i)._2) else None
  }

  /** Returns value for key or throws if absent. */
  def apply[V1 >: V](key: Byte): V1 =
    get(key).getOrElse(throw new NoSuchElementException(s"SigmaMap key not found: $key"))

  /**
    * Returns a new SigmaMap with the given binding applied.
    *
    * Semantics are deterministic:
    * - If `key` is new: it is appended to the end (insertion order preserved).
    * - If `key` exists: its value is replaced in-place (position preserved).
    */
  def updated[V1 >: V](key: Byte, value: V1): SigmaMap[V1] = {
    val idx = byteToIndex(key)
    val pos = index(idx)
    if (pos >= 0) {
      val newEntries = entries.updated(pos, (key, value))
      new SigmaMap[V1](newEntries, index)
    } else {
      val newIndex = index.clone()
      newIndex(idx) = entries.length
      new SigmaMap[V1](entries :+ (key -> value), newIndex)
    }
  }

  /**
    * Deterministically applies bindings in iteration order of `xs`.
    *
    * NOTE: do not pass an unordered `scala.collection.Map` here if you need
    * insertion order semantics; use an ordered sequence.
    */
  def ++[V1 >: V](xs: IterableOnce[(Byte, V1)]): SigmaMap[V1] = {
    val it = xs.iterator
    var res: SigmaMap[V1] = this
    while (it.hasNext) {
      val (k, v) = it.next()
      res = res.updated(k, v)
    }
    res
  }

  /**
    * Deterministically applies bindings from an unordered Map.
    *
    * This method MUST NOT rely on the iteration order of `m`.
    * Keys are applied in ascending unsigned-byte order (0..255).
    */
  def ++[V1 >: V](m: scala.collection.Map[Byte, V1]): SigmaMap[V1] = {
    val sorted = m.toArray.sortBy { case (k, _) => k.toInt & 0xFF }
    this.++(sorted)
  }

  /** Iterator over (key,value) pairs in insertion order. */
  def iterator: Iterator[(Byte, V)] = entries.iterator

  /** Applies `f` to pairs in insertion order. */
  def foreach[U](f: ((Byte, V)) => U): Unit = {
    var i = 0
    while (i < entries.length) {
      f(entries(i))
      i += 1
    }
  }

  /** Key-value pairs in insertion order. */
  def toSeq: Seq[(Byte, V)] = entries

  /**
    * Converts to Scala immutable Map.
    *
    * WARNING: iteration order of the resulting Map is not guaranteed.
    * This method exists only for API interop.
    */
  def toMap[V1 >: V]: scala.collection.immutable.Map[Byte, V1] =
    scala.collection.immutable.Map(entries: _*)

  private def byteToIndex(b: Byte): Int = b.toInt + 128

  override def equals(obj: Any): Boolean = obj match {
    case that: SigmaMap[_] =>
      this.entries == that.entries
    case _ => false
  }

  override def hashCode(): Int = entries.hashCode()
}

object SigmaMap {
  def empty[V]: SigmaMap[V] = new SigmaMap[V](Vector.empty, Array.fill(256)(-1))

  /**
    * Builds a SigmaMap by appending bindings in the given sequence order.
    *
    * Throws if the input contains duplicated keys.
    */
  def fromSeq[V](xs: Seq[(Byte, V)]): SigmaMap[V] = {
    val idx = Array.fill(256)(-1)
    val b = Vector.newBuilder[(Byte, V)]
    var pos = 0
    while (pos < xs.length) {
      val (k, v) = xs(pos)
      val p = k.toInt + 128
      if (idx(p) >= 0)
        throw new IllegalArgumentException(s"Duplicate SigmaMap key: $k")
      idx(p) = pos
      b += (k -> v)
      pos += 1
    }
    new SigmaMap[V](b.result(), idx)
  }
}
