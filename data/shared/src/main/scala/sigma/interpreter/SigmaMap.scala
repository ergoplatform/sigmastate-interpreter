package sigma.interpreter

import debox.cfor
import sigma.{AnyValue, ContextVarsMap}
import sigma.ast.SType.AnyOps
import sigma.ast.{EvaluatedValue, SType}
import sigma.eval.Extensions.toAnyValue
import scala.collection.mutable.ArrayBuffer
import scala.collection.Iterator
import scala.util.hashing.MurmurHash3

/**
  * Map data structure with traversal ordering corresponding to the one used in default
  * scala.collection.immutable.Map implementation as of Scala 2.12.x SDK.
  * Made in order to make order of ContextExtension variables independent of possible Scala SDK
  * changes and to provide a simple data structure for translating into other programming languages.
  *
  * Traversal ordering contract (mirroring Scala 2.12 immutable.Map):
  *  - maps with no more than 4 entries are traversed in the order of insertion (i.e. the order
  *    in which entries are given to the constructors/factory methods);
  *  - maps with more than 4 entries are traversed in the hash-trie order of the corresponding
  *    Scala 2.12 immutable.HashMap, which is a function of the set of keys only (see [[SigmaMap.indices]]);
  *    consequently, for such maps the traversal order is independent of the insertion order.
  */
abstract class SigmaMap extends ContextVarsMap {

  def maxKey: Byte

  def size: Int

  def isEmpty: Boolean = size == 0

  def contains(key: Byte): Boolean

  def getNullable(key: Byte): AnyValue

  def get(key: Byte): Option[EvaluatedValue[_ <: SType]]

  /** Same as [[get]] */
  def apply(key: Byte): Option[EvaluatedValue[_ <: SType]] = get(key)

  def iterator: Iterator[(Byte, EvaluatedValue[_ <: SType])]

  override def anyIterator: Iterator[(Byte, AnyValue)] = {
    iterator.map { case (k, v) => k -> SigmaMap.evalToAny(v) }
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case that: SigmaMap =>
        that.size == this.size &&
          that.iterator.toMap == this.iterator.toMap
      case _ => false
    }
  }

  /** Order-independent hash consistent with [[equals]] and equal to the hash code of an
    * equivalent scala.collection.Map (same accumulation scheme as in the Scala SDK). */
  override def hashCode(): Int = {
    var acc = 0
    var xor = 0
    var mul = 1
    iterator.foreach { case (k, v) =>
      val h = SigmaMap.entryHash(k, v)
      acc += h
      xor ^= h
      if (h != 0) mul *= h
    }
    var h = MurmurHash3.mapSeed
    h = MurmurHash3.mix(h, acc)
    h = MurmurHash3.mix(h, xor)
    h = MurmurHash3.mixLast(h, mul)
    MurmurHash3.finalizeHash(h, size)
  }
}

object EmptySigmaMap extends SigmaMap {
  override val maxKey: Byte = -1
  override val size = 0

  override def iterator: Iterator[(Byte, EvaluatedValue[_ <: SType])] = Iterator.empty

  override def contains(key: Byte): Boolean = false

  override def getNullable(key: Byte): AnyValue = null

  override def get(key: Byte): Option[EvaluatedValue[_ <: SType]] = None
}

class SigmaMap1(key1: Byte, value1: EvaluatedValue[_ <: SType]) extends SigmaMap {
  override val maxKey = key1
  override val size = 1

  override def iterator: Iterator[(Byte, EvaluatedValue[_ <: SType])] = Iterator.single((key1, value1))

  override def contains(key: Byte): Boolean = {
    key == key1
  }

  override def getNullable(key: Byte): AnyValue = {
    if (key == key1) {
      SigmaMap.evalToAny(value1)
    } else {
      null
    }
  }

  override def get(key: Byte): Option[EvaluatedValue[_ <: SType]] = {
    if (key == key1) Some(value1)
    else None
  }
}

class SigmaMap2(key1: Byte, value1: EvaluatedValue[_ <: SType],
                key2: Byte, value2: EvaluatedValue[_ <: SType]) extends SigmaMap {

  override val maxKey: Byte = {
    if (key1 >= key2) key1
    else key2
  }

  override val size = 2

  override def iterator: Iterator[(Byte, EvaluatedValue[_ <: SType])] = {
    new Iterator[(Byte, EvaluatedValue[_ <: SType])] {
      private[this] var i = 0

      override def hasNext: Boolean = i < 2

      override def next(): (Byte, EvaluatedValue[_ <: SType]) = {
        if (i >= 2) Iterator.empty.next()
        val result = if (i == 0) (key1, value1) else (key2, value2)
        i += 1
        result
      }
    }
  }

  override def contains(key: Byte): Boolean = {
    key == key1 || key == key2
  }

  override def getNullable(key: Byte): AnyValue = {
    if (key == key1) SigmaMap.evalToAny(value1)
    else if (key == key2) SigmaMap.evalToAny(value2)
    else null
  }

  override def get(key: Byte): Option[EvaluatedValue[_ <: SType]] = {
    if (key == key1) Some(value1)
    else if (key == key2) Some(value2)
    else None
  }
}

class SigmaMap3(key1: Byte, value1: EvaluatedValue[_ <: SType],
                key2: Byte, value2: EvaluatedValue[_ <: SType],
                key3: Byte, value3: EvaluatedValue[_ <: SType]) extends SigmaMap {
  override val maxKey: Byte = Math.max(Math.max(key1, key2), key3).toByte
  override val size = 3

  override def contains(key: Byte): Boolean = {
    key == key1 || key == key2 || key == key3
  }

  override def getNullable(key: Byte): AnyValue = {
    if (key == key1) SigmaMap.evalToAny(value1)
    else if (key == key2) SigmaMap.evalToAny(value2)
    else if (key == key3) SigmaMap.evalToAny(value3)
    else null
  }

  override def get(key: Byte): Option[EvaluatedValue[_ <: SType]] = {
    if (key == key1) Some(value1)
    else if (key == key2) Some(value2)
    else if (key == key3) Some(value3)
    else None
  }

  override def iterator: Iterator[(Byte, EvaluatedValue[_ <: SType])] = {
    new Iterator[(Byte, EvaluatedValue[_ <: SType])] {
      private[this] var i = 0

      override def hasNext: Boolean = i < 3

      override def next(): (Byte, EvaluatedValue[_ <: SType]) = {
        if (i >= 3) Iterator.empty.next()
        val result = i match {
          case 0 => (key1, value1)
          case 1 => (key2, value2)
          case _ => (key3, value3)
        }
        i += 1
        result
      }
    }
  }
}

class SigmaMap4(key1: Byte, value1: EvaluatedValue[_ <: SType],
                key2: Byte, value2: EvaluatedValue[_ <: SType],
                key3: Byte, value3: EvaluatedValue[_ <: SType],
                key4: Byte, value4: EvaluatedValue[_ <: SType]) extends SigmaMap {
  override val maxKey: Byte = Math.max(Math.max(key1, key2), Math.max(key3, key4)).toByte
  override val size = 4

  override def contains(key: Byte): Boolean = {
    key == key1 || key == key2 || key == key3 || key == key4
  }

  override def getNullable(key: Byte): AnyValue = {
    if (key == key1) SigmaMap.evalToAny(value1)
    else if (key == key2) SigmaMap.evalToAny(value2)
    else if (key == key3) SigmaMap.evalToAny(value3)
    else if (key == key4) SigmaMap.evalToAny(value4)
    else null
  }

  override def get(key: Byte): Option[EvaluatedValue[_ <: SType]] = {
    if (key == key1) Some(value1)
    else if (key == key2) Some(value2)
    else if (key == key3) Some(value3)
    else if (key == key4) Some(value4)
    else None
  }

  override def iterator: Iterator[(Byte, EvaluatedValue[_ <: SType])] = {
    new Iterator[(Byte, EvaluatedValue[_ <: SType])] {
      private[this] var i = 0

      override def hasNext: Boolean = i < 4

      override def next(): (Byte, EvaluatedValue[_ <: SType]) = {
        if (i >= 4) Iterator.empty.next()
        val result = i match {
          case 0 => (key1, value1)
          case 1 => (key2, value2)
          case 2 => (key3, value3)
          case _ => (key4, value4)
        }
        i += 1
        result
      }
    }
  }
}

/**
  * Representation of maps with more than 4 entries.
  *
  * @param sparseValues values indexed by key (the array length is maxKey + 1, entries for
  *                     missing keys are null)
  * @param maxKey       the maximum key present (must be non-negative)
  * @param size         the number of entries (non-null values in sparseValues)
  */
class SigmaMapMulti(private val sparseValues: Array[EvaluatedValue[_ <: SType]],
                    val maxKey: Byte,
                    val size: Int) extends SigmaMap {

  private def inRange(key: Byte): Boolean = key >= 0 && key <= maxKey

  override def contains(key: Byte): Boolean = inRange(key) && sparseValues(key) != null

  override def getNullable(key: Byte): AnyValue = {
    if (inRange(key)) {
      val v = sparseValues(key)
      if (v != null) SigmaMap.evalToAny(v) else null
    } else {
      null
    }
  }

  override def get(key: Byte): Option[EvaluatedValue[_ <: SType]] = {
    if (inRange(key)) Option(sparseValues(key))
    else None
  }

  override def iterator: Iterator[(Byte, EvaluatedValue[_ <: SType])] = {
    val s = size

    new Iterator[(Byte, EvaluatedValue[_ <: SType])] {
      var iteratedOver = 0

      var indexPos = 0

      override def hasNext: Boolean = iteratedOver < s

      override def next(): (Byte, EvaluatedValue[_ <: SType]) = {
        if (iteratedOver >= s) {
          throw new NoSuchElementException("next on empty iterator")
        } else {
          var res: EvaluatedValue[_ <: SType] = null
          var key: Byte = 0
          do {
            key = SigmaMap.indices(indexPos)
            if (key <= maxKey) {
              res = sparseValues(key)
            }
            indexPos += 1
          } while (res == null)
          iteratedOver += 1
          key -> res
        }
      }
    }
  }
}

object SigmaMap {

  def evalToAny(value: EvaluatedValue[_ <: SType]): AnyValue = {
    val tVal = sigma.Evaluation.stypeToRType[SType](value.tpe)
    toAnyValue(value.value.asWrappedType)(tVal).asInstanceOf[AnyValue]
  }

  /** Entry hash mirroring scala.util.hashing.MurmurHash3.product2Hash(k, v), which is used
    * by the hashCode implementation of scala.collection.Map in the Scala SDK. */
  private def entryHash(k: Byte, v: EvaluatedValue[_ <: SType]): Int = {
    var h = MurmurHash3.mix(MurmurHash3.productSeed, k.##)
    h = MurmurHash3.mix(h, v.##)
    MurmurHash3.finalizeHash(h, 2)
  }

  /** Creates a SigmaMap with the given bindings.
    * The traversal order of the resulting map (for maps with no more than 4 entries) follows
    * the iteration order of the argument map, which corresponds to the insertion order for
    * the default scala.collection.Map implementations (Scala 2.12 as well as 2.13 SDKs).
    */
  def apply(values: scala.collection.Map[Byte, EvaluatedValue[_ <: SType]]): SigmaMap = {
    if (values.isEmpty) {
      EmptySigmaMap
    } else {
      val ks = new Array[Byte](values.size)
      val vs = new Array[EvaluatedValue[_ <: SType]](values.size)
      var i = 0
      values.foreach { case (k, v) =>
        ks(i) = k
        vs(i) = v
        i += 1
      }
      SigmaMap(ks, vs)
    }
  }

  /** Creates a SigmaMap from parallel arrays of keys and values.
    * Entries with duplicate keys are reduced to a single entry occupying the position of the
    * first occurrence and carrying the value of the last occurrence (which is consistent with
    * building a scala.collection.Map from the same sequence of bindings).
    *
    * @throws IllegalArgumentException if arrays have different lengths or some key is negative
    */
  def apply(keys: Array[Byte], values: Array[EvaluatedValue[_ <: SType]]): SigmaMap = {
    if (keys.length != values.length) {
      throw new IllegalArgumentException(
        s"Arrays of keys (length ${keys.length}) and values (length ${values.length}) should have equal lengths")
    }
    // resolve duplicates: first-insertion position, last value wins (as in scala Map.updated)
    val positions = new Array[Int](128)
    var j = 0
    while (j < 128) { positions(j) = -1; j += 1 }
    val ks = new ArrayBuffer[Byte](keys.length)
    val vs = new ArrayBuffer[EvaluatedValue[_ <: SType]](keys.length)
    cfor(0)(_ < keys.length, _ + 1) { i =>
      val k = keys(i)
      if (k < 0) {
        throw new IllegalArgumentException(s"Negative key $k in context variables map")
      }
      val pos = positions(k)
      if (pos < 0) {
        positions(k) = ks.length
        ks += k
        vs += values(i)
      } else {
        vs(pos) = values(i)
      }
    }
    val n = ks.length
    if (n == 0) {
      EmptySigmaMap
    } else if (n == 1) {
      new SigmaMap1(ks(0), vs(0))
    } else if (n == 2) {
      new SigmaMap2(ks(0), vs(0), ks(1), vs(1))
    } else if (n == 3) {
      new SigmaMap3(ks(0), vs(0), ks(1), vs(1), ks(2), vs(2))
    } else if (n == 4) {
      new SigmaMap4(ks(0), vs(0), ks(1), vs(1), ks(2), vs(2), ks(3), vs(3))
    } else {
      var mk: Byte = 0
      cfor(0)(_ < n, _ + 1) { i =>
        if (ks(i) > mk) mk = ks(i)
      }
      val res = new Array[EvaluatedValue[_ <: SType]](mk + 1)
      cfor(0)(_ < n, _ + 1) { i =>
        res(ks(i)) = vs(i)
      }
      new SigmaMapMulti(res, mk, n)
    }
  }

  /** Traversal order of the hash-trie of the Scala 2.12 immutable.HashMap for the set of keys
    * {0, ..., 127}: the i-th element of this array is traversed i-th by the Scala 2.12 SDK Map
    * whenever the map contains more than 4 entries. Equivalently, the traversal order of ANY
    * subset of keys with more than 4 elements is this array filtered by the subset. */
  val indices: Array[Byte] = Array[Byte](69, 101, 0, 88, 115, 5, 120, 10, 56, 42, 24, 37, 25, 52, 14, 110, 125, 20, 46, 93, 57, 78, 29, 106, 121, 84, 61, 89, 116, 1, 74, 6, 60, 117, 85, 102, 28, 38, 70, 21, 33, 92, 65, 97, 9, 53, 109, 124, 77, 96, 13, 41, 73, 105, 2, 32, 34, 45, 64, 17, 22, 44, 59, 118, 27, 71, 12, 54, 49, 86, 113, 81, 76, 7, 39, 98, 103, 91, 66, 108, 3, 80, 35, 112, 123, 48, 63, 18, 95, 50, 67, 16, 127, 31, 11, 72, 43, 99, 87, 104, 40, 26, 55, 114, 23, 8, 75, 119, 58, 82, 36, 30, 51, 19, 107, 4, 126, 79, 94, 47, 15, 68, 62, 90, 111, 122, 83, 100)

  def empty: SigmaMap = EmptySigmaMap
}
