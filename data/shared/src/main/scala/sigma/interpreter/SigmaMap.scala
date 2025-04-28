package sigma.interpreter

import debox.cfor
import sigma.{AnyValue, Coll, Colls, ContextVarsMap}
import sigma.ast.SType.AnyOps
import sigma.ast.{EvaluatedValue, SType}
import sigma.eval.Extensions.toAnyValue
import sigma.interpreter.SigmaMap.evalToAny

import scala.collection.Iterator

/**
  * Map data structure with traversal ordering corresponding to used in default scala.collection.Map implementation
  * as of Scala 2.12.18. Made in order to make order of ContextExtension variables independent of possible Scala SDK
  * changes and to provide a simple data structure for translating into other programming languages.
  */
abstract class SigmaMap extends ContextVarsMap {

  def maxKey: Byte

  def size: Int

  def isEmpty: Boolean = size == 0

  def contains(key: Byte): Boolean

  def getNullable(key: Byte): AnyValue

  def get(key: Byte): Option[EvaluatedValue[_ <: SType]]

  def apply(key: Byte): Option[EvaluatedValue[_ <: SType]] = get(key)

  def iterator: Iterator[(Byte, EvaluatedValue[_ <: SType])]

  override def anyIterator: Iterator[(Byte, AnyValue)] = {
    iterator.map { case (k,v) =>
      val tVal = sigma.Evaluation.stypeToRType[SType](v.tpe)
      k -> toAnyValue(v.value.asWrappedType)(tVal).asInstanceOf[AnyValue]
    }

  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case that: SigmaMap =>
        that.size == this.size &&
          that.maxKey == this.maxKey &&
          that.iterator.toMap == this.iterator.toMap
      case _ => false
    }
  }

  // todo: define hashCode()
}

object EmptySigmaMap extends SigmaMap {
  override val maxKey = -1
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
      evalToAny(value1)
    } else {
      null
    }
  }

  override def get(key: Byte): Option[EvaluatedValue[_ <: SType]] = {
    if (key == key1) {
      Some(value1)
    } else {
      None
    }
  }
}

class SigmaMap2(key1: Byte, value1: EvaluatedValue[_ <: SType],
                key2: Byte, value2: EvaluatedValue[_ <: SType]) extends SigmaMap {

  override val maxKey = {
    if (key1 >= key2) key1
    else key2
  }

  override val size = 2

  override def iterator: Iterator[(Byte, EvaluatedValue[_ <: SType])] = {
    new Iterator[(Byte, EvaluatedValue[_ <: SType])] {
      private[this] var i = 0

      override def hasNext: Boolean = i < 2

      override def next(): (Byte, EvaluatedValue[_ <: SType]) = {
        val result = i match {
          case 0 => (key1, value1)
          case 1 => (key2, value2)
          case _ => Iterator.empty.next()
        }
        i += 1
        result
      }
    }
  }

  override def contains(key: Byte): Boolean = {
    key == key1 || key == key2
  }

  override def getNullable(key: Byte): AnyValue = {
    if (key == key1) evalToAny(value1)
    else if (key == key2) evalToAny(value2)
    else null
  }

  override def get(key: Byte): Option[EvaluatedValue[_ <: SType]] = {
    if (key == key1) Some(value1)
    else if (key == key2) Some(value2)
    else null
  }
}



class SigmaMap3(key1: Byte, value1: EvaluatedValue[_ <: SType],
                key2: Byte, value2: EvaluatedValue[_ <: SType],
                key3: Byte, value3: EvaluatedValue[_ <: SType]) extends SigmaMap {
  override val maxKey = Math.max(Math.max(key1, key2), key3).toByte
  override val size = 3

  override def contains(key: Byte): Boolean = {
    key == key1 || key == key2 || key == key3
  }

  override def getNullable(key: Byte): AnyValue = {
    if (key == key1) evalToAny(value1)
    else if (key == key2) evalToAny(value2)
    else if (key == key3) evalToAny(value3)
    else null
  }

  override def get(key: Byte): Option[EvaluatedValue[_ <: SType]] = {
    if (key == key1) Some(value1)
    else if (key == key2) Some(value2)
    else if (key == key3) Some(value3)
    else null
  }

  override def iterator: Iterator[(Byte, EvaluatedValue[_ <: SType])] = {
    new Iterator[(Byte, EvaluatedValue[_ <: SType])] {
      private[this] var i = 0

      override def hasNext: Boolean = i < 3

      override def next(): (Byte, EvaluatedValue[_ <: SType]) = {
        val result = i match {
          case 0 => (key1, value1)
          case 1 => (key2, value2)
          case 2 => (key3, value3)
          case _ => Iterator.empty.next()
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
  override val maxKey = Math.max(Math.max(key1, key2), Math.max(key3, key4)).toByte
  override val size = 4

  override def contains(key: Byte): Boolean = {
    key == key1 || key == key2 || key == key3 || key == key4
  }

  override def getNullable(key: Byte): AnyValue = {
    if (key == key1) evalToAny(value1)
    else if (key == key2) evalToAny(value2)
    else if (key == key3) evalToAny(value3)
    else if (key == key4) evalToAny(value4)
    else null
  }

  override def get(key: Byte): Option[EvaluatedValue[_ <: SType]] = {
    if (key == key1) Some(value1)
    else if (key == key2) Some(value2)
    else if (key == key3) Some(value3)
    else if (key == key4) Some(value4)
    else null
  }

  override def iterator: Iterator[(Byte, EvaluatedValue[_ <: SType])] = {
    new Iterator[(Byte, EvaluatedValue[_ <: SType])] {
      private[this] var i = 0

      override def hasNext: Boolean = i < 4

      override def next(): (Byte, EvaluatedValue[_ <: SType]) = {
        val result = i match {
          case 0 => (key1, value1)
          case 1 => (key2, value2)
          case 2 => (key3, value3)
          case 3 => (key4, value4)
          case _ => Iterator.empty.next()
        }
        i += 1
        result
      }
    }
  }
}

class SigmaMapMulti(private val sparseValues: Array[EvaluatedValue[_ <: SType]],
                    val maxKey: Byte,
                    val size: Int) extends SigmaMap {

  override def contains(key: Byte): Boolean = sparseValues(key) != null

  override def getNullable(key: Byte): AnyValue = {
    val v = sparseValues(key)
    val tVal = sigma.Evaluation.stypeToRType[SType](v.tpe)
    toAnyValue(v.value.asWrappedType)(tVal).asInstanceOf[AnyValue]
  }

  override def get(key: Byte): Option[EvaluatedValue[_ <: SType]] = {
    val res = sparseValues(key)
    Option(res)
  }

  override def iterator: Iterator[(Byte, EvaluatedValue[_ <: SType])] = {

    val s = size

    new Iterator[(Byte, EvaluatedValue[_ <: SType])] {
      var iteratedOver = 0

      var indexPos = 0

      override val knownSize = s

      override val size = s

      override def hasNext: Boolean = {
        iteratedOver < size
      }

      override def next(): (Byte, EvaluatedValue[_ <: SType]) = {
        if (iteratedOver > size) {
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

  def apply(values: scala.collection.Map[Byte, EvaluatedValue[_ <: SType]]): SigmaMap = {
    if (values.isEmpty) {
      EmptySigmaMap
    } else {
      var size = values.size
      val maxKey = values.keys.max
      val ks = new Array[Byte](size)
      val vs = new Array[EvaluatedValue[_ <: SType]](size)

      var i = 0;
      values.foreach { case (k, v) =>
        ks(i) = k
        vs(i) = v
        i = i + 1
      }
      SigmaMap(ks, vs, maxKey)
    }
  }

  // todo: cover with test
  def apply(keys: Array[Byte], values: Array[EvaluatedValue[_ <: SType]], maxKey: Byte): SigmaMap = {
    if(keys.isEmpty) {
      EmptySigmaMap
    } else if (keys.length == 1) {
      new SigmaMap1(keys(0), values(0))
    } else if (keys.length == 2) {
      new SigmaMap2(keys(0), values(0), keys(1), values(1))
    } else if (keys.length == 3) {
      new SigmaMap3(keys(0), values(0), keys(1), values(1), keys(2), values(2))
    } else if (keys.length == 4) {
      new SigmaMap4(keys(0), values(0), keys(1), values(1), keys(2), values(2), keys(3), values(3))
    } else {
      val res = new Array[EvaluatedValue[_ <: SType]](maxKey + 1)
      cfor(0)(_ < keys.length, _ + 1) { i =>
        val k = keys(i)
        val v = values(i)
        res(k) = v
      }
      new SigmaMapMulti(res, maxKey, keys.length)
    }
  }

  val indices: Array[Byte] = Array[Byte](69, 101, 0, 88, 115, 5, 120, 10, 56, 42, 24, 37, 25, 52, 14, 110, 125, 20, 46, 93, 57, 78, 29, 106, 121, 84, 61, 89, 116, 1, 74, 6, 60, 117, 85, 102, 28, 38, 70, 21, 33, 92, 65, 97, 9, 53, 109, 124, 77, 96, 13, 41, 73, 105, 2, 32, 34, 45, 64, 17, 22, 44, 59, 118, 27, 71, 12, 54, 49, 86, 113, 81, 76, 7, 39, 98, 103, 91, 66, 108, 3, 80, 35, 112, 123, 48, 63, 18, 95, 50, 67, 16, 127, 31, 11, 72, 43, 99, 87, 104, 40, 26, 55, 114, 23, 8, 75, 119, 58, 82, 36, 30, 51, 19, 107, 4, 126, 79, 94, 47, 15, 68, 62, 90, 111, 122, 83, 100)

  def empty: SigmaMap = EmptySigmaMap
}


