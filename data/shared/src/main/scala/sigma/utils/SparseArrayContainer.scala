package sigma.utils

import sigma.ast.{SType, Value}
import sigma.serialization.ValueSerializer

import scala.reflect.ClassTag

/**
  * Store values in a sparse array (null if no value for a code)
  * @param values - list of pairs (code, value)
  */
class SparseArrayContainer[T: ClassTag](values: Seq[(Byte, T)]) {

  private val sparseArray: Array[T] = build(values)

  private def build(sers: Seq[(Byte, T)]): Array[T] = {
    val set = sers.map(_._1).toSet
    require(sers.size == set.size, {
      val dupGroups = sers.groupBy { case (b, _) => b }.filter { case (_, g) => g.size > 1 }.toList
      s"expected distinct codes, got duplicated: $dupGroups"
    })
    val array = Array.fill[T](256)(null.asInstanceOf[T]) // one item for each code
    sers.foreach { case (code, value) =>
      array(codeToIndex(code)) = value
    }
    array
  }

  @inline
  private def codeToIndex(code: Byte): Int = code + 128  // -128..127 -> 0..255

  /** @return true if value for the given code is defined
    * @param code of a value
    */
  @inline def contains(code: Byte): Boolean = sparseArray(codeToIndex(code)) != null

  /**
    * Returns value for the given code
    * @param code of a value
    * @return value or null if no value for a given code
    */
  @inline
  def apply(code: Byte): T = sparseArray(codeToIndex(code))

  /**
    * Returns Some(value) for the given code if it is not `null`.
    * @param code of a value
    * @return Some(value) or None if no value for a given code
    */
  @inline
  def get(code: Byte): Option[T] = Option(this(code))

  /** Add new values to this container. */
  def add(code: Byte, value: T) = {
    val index = codeToIndex(code)
    require(sparseArray(index) == null, s"Value with index $index already defined.")
    sparseArray(index) = value
  }

  def remove(code: Byte) = {
    val index = codeToIndex(code)
    require(sparseArray(index) != null,
      s"Value with index $index not-defined, probably incorrect attempt to remove it.")
    sparseArray(index) = null.asInstanceOf[T]
  }
}

object SparseArrayContainer {
  /**  Build a container for the given serializers. */
  def buildForSerializers(sers: Seq[ValueSerializer[_ <: Value[SType]]]): SparseArrayContainer[ValueSerializer[_ <: Value[SType]]] = {
    new SparseArrayContainer(sers.map(s => (s.opCode, s)))
  }
}
