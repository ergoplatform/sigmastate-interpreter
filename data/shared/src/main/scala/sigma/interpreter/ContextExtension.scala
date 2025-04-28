package sigma.interpreter

import debox.cfor
import sigma.ast.{EvaluatedValue, SType}
import sigma.serialization.{SigmaByteReader, SigmaByteWriter, SigmaSerializer}

/**
  * User-defined variables to be put into context.
  * Each variable is identified by `id: Byte` and can be accessed from a script
  * using `getVar[T](id)` operation.
  * The value of the variable is represented by [[sigma.ast.Constant]] instance,
  * which contains both data value and [[SType]] descriptor. The descriptor is checked
  * against the type `T` expected in the script operation. If the types don't match,
  * exception is thrown and the box spending (protected by the script) fails.
  *
  * @param values internal container of the key-value pairs
  */
case class ContextExtension(values: SigmaMap)

object ContextExtension {
  /** Immutable instance of empty ContextExtension, which can be shared to avoid
    * allocations. */
  val empty: ContextExtension = ContextExtension(SigmaMap.empty)

  /** Type of context variable binding. */
  type VarBinding = (Byte, EvaluatedValue[_ <: SType])

  object serializer extends SigmaSerializer[ContextExtension, ContextExtension] {
    override def serialize(obj: ContextExtension, w: SigmaByteWriter): Unit = {
      val size = obj.values.size
      if (size > Byte.MaxValue)
        error(s"Number of ContextExtension values $size exceeds ${Byte.MaxValue}.")
      w.putUByte(size)
      obj.values.iterator.foreach { case (id, v) => w.put(id).putValue(v) }
    }

    override def parse(r: SigmaByteReader): ContextExtension = {
      val extSize = r.getByte()
      if (extSize < 0) {
        error(s"Negative amount of context extension values: $extSize")
      }
      if (extSize > Byte.MaxValue + 1) {
        error(s"Too many context extension values: $extSize")
      }
      if (extSize == 0) {
        ContextExtension.empty
      } else {
        val size = extSize
        val keys = new Array[Byte](size)
        val values = new Array[EvaluatedValue[_ <: SType]](size)
        var maxKey: Byte = -1
        cfor(0)(_ < size, _ + 1) { i =>
          val key = r.getByte()
          if (key < 0) {
            error(s"Negative key in context extension: $key")
          }
          if (key > maxKey) {
            maxKey = key
          }
          keys(i) = key
          values(i) = r.getValue().asInstanceOf[EvaluatedValue[_ <: SType]]
        }
        ContextExtension(SigmaMap(keys, values, maxKey))
      }
    }
  }
}