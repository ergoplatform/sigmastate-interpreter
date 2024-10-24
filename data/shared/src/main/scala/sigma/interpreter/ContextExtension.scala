package sigma.interpreter

import sigma.ast.{EvaluatedValue, SType}
import sigma.interpreter.ContextExtension.VarBinding
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
case class ContextExtension(values: scala.collection.Map[Byte, EvaluatedValue[_ <: SType]]) {
  def add(bindings: VarBinding*): ContextExtension = {
    ContextExtension(values ++ bindings)
  }

  /**
    * @param varId - index of context variable
    * @return context variable with provided index or None if it is not there
    */
  def get(varId: Byte): Option[EvaluatedValue[_ <: SType]] = values.get(varId)
}

object ContextExtension {
  /** Immutable instance of empty ContextExtension, which can be shared to avoid
    * allocations. */
  val empty = ContextExtension(Map())

  /** Type of context variable binding. */
  type VarBinding = (Byte, EvaluatedValue[_ <: SType])

  object serializer extends SigmaSerializer[ContextExtension, ContextExtension] {
    override def serialize(obj: ContextExtension, w: SigmaByteWriter): Unit = {
      val size = obj.values.size
      if (size > Byte.MaxValue)
        error(s"Number of ContextExtension values $size exceeds ${Byte.MaxValue}.")
      w.putUByte(size)
      obj.values.foreach { case (id, v) => w.put(id).putValue(v) }
    }

    override def parse(r: SigmaByteReader): ContextExtension = {
      val extSize = r.getByte()
      if (extSize < 0)
        error(s"Negative amount of context extension values: $extSize")
      val values = (0 until extSize)
          .map(_ => (r.getByte(), r.getValue().asInstanceOf[EvaluatedValue[_ <: SType]]))
      ContextExtension(values.toMap)
    }
  }
}