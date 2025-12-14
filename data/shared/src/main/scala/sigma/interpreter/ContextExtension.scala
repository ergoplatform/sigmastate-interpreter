package sigma.interpreter

import org.ergoplatform.validation.ValidationRules.CheckV6Type
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
case class ContextExtension(values: SigmaMap[EvaluatedValue[_ <: SType]]) {

  /**
    * @return this extension with `bindings` added
    */
  def add(bindings: VarBinding*): ContextExtension = {
    ContextExtension(values ++ bindings)
  }

  /**
    * @param varId - index of context variable
    * @return context variable with provided index or None if it is not there
    */
  def get(varId: Byte): Option[EvaluatedValue[_ <: SType]] = values.get(varId)

  /** Returns true if this extension defines value for the given id. */
  def contains(varId: Byte): Boolean = values.contains(varId)

  /** Unsafe accessor used in some hot paths. Throws if id is missing. */
  def apply(varId: Byte): EvaluatedValue[_ <: SType] = values(varId)

  /**
    * Interop view.
    *
    * WARNING: iteration order of the returned Map is not guaranteed.
    * Do not use it in consensus-critical code paths such as serialization.
    */
  def toMap: scala.collection.immutable.Map[Byte, EvaluatedValue[_ <: SType]] = values.toMap
}

object ContextExtension {
  /**
    * Maximum number of context extension entries which can be serialized.
    *
    * The size is encoded as an unsigned byte.
    */
  private val MaxSerializedEntries: Int = 0xFF

  /** Immutable instance of empty ContextExtension, which can be shared to avoid
    * allocations. */
  val empty = ContextExtension(SigmaMap.empty)

  /** Type of context variable binding. */
  type VarBinding = (Byte, EvaluatedValue[_ <: SType])

  /**
    * Backward-compatible constructor.
    *
    * WARNING: iteration order of `Map` is not guaranteed.
    * For deterministic insertion order, use [[fromSeq]] or `ContextExtension(SigmaMap.fromSeq(...))`.
    */
  def apply(values: scala.collection.Map[Byte, EvaluatedValue[_ <: SType]]): ContextExtension =
    fromMap(values)

  /** Constructs ContextExtension from ordered bindings. */
  def apply(bindings: Seq[VarBinding]): ContextExtension =
    fromSeq(bindings)

  /**
    * Builds deterministic ContextExtension from insertion-ordered bindings.
    *
    * NOTE: Passing a scala.collection.Map here is unsafe unless its iteration order
    * is explicitly controlled by the caller. Prefer Seq/Array of bindings.
    */
  def fromSeq(bindings: Seq[VarBinding]): ContextExtension =
    ContextExtension(SigmaMap.fromSeq(bindings))

  /**
    * Compatibility constructor: builds deterministic ContextExtension from an unordered Map.
    *
    * Since `Map` iteration order is not guaranteed, we apply keys in ascending unsigned-byte
    * order (0..255). This gives deterministic but NOT insertion-ordered results.
    *
    * Consensus rule: insertion order is defined only for serializers and APIs which accept
    * ordered sequences.
    */
  def fromMap(bindings: scala.collection.Map[Byte, EvaluatedValue[_ <: SType]]): ContextExtension =
    ContextExtension(SigmaMap.empty ++ bindings)

  object serializer extends SigmaSerializer[ContextExtension, ContextExtension] {
    override def serialize(obj: ContextExtension, w: SigmaByteWriter): Unit = {
      val size = obj.values.size
      if (size > MaxSerializedEntries)
        error(s"Number of ContextExtension values $size exceeds $MaxSerializedEntries.")
      w.putUByte(size)
      // Consensus-critical: deterministic insertion-ordered traversal.
      obj.values.foreach { case (id, v) => w.put(id).putValue(v) }
    }

    override def parse(r: SigmaByteReader): ContextExtension = {
      val extSize = r.getUByte()
      if (extSize > MaxSerializedEntries)
        error(s"Number of ContextExtension values $extSize exceeds $MaxSerializedEntries.")
      val values = (0 until extSize)
        .map { _ =>
          val k = r.getByte()
          val v = r.getValue().asInstanceOf[EvaluatedValue[_ <: SType]]
          CheckV6Type(v)
          (k, v)
        }
      ContextExtension.fromSeq(values)
    }
  }
}