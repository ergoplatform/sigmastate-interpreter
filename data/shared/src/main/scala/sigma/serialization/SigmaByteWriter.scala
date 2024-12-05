package sigma.serialization

import scorex.util.serialization.Writer
import sigma.ast.syntax._
import sigma.ast._
import sigma.serialization.CoreByteWriter.{ArgInfo, DataInfo, FormatDescriptor, SeqFmt}
import SigmaByteWriter._

/** Implementation of [[Writer]] provided by `sigma-data` module.
  *
  * @param w                         destination [[Writer]] to which all the call got delegated.
  * @param constantExtractionStore   optional store to segregate constants to while
  *                                  replacing them with placeholders.
  * @param addFixedCostCallbackOpt   optional callback to accumulate fixed costs.
  * @param addPerItemCostCallbackOpt optional callback to accumulate per-item costs.
  */
class SigmaByteWriter(
  override val w: Writer,
  val constantExtractionStore: Option[ConstantStore],
  val addFixedCostCallbackOpt: Option[FixedCostCallback],
  val addPerItemCostCallbackOpt: Option[PerItemCostCallback]
) extends CoreByteWriter(w) {
  import CoreByteWriter._
  import ValueSerializer._

  /** Adds the given cost to the callback if it is defined. */
  @inline private def addFixedCost(cost: OperationCostInfo[FixedCost]): Unit = {
    if (addFixedCostCallbackOpt.isDefined)
      addFixedCostCallbackOpt.get(cost)
  }

  /** Adds the given cost to the callback if it is defined. */
  @inline private def addPerItemCost(cost: OperationCostInfo[PerItemCost], nItems: Int): Unit = {
    if (addPerItemCostCallbackOpt.isDefined)
      addPerItemCostCallbackOpt.get(cost, nItems)
  }

  override def putChunk(chunk: w.CH): SigmaByteWriter.this.type = {
    val start = length()
    super.putChunk(chunk)
    addPerItemCost(PutChunkCost, length() - start)
    this
  }

  override def put(x: Byte): this.type = {
    addFixedCost(PutByteCost)
    super.put(x)
  }

  override def put(x: Byte, info: DataInfo[Byte]): this.type = {
    ValueSerializer.addArgInfo(info)
    addFixedCost(PutByteCost)
    w.put(x); this
  }

  override def putUByte(x: Int, info: DataInfo[U[Byte]]): this.type = {
    ValueSerializer.addArgInfo(info)
    super.putUByte(x)
  }

  override def putBoolean(x: Boolean): this.type = {
    addFixedCost(PutByteCost)
    super.putBoolean(x)
  }

  override def putBoolean(x: Boolean, info: DataInfo[Boolean]): this.type = {
    ValueSerializer.addArgInfo(info)
    addFixedCost(PutByteCost)
    w.putBoolean(x); this
  }

  override def putShort(x: Short): this.type = {
    addFixedCost(PutSignedNumericCost)
    super.putShort(x)
  }

  override def putShort(x: Short, info: DataInfo[Short]): this.type = {
    ValueSerializer.addArgInfo(info)
    addFixedCost(PutSignedNumericCost)
    w.putShort(x); this
  }

  override def putUShort(x: Int): this.type = {
    addFixedCost(PutUnsignedNumericCost)
    super.putUShort(x)
  }

  override def putUShort(x: Int, info: DataInfo[Vlq[U[Short]]]): this.type = {
    ValueSerializer.addArgInfo(info)
    addFixedCost(PutUnsignedNumericCost)
    w.putUShort(x); this
  }

  override def putInt(x: Int): this.type = {
    addFixedCost(PutSignedNumericCost)
    super.putInt(x)
  }

  override def putInt(x: Int, info: DataInfo[Int]): this.type = {
    ValueSerializer.addArgInfo(info)
    addFixedCost(PutSignedNumericCost)
    w.putInt(x); this
  }

  override def putUInt(x: Long): SigmaByteWriter.this.type = {
    super.putUInt(x)
  }

  override def putUInt(x: Long, info: DataInfo[Vlq[U[Int]]]): this.type = {
    ValueSerializer.addArgInfo(info)
    addFixedCost(PutUnsignedNumericCost)
    w.putUInt(x); this
  }

  override def putLong(x: Long): SigmaByteWriter.this.type = {
    addFixedCost(PutSignedNumericCost)
    super.putLong(x)
  }

  override def putLong(x: Long, info: DataInfo[Vlq[ZigZag[Long]]]): this.type = {
    ValueSerializer.addArgInfo(info)
    addFixedCost(PutSignedNumericCost)
    w.putLong(x); this
  }

  override def putULong(x: Long): SigmaByteWriter.this.type = {
    addFixedCost(PutUnsignedNumericCost)
    super.putULong(x)
  }

  override def putULong(x: Long, info: DataInfo[Vlq[U[Long]]]): this.type = {
    ValueSerializer.addArgInfo(info)
    addFixedCost(PutUnsignedNumericCost)
    w.putULong(x); this
  }

  override def putBytes(xs: Array[Byte], offset: Int, length: Int): this.type = {
    addPerItemCost(PutChunkCost, length)
    super.putBytes(xs, offset, length)
  }

  override def putBytes(xs: Array[Byte]): SigmaByteWriter.this.type = {
    addPerItemCost(PutChunkCost, xs.length)
    super.putBytes(xs)
  }

  override def putBytes(xs: Array[Byte], info: DataInfo[Array[Byte]]): this.type = {
    ValueSerializer.addArgInfo(info)
    addPerItemCost(PutChunkCost, xs.length)
    w.putBytes(xs); this
  }

  /** Put the two bytes of the big-endian representation of the Short value into the
    * writer. */
  override def putShortBytes(value: Short): SigmaByteWriter.this.type = {
    addPerItemCost(PutChunkCost, 2)
    super.putShortBytes(value)
  }

  override def putBits(xs: Array[Boolean]): SigmaByteWriter.this.type = {
    addPerItemCost(PutChunkCost, xs.length) // number of bits
    super.putBits(xs)
  }

  override def putBits(xs: Array[Boolean], info: DataInfo[Bits]): this.type = {
    ValueSerializer.addArgInfo(info)
    addPerItemCost(PutChunkCost, xs.length) // number of bits
    w.putBits(xs); this
  }

  override def putOption[T](x: Option[T])(putValueC: (this.type, T) => Unit): this.type = {
    addFixedCost(PutByteCost) // cost of option tag byte
    super.putOption(x)(putValueC)
  }

  override def putShortString(s: String): SigmaByteWriter.this.type = {
    addPerItemCost(PutChunkCost, s.length)
    super.putShortString(s)
  }

  override def putType[T <: SType](x: T, info: DataInfo[SType]): this.type = {
    ValueSerializer.addArgInfo(info)
    TypeSerializer.serialize(x, this); // the cost is added in TypeSerializer
    this
  }

  /** Serializes the given expression using [[ValueSerializer]]. */
  def putValue[T <: SType](x: Value[T]): this.type = {
    ValueSerializer.serialize(x, this) // the cost is added in ValueSerializer
    this
  }

  /** Serializes the given expression using [[ValueSerializer]].
    * @param x the ErgoTree expression to serialize
    * @param info meta information about the data being serialized
    */
  def putValue[T <: SType](x: Value[T], info: DataInfo[SValue]): this.type = {
    ValueSerializer.addArgInfo(info)
    ValueSerializer.serialize(x, this); // the cost is added in ValueSerializer
    this
  }

  /** Serializes the given sequence of expressions using [[ValueSerializer]]. */
  def putValues[T <: SType](xs: Seq[Value[T]]): this.type = {
    putUInt(xs.length)
    xs.foreach(putValue(_))
    this
  }

  /** Serializes the given sequence of expressions using [[ValueSerializer]].
    * @param xs the sequence of ErgoTree expressions to serialize
    * @param info additional information about the data being serialized
    */
  def putValues[T <: SType](xs: Seq[Value[T]], info: DataInfo[Seq[SValue]], itemInfo: DataInfo[SValue]): this.type = {
    putUInt(xs.length, valuesLengthInfo)
    foreach("\\#items", xs) { x =>
      putValue(x, itemInfo)
    }
    this
  }
}

object SigmaByteWriter {

  /** Callback to accumulate fixed costs. */
  type FixedCostCallback = OperationCostInfo[FixedCost] => Unit

  /** Callback to accumulate per-item costs (chunked cost). */
  type PerItemCostCallback =  (OperationCostInfo[PerItemCost], Int) => Unit

  /** Cost of instantiating a new serializer.
    * This also include overhead of method calls.
    * This is the minimal possible JitCost value
    */
  val StartWriterCost = OperationCostInfo(FixedCost(JitCost(10)), NamedDesc("SigmaByteWriter.startWriter"))

  /** Cost of writing single byte without any encoding.
    * This also include overhead of method calls.
    * This is the minimal possible JitCost value
    */
  val PutByteCost = OperationCostInfo(FixedCost(JitCost(1)), NamedDesc("SigmaByteWriter.put"))

  /** Cost of writing a signed numeric including:
    * 1) allocation of VLQ buffer array (see putULong in [[scorex.util.serialization.VLQWriter]])
    * 2) VLQ encoding
    * 3) overhead of method calls.
    */
  val PutUnsignedNumericCost =  OperationCostInfo(FixedCost(JitCost(3)), NamedDesc("SigmaByteWriter.putUNumeric"))

  /** Cost of writing a signed numeric including:
    * 1) ZigZag encoding.
    * 2) allocation of VLQ buffer array (see putULong in [[scorex.util.serialization.VLQWriter]])
    * 3) VLQ encoding
    * 4) overhead of method calls.
    */
  val PutSignedNumericCost = OperationCostInfo(FixedCost(JitCost(3)), NamedDesc("SigmaByteWriter.putNumeric"))

  /** Cost of writing a chunk of bytes:
    * 1) method call overhead
    * 2) 1 cost unit per byte
    */
  val PutChunkCost = OperationCostInfo(PerItemCost(JitCost(3), JitCost(1), 1), NamedDesc("SigmaByteWriter.putChunk"))

  implicit case object ValueFmt extends FormatDescriptor[SValue] {
    override def size: String = "[1, *]"
    override def toString: String = "Expr"
  }

  def valuesItemInfo(info: DataInfo[Seq[SValue]]): DataInfo[SValue] = {
    val itemFmt = info.format.asInstanceOf[SeqFmt[SValue]].fmt
    DataInfo(ArgInfo(info.info.name + "_i", s"i-th item in the ${info.info.description}"), itemFmt)
  }
}