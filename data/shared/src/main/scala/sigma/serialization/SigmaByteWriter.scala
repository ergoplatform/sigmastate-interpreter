package sigma.serialization

import scorex.util.serialization.Writer
import sigma.ast.syntax._
import sigma.ast._
import sigma.serialization.CoreByteWriter.{ArgInfo, DataInfo, FormatDescriptor, SeqFmt}

class SigmaByteWriter(override val w: Writer,
                      val constantExtractionStore: Option[ConstantStore])
    extends CoreByteWriter(w) {
  import CoreByteWriter._
  import ValueSerializer._

  override def put(x: Byte, info: DataInfo[Byte]): this.type = {
    ValueSerializer.addArgInfo(info)
    w.put(x); this
  }

  override def putUByte(x: Int, info: DataInfo[U[Byte]]): this.type = {
    ValueSerializer.addArgInfo(info)
    super.putUByte(x)
  }

  @inline override def putBoolean(x: Boolean, info: DataInfo[Boolean]): this.type = {
    ValueSerializer.addArgInfo(info)
    w.putBoolean(x); this
  }

  @inline override def putShort(x: Short, info: DataInfo[Short]): this.type = {
    ValueSerializer.addArgInfo(info)
    w.putShort(x); this
  }

  @inline override def putUShort(x: Int, info: DataInfo[Vlq[U[Short]]]): this.type = {
    ValueSerializer.addArgInfo(info)
    w.putUShort(x); this
  }

  @inline override def putInt(x: Int, info: DataInfo[Int]): this.type = {
    ValueSerializer.addArgInfo(info)
    w.putInt(x); this
  }

  @inline override def putUInt(x: Long, info: DataInfo[Vlq[U[Int]]]): this.type = {
    ValueSerializer.addArgInfo(info)
    w.putUInt(x); this
  }

  @inline override def putLong(x: Long, info: DataInfo[Vlq[ZigZag[Long]]]): this.type = {
    ValueSerializer.addArgInfo(info)
    w.putLong(x); this
  }

  @inline override def putULong(x: Long, info: DataInfo[Vlq[U[Long]]]): this.type = {
    ValueSerializer.addArgInfo(info)
    w.putULong(x); this
  }

  @inline override def putBytes(xs: Array[Byte], info: DataInfo[Array[Byte]]): this.type = {
    ValueSerializer.addArgInfo(info)
    w.putBytes(xs); this
  }

  @inline override def putBits(xs: Array[Boolean], info: DataInfo[Bits]): this.type = {
    ValueSerializer.addArgInfo(info)
    w.putBits(xs);
    this
  }

  @inline override def putType[T <: SType](x: T, info: DataInfo[SType]): this.type = {
    ValueSerializer.addArgInfo(info)
    TypeSerializer.serialize(x, this); this
  }

  @inline def putValue[T <: SType](x: Value[T]): this.type = { ValueSerializer.serialize(x, this); this }
  @inline def putValue[T <: SType](x: Value[T], info: DataInfo[SValue]): this.type = {
    ValueSerializer.addArgInfo(info)
    ValueSerializer.serialize(x, this); this
  }
  @inline def putValues[T <: SType](xs: Seq[Value[T]]): this.type = {
    putUInt(xs.length)
    xs.foreach(putValue(_))
    this
  }
  @inline def putValues[T <: SType](xs: Seq[Value[T]], info: DataInfo[Seq[SValue]], itemInfo: DataInfo[SValue]): this.type = {
    putUInt(xs.length, valuesLengthInfo)
    foreach("\\#items", xs) { x =>
      putValue(x, itemInfo)
    }
    this
  }
}

object SigmaByteWriter {
  implicit case object ValueFmt extends FormatDescriptor[SValue] {
    override def size: String = "[1, *]"
    override def toString: String = "Expr"
  }

  def valuesItemInfo(info: DataInfo[Seq[SValue]]): DataInfo[SValue] = {
    val itemFmt = info.format.asInstanceOf[SeqFmt[SValue]].fmt
    DataInfo(ArgInfo(info.info.name + "_i", s"i-th item in the ${info.info.description}"), itemFmt)
  }
}