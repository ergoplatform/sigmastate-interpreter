package sigma.serialization

import sigma.ast._
import sigma.serialization.ValueSerializer._
import sigma.util.safeNewArray
import SigmaByteWriter._
import debox.cfor
import sigma.ast.SType
import sigma.ast.syntax.{CollectionOps, SValue}
import sigma.serialization.CoreByteWriter.{ArgInfo, DataInfo, U}

case class TupleSerializer(cons: Seq[Value[SType]] => Value[SType])
  extends ValueSerializer[Tuple] {
  override def opDesc = Tuple
  val numItemsInfo: DataInfo[U[Byte]] = ArgInfo("numItems", "number of items in the tuple")
  val itemInfo: DataInfo[SValue] = ArgInfo("item_i", "tuple's item in i-th position")

  override def serialize(obj: Tuple, w: SigmaByteWriter): Unit = {
    // TODO refactor: avoid usage of extension method `length`
    val length = obj.length
    w.putUByte(length, numItemsInfo)
    foreach(numItemsInfo.info.name, obj.items) { i =>
      w.putValue(i, itemInfo)
    }
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val size = r.getByte()
    // note, in v4.x, v5.x tuples always has 2 elements, this may change in v6.0
    // in which case allocation can be avoided for empty tuples
    val values = safeNewArray[SValue](size)
    cfor(0)(_ < size, _ + 1) { i =>
      values(i) = r.getValue()
    }
    cons(values)
  }

}
