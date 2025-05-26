package sigma.serialization

import sigma.ast._
import sigma.ast.syntax._
import ValueSerializer._
import sigma.util.safeNewArray
import sigma.serialization.CoreByteWriter.{ArgInfo, DataInfo, U, Vlq}
import SigmaByteWriter._
import debox.cfor
import sigma.ast.SType

case class BlockValueSerializer(cons: (IndexedSeq[BlockItem], Value[SType]) => Value[SType])
  extends ValueSerializer[BlockValue] {
  override def opDesc = BlockValue
  val numItemsInfo: DataInfo[Vlq[U[Int]]] = ArgInfo("numItems", "number of block items")
  val itemInfo: DataInfo[SValue] = ArgInfo("item_i", "block's item in i-th position")
  val resultInfo: DataInfo[SValue] = ArgInfo("result", "result expression of the block")

  override def serialize(obj: BlockValue, w: SigmaByteWriter): Unit = {
    w.putUInt(obj.items.length, numItemsInfo)
    foreach(numItemsInfo.info.name, obj.items){ i =>
      w.putValue(i, itemInfo)
    }
    w.putValue(obj.result, resultInfo)
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val itemsSize = r.getUIntExact
    // NO-FORK: in v5.x getUIntExact may throw Int overflow exception
    // in v4.x r.getUInt().toInt is used and may return negative Int instead of the overflow
    // in which case the array allocation will throw NegativeArraySizeException
    val values: IndexedSeq[BlockItem] = if (itemsSize == 0) {
      BlockItem.EmptySeq
    }
    else {
      // HOTSPOT:: allocate new array only if it is not empty
      val buf = safeNewArray[BlockItem](itemsSize)
      cfor(0)(_ < itemsSize, _ + 1) { i =>
        buf(i) = r.getValue().asInstanceOf[BlockItem]
      }
      buf
    }
    val result = r.getValue()
    cons(values, result)
  }
}
