package sigma.serialization

import sigma.ast._
import sigma.ast.syntax._
import ValueSerializer._
import sigma.util.safeNewArray
import SigmaByteWriter._
import debox.cfor
import sigma.ast.{SCollection, SType}
import sigma.serialization.CoreByteWriter.{ArgInfo, DataInfo, U, Vlq}

case class ConcreteCollectionSerializer(cons: (IndexedSeq[Value[SType]], SType) => Value[SCollection[SType]])
  extends ValueSerializer[ConcreteCollection[_ <: SType]] {
  override val opDesc = ConcreteCollection

  val numItemsInfo: DataInfo[Vlq[U[Short]]] = ArgInfo("numItems", "number of item in a collection of expressions")
  val elementTypeInfo: DataInfo[SType] = ArgInfo("elementType", "type of each expression in the collection")
  val itemInfo: DataInfo[SValue] = ArgInfo("item_i", "expression in i-th position")

  override def serialize(cc: ConcreteCollection[_ <: SType], w: SigmaByteWriter): Unit = {
    w.putUShort(cc.items.size, numItemsInfo)
    w.putType(cc.tpe.elemType, elementTypeInfo)
    foreach(numItemsInfo.info.name, cc.items)(w.putValue(_, itemInfo))
  }

  /** HOTSPOT: don't beautify this code */
  override def parse(r: SigmaByteReader): Value[SCollection[SType]] = {
    val size = r.getUShort()   // READ
    val tItem = r.getType()    // READ
    val values: IndexedSeq[Value[SType]] = if (size == 0) {
      // reusing pre-allocated immutable instances
      Value.EmptySeq
    } else {
      val values = safeNewArray[SValue](size)
      cfor(0)(_ < size, _ + 1) { i =>
        val v = r.getValue() // READ
        values(i) = v
        assert(v.tpe == tItem, s"Invalid type of collection value in $values")
      }
      values
    }
    cons(values, tItem)
  }
}
