package sigma.serialization

import sigma.ast._
import sigma.ast.syntax._
import sigma.util.safeNewArray
import debox.cfor
import sigma.ast.{SBoolean, SCollection}
import sigma.serialization.CoreByteWriter.{ArgInfo, Bits, DataInfo, U, Vlq, maxBitsInfo}

case class ConcreteCollectionBooleanConstantSerializer(cons: (IndexedSeq[Value[SBoolean.type]], SBoolean.type) => Value[SCollection[SBoolean.type]])
  extends ValueSerializer[ConcreteCollection[SBoolean.type]] {
  override def opDesc = ConcreteCollectionBooleanConstant
  val numBitsInfo: DataInfo[Vlq[U[Short]]] = ArgInfo("numBits", "number of items in a collection of Boolean values")
  val bitsInfo: DataInfo[Bits] = maxBitsInfo("bits", 0x1FFF, "Boolean values encoded as as bits (right most byte is zero-padded on the right)")

  override def serialize(cc: ConcreteCollection[SBoolean.type], w: SigmaByteWriter): Unit = {
    val items = cc.items
    val len = items.length
    w.putUShort(len, numBitsInfo)
    val bits = new Array[Boolean](len)
    cfor(0)(_ < len, _ + 1) { i =>
      bits(i) = items(i) match {
        case v: BooleanConstant if v.tpe == SBoolean =>
          v.value
        case v =>
          error(s"Expected collection of BooleanConstant values, got: $v")
      }
    }
    w.putBits(bits, bitsInfo)
  }

  /** HOTSPOT: don't beautify this code */
  override def parse(r: SigmaByteReader): Value[SCollection[SBoolean.type]] = {
    val size = r.getUShort()    // READ
    val bits = r.getBits(size)  // READ
    val items: IndexedSeq[Value[SBoolean.type]] = if (size == 0) {
      // reusing pre-allocated immutable instances
      Value.EmptySeq.asInstanceOf[IndexedSeq[Value[SBoolean.type]]]
    } else {
      val items = safeNewArray[BoolValue](size)
      cfor(0)(_ < size, _ + 1) { i =>
        items(i) = BooleanConstant.fromBoolean(bits(i))
      }
      items
    }
    cons(items, SBoolean)
  }

}
