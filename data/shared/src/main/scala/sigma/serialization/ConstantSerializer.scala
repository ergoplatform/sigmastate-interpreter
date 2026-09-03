package sigma.serialization

import sigma.ast.{SType, SigmaBuilder}
import sigma.ast._

/** This works in tandem with DataSerializer, if you change one make sure to check the other.*/
case class ConstantSerializer(builder: SigmaBuilder)
  extends ValueSerializer[Constant[SType]] {
  override def opDesc = Constant

  override protected def getValueChildren(obj: Constant[SType]): IndexedSeq[Value[SType]] = Value.EmptySeq

  override protected def rebuildValueNode(
      obj: Constant[SType],
      children: IndexedSeq[Value[SType]]): Value[SType] = obj

  override def parse(r: SigmaByteReader): Value[SType] = deserialize(r)

  override def serialize(c: Constant[SType], w: SigmaByteWriter): Unit = {
    w.putType(c.tpe)
    DataSerializer.serialize(c.value, c.tpe, w)
  }

  def deserialize(r: SigmaByteReader): Constant[SType] = {
    val tpe = r.getType()
    val obj = DataSerializer.deserialize(tpe, r)
    builder.mkConstant(obj, tpe)
  }

}

