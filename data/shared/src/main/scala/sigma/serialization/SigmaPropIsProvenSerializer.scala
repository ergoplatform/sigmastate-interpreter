package sigma.serialization

import sigma.ast.{SType, SigmaPropIsProven, Value}
import sigma.ast.syntax._

object SigmaPropIsProvenSerializer extends ValueSerializer[SigmaPropIsProven] {
  override def opDesc = SigmaPropIsProven

  override protected def getValueChildren(obj: SigmaPropIsProven): IndexedSeq[Value[SType]] =
    IndexedSeq(obj.input)

  override protected def rebuildValueNode(
      obj: SigmaPropIsProven,
      children: IndexedSeq[Value[SType]]): Value[SType] =
    SigmaPropIsProven(children(0).asInstanceOf[Value[sigma.ast.SSigmaProp.type]])

  def serialize(obj: SigmaPropIsProven, w: SigmaByteWriter): Unit = {
    w.putValue(obj.input)
  }

  def parse(r: SigmaByteReader): Value[SType] = {
    val p = r.getValue().asSigmaProp
    SigmaPropIsProven(p)
  }
}
