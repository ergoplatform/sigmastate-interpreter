package sigma.serialization.transformers

import sigma.ast.syntax.SigmaPropValue
import sigma.ast.{AtLeast, SCollection, SInt, SSigmaProp, Value}
import sigma.ast.Operations.AtLeastInfo
import sigma.ast.syntax._
import sigma.serialization.{SigmaByteReader, SigmaByteWriter, ValueSerializer}
import sigma.serialization.SigmaByteWriter._

case class AtLeastSerializer(cons: (Value[SInt.type], Value[SCollection[SSigmaProp.type]]) => SigmaPropValue)
  extends ValueSerializer[AtLeast] {
  override def opDesc = AtLeast

  override protected def getValueChildren(obj: AtLeast): IndexedSeq[Value[sigma.ast.SType]] =
    IndexedSeq(obj.bound, obj.input)

  override protected def rebuildValueNode(
      obj: AtLeast,
      children: IndexedSeq[Value[sigma.ast.SType]]): Value[sigma.ast.SType] =
    cons(children(0).asIntValue, children(1).asCollection[SSigmaProp.type])

  override def serialize(obj: AtLeast, w: SigmaByteWriter): Unit =
    w.putValue(obj.bound, AtLeastInfo.boundArg)
      .putValue(obj.input, AtLeastInfo.childrenArg)

  override def parse(r: SigmaByteReader): SigmaPropValue = {
    val bound = r.getValue().asIntValue
    val input = r.getValue().asCollection[SSigmaProp.type]
    cons(bound, input)
  }
}
