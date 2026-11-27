package sigma.serialization

import sigma.ast.LogicalNot
import sigma.serialization.CoreByteWriter.DataInfo
import sigma.ast.Operations.LogicalNotInfo.inputArg
import sigma.ast.syntax._
import SigmaByteWriter._

case class LogicalNotSerializer(cons: BoolValue => BoolValue)
  extends ValueSerializer[LogicalNot] {
  override def opDesc = LogicalNot
  val inputInfo: DataInfo[SValue] = inputArg

  override protected def getValueChildren(obj: LogicalNot): IndexedSeq[sigma.ast.Value[sigma.ast.SType]] =
    IndexedSeq(obj.input)

  override protected def rebuildValueNode(
      obj: LogicalNot,
      children: IndexedSeq[sigma.ast.Value[sigma.ast.SType]]): sigma.ast.Value[sigma.ast.SType] =
    cons(children(0).asBoolValue)

  override def serialize(obj: LogicalNot, w: SigmaByteWriter): Unit =
    w.putValue(obj.input, inputInfo)

  override def parse(r: SigmaByteReader): BoolValue =
    cons(r.getValue().asBoolValue)
}
