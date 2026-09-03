package sigma.serialization

import sigma.ast.{OneArgumentOperation, OneArgumentOperationCompanion, SType}
import sigma.serialization.CoreByteWriter.DataInfo
import sigma.ast.Value
import sigma.ast.syntax._
import SigmaByteWriter._

case class OneArgumentOperationSerializer[T <: SType](opDesc: OneArgumentOperationCompanion, cons: Value[T] => SValue)
  extends ValueSerializer[OneArgumentOperation[T, SType]] {
  val objInfo: DataInfo[SValue] = opDesc.argInfos(0)

  override protected def getValueChildren(
      obj: OneArgumentOperation[T, SType]): IndexedSeq[Value[SType]] = IndexedSeq(obj.input)

  override protected def rebuildValueNode(
      obj: OneArgumentOperation[T, SType],
      children: IndexedSeq[Value[SType]]): Value[SType] = cons(children(0).asValue[T])

  override def serialize(obj: OneArgumentOperation[T, SType], w: SigmaByteWriter): Unit =
    w.putValue(obj.input, objInfo)
    
  override def parse(r: SigmaByteReader): SValue =
    cons(r.getValue().asValue[T])
}
