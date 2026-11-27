package sigma.serialization.transformers

import sigma.ast.syntax.SValue
import sigma.ast.{LogicalTransformerCompanion, SBoolean, SCollection, SType, Transformer}
import sigma.serialization.CoreByteWriter.DataInfo
import sigma.ast.Value
import sigma.ast.syntax._
import sigma.serialization.{SigmaByteReader, SigmaByteWriter, ValueSerializer}
import sigma.serialization.SigmaByteWriter._

case class LogicalTransformerSerializer[I <: SCollection[SBoolean.type], O <: SBoolean.type]
(opDesc: LogicalTransformerCompanion,
 cons: Value[SCollection[SBoolean.type]] => Value[SBoolean.type])
  extends ValueSerializer[Transformer[I, O]] {
  val inputInfo: DataInfo[SValue] = opDesc.argInfos(0)

  override protected def getValueChildren(obj: Transformer[I, O]): IndexedSeq[Value[SType]] =
    IndexedSeq(obj.input)

  override protected def rebuildValueNode(
      obj: Transformer[I, O],
      children: IndexedSeq[Value[SType]]): Value[SType] =
    cons(children(0).asCollection[SBoolean.type])

  override def serialize(obj: Transformer[I, O], w: SigmaByteWriter): Unit =
    w.putValue(obj.input, inputInfo)

  override def parse(r: SigmaByteReader): Value[SBoolean.type] =
    cons(r.getValue().asCollection[SBoolean.type])
}
