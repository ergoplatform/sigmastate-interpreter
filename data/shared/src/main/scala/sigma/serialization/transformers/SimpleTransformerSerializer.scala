package sigma.serialization.transformers

import sigma.ast.{SType, SimpleTransformerCompanion, Transformer}
import sigma.serialization.CoreByteWriter.DataInfo
import sigma.ast.Value
import sigma.ast.syntax._
import sigma.serialization.{SigmaByteReader, SigmaByteWriter, ValueSerializer}
import sigma.serialization.SigmaByteWriter._

case class SimpleTransformerSerializer[I <: SType, O <: SType]
(opDesc: SimpleTransformerCompanion,
 cons: Value[I] => Value[O]) extends ValueSerializer[Transformer[I, O]] {
  val inputInfo: DataInfo[SValue] = opDesc.argInfos(0)
  
  override def serialize(obj: Transformer[I, O], w: SigmaByteWriter): Unit =
    w.putValue(obj.input, inputInfo)

  override def parse(r: SigmaByteReader): Value[O] =
    cons(r.getValue().asValue[I])
}
