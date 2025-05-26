package sigma.serialization.transformers

import sigma.ast.{ByIndex, Value}
import sigma.ast.syntax._
import sigma.serialization.{SigmaByteReader, SigmaByteWriter, ValueSerializer}
import ValueSerializer._
import sigma.ast.syntax.SValue
import sigma.ast.Operations.ByIndexInfo._
import sigma.serialization.SigmaByteWriter._
import sigma.ast.{SCollection, SInt, SType}
import sigma.serialization.CoreByteWriter.DataInfo

case class ByIndexSerializer(cons: (Value[SCollection[SType]], Value[SInt.type], Option[Value[SType]]) => Value[SType])
  extends ValueSerializer[ByIndex[SType]] {
  override def opDesc = ByIndex
  val inputInfo: DataInfo[SValue] = thisArg
  val indexInfo: DataInfo[SValue] = indexArg
  val defaultInfo: DataInfo[SValue] = defaultArg

  override def serialize(obj: ByIndex[SType], w: SigmaByteWriter): Unit = {
    w.putValue(obj.input, inputInfo)
        .putValue(obj.index, indexInfo)
    opt(w, "default", obj.default)(_.putValue(_, defaultInfo))
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val input = r.getValue().asCollection[SType]
    val index = r.getValue().upcastTo(SInt)
    val default = r.getOption(r.getValue())
    cons(input, index, default)
  }

}
