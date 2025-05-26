package sigma.serialization.transformers

import sigma.ast.syntax.SValue
import sigma.ast.{NumericCastCompanion, SNumericType, SType, Transformer, Value}
import sigma.serialization.CoreByteWriter.{ArgInfo, DataInfo}
import sigma.ast.syntax._
import sigma.serialization.{SigmaByteReader, SigmaByteWriter, ValueSerializer}
import sigma.serialization.SigmaByteWriter._

case class NumericCastSerializer(opDesc: NumericCastCompanion,
                                 cons: (Value[SNumericType], SNumericType) => Value[SNumericType])
  extends ValueSerializer[Transformer[SNumericType, SNumericType]] {
  val inputInfo: DataInfo[SValue] = opDesc.argInfos(0)
  val typeInfo: DataInfo[SType] = ArgInfo("type", "resulting type of the cast operation")

  override def serialize(obj: Transformer[SNumericType, SNumericType], w: SigmaByteWriter): Unit =
    w.putValue(obj.input, inputInfo)
      .putType(obj.tpe, typeInfo)

  override def parse(r: SigmaByteReader): Value[SNumericType] = {
    val input = r.getValue().asNumValue
    val tpe = r.getType().asNumType
    cons(input, tpe)
  }
}
