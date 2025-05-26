package sigma.serialization.transformers

import sigma.ast.Operations.AppendInfo
import sigma.ast.{Append, Value}
import sigma.ast.syntax._
import sigma.serialization.{SigmaByteReader, SigmaByteWriter, ValueSerializer}
import sigma.serialization.SigmaByteWriter._
import sigma.ast.{SCollection, SType}

case class AppendSerializer(cons: (Value[SCollection[SType]], Value[SCollection[SType]]) => Value[SCollection[SType]])
  extends ValueSerializer[Append[SType]] {
  override def opDesc = Append

  override def serialize(obj: Append[SType], w: SigmaByteWriter): Unit =
    w.putValue(obj.input, AppendInfo.thisArg)
      .putValue(obj.col2, AppendInfo.otherArg)

  override def parse(r: SigmaByteReader): Value[SCollection[SType]] = {
    val input = r.getValue().asCollection[SType]
    val col2 = r.getValue().asCollection[SType]
    cons(input, col2)
  }
}
