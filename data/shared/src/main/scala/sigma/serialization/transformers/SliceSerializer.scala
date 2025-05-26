package sigma.serialization.transformers

import sigma.ast.{Slice, Value}
import sigma.ast.syntax._
import sigma.serialization.{SigmaByteReader, SigmaByteWriter, ValueSerializer}
import sigma.serialization.SigmaByteWriter._
import sigma.ast.{SCollection, SInt, SType}
import sigma.serialization.CoreByteWriter.DataInfo

case class SliceSerializer(cons: (Value[SCollection[SType]], Value[SInt.type], Value[SInt.type]) => Value[SCollection[SType]])
  extends ValueSerializer[Slice[SType]] {
  import sigma.ast.Operations.SliceInfo._
  override def opDesc = Slice
  val thisInfo: DataInfo[SValue] = thisArg
  val fromInfo: DataInfo[SValue] = fromArg
  val untilInfo: DataInfo[SValue] = untilArg

  override def serialize(obj: Slice[SType], w: SigmaByteWriter): Unit =
    w.putValue(obj.input, thisInfo)
      .putValue(obj.from, fromInfo)
      .putValue(obj.until, untilInfo)

  override def parse(r: SigmaByteReader): Value[SCollection[SType]] = {
    val input = r.getValue().asCollection[SType]
    val from = r.getValue().asValue[SInt.type]
    val until = r.getValue().asValue[SInt.type]
    cons(input, from, until)
  }
}
