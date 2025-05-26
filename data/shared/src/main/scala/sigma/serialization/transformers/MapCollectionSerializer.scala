package sigma.serialization.transformers

import sigma.ast.syntax.SValue
import sigma.ast.{MapCollection, Value}
import sigma.ast.syntax._
import sigma.serialization.{SigmaByteReader, SigmaByteWriter, ValueSerializer}
import sigma.serialization.SigmaByteWriter._
import sigma.ast.{SCollection, SFunc, SType}
import sigma.serialization.CoreByteWriter.DataInfo

case class MapCollectionSerializer(cons: (Value[SCollection[SType]], Value[SFunc]) => Value[SType])
  extends ValueSerializer[MapCollection[SType, SType]] {
  import sigma.ast.Operations.MapCollectionInfo._
  override def opDesc = MapCollection
  val thisInfo: DataInfo[SValue] = thisArg
  val fInfo: DataInfo[SValue] = fArg

  override def serialize(obj: MapCollection[SType, SType], w: SigmaByteWriter): Unit =
    w.putValue(obj.input, thisInfo)
      .putValue(obj.mapper, fInfo)

  override def parse(r: SigmaByteReader): Value[SType] = {
    val input = r.getValue().asValue[SCollection[SType]]
    val mapper = r.getValue().asFunc
    cons(input, mapper)
  }

}
