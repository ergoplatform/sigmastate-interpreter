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

  override protected def getValueChildren(
      obj: MapCollection[SType, SType]): IndexedSeq[Value[SType]] = IndexedSeq(obj.input, obj.mapper)

  override protected def rebuildValueNode(
      obj: MapCollection[SType, SType],
      children: IndexedSeq[Value[SType]]): Value[SType] =
    cons(children(0).asValue[SCollection[SType]], children(1).asFunc)

  override def serialize(obj: MapCollection[SType, SType], w: SigmaByteWriter): Unit =
    w.putValue(obj.input, thisInfo)
      .putValue(obj.mapper, fInfo)

  override def parse(r: SigmaByteReader): Value[SType] = {
    val input = r.getValue().asValue[SCollection[SType]]
    val mapper = r.getValue().asFunc
    cons(input, mapper)
  }

}
