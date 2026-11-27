package sigma.serialization.transformers

import sigma.ast.{Filter, Value}
import sigma.ast.syntax._
import sigma.serialization.{SigmaByteReader, SigmaByteWriter, ValueSerializer}
import sigma.ast.{SCollection, SType, SFunc}

case class FilterSerializer(cons: (Value[SCollection[SType]], Value[SFunc]) => Value[SCollection[SType]]) extends ValueSerializer[Filter[SType]] {
  override def opDesc = Filter

  override protected def getValueChildren(obj: Filter[SType]): IndexedSeq[Value[SType]] =
    IndexedSeq(obj.input, obj.condition)

  override protected def rebuildValueNode(
      obj: Filter[SType],
      children: IndexedSeq[Value[SType]]): Value[SType] =
    cons(children(0).asCollection[SType], children(1).asFunc)

  override def serialize(obj: Filter[SType], w: SigmaByteWriter): Unit =
    w.putValue(obj.input)
    .putValue(obj.condition)

  override def parse(r: SigmaByteReader): Value[SCollection[SType]] = {
    val input = r.getValue().asCollection[SType]
    val condition = r.getValue().asFunc
    cons(input, condition)
  }
}
