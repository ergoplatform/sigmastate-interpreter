package sigma.serialization.transformers

import sigma.ast.{ByIndex, Value}
import sigma.ast.syntax._
import sigma.serialization.{SigmaByteReader, SigmaByteWriter, ValueSerializer}
import ValueSerializer._
import sigma.VersionContext
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

  override protected def getValueChildren(obj: ByIndex[SType]): IndexedSeq[Value[SType]] =
    obj.default match {
      case Some(default) => IndexedSeq(obj.input, obj.index, default)
      case None => IndexedSeq(obj.input, obj.index)
    }

  override protected def rebuildValueNode(
      obj: ByIndex[SType],
      children: IndexedSeq[Value[SType]]): Value[SType] = {
    val default = if (obj.default.isDefined) Some(children(2)) else None
    cons(children(0).asCollection[SType], children(1).asValue[SInt.type], default)
  }

  override def serialize(obj: ByIndex[SType], w: SigmaByteWriter): Unit = {
    w.putValue(obj.input, inputInfo)
        .putValue(obj.index, indexInfo)
    opt(w, "default", obj.default)(_.putValue(_, defaultInfo))
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val input = r.getValue().asCollection[SType]
    val index = if (VersionContext.current.isV3OrLaterErgoTreeVersion){
      r.getValue().asValue[SInt.type]
    } else {
      r.getValue().upcastTo(SInt)
    }
    val default = r.getOption(r.getValue())
    cons(input, index, default)
  }

}
