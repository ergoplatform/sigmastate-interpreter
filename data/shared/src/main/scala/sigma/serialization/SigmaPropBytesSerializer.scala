package sigma.serialization

import sigma.ast.{SType, SigmaPropBytes, Value}
import sigma.serialization.CoreByteWriter.DataInfo
import sigma.ast.syntax._
import SigmaByteWriter._

object SigmaPropBytesSerializer extends ValueSerializer[SigmaPropBytes] {
  import sigma.ast.Operations.SigmaPropBytesInfo._
  override def opDesc = SigmaPropBytes
  val thisInfo: DataInfo[SValue] = thisArg

  override protected def getValueChildren(obj: SigmaPropBytes): IndexedSeq[Value[SType]] =
    IndexedSeq(obj.input)

  override protected def rebuildValueNode(
      obj: SigmaPropBytes,
      children: IndexedSeq[Value[SType]]): Value[SType] =
    SigmaPropBytes(children(0).asInstanceOf[Value[sigma.ast.SSigmaProp.type]])

  def serialize(obj: SigmaPropBytes, w: SigmaByteWriter): Unit = {
    w.putValue(obj.input, thisInfo)
  }

  def parse(r: SigmaByteReader): Value[SType] = {
    val p = r.getValue().asSigmaProp
    SigmaPropBytes(p)
  }
}
