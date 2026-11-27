package sigma.serialization.trees

import sigma.serialization.CoreByteWriter.DataInfo
import sigma.ast._
import sigma.ast.syntax._
import sigma.serialization.{SigmaByteReader, SigmaByteWriter, ValueSerializer}
import sigma.serialization.SigmaByteWriter._

case class QuadrupleSerializer[S1 <: SType, S2 <: SType, S3 <: SType, S4 <: SType]
(override val opDesc: QuadrupleCompanion,
 cons: (Value[S1], Value[S2], Value[S3]) => Value[S4])
  extends ValueSerializer[Quadruple[S1, S2, S3, S4]] {
  val firstInfo: DataInfo[SValue] = opDesc.argInfos(0)
  val secondInfo: DataInfo[SValue] = opDesc.argInfos(1)
  val thirdInfo: DataInfo[SValue] = opDesc.argInfos(2)

  override protected def getValueChildren(
      obj: Quadruple[S1, S2, S3, S4]): IndexedSeq[Value[SType]] =
    IndexedSeq(obj.first, obj.second, obj.third)

  override protected def rebuildValueNode(
      obj: Quadruple[S1, S2, S3, S4],
      children: IndexedSeq[Value[SType]]): Value[SType] =
    cons(children(0).asValue[S1], children(1).asValue[S2], children(2).asValue[S3])

  override def serialize(obj: Quadruple[S1, S2, S3, S4], w: SigmaByteWriter): Unit = {
    w.putValue(obj.first, firstInfo)
    w.putValue(obj.second, secondInfo)
    w.putValue(obj.third, thirdInfo)
  }

  override def parse(r: SigmaByteReader): Value[S4] = {
    val arg1 = r.getValue().asValue[S1]
    val arg2 = r.getValue().asValue[S2]
    val arg3 = r.getValue().asValue[S3]
    cons(arg1, arg2, arg3)
  }
}
