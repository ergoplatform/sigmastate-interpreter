package sigma.serialization

import sigma.ast.{SType, TwoArgumentOperationCompanion, TwoArgumentsOperation}
import sigma.serialization.CoreByteWriter.DataInfo
import sigma.ast.Value
import sigma.ast.syntax._
import SigmaByteWriter._

case class TwoArgumentsSerializer[LIV <: SType, RIV <: SType, OV <: Value[SType]]
(override val opDesc: TwoArgumentOperationCompanion, constructor: (Value[LIV], Value[RIV]) => Value[SType])
  extends ValueSerializer[OV] {
  val leftInfo: DataInfo[SValue] = opDesc.argInfos(0)
  val rightInfo: DataInfo[SValue] = opDesc.argInfos(1)

  override def serialize(obj: OV, w: SigmaByteWriter): Unit = {
    val typedOp = obj.asInstanceOf[TwoArgumentsOperation[LIV, RIV, LIV]]
    w.putValue(typedOp.left, leftInfo)
      .putValue(typedOp.right, rightInfo)
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val arg1 = r.getValue().asValue[LIV]
    val arg2 = r.getValue().asValue[RIV]
    constructor(arg1, arg2)
  }
}
