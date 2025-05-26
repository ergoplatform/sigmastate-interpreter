package sigma.serialization

import sigma.ast.{ModQArithOp, ModQArithOpCompanion, SType}
import sigma.ast.syntax.{BigIntValue, SValue, ValueOps}
import sigma.serialization.CoreByteWriter.DataInfo
import sigma.ast.Value
import SigmaByteWriter._

// TODO v6.0: make sure it is covered with tests (see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/327)
case class ModQArithOpSerializer(override val opDesc: ModQArithOpCompanion, cons: (BigIntValue, BigIntValue) => BigIntValue)
  extends ValueSerializer[ModQArithOp] {
  val leftInfo: DataInfo[SValue] = opDesc.argInfos(0)
  val rightInfo: DataInfo[SValue] = opDesc.argInfos(1)

  override def serialize(obj: ModQArithOp, w: SigmaByteWriter): Unit = {
    w.putValue(obj.left, leftInfo)
      .putValue(obj.right, rightInfo)
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val arg1 = r.getValue().asBigInt
    val arg2 = r.getValue().asBigInt
    cons(arg1, arg2)
  }
}
