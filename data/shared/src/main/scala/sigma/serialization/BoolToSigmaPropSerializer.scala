package sigma.serialization

import sigma.ast._
import sigma.ast.syntax._
import sigma.serialization.CoreByteWriter.DataInfo
import SigmaByteWriter._

case class BoolToSigmaPropSerializer(cons: BoolValue => SigmaPropValue) extends ValueSerializer[BoolToSigmaProp] {
  import Operations.BoolToSigmaPropInfo._
  override def opDesc = BoolToSigmaProp
  val conditionInfo: DataInfo[SValue] = conditionArg

  def serialize(obj: BoolToSigmaProp, w: SigmaByteWriter): Unit = {
    w.putValue(obj.value, conditionInfo)
  }

  def parse(r: SigmaByteReader): Value[SType] = {
    val p = r.getValue().asBoolValue
    cons(p)
  }
}
