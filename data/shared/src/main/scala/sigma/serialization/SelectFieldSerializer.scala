package sigma.serialization

import sigma.ast.Operations.SelectFieldInfo
import sigma.ast.{SelectField, Value}
import sigma.ast.syntax._
import SelectFieldInfo._
import sigma.ast.{STuple, SType}
import sigma.serialization.CoreByteWriter.DataInfo
import SigmaByteWriter._

case class SelectFieldSerializer(cons: (Value[STuple], Byte) => Value[SType]) extends ValueSerializer[SelectField] {
  override def opDesc = SelectField
  val inputInfo: DataInfo[SValue] = inputArg
  val fieldIndexInfo: DataInfo[Byte] = fieldIndexArg

  override protected def getValueChildren(obj: SelectField): IndexedSeq[Value[SType]] =
    IndexedSeq(obj.input)

  override protected def rebuildValueNode(
      obj: SelectField,
      children: IndexedSeq[Value[SType]]): Value[SType] =
    cons(children(0).asValue[STuple], obj.fieldIndex)

  override def serialize(obj: SelectField, w: SigmaByteWriter): Unit =
    w.putValue(obj.input, inputInfo)
      .put(obj.fieldIndex, fieldIndexInfo)

  override def parse(r: SigmaByteReader): Value[SType] = {
    val tuple = r.getValue().asValue[STuple]
    val fieldIndex = r.getByte()
    cons(tuple, fieldIndex)
  }

}
