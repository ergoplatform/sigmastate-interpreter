package sigma.serialization.transformers

import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.RegisterId
import sigma.ast.syntax.SValue
import sigma.ast.{ExtractRegisterAs, Value}
import sigma.serialization.{SigmaByteReader, SigmaByteWriter, ValueSerializer}
import sigma.serialization.SigmaByteWriter._
import sigma.ast.{SBox, SOption, SType}
import sigma.serialization.CoreByteWriter.{ArgInfo, DataInfo}

case class ExtractRegisterAsSerializer(cons: (Value[SBox.type], RegisterId, SOption[SType]) => Value[SType])
  extends ValueSerializer[ExtractRegisterAs[SType]] {
  import sigma.ast.Operations.ExtractRegisterAsInfo._
  override def opDesc = ExtractRegisterAs
  val thisInfo: DataInfo[SValue] = thisArg
  val regIdInfo: DataInfo[Byte]  = regIdArg
  val typeInfo: DataInfo[SType] = ArgInfo("type", "expected type of the value in register")

  override def serialize(obj: ExtractRegisterAs[SType], w: SigmaByteWriter): Unit =
    w.putValue(obj.input, thisInfo)
      .put(obj.registerId.number, regIdInfo)
      .putType(obj.tpe.elemType, typeInfo)

  override def parse(r: SigmaByteReader): Value[SType] = {
    val input = r.getValue()
    val regId = r.getByte()
    val register = ErgoBox.findRegisterByIndex(regId).get
    val tpe = r.getType()
    cons(input.asInstanceOf[Value[SBox.type]], register, SOption(tpe))
  }
}
