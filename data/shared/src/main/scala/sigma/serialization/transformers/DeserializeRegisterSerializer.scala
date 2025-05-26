package sigma.serialization.transformers

import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.RegisterId
import sigma.ast.{DeserializeRegister, Value}
import sigma.serialization.{SigmaByteReader, SigmaByteWriter, ValueSerializer}
import ValueSerializer._
import sigma.ast.SType
import sigma.ast.syntax.SValue
import sigma.serialization.CoreByteWriter.{ArgInfo, DataInfo}
import sigma.ast.Operations.DeserializeRegisterInfo._
import sigma.serialization.SigmaByteWriter._

case class DeserializeRegisterSerializer(cons: (RegisterId, SType, Option[Value[SType]]) => Value[SType])
  extends ValueSerializer[DeserializeRegister[SType]] {
  override def opDesc = DeserializeRegister
  val idInfo: DataInfo[Byte] = idArg
  val typeInfo: DataInfo[SType] = ArgInfo("type", "expected type of the deserialized script")
  val defaultInfo: DataInfo[SValue] = defaultArg

  override def serialize(obj: DeserializeRegister[SType], w: SigmaByteWriter): Unit = {
    w.put(obj.reg.number, idInfo)
    w.putType(obj.tpe, typeInfo)
    opt(w, "default", obj.default)(_.putValue(_, defaultInfo))
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val registerId = ErgoBox.findRegisterByIndex(r.getByte()).get
    val tpe = r.getType()
    val dv = r.getOption(r.getValue())
    r.wasDeserialize ||= true // mark the flag
    cons(registerId, tpe, dv)
  }

}
