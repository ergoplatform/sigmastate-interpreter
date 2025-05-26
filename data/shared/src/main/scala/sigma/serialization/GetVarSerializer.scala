package sigma.serialization

import sigma.ast._
import sigma.serialization.CoreByteWriter.{ArgInfo, DataInfo}

case class GetVarSerializer(cons: (Byte, SType) => Value[SOption[SType]])
  extends ValueSerializer[GetVar[_ <: SType]] {
  import Operations.GetVarInfo._
  override def opDesc = GetVar
  val typeInfo: DataInfo[SType] = ArgInfo("type", "expected type of context variable")
  val varIdInfo: DataInfo[Byte] = varIdArg

  override def serialize(obj: GetVar[_ <: SType], w: SigmaByteWriter): Unit =
    w.put(obj.varId, varIdInfo)
      .putType(obj.tpe.elemType, typeInfo)

  override def parse(r: SigmaByteReader): Value[SType] = {
    val varId = r.getByte()
    val tpe = r.getType()
    cons(varId, tpe)
  }
}
