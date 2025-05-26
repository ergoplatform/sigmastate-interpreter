package sigma.serialization

import sigma.ast.{EmptySubst, SType, STypeSubst}
import sigma.serialization.CoreByteWriter.{ArgInfo, DataInfo}
import sigma.ast._
import sigma.ast.syntax.SValue
import SigmaByteWriter._

case class PropertyCallSerializer(cons: (Value[SType], SMethod, IndexedSeq[Value[SType]], STypeSubst) => Value[SType])
  extends ValueSerializer[MethodCall] {
  override def opDesc: ValueCompanion = PropertyCall
  val typeCodeInfo: DataInfo[Byte] = ArgInfo("typeCode", "type of the method (see Table~\\ref{table:predeftypes})")
  val methodCodeInfo: DataInfo[Byte] = ArgInfo("methodCode", "a code of the property")
  val objInfo: DataInfo[SValue] = ArgInfo("obj", "receiver object of this property call")

  override def serialize(mc: MethodCall, w: SigmaByteWriter): Unit = {
    w.put(mc.method.objType.typeId, typeCodeInfo)
    w.put(mc.method.methodId, methodCodeInfo)
    w.putValue(mc.obj, objInfo)
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val typeId = r.getByte()
    val methodId = r.getByte()
    val obj = r.getValue()
    val method = SMethod.fromIds(typeId, methodId)
    val specMethod = method.specializeFor(obj.tpe, SType.EmptySeq)
    cons(obj, specMethod, Value.EmptySeq, EmptySubst)
  }
}
