package sigma.serialization

import sigma.ast.{EmptySubst, SType, STypeSubst}
import sigma.serialization.CoreByteWriter.{ArgInfo, DataInfo}
import sigma.ast._
import sigma.ast.syntax.SValue
import SigmaByteWriter._
import debox.cfor
import sigma.util.safeNewArray

import scala.collection.compat.immutable.ArraySeq

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
    mc.method.explicitTypeArgs.foreach { a =>
      val tpe = mc.typeSubst(a)  // existence is checked in MethodCall constructor
      w.putType(tpe)
    }
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val typeId = r.getByte()
    val methodId = r.getByte()
    val obj = r.getValue()
    val method = SMethod.fromIds(typeId, methodId)

    val (explicitTypeSubst: Map[STypeVar, SType], specMethod: SMethod) = if (method.hasExplicitTypeArgs) {
      val nTypes = method.explicitTypeArgs.length
      val res = safeNewArray[SType](nTypes)
      cfor(0)(_ < nTypes, _ + 1) { i =>
        res(i) = r.getType()
      }
      val explicitTypes = ArraySeq.unsafeWrapArray(res)
      val explicitTypeSubst = method.explicitTypeArgs.zip(explicitTypes).toMap
      val specMethod = method.withConcreteTypes(explicitTypeSubst)
      (explicitTypeSubst, specMethod)
    } else {
      val specMethod = method.specializeFor(obj.tpe, SType.EmptySeq)
      (EmptySubst, specMethod)
    }

    cons(obj, specMethod, Value.EmptySeq, explicitTypeSubst)
  }
}
