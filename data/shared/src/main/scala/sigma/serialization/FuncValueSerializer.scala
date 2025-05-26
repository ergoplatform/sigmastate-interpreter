package sigma.serialization

import sigma.ast._
import sigma.ast.syntax._
import ValueSerializer._
import sigma.util.safeNewArray
import SigmaByteWriter._
import debox.cfor
import sigma.ast.SType
import sigma.serialization.CoreByteWriter.{ArgInfo, DataInfo, U, Vlq}

case class FuncValueSerializer(cons: (IndexedSeq[(Int, SType)], Value[SType]) => Value[SType])
  extends ValueSerializer[FuncValue] {
  override def opDesc = FuncValue
  val numArgsInfo: DataInfo[Vlq[U[Int]]] = ArgInfo("numArgs", "number of function arguments")
  val idInfo: DataInfo[Vlq[U[Int]]] = ArgInfo("id_i", "identifier of the i-th argument")
  val typeInfo: DataInfo[SType] = ArgInfo("type_i", "type of the i-th argument")
  val bodyInfo: DataInfo[SValue] = ArgInfo("body", "function body, which is parameterized by arguments")

  override def serialize(obj: FuncValue, w: SigmaByteWriter): Unit = {
    w.putUInt(obj.args.length, numArgsInfo)
    foreach(numArgsInfo.info.name, obj.args) { case (idx, tpe) =>
      w.putUInt(idx, idInfo)
        .putType(tpe, typeInfo)
    }
    w.putValue(obj.body, bodyInfo)
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val argsSize = r.getUIntExact
    // NO-FORK: in v5.x getUIntExact may throw Int overflow exception
    // in v4.x r.getUInt().toInt is used and may return negative Int instead of the overflow
    // in which case the array allocation will throw NegativeArraySizeException
    val args = safeNewArray[(Int, SType)](argsSize)
    cfor(0)(_ < argsSize, _ + 1) { i =>
      val id = r.getUInt().toInt
      // Note, when id < 0 as a result of Int overflow, the r.valDefTypeStore(id) won't throw
      // More over evaluation of such FuncValue will not throw either if the body contains
      // ValUse with the same negative id
      val tpe = r.getType()
      r.valDefTypeStore(id) = tpe
      args(i) = (id, tpe)
    }
    val body = r.getValue()
    cons(args, body)
  }
}
