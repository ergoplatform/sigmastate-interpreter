package sigma.serialization

import debox.cfor
import scorex.util.Extensions._
import sigma.ast._
import sigma.util.safeNewArray
import sigma.serialization.OpCodes._
import sigma.serialization.ValueSerializer._

case class ValDefSerializer(override val opDesc: ValueCompanion) extends ValueSerializer[ValDef] {

  override def serialize(obj: ValDef, w: SigmaByteWriter): Unit = {
    w.putUInt(obj.id)
    optional("type arguments") {
      if (opCode == FunDefCode) {
        val args = obj.tpeArgs
        val len = args.length
        require(!obj.isValDef, s"expected FunDef, got $obj")
        require(len > 0, s"expected FunDef with type args, got $obj")
        w.put(len.toByteExact)
        cfor(0)(_ < len, _ + 1) { i =>
          val arg = args(i)
          w.putType(arg)
        }
      }
    }
    w.putValue(obj.rhs)
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val id = r.getUIntExact
    // NO-FORK: in v5.x getUIntExact may throw Int overflow exception
    // in v4.x r.getUInt().toInt is used and may return negative Int instead of the overflow
    // When id < 0 as a result of Int overflow, the r.valDefTypeStore(id) won't throw
    // but ValDef constructor fails on require(id >= 0, "id must be >= 0")
    val tpeArgs: Seq[STypeVar] = opCode match {
      case FunDefCode =>
        val nTpeArgs = r.getByte()
        val inputs = safeNewArray[STypeVar](nTpeArgs)
        cfor(0)(_ < nTpeArgs, _ + 1) { i =>
          inputs(i) = r.getType().asInstanceOf[STypeVar]
        }
        inputs
      case ValDefCode =>
        STypeVar.EmptySeq
    }
    val rhs = r.getValue()
    r.valDefTypeStore(id) = rhs.tpe
    ValDef(id, tpeArgs, rhs)
  }
}
