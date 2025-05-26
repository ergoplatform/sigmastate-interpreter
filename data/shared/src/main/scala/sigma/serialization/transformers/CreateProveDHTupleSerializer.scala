package sigma.serialization.transformers

import sigma.ast.{CreateProveDHTuple, SGroupElement}
import sigma.ast.syntax.SigmaPropValue
import sigma.ast.Value
import sigma.ast.syntax._
import sigma.serialization._
import SigmaByteWriter._

case class CreateProveDHTupleSerializer(cons: (Value[SGroupElement.type],
    Value[SGroupElement.type],
    Value[SGroupElement.type],
    Value[SGroupElement.type]) => SigmaPropValue)
    extends ValueSerializer[CreateProveDHTuple] {
  import sigma.ast.Operations.CreateProveDHTupleInfo._
  override def opDesc = CreateProveDHTuple

  override def serialize(obj: CreateProveDHTuple, w: SigmaByteWriter): Unit = {
    w.putValue(obj.gv, gArg)
    w.putValue(obj.hv, hArg)
    w.putValue(obj.uv, uArg)
    w.putValue(obj.vv, vArg)
  }

  override def parse(r: SigmaByteReader) = {
    val gv = r.getValue().asValue[SGroupElement.type]
    val hv = r.getValue().asValue[SGroupElement.type]
    val uv = r.getValue().asValue[SGroupElement.type]
    val vv = r.getValue().asValue[SGroupElement.type]
    cons(gv, hv, uv, vv)
  }
}
