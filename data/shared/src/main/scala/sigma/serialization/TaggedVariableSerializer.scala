package sigma.serialization

import sigma.ast.SType
import sigma.ast._

// TODO v6.0: remove this class (https://github.com/ScorexFoundation/sigmastate-interpreter/issues/584)
case class TaggedVariableSerializer(cons: (Byte, SType) => Value[SType])
  extends ValueSerializer[TaggedVariable[_ <: SType]] {
  override def opDesc = TaggedVariable

  override def serialize(obj: TaggedVariable[_ <: SType], w: SigmaByteWriter): Unit =
    w.put(obj.varId)
      .putType(obj.tpe)

  override def parse(r: SigmaByteReader): Value[SType] = {
    val varId = r.getByte()
    val tpe = r.getType()
    cons(varId, tpe)
  }
}
