package sigma.serialization

import sigma.crypto.EcPointType
import sigma.data.ProveDHTuple

case class ProveDHTupleSerializer(
     cons: (EcPointType, EcPointType, EcPointType, EcPointType) => ProveDHTuple
  ) extends CoreSerializer[ProveDHTuple, ProveDHTuple] {

  override def serialize(obj: ProveDHTuple, w: CoreByteWriter): Unit = {
    GroupElementSerializer.serialize(obj.gv, w)
    GroupElementSerializer.serialize(obj.hv, w)
    GroupElementSerializer.serialize(obj.uv, w)
    GroupElementSerializer.serialize(obj.vv, w)
  }

  override def parse(r: CoreByteReader) = {
    val gv = GroupElementSerializer.parse(r)
    val hv = GroupElementSerializer.parse(r)
    val uv = GroupElementSerializer.parse(r)
    val vv = GroupElementSerializer.parse(r)
    cons(gv, hv, uv, vv)
  }
}

