package sigma.serialization

import sigma.crypto.EcPointType
import sigma.data.ProveDlog

case class ProveDlogSerializer(cons: EcPointType => ProveDlog)
  extends CoreSerializer[ProveDlog, ProveDlog] {

  override def serialize(obj: ProveDlog, w: CoreByteWriter): Unit =
    GroupElementSerializer.serialize(obj.value, w)

  override def parse(r: CoreByteReader) = {
    val res = GroupElementSerializer.parse(r)
    cons(res)
  }
}




