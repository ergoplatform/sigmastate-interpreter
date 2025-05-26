package sigma.serialization

import sigma.ast.{SType, SigmaPropIsProven, Value}
import sigma.ast.syntax._

object SigmaPropIsProvenSerializer extends ValueSerializer[SigmaPropIsProven] {
  override def opDesc = SigmaPropIsProven

  def serialize(obj: SigmaPropIsProven, w: SigmaByteWriter): Unit = {
    w.putValue(obj.input)
  }

  def parse(r: SigmaByteReader): Value[SType] = {
    val p = r.getValue().asSigmaProp
    SigmaPropIsProven(p)
  }
}
