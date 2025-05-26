package sigma.serialization.transformers

import sigma.ast.syntax.SigmaPropValue
import sigma.ast.{AtLeast, SCollection, SInt, SSigmaProp, Value}
import sigma.ast.Operations.AtLeastInfo
import sigma.ast.syntax._
import sigma.serialization.{SigmaByteReader, SigmaByteWriter, ValueSerializer}
import sigma.serialization.SigmaByteWriter._

case class AtLeastSerializer(cons: (Value[SInt.type], Value[SCollection[SSigmaProp.type]]) => SigmaPropValue)
  extends ValueSerializer[AtLeast] {
  override def opDesc = AtLeast

  override def serialize(obj: AtLeast, w: SigmaByteWriter): Unit =
    w.putValue(obj.bound, AtLeastInfo.boundArg)
      .putValue(obj.input, AtLeastInfo.childrenArg)

  override def parse(r: SigmaByteReader): SigmaPropValue = {
    val bound = r.getValue().asIntValue
    val input = r.getValue().asCollection[SSigmaProp.type]
    cons(bound, input)
  }
}
