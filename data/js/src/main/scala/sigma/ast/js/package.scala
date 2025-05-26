package sigma.ast

import sigma.Evaluation
import sigma.data.Iso
import sigma.js.{Type, Value}

package object js {
  /** Conversion between `Value` and `Constant[SType]`. */
  implicit val isoValueToConstant: Iso[Value, Constant[SType]] = new Iso[Value, Constant[SType]] {
    override def to(x: Value): Constant[SType] =
      Constant(x.runtimeData.asInstanceOf[SType#WrappedType], Evaluation.rtypeToSType(x.tpe.rtype))

    override def from(x: Constant[SType]): Value = {
      val rtype   = Evaluation.stypeToRType(x.tpe)
      val jsvalue = Value.fromRuntimeData(x.value, rtype)
      new Value(jsvalue, new Type(rtype))
    }
  }
}
