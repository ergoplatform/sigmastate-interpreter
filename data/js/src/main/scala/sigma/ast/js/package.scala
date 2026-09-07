package sigma.ast

import sigma.Evaluation
import sigma.data.Iso
import sigma.js.{Type, Value => JsValue}

package object js {
  /** Conversion between `Value` and `Constant[SType]`. */
  implicit val isoValueToConstant: Iso[JsValue, Constant[SType]] = new Iso[JsValue, Constant[SType]] {
    override def to(x: JsValue): Constant[SType] =
      Constant(x.runtimeData.asInstanceOf[SType#WrappedType], Evaluation.rtypeToSType(x.tpe.rtype))

    override def from(x: Constant[SType]): JsValue = {
      val rtype   = Evaluation.stypeToRType(x.tpe)
      val jsvalue = JsValue.fromRuntimeData(x.value, rtype)
      new JsValue(jsvalue, new Type(rtype))
    }
  }
}
