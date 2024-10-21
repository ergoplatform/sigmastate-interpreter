package sigma.compiler.ir.primitives

import sigma.compiler.ir.{Base, IRContext}

/** Defines IR representation of miscellaneous operations that doesn't fit into any
  * specific category.
  */
trait MiscOps extends Base { self: IRContext =>
  case class HashCode[A]() extends UnOp[A, Int]("hashCode") {
    override def applySeq(x: A): Int = x.hashCode
  }

  case class ToString[A]() extends UnOp[A, String]("toString") {
    override def applySeq(x: A): String = x.toString
  }

  case class Downcast[From, To](input: Ref[From], eTo: Elem[To]) extends BaseDef[To]()(eTo) {
    override def transform(t: Transformer) = Downcast(t(input), eTo)
  }
  case class Upcast[From, To](input: Ref[From], eTo: Elem[To]) extends BaseDef[To]()(eTo) {
    override def transform(t: Transformer) = Upcast(t(input), eTo)
  }

  def downcast[To:Elem](value: Ref[_]): Ref[To] = Downcast(value, element[To])
  def upcast[To:Elem](value: Ref[_]): Ref[To] = Upcast(value, element[To])

  implicit class RepUniversalOps[A](x: Ref[A]) {
    def hashCodeRep: Ref[Int] = HashCode[A]().apply(x)
    def toStringRep = ToString[A]().apply(x)
  }

  case class Convert[From,To](eFrom: Elem[From], eTo: Elem[To], x: Ref[Def[_]], conv: Ref[From => To])
      extends BaseDef[To]()(eTo) {
    override def transform(t: Transformer) = Convert(eFrom, eTo, t(x), t(conv))
  }

  def tryConvert[From, To](eFrom: Elem[From], eTo: Elem[To], x: Ref[Def[_]], conv: Ref[From => To]): Ref[To] = {
    if (x.elem <:< eFrom)
      conv(asRep[From](x))
    else
      Convert(eFrom, eTo, x, conv)
  }

}
