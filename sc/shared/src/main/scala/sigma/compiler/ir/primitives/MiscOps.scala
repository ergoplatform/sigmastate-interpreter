package sigma.compiler.ir.primitives

import sigma.compiler.ir.{Base, IRContext}

/** Defines IR representation of miscellaneous operations that doesn't fit into any
  * specific category.
  */
trait MiscOps extends Base { self: IRContext =>
  case class Downcast[From, To](input: Ref[From], eTo: Elem[To]) extends BaseDef[To]()(eTo) {
    override def transform(t: Transformer) = Downcast(t(input), eTo)
  }
  case class Upcast[From, To](input: Ref[From], eTo: Elem[To]) extends BaseDef[To]()(eTo) {
    override def transform(t: Transformer) = Upcast(t(input), eTo)
  }

  def downcast[To:Elem](value: Ref[_]): Ref[To] = Downcast(value, element[To])
  def upcast[To:Elem](value: Ref[_]): Ref[To] = Upcast(value, element[To])
}
