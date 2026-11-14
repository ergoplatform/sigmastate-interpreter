package sigma.compiler.ir.wrappers.sigma.impl

import sigma.compiler.ir.IRContext
import sigma.data.Nullable

import scala.language.existentials

/** Retains the existing existential tuple representation on Scala 2. */
trait FromItemsSupport { self: IRContext =>
  type FromItemsArgs = (Ref[CollBuilder], Seq[Ref[T]], Elem[T]) forSome { type T }

  protected def makeFromItemsArgs[T](builder: Ref[CollBuilder], items: Seq[Ref[T]], elem: Elem[T]): FromItemsArgs =
    (builder, items, elem)

  protected def fromItemsSigmaTuple(args: Nullable[FromItemsArgs]): Nullable[(Ref[CollBuilder], Seq[Ref[SigmaProp]], Elem[SigmaProp])] =
    args.asInstanceOf[Nullable[(Ref[CollBuilder], Seq[Ref[SigmaProp]], Elem[SigmaProp])]]
}
