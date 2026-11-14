package sigma.compiler.ir.wrappers.sigma.impl

import sigma.compiler.ir.IRContext
import sigma.data.Nullable

/** Keeps the recovered items and their element descriptor tied to one type on Scala 3. */
trait FromItemsSupport { self: IRContext =>
  trait FromItemsArgs {
    type Item
    def _1: Ref[CollBuilder]
    def _2: Seq[Ref[Item]]
    def _3: Elem[Item]
  }

  private final case class TypedFromItemsArgs[T](
      _1: Ref[CollBuilder], _2: Seq[Ref[T]], _3: Elem[T]) extends FromItemsArgs {
    type Item = T
  }

  protected def makeFromItemsArgs[T](builder: Ref[CollBuilder], items: Seq[Ref[T]], elem: Elem[T]): FromItemsArgs =
    TypedFromItemsArgs(builder, items, elem)

  protected def fromItemsSigmaTuple(args: Nullable[FromItemsArgs]): Nullable[(Ref[CollBuilder], Seq[Ref[SigmaProp]], Elem[SigmaProp])] = {
    if (args.isEmpty) Nullable.None
    else {
      val packed = args.get
      Nullable((packed._1, packed._2, packed._3))
        .asInstanceOf[Nullable[(Ref[CollBuilder], Seq[Ref[SigmaProp]], Elem[SigmaProp])]]
    }
  }
}
