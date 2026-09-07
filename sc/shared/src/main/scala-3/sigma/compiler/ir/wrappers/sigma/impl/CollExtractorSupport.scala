package sigma.compiler.ir.wrappers.sigma.impl

import sigma.compiler.ir.IRContext
import sigma.data.Nullable

/** Recovers correlated MethodCall operands without widening their shared types. */
trait CollExtractorSupport { self: IRContext =>
  // Each parameter occurs once, so these aliases need no correlated carrier.
  type CollReceiverArgsOf[A] = Ref[Coll[A]]
  type CollReceiverArgs = CollReceiverArgsOf[_]
  type CollApplyArgsOf[A] = (Ref[Coll[A]], Ref[Int])
  type CollApplyArgs = CollApplyArgsOf[_]
  type CollZipArgsOf[A, B] = (Ref[Coll[A]], Ref[Coll[B]])
  type CollZipArgs = CollZipArgsOf[_, _]
  type CollSliceArgsOf[A] = (Ref[Coll[A]], Ref[Int], Ref[Int])
  type CollSliceArgs = CollSliceArgsOf[_]
  type CollReplicateArgsOf[T] = (Ref[CollBuilder], Ref[Int], Ref[T])
  type CollReplicateArgs = CollReplicateArgsOf[_]

  trait CollGetOrElseArgs {
    type Item
    def _1: Ref[Coll[Item]]
    def _2: Ref[Int]
    def _3: Ref[Item]
  }

  private final case class TypedCollGetOrElseArgs[A](
      _1: Ref[Coll[A]], _2: Ref[Int], _3: Ref[A]) extends CollGetOrElseArgs {
    type Item = A
  }

  trait CollMapArgs {
    type Item
    type Result
    def _1: Ref[Coll[Item]]
    def _2: Ref[Item => Result]
  }

  private final case class TypedCollMapArgs[A, B](
      _1: Ref[Coll[A]], _2: Ref[A => B]) extends CollMapArgs {
    type Item = A
    type Result = B
  }

  trait CollPredicateArgs {
    type Item
    def _1: Ref[Coll[Item]]
    def _2: Ref[Item => Boolean]
  }

  private final case class TypedCollPredicateArgs[A](
      _1: Ref[Coll[A]], _2: Ref[A => Boolean]) extends CollPredicateArgs {
    type Item = A
  }

  trait CollFoldLeftArgs {
    type Item
    type Result
    def _1: Ref[Coll[Item]]
    def _2: Ref[Result]
    def _3: Ref[((Result, Item)) => Result]
  }

  private final case class TypedCollFoldLeftArgs[A, B](
      _1: Ref[Coll[A]], _2: Ref[B], _3: Ref[((B, A)) => B]) extends CollFoldLeftArgs {
    type Item = A
    type Result = B
  }

  trait CollFlatMapArgs {
    type Item
    type Result
    def _1: Ref[Coll[Item]]
    def _2: Ref[Item => Coll[Result]]
  }

  private final case class TypedCollFlatMapArgs[A, B](
      _1: Ref[Coll[A]], _2: Ref[A => Coll[B]]) extends CollFlatMapArgs {
    type Item = A
    type Result = B
  }

  trait CollAppendArgs {
    type Item
    def _1: Ref[Coll[Item]]
    def _2: Ref[Coll[Item]]
  }

  private final case class TypedCollAppendArgs[A](
      _1: Ref[Coll[A]], _2: Ref[Coll[A]]) extends CollAppendArgs {
    type Item = A
  }

  // The caller has identified a Coll MethodCall. Capture its descriptor types
  // before restoring the erased operands; only this boundary needs casts.
  protected def recoverCollGetOrElseArgs(receiver: Sym, index: AnyRef, default: AnyRef): Nullable[CollGetOrElseArgs] = {
    def recover[A](eA: Elem[A]): CollGetOrElseArgs =
      TypedCollGetOrElseArgs(asRep[Coll[A]](receiver), index.asInstanceOf[Ref[Int]], default.asInstanceOf[Ref[A]])
    Nullable(recover(receiver.elem.asInstanceOf[Coll.CollElem[_, _]].eA))
  }

  protected def recoverCollMapArgs(receiver: Sym, f: AnyRef): Nullable[CollMapArgs] = {
    def recover[A, B](eA: Elem[A], eB: Elem[B]): CollMapArgs =
      TypedCollMapArgs(asRep[Coll[A]](receiver), f.asInstanceOf[Ref[A => B]])
    Nullable(recover(receiver.elem.asInstanceOf[Coll.CollElem[_, _]].eA,
      f.asInstanceOf[Sym].elem.asInstanceOf[FuncElem[_, _]].eRange))
  }

  protected def recoverCollPredicateArgs(receiver: Sym, p: AnyRef): Nullable[CollPredicateArgs] = {
    def recover[A](eA: Elem[A]): CollPredicateArgs =
      TypedCollPredicateArgs(asRep[Coll[A]](receiver), p.asInstanceOf[Ref[A => Boolean]])
    Nullable(recover(receiver.elem.asInstanceOf[Coll.CollElem[_, _]].eA))
  }

  protected def recoverCollFoldLeftArgs(receiver: Sym, zero: AnyRef, op: AnyRef): Nullable[CollFoldLeftArgs] = {
    def recover[A, B](eA: Elem[A], eB: Elem[B]): CollFoldLeftArgs =
      TypedCollFoldLeftArgs(asRep[Coll[A]](receiver), zero.asInstanceOf[Ref[B]], op.asInstanceOf[Ref[((B, A)) => B]])
    Nullable(recover(receiver.elem.asInstanceOf[Coll.CollElem[_, _]].eA, zero.asInstanceOf[Sym].elem))
  }

  protected def recoverCollFlatMapArgs(receiver: Sym, f: AnyRef): Nullable[CollFlatMapArgs] = {
    def recover[A, B](eA: Elem[A], eB: Elem[B]): CollFlatMapArgs =
      TypedCollFlatMapArgs(asRep[Coll[A]](receiver), f.asInstanceOf[Ref[A => Coll[B]]])
    Nullable(recover(receiver.elem.asInstanceOf[Coll.CollElem[_, _]].eA,
      f.asInstanceOf[Sym].elem.asInstanceOf[FuncElem[_, _]].eRange.asInstanceOf[Coll.CollElem[_, _]].eA))
  }

  protected def recoverCollAppendArgs(receiver: Sym, other: AnyRef): Nullable[CollAppendArgs] = {
    def recover[A](eA: Elem[A]): CollAppendArgs =
      TypedCollAppendArgs(asRep[Coll[A]](receiver), other.asInstanceOf[Ref[Coll[A]]])
    Nullable(recover(receiver.elem.asInstanceOf[Coll.CollElem[_, _]].eA))
  }
}
