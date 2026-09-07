package sigma.compiler.ir.wrappers.sigma.impl

import sigma.compiler.ir.IRContext
import sigma.data.Nullable

import scala.language.existentials

/** Retains the original existential return types and tuple recovery on Scala 2. */
trait CollExtractorSupport { self: IRContext =>
  type CollReceiverArgs = Ref[Coll[A]] forSome { type A }
  type CollApplyArgs = (Ref[Coll[A]], Ref[Int]) forSome { type A }
  type CollGetOrElseArgs = (Ref[Coll[A]], Ref[Int], Ref[A]) forSome { type A }
  type CollMapArgs = (Ref[Coll[A]], Ref[A => B]) forSome { type A; type B }
  type CollZipArgs = (Ref[Coll[A]], Ref[Coll[B]]) forSome { type A; type B }
  type CollPredicateArgs = (Ref[Coll[A]], Ref[A => Boolean]) forSome { type A }
  type CollFoldLeftArgs = (Ref[Coll[A]], Ref[B], Ref[((B, A)) => B]) forSome { type A; type B }
  type CollFlatMapArgs = (Ref[Coll[A]], Ref[A => Coll[B]]) forSome { type A; type B }
  type CollSliceArgs = (Ref[Coll[A]], Ref[Int], Ref[Int]) forSome { type A }
  type CollAppendArgs = (Ref[Coll[A]], Ref[Coll[A]]) forSome { type A }
  type CollReplicateArgs = (Ref[CollBuilder], Ref[Int], Ref[T]) forSome { type T }

  protected def recoverCollGetOrElseArgs(receiver: Sym, index: AnyRef, default: AnyRef): Nullable[CollGetOrElseArgs] = {
    val res = (receiver, index, default)
    Nullable(res).asInstanceOf[Nullable[CollGetOrElseArgs]]
  }

  protected def recoverCollMapArgs(receiver: Sym, f: AnyRef): Nullable[CollMapArgs] = {
    val res = (receiver, f)
    Nullable(res).asInstanceOf[Nullable[CollMapArgs]]
  }

  protected def recoverCollPredicateArgs(receiver: Sym, p: AnyRef): Nullable[CollPredicateArgs] = {
    val res = (receiver, p)
    Nullable(res).asInstanceOf[Nullable[CollPredicateArgs]]
  }

  protected def recoverCollFoldLeftArgs(receiver: Sym, zero: AnyRef, op: AnyRef): Nullable[CollFoldLeftArgs] = {
    val res = (receiver, zero, op)
    Nullable(res).asInstanceOf[Nullable[CollFoldLeftArgs]]
  }

  protected def recoverCollFlatMapArgs(receiver: Sym, f: AnyRef): Nullable[CollFlatMapArgs] = {
    val res = (receiver, f)
    Nullable(res).asInstanceOf[Nullable[CollFlatMapArgs]]
  }

  protected def recoverCollAppendArgs(receiver: Sym, other: AnyRef): Nullable[CollAppendArgs] = {
    val res = (receiver, other)
    Nullable(res).asInstanceOf[Nullable[CollAppendArgs]]
  }
}
