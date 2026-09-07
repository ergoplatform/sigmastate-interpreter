package sigma.compiler.ir.wrappers.scala.impl

import sigma.compiler.ir.IRContext

import scala.language.existentials

trait WOptionExtractorSupport { self: IRContext =>
  type WOptionRef = Ref[WOption[A]] forSome { type A }
  type WOptionFilterArgs = (Ref[WOption[A]], Ref[A => Boolean]) forSome { type A }
  type WOptionMapArgs = (Ref[WOption[A]], Ref[A => B]) forSome { type A; type B }
  type WOptionGetOrElseArgs = (Ref[WOption[A]], Ref[Thunk[B]]) forSome { type A; type B }

  protected def makeWOptionFilterArgs[A](receiver: Ref[WOption[A]], predicate: Ref[A => Boolean]): WOptionFilterArgs =
    (receiver, predicate)

  protected def makeWOptionMapArgs[A, B](receiver: Ref[WOption[A]], function: Ref[A => B]): WOptionMapArgs =
    (receiver, function)

  protected def makeWOptionGetOrElseArgs[A, B](receiver: Ref[WOption[A]], default: Ref[Thunk[B]]): WOptionGetOrElseArgs =
    (receiver, default)

  protected def recoverWOptionFilterArgs(receiver: Ref[_], predicate: AnyRef): WOptionFilterArgs =
    (receiver, predicate).asInstanceOf[WOptionFilterArgs]

  protected def recoverWOptionMapArgs(receiver: Ref[_], function: AnyRef): WOptionMapArgs =
    (receiver, function).asInstanceOf[WOptionMapArgs]

  protected def recoverWOptionGetOrElseArgs(receiver: Ref[_], default: AnyRef): WOptionGetOrElseArgs =
    (receiver, default).asInstanceOf[WOptionGetOrElseArgs]
}
