package sigma.compiler.ir.wrappers.scala.impl

import sigma.compiler.ir.IRContext

trait WOptionExtractorSupport { self: IRContext =>
  type WOptionRef = Ref[WOption[_]]
  type WOptionGetOrElseArgs = (Ref[WOption[_]], Ref[Thunk[_]])

  trait WOptionFilterArgs {
    type Item
    def _1: Ref[WOption[Item]]
    def _2: Ref[Item => Boolean]
  }

  private final case class TypedWOptionFilterArgs[A](
      _1: Ref[WOption[A]], _2: Ref[A => Boolean]) extends WOptionFilterArgs {
    type Item = A
  }

  trait WOptionMapArgs {
    type Item
    type Result
    def _1: Ref[WOption[Item]]
    def _2: Ref[Item => Result]
  }

  private final case class TypedWOptionMapArgs[A, B](
      _1: Ref[WOption[A]], _2: Ref[A => B]) extends WOptionMapArgs {
    type Item = A
    type Result = B
  }

  protected def makeWOptionFilterArgs[A](receiver: Ref[WOption[A]], predicate: Ref[A => Boolean]): WOptionFilterArgs =
    TypedWOptionFilterArgs(receiver, predicate)

  protected def makeWOptionMapArgs[A, B](receiver: Ref[WOption[A]], function: Ref[A => B]): WOptionMapArgs =
    TypedWOptionMapArgs(receiver, function)

  protected def makeWOptionGetOrElseArgs[A, B](receiver: Ref[WOption[A]], default: Ref[Thunk[B]]): WOptionGetOrElseArgs =
    (receiver, default)

  private def captureFilterArgs[A](receiver: Ref[_], predicate: AnyRef, item: Elem[A]): WOptionFilterArgs =
    makeWOptionFilterArgs(receiver.asInstanceOf[Ref[WOption[A]]], predicate.asInstanceOf[Ref[A => Boolean]])

  protected def recoverWOptionFilterArgs(receiver: Ref[_], predicate: AnyRef): WOptionFilterArgs =
    captureFilterArgs(receiver, predicate, receiver.elem.typeArgs("A")._1.asInstanceOf[Elem[_]])

  private def captureMapArgs[A, B](receiver: Ref[_], function: AnyRef, item: Elem[A], result: Elem[B]): WOptionMapArgs =
    makeWOptionMapArgs(receiver.asInstanceOf[Ref[WOption[A]]], function.asInstanceOf[Ref[A => B]])

  protected def recoverWOptionMapArgs(receiver: Ref[_], function: AnyRef): WOptionMapArgs = {
    val result: Elem[_] = function.asInstanceOf[Ref[Any => Any]].elem.eRange
    captureMapArgs(receiver, function, receiver.elem.typeArgs("A")._1.asInstanceOf[Elem[_]], result)
  }

  protected def recoverWOptionGetOrElseArgs(receiver: Ref[_], default: AnyRef): WOptionGetOrElseArgs =
    (receiver, default).asInstanceOf[WOptionGetOrElseArgs]
}
