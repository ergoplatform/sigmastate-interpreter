package sigma.compiler.ir.primitives

import sigma.compiler.ir.{Base, IRContext}

import scala.annotation.unused

/** Defines IR representation of equality operations. */
trait Equal extends Base { self: IRContext =>
  /** Binary operation representing structural equality between arguments. */
  case class Equals[A: Elem]() extends BinOp[A, Boolean]("==") {
    override def applySeq(x: A, y: A): Boolean = equalValues[A](x, y)
  }

  /** Binary operation representing structural inequality between arguments. */
  case class NotEquals[A: Elem]() extends BinOp[A, Boolean]("!=") {
    override def applySeq(x: A, y: A): Boolean = !equalValues[A](x, y)
  }

  protected def equalValues[A](x: Any, y: Any)(implicit @unused eA: Elem[A]) = x == y

  /** Extension methods to construct ApplyBinOp nodes */
  implicit class EqualOps[A](x: Ref[A]) {
    implicit private val eA: Elem[A] = x.elem

    /** Apply Equals binary operation and return Ref to ApplyBinOp node. */
    def ===(y: Ref[A]): Ref[Boolean] = Equals[A].apply(x, y)

    /** Apply NotEquals binary operation and return Ref to ApplyBinOp node. */
    def !==(y: Ref[A]): Ref[Boolean] = NotEquals[A].apply(x, y)
  }
}
