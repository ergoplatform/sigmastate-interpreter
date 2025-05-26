package sigma.compiler.ir.primitives

import sigma.compiler.ir.{Base, IRContext}

/** Defines IR representation of unary and binary operations. */
trait UnBinOps extends Base { self: IRContext =>

  /** Base class for descriptors of unary operations. */
  abstract class UnOp[A, R](val opName: String)(implicit val eResult: Elem[R]) {
    override def toString = opName
    /** Called as part of graph interpretation to execute the given unary operation.
      * @param x operation argument
      * @return result of applying this operation to x
      */
    def applySeq(x: A): R

    /** Builds a new graph node by applying this operation to the given argument. */
    def apply(arg: Ref[A]) = applyUnOp(this, arg)

    /** Whether the constants should be propagated through this operations by rewriting. */
    def shouldPropagate(arg: A) = true
  }

  /** Base class for descriptors of binary operations. */
  abstract class BinOp[A, R](val opName: String)(implicit val eResult: Elem[R]) {
    override def toString = opName

    /** Called as part of graph interpretation to execute the given binary operation.
      * @param x operation argument
      * @param y operation argument
      * @return result of applying this operation to (x, y)
      */
    def applySeq(x: A, y: A): R

    /** Builds a new graph node by applying this operation to the given arguments. */
    def apply(lhs: Ref[A], rhs: Ref[A]) = applyBinOp(this, lhs, rhs)

    /** Builds a new graph node by applying this operation to the given arguments.
      * This is a short-cuting (aka lazy) version of the operation, where the lazyness is
      * represented by Thunk.
      */
    def applyLazy(lhs: Ref[A], rhs: Ref[Thunk[A]]) = applyBinOpLazy(this, lhs, rhs)

    /** Whether the constants should be propagated through this operations by rewriting. */
    def shouldPropagate(lhs: A, rhs: A) = true
  }

  type EndoUnOp[A] = UnOp[A, A]
  type EndoBinOp[A] = BinOp[A, A]

  /** Graph node which represents application of the given unary operation to the given argument. */
  case class ApplyUnOp[A, R](op: UnOp[A, R], arg: Ref[A]) extends BaseDef[R]()(op.eResult) {
    override def toString = s"$op($arg)"
    override def transform(t: Transformer): Def[R] = ApplyUnOp[A,R](op, t(arg))
  }

  /** Graph node which represents application of the given binary operation to the given arguments. */
  case class ApplyBinOp[A, R](op: BinOp[A, R], lhs: Ref[A], rhs: Ref[A]) extends BaseDef[R]()(op.eResult) {
    override def toString = s"$op($lhs, $rhs)"
    override def transform(t: Transformer): Def[R] = ApplyBinOp[A,R](op, t(lhs), t(rhs))
  }

  /** Graph node which represents application of the given binary operation to the given arguments
    * where the second argument is lazy.
    */
  case class ApplyBinOpLazy[A, R](op: BinOp[A, R], lhs: Ref[A], rhs: Ref[Thunk[A]]) extends BaseDef[R]()(op.eResult) {
    override def toString = s"$lhs $op { $rhs }"
    override def transform(t: Transformer): Def[R] = ApplyBinOpLazy[A,R](op, t(lhs), t(rhs))
  }

  /** Overridable constructor of an unary operation node. */
  def applyUnOp[A, R](op: UnOp[A, R], arg: Ref[A]): Ref[R] = ApplyUnOp(op, arg)

  /** Overridable constructor of a binary operation node. */
  def applyBinOp[A, R](op: BinOp[A, R], lhs: Ref[A], rhs: Ref[A]): Ref[R] = ApplyBinOp(op, lhs, rhs)

  /** Overridable constructor of a binary operation node with lazy argument. */
  def applyBinOpLazy[A, R](op: BinOp[A, R], lhs: Ref[A], rhs: Ref[Thunk[A]]): Ref[R] = ApplyBinOpLazy(op, lhs, rhs)
}