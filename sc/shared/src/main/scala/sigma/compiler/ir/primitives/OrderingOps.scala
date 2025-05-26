package sigma.compiler.ir.primitives

import sigma.compiler.ir.{Base, IRContext}
import sigma.data.ExactOrdering

import scala.language.implicitConversions

/** Slice in IRContext cake with definitions of comparison operations. */
trait OrderingOps extends Base { self: IRContext =>
  implicit def repOrderingToOrderingOps[T](x: Ref[T])(implicit n: ExactOrdering[T]) = new OrderingOpsCls(x)
  implicit def OrderingToOrderingOps[T](x: T)(implicit n: ExactOrdering[T], et: Elem[T]) = new OrderingOpsCls(toRep(x))

  /** Extension method over `Ref[T]` given an instance of ExactOrdering for T. */
  class OrderingOpsCls[T](lhs: Ref[T])(implicit val n: ExactOrdering[T]) {
    def <(rhs: Ref[T]) = OrderingLT(n).apply(lhs,rhs)
    def <=(rhs: Ref[T]) = OrderingLTEQ(n).apply(lhs,rhs)
    def >(rhs: Ref[T]) = OrderingGT(n).apply(lhs,rhs)
    def >=(rhs: Ref[T]) = OrderingGTEQ(n).apply(lhs,rhs)
    def max(rhs: Ref[T]): Ref[T] = OrderingMax(n)(lhs.elem).apply(lhs,rhs)
    def min(rhs: Ref[T]): Ref[T] = OrderingMin(n)(lhs.elem).apply(lhs,rhs)
    def compare(rhs: Ref[T]): Ref[Int] = OrderingCompare(n).apply(lhs,rhs)
  }

  /** Descriptor of binary `<` operation. */
  case class OrderingLT[T](ord: ExactOrdering[T]) extends BinOp[T, Boolean]("<") {
    override def applySeq(x: T, y: T): Boolean = ord.lt(x, y)
  }

  /** Descriptor of binary `<=` operation. */
  case class OrderingLTEQ[T](ord: ExactOrdering[T]) extends BinOp[T, Boolean]("<=") {
    override def applySeq(x: T, y: T): Boolean = ord.lteq(x, y)
  }

  /** Descriptor of binary `>` operation. */
  case class OrderingGT[T](ord: ExactOrdering[T]) extends BinOp[T, Boolean](">") {
    override def applySeq(x: T, y: T): Boolean = ord.gt(x, y)
  }

  /** Descriptor of binary `>=` operation. */
  case class OrderingGTEQ[T](ord: ExactOrdering[T]) extends BinOp[T, Boolean](">=") {
    override def applySeq(x: T, y: T): Boolean = ord.gteq(x, y)
  }

  /** Descriptor of binary `max` operation. */
  case class OrderingMax[T: Elem](ord: ExactOrdering[T]) extends BinOp[T, T]("max") {
    override def applySeq(x: T, y: T): T = ord.max(x, y)
  }

  /** Descriptor of binary `min` operation. */
  case class OrderingMin[T: Elem](ord: ExactOrdering[T]) extends BinOp[T, T]("min") {
    override def applySeq(x: T, y: T): T = ord.min(x, y)
  }

  /** Descriptor of binary `compare` operation. */
  case class OrderingCompare[T](ord: ExactOrdering[T]) extends BinOp[T, Int]("compare") {
    override def applySeq(x: T, y: T): Int = ord.compare(x, y)
  }
}