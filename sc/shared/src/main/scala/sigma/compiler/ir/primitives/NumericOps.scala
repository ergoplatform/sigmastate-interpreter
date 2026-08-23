package sigma.compiler.ir.primitives

import sigma.compiler.ir.{Base, IRContext}
import sigma.data.{ExactIntegral, ExactNumeric}

/** Slice in IRContext cake with definitions of numeric operations. */
trait NumericOps extends Base { self: IRContext =>

  /** Extension methods over `Ref[T]` where T is instance of ExactNumeric type-class. */
  implicit class NumericOpsCls[T](x: Ref[T])(implicit val n: ExactNumeric[T]) {
    def +(y: Ref[T]): Ref[T] = NumericPlus(n)(x.elem).apply(x, y)
    def -(y: Ref[T]): Ref[T] = NumericMinus(n)(x.elem).apply(x, y)
    def *(y: Ref[T]): Ref[T] = NumericTimes(n)(x.elem).apply(x, y)
    def unary_- : Ref[T] = NumericNegate(n)(x.elem).apply(x)
    def toInt: Ref[Int] = NumericToInt(n).apply(x)
    def toLong: Ref[Long] = NumericToLong(n).apply(x)
    def toBigEndianBytes: Ref[Coll[Byte]] = NumericToBigEndianBytes(n).apply(x)
  }

  /** Extension methods over `Ref[T]` where T is instance of ExactIntegral type-class. */
  implicit class IntegralOpsCls[T](x: Ref[T])(implicit i: ExactIntegral[T]) {
    def div(y: Ref[T]): Ref[T] = IntegralDivide(i)(x.elem).apply(x, y)
    def mod(y: Ref[T]): Ref[T] = IntegralMod(i)(x.elem).apply(x, y)
    // avoid / due to conflicts
    def /!(y: Ref[T]): Ref[T] = div(y)
    def %(y: Ref[T]): Ref[T] = mod(y)
  }

  /** Return an ExactNumeric for a given type T. */
  def numeric[T:ExactNumeric]: ExactNumeric[T] = implicitly[ExactNumeric[T]]

  /** Return an ExactIntegral for a given type T. */
  def integral[T:ExactIntegral]: ExactIntegral[T] = implicitly[ExactIntegral[T]]

  /** Descriptor of binary `+` operation. */
  case class NumericPlus[T: Elem](n: ExactNumeric[T]) extends EndoBinOp[T]("+") {
    override def applySeq(x: T, y: T): T = n.plus(x, y)
  }

  /** Descriptor of binary `-` operation. */
  case class NumericMinus[T: Elem](n: ExactNumeric[T]) extends EndoBinOp[T]("-") {
    override def applySeq(x: T, y: T): T = n.minus(x, y)
  }

  /** Descriptor of binary `*` operation. */
  case class NumericTimes[T: Elem](n: ExactNumeric[T]) extends EndoBinOp[T]("*") {
    override def applySeq(x: T, y: T): T = n.times(x, y)
  }

  /** Descriptor of unary `ToBits` conversion operation. */
  case class NumericBitwiseOr[T: Elem](n: ExactNumeric[T]) extends EndoBinOp[T]("|") {
    override def applySeq(x: T, y: T): T = n.bitwiseOr(x, y)
  }

  case class NumericBitwiseAnd[T: Elem](n: ExactNumeric[T]) extends EndoBinOp[T]("&") {
    override def applySeq(x: T, y: T): T = n.bitwiseAnd(x, y)
  }

  case class NumericBitwiseXor[T: Elem](n: ExactNumeric[T]) extends EndoBinOp[T]("^") {
    override def applySeq(x: T, y: T): T = n.bitwiseXor(x, y)
  }

  case class NumericShiftLeft[T: Elem](n: ExactNumeric[T]) extends BinDiffArgsOp[T, Int]("<<") {
    override def applySeq(x: T, y: Int): T = n.shiftLeft(x, y)
  }

  case class NumericShiftRight[T: Elem](n: ExactNumeric[T]) extends BinDiffArgsOp[T, Int](">>") {
    override def applySeq(x: T, y: Int): T = n.shiftRight(x, y)
  }

  /** Base class for descriptors of binary division operations. */
  abstract class DivOp[T: Elem](opName: String, n: ExactIntegral[T]) extends EndoBinOp[T](opName) {
    override def shouldPropagate(lhs: T, rhs: T) = rhs != n.zero
  }

  /** Descriptor of unary `-` operation. */
  case class NumericNegate[T: Elem](n: ExactNumeric[T]) extends UnOp[T, T]("-") {
    override def applySeq(x: T): T = n.negate(x)
  }

  /** Descriptor of unary `ToInt` conversion operation. */
  case class NumericToInt[T](n: ExactNumeric[T]) extends UnOp[T,Int]("ToInt") {
    override def applySeq(x: T): Int = n.toInt(x)
  }

  /** Descriptor of unary `ToLong` conversion operation. */
  case class NumericToLong[T](n: ExactNumeric[T]) extends UnOp[T,Long]("ToLong") {
    override def applySeq(x: T): Long = n.toLong(x)
  }

  import Coll._
  /** Descriptor of unary `ToBigEndianBytes` conversion operation. */
  case class NumericToBigEndianBytes[T](n: ExactNumeric[T])
    extends UnOp[T, Coll[Byte]]("ToBigEndianBytes")(element[Coll[Byte]]) {
    override def applySeq(x: T): Coll[Byte] = {
      liftableColl(Liftables.ByteIsLiftable).lift(n.toBigEndianBytes(x))
    }
  }

  /** Descriptor of unary `ToBits` conversion operation. */
  case class NumericToBits[T](n: ExactNumeric[T])
    extends UnOp[T, Coll[Boolean]]("ToBits")(element[Coll[Boolean]]) {
    override def applySeq(x: T): Coll[Boolean] = {
      liftableColl(Liftables.BooleanIsLiftable).lift(n.toBits(x))
    }
  }

  /** Descriptor of unary `BitwiseInverse` conversion operation. */
  case class NumericBitwiseInverse[T: Elem](n: ExactNumeric[T]) extends UnOp[T, T]("~") {
    override def applySeq(x: T): T = n.bitwiseInverse(x)
  }

  /** Descriptor of binary `/` operation (integral division). */
  case class IntegralDivide[T](i: ExactIntegral[T])(implicit elem: Elem[T]) extends DivOp[T]("/", i) {
    override def applySeq(x: T, y: T): T = i.quot(x, y)
  }

  /** Descriptor of binary `%` operation (remainder of integral division). */
  case class IntegralMod[T](i: ExactIntegral[T])(implicit elem: Elem[T]) extends DivOp[T]("%", i) {
    override def applySeq(x: T, y: T): T = i.divisionRemainder(x, y)
  }

  /** Compares the given value with zero of the given ExactNumeric instance. */
  @inline final def isZero[T](x: T, n: ExactNumeric[T]) = x == n.zero

  /** Compares the given value with 1 of the given ExactNumeric instance. */
  @inline final def isOne[T](x: T, n: ExactNumeric[T]) = x == n.fromInt(1)
}
