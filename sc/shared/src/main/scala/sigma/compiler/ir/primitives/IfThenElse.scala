package sigma.compiler.ir.primitives

import sigma.compiler.ir.{Base, IRContext}

/** Defines IR representation of conditional expressions `if (c) t else e`. */
trait IfThenElse extends Base { self: IRContext =>

  /** If c then t else e construction with standard lazy evaluation of branches.
    * The representation uses Thunk for each branch */
  def IF(cond: Ref[Boolean]): IfBranch = new IfBranch(cond)

  /** Defines syntax available after `IF (cond) ` */
  class IfBranch(cond: Ref[Boolean]) {
    def apply[T](thenp: => Ref[T]) = THEN(thenp)

    def THEN[T](thenp: => Ref[T]) = new ThenIfBranch[T](cond, thenp)
  }

  /** Defines syntax available after `IF (cond) THEN thenp ELSEIF (cond1) ` */
  class ElseIfBranch[T](cond: Ref[Boolean], outer: ThenIfBranch[T]) {
    def apply(thenp: => Ref[T]) = THEN(thenp)

    def THEN(thenp: => Ref[T]) = new ThenIfBranch[T](cond, thenp) {
      override def ELSE(elsep: => Ref[T]) = outer.elseIf(cond, thenp, elsep)
    }
  }

  /** Defines syntax available after `IF (cond) THEN thenp ` */
  class ThenIfBranch[T](cond: Ref[Boolean], thenp: => Ref[T]) {
    def ELSE(elsep: => Ref[T]): Ref[T] = ifThenElseLazy(cond, thenp, elsep)

    def elseIf(cond1: => Ref[Boolean], thenp1: => Ref[T], elsep1: => Ref[T]) =
      ELSE(ifThenElseLazy(cond1, thenp1, elsep1))

    def ELSEIF(cond1: => Ref[Boolean]) = new ElseIfBranch[T](cond1, this)
  }

  /** IR node to represent IF condition with lazy branches. */
  case class IfThenElseLazy[T](cond: Ref[Boolean], thenp: Ref[Thunk[T]], elsep: Ref[Thunk[T]]) extends Def[T] {
    lazy val resultType = {
      val eThen = thenp.elem.eItem
      val eElse = elsep.elem.eItem
      assert(eThen == eElse, s"Both branched of IfThenElseLazy should have the same type, but was $eThen and $eElse")
      eThen
    }
    override def transform(t: Transformer) = IfThenElseLazy(t(cond), t(thenp), t(elsep))
  }

  /** Constructs IfThenElse node wrapping by-name args into ThunkDef nodes. */
  def ifThenElseLazy[T](cond: Ref[Boolean], thenp: => Ref[T], elsep: => Ref[T]): Ref[T] = {
    val t = Thunk(thenp)
    val e = Thunk(elsep)
    IfThenElseLazy(cond, t, e)
  }

}
