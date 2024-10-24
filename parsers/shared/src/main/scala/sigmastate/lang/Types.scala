package sigmastate.lang

import fastparse._
import ScalaWhitespace._
import sigmastate.lang.parsers.Core
import parsers.Basic.error
import sigma.ast._

//noinspection ForwardReference
/** Parsers of type terms. Can produce values of SType. */
trait Types extends Core {
  /** Parser of typed expressions.
    * @return expression of ErgoTree IR
    */
  def TypeExpr[_:P]: P[Value[SType]]

  /** Parser of `name = expr` syntax.
    * @return an instance of ValNode
    */
  def ValVarDef[_:P]: P[Value[SType]]

  /** Parser of `val name = expr` syntax.
    * @return an instance of ValNode
    */
  def Dcl[_:P]: P[Value[SType]] = {
    P( `val` ~/ ValVarDef )
  }

  /** This map should be in sync with SType.allPredefTypes */
  val predefTypes = Map(
    "Boolean" -> SBoolean,
    "Byte"    -> SByte,
    "Short"   -> SShort,
    "Int"     -> SInt,
    "Long"    -> SLong,
    "BigInt"  -> SBigInt,
    "UnsignedBigInt"  -> SUnsignedBigInt,
    "AvlTree" -> SAvlTree,
    "Context" -> SContext,
    "GroupElement" -> SGroupElement,
    "SigmaProp"   -> SSigmaProp,
    "Global" -> SGlobal,
    "Header" -> SHeader,
    "PreHeader" -> SPreHeader,
    "String" -> SString,
    "Box" -> SBox,
    "Unit" -> SUnit,
    "Any" -> SAny
  )

  /** Lookup pre-defined type by name. */
  private def typeFromName(tn: String): Option[SType] = predefTypes.get(tn)

  def PostfixType[_:P]: P[SType] = P( InfixType ~ (`=>` ~/ Type ).? ).map {
    case (t, None) => t
    case (d, Some(r)) => d match {
      case STuple(items) =>
        SFunc(items, r)
      case _ =>
        SFunc(Array(d), r)
    }
  }
  def Type[_:P]: P[SType] = P( `=>`.? ~~ PostfixType ~ TypeBounds ~ `*`.? )


  // Can't cut after `Id` because it may be a `*`, in which case
  // we may need to backtrack and settle for the `*`-postfix rather than
  // an infix type
  // See http://www.scala-lang.org/files/archive/spec/2.12/03-types.html
  def InfixType[_:P]: P[SType] = {
    val RightAssoc = 1; val LeftAssoc = -1
    /** All operators op1,…,opn must have the same associativity */
    def checkAssoc(ops: Seq[String], index: Int): Int = {
      val right = ops.forall(_.endsWith(":"))
      if (right) RightAssoc
      else {
        val left = ops.forall(!_.endsWith(":"))
        if (left) LeftAssoc
        else
          error(s"All operators $ops must have the same associativity.", Some(srcCtx(index)))
      }
    }
    def buildInfix(head: SType, tail: Seq[(String, SType)], index: Int): SType = {
      val associativity = checkAssoc(tail.map(_._1), index)
      if (associativity == RightAssoc) {
        tail.foldRight(head) { case ((op, t), acc) => STypeApply(op, IndexedSeq(t, acc)) }
      }
      else {
        tail.foldLeft(head) { case (acc, (op, t)) => STypeApply(op, IndexedSeq(acc, t)) }
      }
    }
    P( Index ~ CompoundType ~~ (NotNewline ~ Id.! ~~ OneNLMax ~ CompoundType).repX ).map {
      case (index, t, h) => buildInfix(t, h, index)
    }
  }

  def CompoundType[_:P]: P[SType] = {
    def NamedType = P( (Pass ~ AnnotType).rep(1, `with`./) )
    P( Index ~ NamedType ).map {
      case (_, Seq(t)) => t
      case (index, ts) => error(s"Compound types are not supported: $ts", Some(srcCtx(index)))
    }
  }

  private def NLAnnot[_:P] = P( NotNewline ~ Annot )
  def AnnotType[_:P] = P(SimpleType ~~ NLAnnot.repX )

  def TypeId[_:P] = P( StableId ).map {
    case Ident(tn, _) =>
      typeFromName(tn) match {
        case Some(t) => t
        case None => STypeApply(tn)
      }
    case path => error(s"Path types are not supported: $path", path.sourceContext)
  }

  def TypeArgs[_:P] = P( "[" ~/ Type.rep(0, ",") ~ TrailingComma ~ "]" )

  def SimpleType[_:P] = {
    // Can't `cut` after the opening paren, because we might be trying to parse `()`
    // or `() => T`! only cut after parsing one type
    def TupleType = P( "(" ~/ Type.rep(0, ",") ~ TrailingComma ~ ")" ).map(items => STuple(items.toArray))
    def BasicType = P( TupleType | TypeId )
    P( Index ~ BasicType ~ TypeArgs.rep ).map {
      case (_, t: STuple, Seq()) => t
      case (_, STypeApply("Coll", IndexedSeq()), Seq(Seq(t))) => SCollection(t)
      case (_, STypeApply("Option", IndexedSeq()), Seq(Seq(t))) => SOption(t)
      case (_, SPrimType(t), Seq()) => t
      case (_, STypeApply(tn, IndexedSeq()), args) if args.isEmpty => STypeVar(tn)
      case (index, t, typeArgs) =>
        error(s"Unsupported type $t[$typeArgs]", Some(srcCtx(index)))
    }
  }

  /** Parses [T1,T2](a1: T, a2: S) */
  def FunSig[_:P] = {
    def FunArg = P( Annot.rep ~ Id.! ~ (`:` ~/ Type).? ).map {
      case (n, Some(t)) => (n, t)
      case (n, None) => (n, NoType)
    }
    def Args = P( FunArg.rep(1, ",") ~ TrailingComma )
    def FunArgs = P( OneNLMax ~ "(" ~/ Args.? ~ ")" ).map(_.toSeq.flatten)
    def FunTypeArgs = P( "[" ~/ (Annot.rep ~ TypeArg).rep(1, ",") ~ TrailingComma ~ "]" )
    P( FunTypeArgs.? ~~ FunArgs.rep )
  }

  // TODO refactor: extensions syntax is not fully implemented, so this probably can be removed
  // extension method subject (type that being extended)
  // see dotty extension method http://dotty.epfl.ch/blog/2019/01/21/12th-dotty-milestone-release.html
  def DottyExtMethodSubj[_:P] = P( "(" ~/ Id.! ~  `:` ~/ Type ~ ")" )

  private def TypeBounds[_:P]: P0 = P( (`>:` ~/ Type).? ~ (`<:` ~/ Type).? ).ignore
  private def TypeArg[_:P]: P0 = {
    def CtxBounds = P((`:` ~/ Type).rep)
    P((Id | `_`) ~ TypeArgList.? ~ TypeBounds ~ CtxBounds).ignore
  }

  /** Annotation with optional arguments, result is ignored. */
  def Annot[_:P]: P0 = P( `@` ~/ SimpleType ~  ("(" ~/ (Exprs ~ (`:` ~/ `_*`).?).? ~ TrailingComma ~ ")").rep ).ignore

  private def TypeArgVariant[_:P]: P0 = P( Annot.rep ~ ("+" | "-").? ~ TypeArg )

  private def TypeArgList[_:P]: P0 = {
    P( "[" ~/ TypeArgVariant.rep(1, ",") ~ TrailingComma ~ "]" )  // fix
  }

  /** Sequence of comma separated expressions. */
  def Exprs[_:P]: P[Seq[Value[SType]]] = P( TypeExpr.rep(1, ",") )
}
