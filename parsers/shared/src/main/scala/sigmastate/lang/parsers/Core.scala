package sigmastate.lang.parsers

import sigma.ast.syntax.SValue
import sigma.ast._
import sigmastate.lang.parsers

/** Keywords and identifiers used in expressions. */
trait Core extends parsers.Literals with CoreUnderscore {
  import fastparse._
  import ScalaWhitespace._

  /** Constructor of ErgoTree unary operation. */
  def mkUnaryOp(opName: String, arg: Value[SType]): Value[SType]
  /** Constructor of ErgoTree binary operation. */
  def mkBinaryOp(l: Value[SType], opName: String, r: Value[SType]): Value[SType]

  // Aliases for common things. These things are used in almost every parser
  // in the file, so it makes sense to keep them short.

  import Key._

  // Keywords that match themselves and nothing else
  def `=>`[Ctx:P] = O("=>") | O("⇒")
  def `:`[Ctx:P] = O(":")
  def `=`[Ctx:P] = O("=")
  def `@`[Ctx:P] = O("@")
  def `type`[Ctx:P] = W("type")
  def `val`[Ctx:P] = W("val")
  def `def`[Ctx:P] = W("def")
  def `case`[Ctx:P] = W("case")
  def `else`[Ctx:P] = W("else")
  def `if`[Ctx:P] = W("if")
  def `match`[Ctx:P] = W("match")
  def `this`[Ctx:P] = W("this")
  def `super`[Ctx:P] = W("super")
  def `with`[Ctx:P] = W("with")
  def `extends`[Ctx:P] = W("extends")
  def `implicit`[Ctx:P] = W("implicit")
  def `new`[Ctx:P] = W("new")
  def `lazy`[Ctx:P] = W("lazy")
  def `>:`[Ctx:P] = O(">:")
  def `<:`[Ctx:P] = O("<:")

  // kinda-sorta keywords that are common patterns even if not
  // really-truly keywords
  def `*`[Ctx:P] = O("*")
  def `_*`[Ctx:P] = P( Underscore ~ `*` )
  def `}`[Ctx:P] = P( Semis.? ~ "}" )
  def `{`[Ctx:P] = P( "{" ~ Semis.? )

  def Id[Ctx:P] = P( WL ~ Identifiers.Id )
  def VarId[Ctx:P] = P( WL ~ Identifiers.VarId )
  def BacktickId[Ctx:P] = P( WL ~ Identifiers.BacktickId )
  def ExprLiteral[Ctx:P] = P( WL ~ Literals.Expr.Literal )

  /**
   * Sketchy way to whitelist a few suffixes that come after a . select;
   * apart from these and IDs, everything else is illegal
   */
  def PostDotCheck[Ctx:P]: P0 = P( WL ~ !(`super` | `this` | "{" |  Underscore | `type`) )
  def StableId[Ctx:P] = {
    def IdPath = P( Index ~ Id.! ~ ("." ~ PostDotCheck ~/ Index ~ (`this`.! | Id.!)).rep ).map {
      case (hi, hs, t) => t.foldLeft[SValue](atSrcPos(hi){builder.mkIdent(hs, NoType)}){
        case (obj, (i, s)) => atSrcPos(i) { builder.mkSelect(obj, s) }
      }
    }
    P( IdPath )
  }
}
