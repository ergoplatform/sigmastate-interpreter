package sigmastate.lang.parsers

import sigma.ast.syntax.SValue
import sigma.ast._
import sigmastate.lang.parsers

/** Keywords and identifiers used in expressions. */
trait Core extends parsers.Literals {
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
  def `=>`[_:P] = O("=>") | O("⇒")
  def `:`[_:P] = O(":")
  def `=`[_:P] = O("=")
  def `@`[_:P] = O("@")
  def `_`[_:P] = W("_")
  def `type`[_:P] = W("type")
  def `val`[_:P] = W("val")
  def `def`[_:P] = W("def")
  def `case`[_:P] = W("case")
  def `else`[_:P] = W("else")
  def `if`[_:P] = W("if")
  def `match`[_:P] = W("match")
  def `this`[_:P] = W("this")
  def `super`[_:P] = W("super")
  def `with`[_:P] = W("with")
  def `extends`[_:P] = W("extends")
  def `implicit`[_:P] = W("implicit")
  def `new`[_:P] = W("new")
  def `lazy`[_:P] = W("lazy")
  def `>:`[_:P] = O(">:")
  def `<:`[_:P] = O("<:")

  // kinda-sorta keywords that are common patterns even if not
  // really-truly keywords
  def `*`[_:P] = O("*")
  def `_*`[_:P] = P( `_` ~ `*` )
  def `}`[_:P] = P( Semis.? ~ "}" )
  def `{`[_:P] = P( "{" ~ Semis.? )

  def Id[_:P] = P( WL ~ Identifiers.Id )
  def VarId[_:P] = P( WL ~ Identifiers.VarId )
  def BacktickId[_:P] = P( WL ~ Identifiers.BacktickId )
  def ExprLiteral[_:P] = P( WL ~ Literals.Expr.Literal )

  /**
   * Sketchy way to whitelist a few suffixes that come after a . select;
   * apart from these and IDs, everything else is illegal
   */
  def PostDotCheck[_:P]: P0 = P( WL ~ !(`super` | `this` | "{" |  `_` | `type`) )
  def StableId[_:P] = {
    def IdPath = P( Index ~ Id.! ~ ("." ~ PostDotCheck ~/ Index ~ (`this`.! | Id.!)).rep ).map {
      case (hi, hs, t) => t.foldLeft[SValue](atSrcPos(hi){builder.mkIdent(hs, NoType)}){
        case (obj, (i, s)) => atSrcPos(i) { builder.mkSelect(obj, s) }
      }
    }
    P( IdPath )
  }
}
