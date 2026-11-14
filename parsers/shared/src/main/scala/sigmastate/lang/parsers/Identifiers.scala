package sigmastate.lang.parsers

import fastparse.CharPredicates.{isDigit, isLetter}
import fastparse._
import NoWhitespace._
import sigmastate.lang.parsers.Basic._

//noinspection ForwardReference
/** Identifiers and keywords */
object Identifiers {
  /** Helper wrapper to capture the name of the use site. */
  private case class NamedFunction(f: Char => Boolean)
      (implicit name: sourcecode.Name) extends (Char => Boolean){
    def apply(t: Char): Boolean = f(t)
    override def toString: String = name.value
  }

  private val OpCharNotSlash = NamedFunction(x => isOpChar(x) && x != '/')

  private val NotBackTick = NamedFunction(_ != '`')

  private def Operator[Ctx:P]: P[Unit] = P(
    !Keywords ~ (!("/*" | "//") ~ (CharsWhile(OpCharNotSlash) | "/")).rep(1)
  )

  def VarId[Ctx:P]: P[Unit] = VarId0(true)

  private def VarId0[Ctx:P](dollar: Boolean): P[Unit] = P( !Keywords ~ Lower ~ IdRest(dollar) )

  private def UppercaseId[Ctx: P](dollar: Boolean): P[Unit] = P( !Keywords ~ Upper ~ IdRest(dollar) )

  def PlainId[Ctx:P]: P[Unit] = P( UppercaseId(true) | VarId | Operator ~ (!OpChar | &("/*" | "//")) )
    .opaque("plain-id")

  def PlainIdNoDollar[Ctx:P]: P[Unit] = P( !Keywords ~ Upper ~ IdRest(false) | VarId0(false) | Operator )
  def BacktickId[Ctx:P]: P[Unit] = P( "`" ~ CharsWhile(NotBackTick) ~ "`" )
  def Id[Ctx:P]: P0 = P( BacktickId | PlainId )

  private def IdRest[Ctx:P](allowDollar: Boolean): P[Unit] = {

    def IdCharacter: NamedFunction =
      if(allowDollar) NamedFunction(c => c == '$' || isLetter(c) || isDigit(c))
      else NamedFunction(c => isLetter(c) || isDigit(c))

    def IdUnderscoreChunk: P[Unit] = P( CharsWhileIn("_", 0) ~ CharsWhile(IdCharacter) )

    P( IdUnderscoreChunk.rep ~ (CharsWhileIn("_") ~ CharsWhile(isOpChar, 0)).? )
  }

  private def AlphabetKeywords[Ctx:P]: P[Unit] = P {
    StringIn("case", "else", "false", "function", "if", "match", "return", "then", "true") ~
      !Basic.LetterDigitDollarUnderscore
  }

  private def SymbolicKeywords[Ctx:P]: P[Unit] = P{
    (":" | ";" | "=>" | "=" | "#" | "@") ~ !OpChar
  }

  def Keywords[Ctx:P]: P[Unit] = P( AlphabetKeywords | SymbolicKeywords )
}
