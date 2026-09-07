package sigmastate.lang.parsers

import fastparse._
import NoWhitespace._
import fastparse.CharPredicates._
import sigma.ast.SourceContext
import sigma.data.Nullable
import sigma.exceptions.CompilerException

/** Basic lexical parsers for ErgoScript. */
object Basic {
  val digits = "0123456789"
  val hexDigits: String = digits + "abcdefABCDEF"

  /** Matches a decimal digit. */
  def Digit[Ctx:P]: P[Unit] = P( CharPred(digits.contains(_)) )
  /** Matches a hexadecimal digit. */
  def HexDigit[Ctx:P]: P[Unit] = P( CharPred(hexDigits.contains(_)) )

  def UnicodeEscape[Ctx:P]: P[Unit] = P( "u" ~ HexDigit ~ HexDigit ~ HexDigit ~ HexDigit )

  //Numbers and digits
  /** Matches a positive hexadecimal number. */
  def HexNum[Ctx:P]: P[Unit] = P( "0x" ~ CharsWhile(hexDigits.contains(_), 1) )
  /** Matches a positive decimal number. */
  def DecNum[Ctx:P]: P[Unit] = P( CharsWhile(digits.contains(_), 1) )
  /** Matches a exponent part of floating point number. */
  def Exp[Ctx:P]: P[Unit] = P( CharPred("Ee".contains(_)) ~ CharPred("+-".contains(_)).? ~ DecNum )

  def FloatType[Ctx:P]: P[Unit] = P( CharIn("fFdD") )

  /** Matches a sequience of whitespace character. */
  def WSChars[Ctx:P]: P[Unit] = P( CharsWhileIn("\u0020\u0009") )
  def Newline[Ctx:P]: P[Unit] = P( StringIn("\r\n", "\n") )
  def Semi[Ctx:P]: P[Unit] = P( ";" | Newline.rep(1) )

  /** Matches a single operation character. */
  def OpChar[Ctx:P]: P[Unit] = P ( CharPred(isOpChar) )

  /** Characters allowed in as operations. */
  def isOpChar(c: Char): Boolean = c match{
    case '!' | '#' | '%' | '&' | '*' | '+' | '-' | '/' |
         ':' | '<' | '=' | '>' | '?' | '@' | '\\' | '^' | '|' | '~' => true
    case _ => isOtherSymbol(c) || isMathSymbol(c)
  }

  /** Character allowed in identificators. */
  def LetterDigitDollarUnderscore[Ctx:P]: P[Unit] =  P(
    CharPred(c => isLetter(c) | isDigit(c) | c == '$' | c == '_' )
  )
  /** Lower-case letters. */
  def Lower[Ctx:P]: P[Unit] = P( CharPred(c => isLower(c) || c == '$' | c == '_') )
  /** Upper-case letters. */
  def Upper[Ctx:P]: P[Unit] = P( CharPred(isUpper) )

  def error(msg: String, srcCtx: Option[SourceContext]) = throw new ParserException(msg, srcCtx)
  def error(msg: String, srcCtx: Nullable[SourceContext]) = throw new ParserException(msg, srcCtx.toOption)
}

/** Exception thrown during the parsing phase of the compiler.
  *
  * @param message the error message
  * @param source an optional source context with location information
  */
class ParserException(message: String, source: Option[SourceContext])
  extends CompilerException(message, source)

/**
  * Most keywords don't just require the correct characters to match,
  * they have to ensure that subsequent characters *don't* match in
  * order for it to be a keyword. This enforces that rule for key-words
  * (W) and key-operators (O) which have different non-match criteria.
  */
object Key {
  def W[Ctx:P](s: String) = P( s ~ !Basic.LetterDigitDollarUnderscore )(sourcecode.Name(s"`$s`"), implicitly[P[_]])
  // If the operator is followed by a comment, stop early so we can parse the comment
  def O[Ctx:P](s: String) = P( s ~ (!Basic.OpChar | &("/*" | "//")) )(sourcecode.Name(s"`$s`"), implicitly[P[_]])
}
