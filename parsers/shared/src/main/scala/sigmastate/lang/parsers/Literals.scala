package sigmastate.lang.parsers

import fastparse.NoWhitespace._
import fastparse._
import sigma.ast.syntax.BooleanConstant
import sigma.ast._
import sigmastate.lang.parsers.Identifiers._
import java.lang.Integer.parseInt
import java.lang.Long.parseLong

/** Parsers of literal expressions. */
trait Literals { l =>
  /** A builder instance used by the parsers to create ErgoTree expressions. */
  val builder: SigmaBuilder = StdSigmaBuilder
  import builder._

  /** Set the current source position (dynamic variable) and execute the given thunk. */
  def atSrcPos[A](parserIndex: Int)(thunk: => A): A

  /** Create SourceContext using current input string and the given index. */
  def srcCtx(parserIndex: Int): SourceContext

  /** Parses simple blocks `{ ... }` */
  def Block[_:P]: P[Value[SType]]

  /** Parses pattern, like in the expression `val <pattern> = expr` */
  def Pattern[_:P]: P0

  implicit class ParserOps[+T](p: P[T]) {
    /** Ignores the result produced by `p`. */
    def ignore: P[Unit] = p.map(_ => ())
  }
  
  /**
    * Parses all whitespace, excluding newlines. This is only
    * really useful in e.g. {} blocks, where we want to avoid
    * capturing newlines so semicolon-inference would work
    */
  def WS[_:P]: P[Unit] = P( NoCut(NoTrace((Basic.WSChars | Literals.Comment).rep)) )

  /**
    * Parses whitespace, including newlines.
    * This is the default for most things
    */
  def WL0[_: P]: P[Unit] =
    P( NoTrace((Basic.WSChars | Literals.Comment | Basic.Newline).rep) )(sourcecode.Name("WL"), implicitly[P[_]])

  def WL[_:P]: P[Unit] = P( NoCut(WL0) )

  def Semi[_:P]: P[Unit] = P( WS ~ Basic.Semi )
  def Semis[_:P]: P[Unit] = P( Semi.rep(1) ~ WS )
  def Newline[_:P]: P[Unit] = P( WL ~ Basic.Newline )

  /** Look ahead whitespaces, but not new line. */
  def NotNewline[_:P]: P0 = P( &( WS ~ !Basic.Newline ) )

  def OneNLMax[_:P]: P0 = {
    def ConsumeComments = P( (Basic.WSChars.? ~ Literals.Comment ~ Basic.WSChars.? ~ Basic.Newline).rep )
    P( NoCut( WS ~ Basic.Newline.? ~ ConsumeComments ~ NotNewline) )
  }

  /** Parses optional trailing comma. */
  def TrailingComma[_:P]: P0 = P( ("," ~ WS ~ Basic.Newline).? )

  //noinspection ForwardReference
  object Literals{
    import Basic._

    /** Decimal or hex integers or longs. */
    def Int[_:P]: P[Unit] = P( (HexNum | DecNum) ~ CharIn("Ll").? )

    def Bool[_:P]: P[BooleanConstant] =
      P( (Index ~ (Key.W("true").! | Key.W("false").!)).map { case (i, lit) =>
        atSrcPos(i) {
          mkConstant[SBoolean.type](if (lit == "true") true else false, SBoolean)
        }
      })

    // Comments cannot have cuts in them, because they appear before every
    // terminal node. That means that a comment before any terminal will
    // prevent any backtracking from working, which is not what we want!
    def CommentChunk[_:P]: P[Unit] = P( CharsWhile(c => c != '/' && c != '*') | MultilineComment | !"*/" ~ AnyChar )
    def MultilineComment[_:P]: P0 = P( "/*" ~/ CommentChunk.rep ~ "*/" )
    def SameLineCharChunks[_:P]: P[Unit] = P( CharsWhile(c => c != '\n' && c != '\r')  | !Basic.Newline ~ AnyChar )
    def LineComment[_:P]: P[Unit] = P( "//" ~ SameLineCharChunks.rep ~ &(Basic.Newline | End) )
    def Comment[_:P]: P0 = P( MultilineComment | LineComment )

    def Null[_:P]: P[Unit] = Key.W("null")

    def OctalEscape[_:P]: P[Unit] = P( Digit ~ Digit.? ~ Digit.? )
    def Escape[_:P]: P[Unit] = P( "\\" ~/ (CharIn("""btnfr'\"]""") | OctalEscape | UnicodeEscape ) )

    // Note that symbols can take on the same values as keywords!
    def Symbol[_:P]: P[Unit] = P( Identifiers.PlainId | Identifiers.Keywords )

    def Char[_:P]: P[Unit] = {
      // scalac 2.10 crashes if PrintableChar below is substituted by its body
      def PrintableChar = CharPred(CharPredicates.isPrintableChar)

      P( (Escape | PrintableChar) ~ "'" )
    }

    class InterpCtx(interp: Option[() => P0]){
      //noinspection TypeAnnotation
      def Literal[_:P] = P(
        ("-".!.? ~ Index ~ ( /*Float |*/ Int.!)).map {
            case (signOpt, index, lit) =>
              val sign = if (signOpt.isDefined) -1 else 1
              val suffix = lit.charAt(lit.length - 1)
              val (digits, radix) = if (lit.startsWith("0x")) (lit.substring(2), 16) else (lit, 10)
              atSrcPos(index) {
                if (suffix == 'L' || suffix == 'l')
                  mkConstant[SLong.type](sign * parseLong(digits.substring(0, digits.length - 1), radix), SLong)
                else
                  mkConstant[SInt.type](sign * parseInt(digits, radix), SInt)
              }
          }
        | Bool
        | (Index ~ (String | "'" ~/ (Char | Symbol) | Null).!).map { case (index, lit) =>
          // strip single or triple quotes
          def strip(s: String): String = if (!s.startsWith("\"")) s else strip(s.stripPrefix("\"").stripSuffix("\""))
          atSrcPos(index) {
            mkConstant[SString.type](strip(lit), SString)
          }
        })

      private def Interp[_:P]: P[Unit] = interp match{
        case None => P ( Fail )
        case Some(p) => P( "$" ~ Identifiers.PlainIdNoDollar | ("${" ~ p() ~ WL ~ "}") | "$$" )
      }

      private def TQ[_:P]: P[Unit] = P( "\"\"\"" )

      /**
        * Helper to quickly gobble up large chunks of un-interesting
        * characters. We break out conservatively, even if we don't know
        * it's a "real" escape sequence: worst come to worst it turns out
        * to be a dud and we go back into a CharsChunk next rep
        */
      private def StringChars[_:P]: P[Unit] = P( CharsWhile(c => c != '\n' && c != '"' && c != '\\' && c != '$') )
      private def NonTripleQuoteChar[_:P]: P[Unit] = P( "\"" ~ "\"".? ~ !"\"" | CharIn("\\$\n") )
      def TripleChars[_:P]: P[Unit] = P( (StringChars | Interp | NonTripleQuoteChar).rep )
      private def TripleTail[_:P]: P[Unit] = P( TQ ~ "\"".rep )
      def SingleChars[_:P](allowSlash: Boolean): P[Unit] = {
        def LiteralSlash = P( if(allowSlash) "\\" else Fail )
        def NonStringEnd = P( !CharIn("\n\"") ~ AnyChar )
        P( (StringChars | Interp | LiteralSlash | Escape | NonStringEnd ).rep )
      }
      def String[_:P]: P[Unit] = {
        P {
          (Id ~ TQ ~/ TripleChars ~ TripleTail) |
              (Id ~ "\"" ~/ SingleChars(true)  ~ "\"") |
              (TQ ~/ NoInterp.TripleChars ~ TripleTail) |
              ("\"" ~/ NoInterp.SingleChars(false) ~ "\"")
        }
      }

    }
    object NoInterp extends InterpCtx(None)
    def Expr[_:P] = new InterpCtx(Some(() => Block))
  }
}

