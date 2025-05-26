package sigmastate.lang

import fastparse.internal.Logger
import sigma.ast._
import sigma.data.Nullable
import sigma.ast.syntax._
import sigmastate.lang.parsers.{Basic, Core, Exprs}

import scala.collection.mutable
import scala.util.DynamicVariable

/** Main facade to ErgoScript parser implementation. */
object SigmaParser extends Exprs with Types with Core { parser =>
  import fastparse._
  import ScalaWhitespace._
  import builder._

  private val currentInput = new DynamicVariable[String]("")

  override def atSrcPos[A](parserIndex: Int)(thunk: => A): A =
    builder.currentSrcCtx.withValue(Nullable(srcCtx(parserIndex))) { thunk }

  override def srcCtx(parserIndex: Int): SourceContext =
    SourceContext.fromParserIndex(parserIndex, currentInput.value)

  override def ValVarDef[_:P] = P( Index ~ BindPattern ~ (`:` ~/ Type).? ~ (`=` ~/ FreeCtx.Expr) ).map {
    case (index, Ident(n,_), t, body) =>
      atSrcPos(index) {
        mkVal(n, t.getOrElse(NoType), body)
      }
    case (index, pat,_,_) =>
      Basic.error(s"Only single name patterns supported but was $pat", Some(srcCtx(index)))
  }

  override def BlockDef[_:P] = P( Dcl )

  private val logged = mutable.Buffer.empty[String]
  implicit val logger: Logger = Logger(m => this.synchronized { logged.append(m) })

  override def mkUnaryOp(opName: String, arg: Value[SType]) =
    builder.currentSrcCtx.withValue(arg.sourceContext) {
      opName match {
        case "-" if arg.isInstanceOf[Constant[_]] && arg.tpe.isNumType =>
          arg match {
            case IntConstant(value) =>
              mkConstant[SInt.type](-value, SInt)
            case LongConstant(value) =>
              mkConstant[SLong.type](-value, SLong)
            case _ => Basic.error(s"cannot prefix $arg with op $opName", arg.sourceContext)
          }

        case "!" => mkLogicalNot(arg.asBoolValue)

        case "-" =>
          if (arg.tpe.isNumTypeOrNoType)
            mkNegation(arg.asNumValue)
          else
            Basic.error(s"Numeric argument expected for '$opName' operation: $arg", arg.sourceContext)

        case "~" =>
          if (arg.tpe.isNumTypeOrNoType)
            mkBitInversion(arg.asNumValue)
          else
            Basic.error(s"Numeric argument expected for '$opName' operation: $arg", arg.sourceContext)

        case _ =>
          Basic.error(s"Unknown prefix operation $opName for $arg", arg.sourceContext)
      }
    }

  private val parseAsMethods = Set("*", "++", "||", "&&", "+", "^", "<<", ">>", ">>>")

  override def mkBinaryOp(l: Value[SType], opName: String, r: Value[SType]): Value[SType] =
    builder.currentSrcCtx.withValue(l.sourceContext) {
      opName match {
        case "==" => mkEQ(l, r)
        case "!=" => mkNEQ(l, r)
        case ">=" => mkGE(l, r)
        case ">" => mkGT(l, r)
        case "<=" => mkLE(l, r)
        case "<" => mkLT(l, r)
        case "-" => mkMinus(l.asNumValue, r.asNumValue)

        case "|" =>
          if (l.tpe.isNumTypeOrNoType && r.tpe.isNumTypeOrNoType)
            mkBitOr(l.asNumValue, r.asNumValue)
          else
            Basic.error(s"Numeric arguments expected for '$opName' operation: ($l, $r)", l.sourceContext)

        case "&" =>
          if (l.tpe.isNumTypeOrNoType && r.tpe.isNumTypeOrNoType)
            mkBitAnd(l.asNumValue, r.asNumValue)
          else
            Basic.error(s"Numeric arguments expected for '$opName' operation: ($l, $r)", l.sourceContext)

        case _ if parseAsMethods.contains(opName) => mkMethodCallLike(l, opName, IndexedSeq(r))
        case "/" => mkDivide(l.asNumValue, r.asNumValue)
        case "%" => mkModulo(l.asNumValue, r.asNumValue)
        case _ => Basic.error(s"Unknown binary operation $opName", l.sourceContext)
      }
    }

  private def parsedType(str: String): Parsed[SType] = parse(str, implicit p => Type ~ End)

  /** Parse `str` into SType.
    * @param str string representation of type in ErgoScript syntax
    */
  def parseType(str: String): SType = {
    val res = parsedType(str).get.value
    res
  }

  /** Parse `script` into ErgoTree expression. */
  def apply(script: String): Parsed[Value[_ <: SType]] =
    currentInput.withValue(script) {
      parse(script, implicit p => (StatCtx.Expr ~ End))
    }
}
