package sigma.compiler.phases

import org.ergoplatform.ErgoAddressEncoder.NetworkPrefix
import sigma.ast.NoType
import sigma.data.Nullable
import sigma.kiama.rewriting.{CallbackRewriter, Strategy}
import sigma.ast._
import sigma.ast.syntax.SValue
import sigmastate.interpreter.Interpreter.ScriptEnv
import SigmaPredef.PredefinedFuncRegistry
import sigma.ast.syntax._
import sigma.exceptions.{BinderException, InvalidArguments}

object SrcCtxCallbackRewriter extends CallbackRewriter {
  override def rewriting[T](oldTerm: T, newTerm: T): T = (oldTerm, newTerm) match {
    case (o: SValue, n: SValue) if o.sourceContext.isDefined && n.sourceContext.isEmpty =>
      n.withSrcCtx(o.sourceContext).asInstanceOf[T]
    case _ => newTerm
  }
}

/**
  * @param env
  * @param builder
  * @param networkPrefix network prefix to decode an ergo address from string (PK op)
  */
class SigmaBinder(env: ScriptEnv, builder: SigmaBuilder,
                  networkPrefix: NetworkPrefix,
                  predefFuncRegistry: PredefinedFuncRegistry) {
  import SigmaBinder._
  import builder._
  import SrcCtxCallbackRewriter._

  private val PKFunc = predefFuncRegistry.PKFunc(networkPrefix)

  /** Rewriting of AST with respect to environment to resolve all references to global names
    * and infer their types. */
  private def eval(e: SValue, env: ScriptEnv, boundNames: Set[String] = Set.empty): SValue = {
    val scopeRule: Strategy = strategy[Any] {
      // Rule: lambda (...) = ... --> lambda (...): T = ...
      case lam @ Lambda(params, args, t, Some(body)) =>
        require(params.isEmpty)
        val argNames = args.map(_._1).toSet
        val b1 = eval(body, env -- argNames, boundNames ++ argNames)
        val newLam = mkLambda(args, t, Some(b1))
        Some(if (newLam != lam) newLam else lam)

      // Rule: { e } --> e
      case Block(Seq(), body) =>
        Some(eval(body, env, boundNames))

      case block @ Block(binds, t) =>
        val newBinds = for (v @ Val(n, vt, b) <- binds) yield {
          if (env.contains(n)) error(s"Variable $n already defined ($n = ${env(n)}", v.sourceContext)
          val b1 = eval(b, env, boundNames)
          builder.currentSrcCtx.withValue(v.sourceContext) {
            mkVal(n, if (vt != NoType) vt else b1.tpe, b1)
          }
        }
        val t1 = eval(t, env, boundNames)
        val newBlock = mkBlock(newBinds, t1)
        Some(if (newBlock != block) newBlock else block)
    }

    val exprRule: Strategy = strategy[Any] {
      case i @ Ident(n, NoType) if !boundNames.contains(n) => env.get(n) match {
        case Some(v) =>
          liftAny(v).toOption.map { lifted =>
            val withCtx = lifted.withPropagatedSrcCtx(i.sourceContext)
            v match {
              case _: SValue => eval(withCtx, env, boundNames)
              case _ => withCtx
            }
          }
        case None => n match {
          case "HEIGHT" => Some(Height)
          case "MinerPubkey" => Some(MinerPubkey)
          case "INPUTS" => Some(Inputs)
          case "OUTPUTS" => Some(Outputs)
          case "LastBlockUtxoRootHash" => Some(LastBlockUtxoRootHash)
          case "EmptyByteArray" => Some(ByteArrayConstant(Array.emptyByteArray))
          case "SELF" => Some(Self)
          case "CONTEXT" => Some(Context)
          case "Global" => Some(Global)
          case _ => None
        }
      }

      // Rule: Coll[Int](...) -->
      case _ @ Apply(ApplyTypes(Ident("Coll", _), Seq(tpe)), args) if !boundNames.contains("Coll") =>
        Some(mkConcreteCollection(args, tpe))

      // Rule: Coll(...) -->
      case Apply(Ident("Coll", _), args) if !boundNames.contains("Coll") =>
        val tpe = if (args.isEmpty) NoType else args(0).tpe
        Some(mkConcreteCollection(args, tpe))

      // Rule: min(x, y) -->
      case Apply(i @ Ident("min", _), args) if !boundNames.contains("min") => args match {
        case Seq(l: SValue, r: SValue) =>
          Some(mkMin(l.asNumValue, r.asNumValue))
        case _ =>
          throw new InvalidArguments(s"Invalid arguments for min: $args", i.sourceContext.toOption)
      }

      // Rule: max(x, y) -->
      case Apply(i @ Ident("max", _), args) if !boundNames.contains("max") => args match {
        case Seq(l: SValue, r: SValue) =>
          Some(mkMax(l.asNumValue, r.asNumValue))
        case _ =>
          throw new InvalidArguments(s"Invalid arguments for max: $args", i.sourceContext.toOption)
      }

      case a @ Apply(PKFunc.symNoType, args) if !boundNames.contains(PKFunc.name) =>
        Some(PKFunc.irInfo.irBuilder(PKFunc.sym, args).withPropagatedSrcCtx(a.sourceContext))

      case a @ Apply(predefFuncRegistry.SerializeFunc.symNoType, args)
        if !boundNames.contains(predefFuncRegistry.SerializeFunc.name) =>
        Some(predefFuncRegistry.SerializeFunc.irInfo.irBuilder(PKFunc.sym, args).withPropagatedSrcCtx(a.sourceContext))

      case sel @ Select(obj, "isEmpty", _) =>
        Some(mkLogicalNot(mkSelect(obj, "isDefined").asBoolValue).withPropagatedSrcCtx(sel.sourceContext))
    }

    lazy val nonScope: Strategy = all(step) <* attempt(exprRule)
    lazy val step: Strategy = scopeRule <+ nonScope
    rewrite(step)(e)
  }

  def bind(e: SValue): SValue =
    eval(e, env)
}

object SigmaBinder {
  def error(msg: String, srcCtx: Nullable[SourceContext]) = throw new BinderException(msg, srcCtx.toOption)
}
