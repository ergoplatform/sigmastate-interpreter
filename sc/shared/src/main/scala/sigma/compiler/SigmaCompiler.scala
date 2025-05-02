package sigma.compiler

import fastparse.Parsed
import fastparse.Parsed.Success
import sigma.kiama.rewriting.Rewriter.{everywherebu, rewrite, rule}
import org.ergoplatform.ErgoAddressEncoder.NetworkPrefix
import sigma.ast.{Exponentiate, MultiplyGroup, SCollectionMethods, SGlobalMethods, SGroupElementMethods, Value, Xor}
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigma.ast.SigmaPredef.PredefinedFuncRegistry
import sigma.ast.MethodCall
import sigmastate.lang.parsers.ParserException
import sigma.ast._
import sigma.ast.syntax.SValue
import SCollectionMethods.{ExistsMethod, ForallMethod, MapMethod}
import sigma.compiler.ir.{GraphIRReflection, IRContext}
import sigma.compiler.phases.{SigmaBinder, SigmaTyper}
import sigmastate.InterpreterReflection
import sigmastate.lang.{DirectCompiler, SigmaParser}

/**
  * @param networkPrefix    network prefix to decode an ergo address from string (PK op)
  * @param builder          used to create ErgoTree nodes
  * @param lowerMethodCalls if true, then MethodCall nodes are lowered to ErgoTree nodes
  *                         when [[sigma.ast.SMethod.irInfo.irBuilder]] is defined. For
  *                         example, in the `coll.map(x => x+1)` code, the `map` method
  *                         call can be lowered to MapCollection node.
  *                         The lowering if preferable, because it is more compact (1 byte
  *                         for MapCollection instead of 3 bytes for MethodCall).
  * @param enableOptimizations if true, then optimizations are applied to the compiled ErgoTree
  */
case class CompilerSettings(
    networkPrefix: NetworkPrefix,
    builder: SigmaBuilder,
    lowerMethodCalls: Boolean,
    enableOptimizations: Boolean = true
)

/** Result of ErgoScript source code compilation.
  * @param env compiler environment used to compile the code
  * @param code ErgoScript source code
  * @param compiledGraph graph obtained by using new [[GraphBuilding]]
  * @param buildTree ErgoTree expression obtained from graph created by [[GraphBuilding]]
  */
case class CompilerResult[Ctx <: IRContext](
  env: ScriptEnv,
  code: String,
  compiledGraph: Option[Ctx#Ref[Ctx#Context => Any]],
  /** Tree obtained from graph created by GraphBuilding */
  buildTree: SValue
)

/** Compiler which compiles ErgoScript source code into ErgoTree.
  * @param settings compilation parameters \
  */
class SigmaCompiler private(settings: CompilerSettings) {
  /** Constructs an instance for the given network type and with default settings. */
  def this(networkPrefix: Byte) = this(
    CompilerSettings(networkPrefix, TransformingSigmaBuilder, lowerMethodCalls = true)
  )

  @inline final def builder = settings.builder
  @inline final def networkPrefix = settings.networkPrefix

  /** Parses the given ErgoScript source code and produces expression tree. */
  def parse(x: String): SValue = {
    SigmaParser(x) match {
      case Success(v, _) => v
      case f: Parsed.Failure =>
        throw new ParserException(s"Syntax error: $f", Some(SourceContext.fromParserFailure(f)))
    }
  }

  /** Typechecks the given parsed expression and assigns types for all sub-expressions. */
  def typecheck(env: ScriptEnv, parsed: SValue): Value[SType] = {
    val predefinedFuncRegistry = new PredefinedFuncRegistry(builder)
    val binder = new SigmaBinder(env, builder, networkPrefix, predefinedFuncRegistry)
    val bound = binder.bind(parsed)
    val typeEnv = env.collect { case (k, v: SType) => k -> v }
    val typer = new SigmaTyper(builder, predefinedFuncRegistry, typeEnv, settings.lowerMethodCalls)
    val typed = typer.typecheck(bound)
    typed
  }

  def typecheck(env: ScriptEnv, code: String): Value[SType] = {
    val parsed = parse(code)
    typecheck(env, parsed)
  }

  /** Compiles the given ErgoScript source code. */
  def compile(env: ScriptEnv, code: String)(implicit IR: IRContext): CompilerResult[IR.type] = {
    val typed = typecheck(env, code)
    val res = compileTyped(env, typed).copy(code = code)
    res
  }

  /** Compiles the given typed expression. */
  def compileTyped(env: ScriptEnv, typedExpr: SValue)(implicit IR: IRContext): CompilerResult[IR.type] = {
    val placeholdersEnv = env
        .collect { case (name, t: SType) => name -> t }
        .zipWithIndex
        .map { case ((name, t), index) => name -> ConstantPlaceholder(index, t) }
        .toMap
    if (settings.enableOptimizations) {
      val compiledGraph = IR.buildGraph(env ++ placeholdersEnv, typedExpr)
      val compiledTree = IR.buildTree(compiledGraph)
      CompilerResult(env, "<no source code>", Some(compiledGraph), compiledTree)
    } else {
      val dc = new DirectCompiler(settings)
      val compiledTree = dc.compileTyped(typedExpr)
      CompilerResult(env, "<no source code>", None, compiledTree)
    }
  }

  /** Compiles the given parsed contract source. */
  def compileParsed(env: ScriptEnv, parsedExpr: SValue)(implicit IR: IRContext): CompilerResult[IR.type] = {
    val typed = typecheck(env, parsedExpr)
    compileTyped(env, typed)
  }

  /** Unlowering transformation, which replaces some operations with equivalent MethodCall
    * node. This replacement is only defined for some operations.
    * This is inverse to `lowering` which is performed during compilation.
    */
  def unlowerMethodCalls(expr: SValue): SValue = {
    import SCollection._
    val r = rule[Any]({
      case MultiplyGroup(l, r) =>
        MethodCall(l, SGroupElementMethods.MultiplyMethod, Vector(r), Map())
      case Exponentiate(l, r) =>
        MethodCall(l, SGroupElementMethods.ExponentiateMethod, Vector(r), Map())
      case ForAll(xs, p) =>
        MethodCall(xs, ForallMethod.withConcreteTypes(Map(tIV -> xs.tpe.elemType)), Vector(p), Map())
      case Exists(xs, p) =>
        MethodCall(xs, ExistsMethod.withConcreteTypes(Map(tIV -> xs.tpe.elemType)), Vector(p), Map())
      case MapCollection(xs, f) =>
        MethodCall(xs,
          MapMethod.withConcreteTypes(Map(tIV -> xs.tpe.elemType, tOV -> f.tpe.tRange)),
          Vector(f), Map())
      case Fold(xs, z, op) =>
        MethodCall(xs,
          SCollectionMethods.FoldMethod.withConcreteTypes(Map(tIV -> xs.tpe.elemType, tOV -> z.tpe)),
          Vector(z, op), Map())
      case Slice(xs, from, until) =>
        MethodCall(xs,
          SCollectionMethods.SliceMethod.withConcreteTypes(Map(tIV -> xs.tpe.elemType)),
          Vector(from, until), Map())
      case Append(xs, ys) =>
        MethodCall(xs,
          SCollectionMethods.AppendMethod.withConcreteTypes(Map(tIV -> xs.tpe.elemType)),
          Vector(ys), Map())
      case Xor(l, r) =>
        MethodCall(Global, SGlobalMethods.xorMethod, Vector(l, r), Map())
      case ByIndex(xs, index, Some(default)) =>
        MethodCall(xs,
          SCollectionMethods.GetOrElseMethod.withConcreteTypes(Map(tIV -> xs.tpe.elemType)),
          Vector(index, default), Map())
    })
    rewrite(everywherebu(r))(expr)
  }
}

object SigmaCompiler {
  /** Force initialization of reflection before any instance of SigmaCompiler is used. */
  val _ = (InterpreterReflection, GraphIRReflection)

  /** Constructs an instance for the given settings. */
  def apply(settings: CompilerSettings): SigmaCompiler =
    new SigmaCompiler(settings)

  /** Constructs an instance for the given network type. */
  def apply(networkPrefix: Byte): SigmaCompiler =
    new SigmaCompiler(networkPrefix)
}
