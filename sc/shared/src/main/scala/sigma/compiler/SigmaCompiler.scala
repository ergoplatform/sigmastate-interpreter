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
import sigma.exceptions.CompilerException
import sigma.Environment
import sigmastate.InterpreterReflection
import sigmastate.lang.SigmaParser

/**
  * @param networkPrefix    network prefix to decode an ergo address from string (PK op)
  * @param builder          used to create ErgoTree nodes
  * @param lowerMethodCalls if true, then MethodCall nodes are lowered to ErgoTree nodes
  *                         when [[sigma.ast.SMethod.irInfo.irBuilder]] is defined. For
  *                         example, in the `coll.map(x => x+1)` code, the `map` method
  *                         call can be lowered to MapCollection node.
  *                         The lowering if preferable, because it is more compact (1 byte
  *                         for MapCollection instead of 3 bytes for MethodCall).
  */
case class CompilerSettings(
    networkPrefix: NetworkPrefix,
    builder: SigmaBuilder,
    lowerMethodCalls: Boolean
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
  compiledGraph: Ctx#Ref[Ctx#Context => Any],
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
    SigmaCompiler.checkStackOverflow(source = None) {
      SigmaParser(x) match {
        case Success(v, _) => v
        case f: Parsed.Failure =>
          throw new ParserException(s"Syntax error: $f", Some(SourceContext.fromParserFailure(f)))
      }
    }
  }

  /** Typechecks the given parsed expression and assigns types for all sub-expressions. */
  def typecheck(env: ScriptEnv, parsed: SValue): Value[SType] = {
    SigmaCompiler.checkStackOverflow(parsed.sourceContext.toOption) {
      val predefinedFuncRegistry = new PredefinedFuncRegistry(builder)
      val binder = new SigmaBinder(env, builder, networkPrefix, predefinedFuncRegistry)
      val bound = binder.bind(parsed)
      val typeEnv = env.collect { case (k, v: SType) => k -> v }
      val typer = new SigmaTyper(builder, predefinedFuncRegistry, typeEnv, settings.lowerMethodCalls)
      typer.typecheck(bound)
    }
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

  /** Compiles the given typed expression.
    *
    * @note If this method throws a [[CompilerException]] because of a stack overflow,
    *       the provided [[IRContext]] may be in a partially-built state and should not be
    *       reused; callers should create a fresh IR context before compiling again.
    */
  def compileTyped(env: ScriptEnv, typedExpr: SValue)(implicit IR: IRContext): CompilerResult[IR.type] = {
    SigmaCompiler.checkStackOverflow(typedExpr.sourceContext.toOption) {
      val placeholdersEnv = env
          .collect { case (name, t: SType) => name -> t }
          .zipWithIndex
          .map { case ((name, t), index) => name -> ConstantPlaceholder(index, t) }
          .toMap
      val compiledGraph = IR.buildGraph(env ++ placeholdersEnv, typedExpr)
      val compiledTree = IR.buildTree(compiledGraph)
      CompilerResult(env, "<no source code>", compiledGraph, compiledTree)
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

  /** Returns true if the given throwable represents a stack overflow.
    *
    * On the JVM a stack overflow is signalled as a [[StackOverflowError]]. On Scala.js
    * there is no `StackOverflowError`; instead the JavaScript engine throws a native
    * `RangeError` ("Maximum call stack size exceeded" / "too much recursion"), which
    * Scala.js wraps into a `scala.scalajs.js.JavaScriptException`. Since shared code cannot
    * reference JS-only types, the JS case is detected by inspecting the exception class name
    * and message.
    *
    * The overload taking `isJVM` is package-private so tests can assert both platform
    * predicates without running on the actual platform.
    */
  private[sigma] def isStackOverflow(t: Throwable): Boolean =
    isStackOverflow(t, Environment.current.isJVM)

  private[sigma] def isStackOverflow(t: Throwable, isJVM: Boolean): Boolean = {
    if (isJVM)
      t.isInstanceOf[StackOverflowError]
    else {
      // Scala.js: detect the native RangeError wrapped in js.JavaScriptException.
      // The message strings are engine-specific (V8/JSC vs. SpiderMonkey). If an engine
      // uses different wording the predicate returns false and the original error
      // propagates unchanged, which is the safe fallback direction.
      val className = t.getClass.getName
      val message = String.valueOf(t.getMessage)
      className.contains("JavaScriptException") &&
        (message.contains("call stack size exceeded") ||
          message.contains("too much recursion"))
    }
  }

  /** Creates a [[CompilerException]] for a stack-overflow condition, preserving the
    * original throwable as its cause.
    */
  private[sigma] def compilerStackOverflowException(
      source: Option[SourceContext],
      cause: Throwable): CompilerException = {
    new CompilerException(
      "Script compilation failed (stack overflow): script is too complex or recursive",
      source,
      Some(cause))
  }

  /** Runs the given computation and converts a stack overflow into a recoverable
    * [[CompilerException]] while rethrowing all unrelated throwables (including
    * [[OutOfMemoryError]]) unchanged.
    */
  private def checkStackOverflow[A](source: Option[SourceContext])(body: => A): A = {
    try {
      body
    } catch {
      // Convert a stack overflow (JVM StackOverflowError or JS RangeError) into a checked
      // CompilerException so that a malformed/pathological script results in a normal
      // compilation error instead of a fatal runtime error. Unrelated throwables
      // (including OutOfMemoryError, InternalError, etc.) propagate unchanged because
      // this partial function does not match them.
      case t: Throwable if isStackOverflow(t) =>
        throw compilerStackOverflowException(source, t)
    }
  }

  /** Constructs an instance for the given settings. */
  def apply(settings: CompilerSettings): SigmaCompiler =
    new SigmaCompiler(settings)

  /** Constructs an instance for the given network type. */
  def apply(networkPrefix: Byte): SigmaCompiler =
    new SigmaCompiler(networkPrefix)
}
