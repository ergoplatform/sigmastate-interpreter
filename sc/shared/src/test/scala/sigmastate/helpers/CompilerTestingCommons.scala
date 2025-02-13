package sigmastate.helpers

import org.ergoplatform._
import org.ergoplatform.validation.ValidationSpecification
import org.scalacheck.Arbitrary.arbByte
import org.scalacheck.Gen
import sigma.util.BenchmarkUtil
import scalan.TestContexts
import sigma.ast.{Constant, CostItem, ErgoTree, JitCost, SOption, SType}
import sigma.{Colls, Evaluation, TestUtils}
import sigma.data.{RType, SigmaBoolean}
import sigma.validation.ValidationException
import sigma.validation.ValidationRules.CheckSerializableTypeCode
import sigma.ast.syntax.{SValue, SigmaPropValue}
import sigma.compiler.{CompilerSettings, SigmaCompiler}
import sigma.compiler.ir.IRContext
import sigma.eval.{CostDetails, EvalSettings, Extensions, GivenCost, TracedCost}
import sigmastate.helpers.TestingHelpers._
import sigma.interpreter.ContextExtension.VarBinding
import sigma.kiama.rewriting.Rewriter
import sigma.kiama.rewriting.Rewriter.{everywherebu, strategy}
import sigmastate.interpreter.CErgoTreeEvaluator.DefaultProfiler
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.interpreter._
import sigma.serialization.SigmaSerializer
import sigmastate.CompilerTestsBase
import sigmastate.eval.CContext

import scala.util.DynamicVariable

trait CompilerTestingCommons extends TestingCommons
    with TestUtils with TestContexts with ValidationSpecification
    with CompilerTestsBase {

  class TestingIRContext extends TestContext with IRContext

  case class CompiledFunc[A,B]
    (script: String, bindings: Seq[VarBinding], expr: SValue, compiledTree: SValue, func: A => (B, CostDetails))
    (implicit val tA: RType[A], val tB: RType[B]) extends Function1[A, (B, CostDetails)] {
    override def apply(x: A): (B, CostDetails) = func(x)
  }

  /** This value is used as Context.initCost value. The default value is used for most
    * test vectors.
    * Change this value using `withValue` method to test behavior with non-default
    * initial cost.
    */
  protected val initialCostInTests = new DynamicVariable[Long](0)

  def createContexts[A](in: A, bindings: Seq[VarBinding])(implicit tA: RType[A]) = {
    val tpeA = Evaluation.rtypeToSType(tA)
    in match {
      case ctx: CContext =>
        // the context is passed as function argument (this is for testing only)
        // This is to overcome non-functional semantics of context operations
        // (such as Inputs, Height, etc which don't have arguments and refer to the
        // context implicitly).
        // These context operations are introduced by buildTree frontend function
        // (ctx.HEIGHT method call compiled to Height IR node)
        // -------
        // We add ctx as it's own variable with id = 1
        val ctxVar = Extensions.toAnyValue[sigma.Context](ctx)(sigma.ContextRType)
        val newVars = if (ctx.vars.length < 2) {
          val vars = ctx.vars.toArray
          val buf = new Array[sigma.AnyValue](2)
          Array.copy(vars, 0, buf, 0, vars.length)
          buf(1) = ctxVar
          Colls.fromArray(buf)
        } else {
          ctx.vars.updated(1, ctxVar)
        }
        val calcCtx = ctx.copy(vars = newVars)
        calcCtx
      case _ =>
        val box = createBox(0, TrueTree)

        // make sure we are doing tests with the box with is actually serializable
        try roundTripTest(box)(ErgoBox.sigmaSerializer)
        catch {
          case ValidationException(_, r: CheckSerializableTypeCode.type, Seq(SOption.OptionTypeCode), _) =>
          // ignore the problem with Option serialization, but test all the other cases
        }

        val ergoCtx = ErgoLikeContextTesting.dummy(box, activatedVersionInTests)
          .withErgoTreeVersion(ergoTreeVersionInTests)
          .withBindings(1.toByte -> Constant[SType](in.asInstanceOf[SType#WrappedType], tpeA))
          .withBindings(bindings: _*)
        val calcCtx = ergoCtx.toSigmaContext().asInstanceOf[CContext]
        calcCtx
    }
  }

  def compileTestScript[A]
      (env: ScriptEnv, funcScript: String)
      (implicit tA: RType[A],
                IR: IRContext,
                compilerSettings: CompilerSettings): SValue = {
    val code =
      s"""{
        |  val func = $funcScript
        |  val res = func(getVar[${tA.name}](1).get)
        |  res
        |}
      """.stripMargin

    // The following ops are performed by frontend
    // typecheck, create graphs, compile to Tree
    // The resulting tree should be serializable
    val compiledTree = {
      val compiler = SigmaCompiler(compilerSettings)
      val res = compiler.compile(env, code)
      checkCompilerResult(res)
      if (lowerMethodCallsInTests) res.buildTree
      else {
        compiler.unlowerMethodCalls(res.buildTree)
      }
    }
    compiledTree
  }

  def evalSettings = CErgoTreeEvaluator.DefaultEvalSettings

  def printCostDetails(script: String, details: CostDetails) = {
    val traceLines = SigmaPPrint(details, height = 550, width = 150)
    println(
      s"""------------------------
        |Script: $script
        |$traceLines
        |""".stripMargin)
  }

  def funcJitFromExpr[A: RType, B: RType]
      (funcScript: String, expr: SValue, bindings: VarBinding*)
      (implicit IR: IRContext,
                evalSettings: EvalSettings,
                compilerSettings: CompilerSettings): CompiledFunc[A, B] = {
    val f = (in: A) => {
      val sigmaCtx = createContexts(in, bindings)
      val accumulator = new CostAccumulator(
        initialCost = JitCost(0),
        costLimit = Some(JitCost.fromBlockCost(evalSettings.scriptCostLimitInEvaluator)))
      val evaluator = new CErgoTreeEvaluator(
        context = sigmaCtx,
        constants = ErgoTree.EmptyConstants,
        coster = accumulator, evalSettings.profilerOpt.getOrElse(DefaultProfiler), evalSettings)

      val (res, actualTime) = BenchmarkUtil.measureTimeNano(
        evaluator.evalWithCost[B](CErgoTreeEvaluator.EmptyDataEnv, expr))
      val costDetails = if (evalSettings.costTracingEnabled) {
        val trace: Seq[CostItem] = evaluator.getCostTrace()
        val costDetails = TracedCost(trace, Some(actualTime))
        assert(res.cost == costDetails.cost)
        costDetails
      } else
        GivenCost(res.cost, Some(actualTime))

      if (evalSettings.isMeasureScriptTime) {
        evaluator.profiler.addJitEstimation(funcScript, res.cost, actualTime)
      }

      if (evalSettings.isLogEnabled) {
        printCostDetails(funcScript, costDetails)
      }
      (res.value, costDetails)
    }
    if (evalSettings.isDebug) {
      // Deep clone expr using kiama. This is to make sure JS reflection works correctly.
      val copyRule = strategy[Any] { case x: SValue => Some(Rewriter.copy(x)) }
      val Some(copy) = everywherebu(copyRule)(expr)
    }
    val sigma.ast.Apply(funcVal, _) = expr.asInstanceOf[SValue]
    CompiledFunc(funcScript, bindings, funcVal, expr, f)
  }

  def funcJit[A: RType, B: RType]
      (funcScript: String, bindings: VarBinding*)
      (implicit IR: IRContext,
                evalSettings: EvalSettings,
                compilerSettings: CompilerSettings): CompiledFunc[A, B] = {
    val compiledTree = compileTestScript[A](Interpreter.emptyEnv, funcScript)
    funcJitFromExpr(funcScript, compiledTree, bindings:_*)
  }

  protected def roundTripTest[T](v: T)(implicit serializer: SigmaSerializer[T, T]): T = {
    // using default sigma reader/writer
    val bytes = serializer.toBytes(v)
    bytes.nonEmpty shouldBe true
    val r = SigmaSerializer.startReader(bytes)
    val positionLimitBefore = r.positionLimit
    val parsed = serializer.parse(r)
    parsed shouldBe v
    r.positionLimit shouldBe positionLimitBefore
    parsed
  }

  protected def roundTripTestWithPos[T](v: T)(implicit serializer: SigmaSerializer[T, T]): T = {
    val randomBytesCount = Gen.chooseNum(1, 20).sample.get
    val randomBytes = Gen.listOfN(randomBytesCount, arbByte.arbitrary).sample.get.toArray
    val bytes = serializer.toBytes(v)
    val parsed = serializer.parse(SigmaSerializer.startReader(bytes))
    parsed shouldBe v
    serializer.parse(SigmaSerializer.startReader(randomBytes ++ bytes, randomBytesCount)) shouldBe v
    parsed
  }

  def testReduce(I: Interpreter)(ctx: I.CTX, prop: SigmaPropValue): SigmaBoolean = {
    val ergoTree = ErgoTree.fromProposition(ergoTreeHeaderInTests, prop)
    I.fullReduction(ergoTree, ctx).value
  }

}
