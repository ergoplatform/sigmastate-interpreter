package sigmastate

import scala.util.DynamicVariable
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigma.ast.{ErgoTree, SType, TransformingSigmaBuilder, Value}
import org.ergoplatform.ErgoAddressEncoder.TestnetNetworkPrefix
import sigma.ast.syntax.{SValue, SigmaPropValue}
import sigma.serialization.ValueSerializer
import sigma.compiler.ir.IRContext
import sigma.ast.syntax.ValueOps
import sigma.compiler.{CompilerResult, CompilerSettings, SigmaCompiler}
import sigmastate.helpers.{NegativeTesting, SigmaPPrint}

trait CompilerTestsBase extends TestsBase with NegativeTesting {
  protected val _lowerMethodCalls = new DynamicVariable[Boolean](true)

  /** Returns true if MethodCall nodes should be lowered by TypeChecker to the
    * corresponding ErgoTree nodes. E.g. xs.map(f) -->  MapCollection(xs, f).
    * NOTE: The value of the flag is assigned dynamically using _lowerMethodCalls
    * DynamicVariable. */
  def lowerMethodCallsInTests: Boolean = _lowerMethodCalls.value

  protected val _enableCompilerOptimization = new DynamicVariable[Boolean](true)

  /** Returns true (by default) if compiler should optimize the tree.
    * NOTE: The value of the flag is assigned dynamically using _enableCompilerOptimization
    * DynamicVariable. */
  def enableCompilerOptimizationInTests: Boolean = _enableCompilerOptimization.value

  /** If true, then all suite properties are executed with _lowerMethodCalls set to false.
    * This allow to test execution of MethodCall nodes in ErgoTree.
    */
  val okRunTestsWithoutMCLowering: Boolean = false

  /** Compiler settings used in tests. */
  val defaultCompilerSettings: CompilerSettings = CompilerSettings(
    TestnetNetworkPrefix, TransformingSigmaBuilder,
    lowerMethodCalls = true
  )

  def compilerSettingsInTests: CompilerSettings =
    defaultCompilerSettings.copy(
      lowerMethodCalls = lowerMethodCallsInTests,
      enableOptimizations = enableCompilerOptimizationInTests
    )

  def compiler = SigmaCompiler(compilerSettingsInTests)

  def checkSerializationRoundTrip(v: SValue): Unit = {
    val compiledTreeBytes = ValueSerializer.serialize(v)
    withClue(s"(De)Serialization roundtrip failed for the tree:") {
      ValueSerializer.deserialize(compiledTreeBytes) shouldEqual v
    }
  }

  /** Compile the given code to ErgoTree expression. */
  def compile(env: ScriptEnv, code: String)(implicit IR: IRContext): Value[SType] = {
    val res = compiler.compile(env, code)
    checkCompilerResult(res)
    res.buildTree
  }

  /** Check the given [[CompilerResult]] meets equality and sanity requirements. */
  def checkCompilerResult[Ctx <: IRContext](res: CompilerResult[Ctx]): Unit = {
    checkSerializationRoundTrip(res.buildTree)
  }


  /** Compiles the given code and checks the resulting `prop` against `expected`. */
  def compileAndCheck(env: ScriptEnv, code: String, expected: SValue)
      (implicit IR: IRContext): (ErgoTree, SigmaPropValue) = {
    val prop = compile(env, code).asSigmaProp
    prop shouldBe expected
    val tree = mkTestErgoTree(prop)
    (tree, prop)
  }

  /** Checks expectation pretty printing the actual value if there is a difference. */
  def checkEquals[T](actual: T, expected: T): Unit = {
    if (expected != actual) {
      SigmaPPrint.pprintln(actual, width = 100)
    }
    actual shouldBe expected
  }
}
