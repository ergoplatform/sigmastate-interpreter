package sigma

import org.scalatest.BeforeAndAfterAll
import sigma.ast.{Apply, FixedCostItem, FuncValue, GetVar, JitCost, OptionGet, ValUse}
import sigma.eval.{CostDetails, EvalSettings, Profiler}
import sigmastate.CompilerCrossVersionProps
import sigmastate.interpreter.CErgoTreeEvaluator

import scala.util.Success

/** Base class for language test suites (one suite for each language version: 5.0, 6.0, etc.)
  * Each suite tests every method of every SigmaDsl type to be equivalent to
  * the evaluation of the corresponding ErgoScript operation.
  *
  * The properties of this suite exercise two interpreters: the current (aka `old`
  * interpreter) and the new interpreter for a next soft-fork. After the soft-fork is
  * released, the new interpreter becomes current at which point the `old` and `new`
  * interpreters in this suite should be equivalent. This change is reflected in this
  * suite by commiting changes in expected values.
  * The `old` and `new` interpreters are compared like the following:
  * 1) for existingFeature the interpreters should be equivalent
  * 2) for changedFeature the test cases contain different expected values
  * 3) for newFeature the old interpreter should throw and the new interpreter is checked
  * against expected values.
  *
  * This suite can be used for Cost profiling, i.e. measurements of operations times and
  * comparing them with cost parameters of the operations.
  *
  * The following settings should be specified for profiling:
  * isMeasureOperationTime = true
  * isMeasureScriptTime = true
  * isLogEnabled = false
  * printTestVectors = false
  * costTracingEnabled = false
  * isTestRun = true
  * perTestWarmUpIters = 1
  * nBenchmarkIters = 1
  */
abstract class LanguageSpecificationBase extends SigmaDslTesting
  with CompilerCrossVersionProps
  with BeforeAndAfterAll { suite =>

  /** Version of the language (ErgoScript/ErgoTree) which is specified by this suite. */
  def languageVersion: Byte

  /** Use VersionContext so that each property in this suite runs under correct
    * parameters.
    */
  protected override def testFun_Run(testName: String, testFun: => Any): Unit = {
    VersionContext.withVersions(activatedVersionInTests, ergoTreeVersionInTests) {
      super.testFun_Run(testName, testFun)
    }
  }

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 30)

  val evalSettingsInTests = CErgoTreeEvaluator.DefaultEvalSettings.copy(
    isMeasureOperationTime = true,
    isMeasureScriptTime = true,
    isLogEnabled = false, // don't commit the `true` value (travis log is too high)
    printTestVectors = false, // don't commit the `true` value (travis log is too high)

    /** Should always be enabled in tests (and false by default)
      * Should be disabled for cost profiling, which case the new costs are not checked.
      */
    costTracingEnabled = true,
    profilerOpt = Some(CErgoTreeEvaluator.DefaultProfiler),
    isTestRun = true
  )

  def warmupSettings(p: Profiler) = evalSettingsInTests.copy(
    isLogEnabled = false,
    printTestVectors = false,
    profilerOpt = Some(p)
  )

  implicit override def evalSettings: EvalSettings = {
    warmupProfiler match {
      case Some(p) => warmupSettings(p)
      case _ => evalSettingsInTests
    }
  }

  override val perTestWarmUpIters = 0

  override val nBenchmarkIters = 0

  override val okRunTestsWithoutMCLowering: Boolean = true

  implicit def IR = createIR()

  def testCases[A, B](cases: Seq[(A, Expected[B])], f: Feature[A, B]) = {
    val table = Table(("x", "y"), cases: _*)
    forAll(table) { (x, expectedRes) =>
      val res                       = f.checkEquality(x)
      val resValue                  = res.map(_._1)
      val (expected, expDetailsOpt) = expectedRes.newResults(ergoTreeVersionInTests)
      checkResult(resValue, expected.value, failOnTestVectors = true,
        "SigmaDslSpecifiction#testCases: compare expected new result with res = f.checkEquality(x)")
      res match {
        case Success((value, details)) =>
          details.cost shouldBe JitCost(expected.verificationCost.get)
          expDetailsOpt.foreach(expDetails =>
            if (details.trace != expDetails.trace) {
              printCostDetails(f.script, details)
              details.trace shouldBe expDetails.trace
            }
          )
      }
    }
  }

  override protected def beforeAll(): Unit = {
    prepareSamples[BigInt]
    prepareSamples[GroupElement]
    prepareSamples[AvlTree]
    prepareSamples[Box]
    prepareSamples[PreHeader]
    prepareSamples[Header]
    prepareSamples[(BigInt, BigInt)]
    prepareSamples[(GroupElement, GroupElement)]
    prepareSamples[(AvlTree, AvlTree)]
    prepareSamples[(Box, Box)]
    prepareSamples[(PreHeader, PreHeader)]
    prepareSamples[(Header, Header)]
  }

  ///=====================================================
  ///         CostDetails shared among test cases
  ///-----------------------------------------------------
  val traceBase = Array(
    FixedCostItem(Apply),
    FixedCostItem(FuncValue),
    FixedCostItem(GetVar),
    FixedCostItem(OptionGet),
    FixedCostItem(FuncValue.AddToEnvironmentDesc, FuncValue.AddToEnvironmentDesc_CostKind),
    FixedCostItem(ValUse)
  )

  /** Helper method to create the given expected results for all tree versions. */
  def expectedSuccessForAllTreeVersions[A](value: A, cost: Int, costDetails: CostDetails) = {
    val res = ExpectedResult(Success(value), Some(cost)) -> Some(costDetails)
    Seq(0, 1, 2, 3).map(version => version -> res)
  }

}
