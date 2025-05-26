package sigma

import org.scalatest.BeforeAndAfterAll
import sigma.data.{CSigmaProp, DataValueComparer, RType, TrivialProp}
import sigma.util.BenchmarkUtil
import sigmastate.eval.CProfiler
import sigmastate.helpers.SigmaPPrint
import sigmastate.interpreter.{CostAccumulator, CErgoTreeEvaluator}
import sigma.ast.{ErgoTree, JitCost}
import sigma.eval.{EvalSettings, SigmaDsl, TracedCost}

import scala.util.{Success, Try}

class DataValueComparerSpecification extends SigmaDslTesting
    with BeforeAndAfterAll { suite =>

  implicit override val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 30)

  implicit override val evalSettings: EvalSettings =
    CErgoTreeEvaluator.DefaultEvalSettings.copy(
    isMeasureOperationTime = true,
    isMeasureScriptTime = true,
    isLogEnabled = false, // don't commit the `true` value (CI log is too high)
    costTracingEnabled = true,  // should always be enabled in tests (and false by default)
    scriptCostLimitInEvaluator = scriptCostLimitInTests
  )
  override val nBenchmarkIters = 10

  val nWarmUpIterations = 100

  implicit val suiteProfiler = new CProfiler

  import TestData._

  def createEvaluator(settings: EvalSettings, profiler: CProfiler): CErgoTreeEvaluator = {
    val accumulator = new CostAccumulator(
      initialCost = JitCost(0),
      costLimit = Some(JitCost.fromBlockCost(settings.scriptCostLimitInEvaluator)))
    val evaluator = new CErgoTreeEvaluator(
      context = null,
      constants = ErgoTree.EmptyConstants,
      coster = accumulator, profiler, settings)
    evaluator
  }

  /** Checks (on positive cases) that EQ.equalDataValues used in v5.0 is equivalent to
    * `==` used in v4.0
    * NOTE: the computations `x` and `y` are expected to be stable (i.e. always producing
    * equal values)
    * @param x computation which produced first argument
    * @param y computation which produced second argument
    */
  def check(x: => Any, y: => Any, expected: Boolean)(implicit settings: EvalSettings, profiler: CProfiler) = {
    val _x = x // force computation and obtain value
    val _y = y
    withClue(s"EQ.equalDataValues(${_x}, ${_y})") {
      val res = sameResultOrError(
        repeatAndReturnLast(nBenchmarkIters + 1) {
          val evaluator = createEvaluator(settings, profiler)
          // it's important to use fresh values to neutralize memory cache to some extent
          val fresh_x = x
          val fresh_y = y
          DataValueComparer.equalDataValues(fresh_x, fresh_y)(evaluator)
        },
        _x == _y)
      res match {
        case Success(res) => res shouldBe expected
        case _ =>
      }
    }
    if (settings.isMeasureScriptTime) {
      val evaluator = createEvaluator(settings, profiler)
      val fresh_x = x
      val fresh_y = y
      val (res, actualTime) = BenchmarkUtil.measureTimeNano {
        Try(DataValueComparer.equalDataValues(fresh_x, fresh_y)(evaluator))
      }
      if (res.isSuccess) {
        val costDetails = TracedCost(evaluator.getCostTrace(), Some(actualTime))
        val xStr = SigmaPPrint(fresh_x).plainText
        val yStr = SigmaPPrint(fresh_y).plainText
        val script = s"$xStr == $yStr"
        evaluator.profiler.addJitEstimation(script, costDetails.cost, actualTime)
      }
    }

  }

  /** It is important for profiling to return a new array on every method call.
    * This is to avoid reusing the same memory location during numerous iterations
    * which doesn't reflect the real world scenario. Thus, creating a new array on every
    * request neutralizes the effects of cache and makes profiling more accurate. */
  def zeros = Array[Any](0.toByte, 0.toShort, 0, 0.toLong)
  def ones = Array[Any](1.toByte, 1.toShort, 1, 1.toLong)

  override protected def beforeAll(): Unit = {
    // this method warms up the code in DataValueComparer
    val warmUpProfiler = new CProfiler
    warmUpBeforeAllTest(nTotalIters = nWarmUpIterations) {
      runBaseCases(warmUpProfiler)(evalSettings = evalSettings.copy(isLogEnabled = false))
    }
  }

  /** Runs a number of equality checks for a value produced by the given computation.
    * @param x computation which produces value to be exercised. */
  def checkIsEqual(x: => Any) = {
    check(x, x, true)
    check(Some(x), Some(x), true)
    check((x, x), (x, x), true)
  }

  /** This is NOT comprehensive list of possible checks.
    * See also DataSerializerSpecification.roundtrip where comprehensive
    * checking of positive cases is done.
    * This method is used:
    * 1) to warm up DataValueComparer in the beforeAll method
    * 2) to profile DataValueComparer operations */
  def runBaseCases(profiler: CProfiler)(implicit evalSettings: EvalSettings) = {
    implicit val suiteProfiler = profiler  // hide suite's profiler and use explicitly passed
    ones.foreach { x =>
      ones.foreach { y =>
        check(x, y, true)  // numeric values are equal regardless of their type
        check(Some(x), Some(y), true)  // numeric values in Option
        check(Some(x), y, false)
        check(x, Some(y), false)
        check(Some(x), None, false)
        check(None, Some(x), false)
        check((x, 1), (y, 1), true)        // and in Tuple
        check((1, x), (1, y), true)
        check((1, x), y, false)
        check(x, (1, y), false)
      }
    }
    val sizes = Array(0, 1, 4, 8, 16, 32, 64, 128, 256, 512)
    def coll[T: RType](s: Int, v: => T): Coll[T] = {
      val arr = Array.fill(s)(v)(RType[T].classTag)
      builder.Colls.fromArray(arr)
    }
    sizes.foreach { s =>
      checkIsEqual(coll(s, 1.toByte))
      checkIsEqual(coll(s, 1.toShort))
      checkIsEqual(coll(s, 1))
      checkIsEqual(coll(s, 1L))
      checkIsEqual(coll(s, createBigIntMaxValue()))
      checkIsEqual(coll(s, create_ge1()))
      checkIsEqual(coll(s, create_t1()))
      checkIsEqual(coll(s, create_b1()))
      checkIsEqual(coll(s, create_preH1()))
      checkIsEqual(coll(s, create_h1()))
      // collections of complex types
      checkIsEqual(coll(s, (1.toByte, 1)))
      checkIsEqual(coll(s, Option((1.toByte, 1))))
      checkIsEqual(coll(s, (create_ge1(), Option((1.toByte, 1)))))
      checkIsEqual(coll(s, (create_ge1(), (Option((1.toByte, 1)), coll(32, 7.toByte)))))
      checkIsEqual(coll(s, SigmaDsl.SigmaProp(create_dlog())))
      checkIsEqual(coll(s, SigmaDsl.SigmaProp(create_dht())))
      checkIsEqual(coll(s, SigmaDsl.SigmaProp(create_and())))
      checkIsEqual(coll(s, SigmaDsl.SigmaProp(create_or())))
      checkIsEqual(coll(s, SigmaDsl.SigmaProp(TrivialProp.TrueProp)))
      checkIsEqual(coll(s, SigmaDsl.SigmaProp(TrivialProp.FalseProp)))
    }

    checkIsEqual(createBigIntMaxValue())
    checkIsEqual(create_ge1())
    checkIsEqual(create_t1)
    checkIsEqual(create_b1())
    checkIsEqual(create_preH1())
    checkIsEqual(create_h1())
    checkIsEqual(CSigmaProp(create_dlog()))
    checkIsEqual(CSigmaProp(create_dht()))
    checkIsEqual(CSigmaProp(create_and()))
    checkIsEqual(CSigmaProp(create_or()))
    checkIsEqual(CSigmaProp(TrivialProp.TrueProp))
    checkIsEqual(CSigmaProp(TrivialProp.FalseProp))
  }

  /** Run this property alone for profiling and see the report generated in afterAll. */
  property("equalDataValues base cases (use for profiling)") {
    runBaseCases(suiteProfiler)(evalSettings)
  }

  property("equalDataValues positive cases (Coll)") {
    checkIsEqual(Coll[Int]())
    checkIsEqual(Coll[Int](1))
  }

  property("equalDataValues negative cases") {
    check(BigIntZero, BigIntOne, false)
    check(ge1, ge2, false)
    check(t1, t2, false)
    check(b1, b2, false)
    check(preH1, preH2, false)
    check(h1, h2, false)

    ones.foreach { x =>
      zeros.foreach { y =>
        check(x, y, false)
      }
    }
    ones.foreach { x =>
      check(BigIntOne, x, false)
    }
    val values = Array[AnyRef](
      1.asInstanceOf[AnyRef], BigIntOne, ge1, t1, b1, preH1, h1,
      Coll[Int](1), None, Option(1), (1, 1))
    values.foreach { x =>
      values.foreach { y =>
        if (!(x eq y)) {
          check(x, y, false)
          check(y, x, false)
        }
      }
    }
  }

  property("equalDataValues negative cases (in Coll)") {
    val onesInColl = Array[AnyRef](Coll(1.toByte), Coll(1.toShort), Coll(1), Coll(1.toLong))
    onesInColl.foreach { x =>
      onesInColl.foreach { y =>
        if (!(x eq y)) {
          check(x, y, false)
          check(y, x, false)
        }
      }
    }
    check(Coll(1), Coll(1, 2), false)
  }

  override protected def afterAll(): Unit = {
    val report = suiteProfiler.generateReport
    if (printDebugInfo) println(report)
  }

}
