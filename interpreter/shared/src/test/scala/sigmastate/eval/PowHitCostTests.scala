package sigmastate.eval

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import sigma.ast.{ErgoTree, JitCost, PowHitCostKind, SGlobalMethods, SHeaderMethods}
import sigma.data.CSigmaDslBuilder
import sigma.eval.EvalSettings
import sigma.exceptions.CostLimitException
import sigma.{ContractsTestkit, VersionContext}
import sigmastate.interpreter.CErgoTreeEvaluator.DefaultProfiler
import sigmastate.interpreter.{CErgoTreeEvaluator, CostAccumulator}

class PowHitCostTests extends AnyFunSuite with ContractsTestkit with Matchers {

  private val emptyBytes = CSigmaDslBuilder.Colls.emptyColl[Byte]

  private def newEvaluator(costLimit: Option[JitCost]): (CostAccumulator, CErgoTreeEvaluator) = {
    val evalSettings = EvalSettings(
      isMeasureOperationTime = false,
      isMeasureScriptTime = false,
      costTracingEnabled = false)
    val accumulator = new CostAccumulator(initialCost = JitCost(0), costLimit = costLimit)
    val evaluator = new CErgoTreeEvaluator(
      context = null,
      constants = ErgoTree.EmptyConstants,
      coster = accumulator,
      DefaultProfiler,
      evalSettings)
    (accumulator, evaluator)
  }

  test("powHit cost includes the M payload") {
    val (accumulator, evaluator) = newEvaluator(costLimit = None)

    VersionContext.withVersions(VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion) {
      SGlobalMethods.powHit_eval(null, CSigmaDslBuilder, 32, emptyBytes, emptyBytes, emptyBytes, 16)(evaluator)
    }

    PowHitCostKind.cost(32, emptyBytes, emptyBytes, emptyBytes).value shouldBe 15515
    accumulator.totalCost.value shouldBe 15515
  }

  test("powHit enforces the block cost limit") {
    val (accumulator, evaluator) = newEvaluator(
      costLimit = Some(JitCost.fromBlockCost(8000000)))
    var completed = 0

    val start = System.nanoTime()
    val error = the[CostLimitException] thrownBy {
      VersionContext.withVersions(VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion) {
        (1 to 6000).foreach { _ =>
          SGlobalMethods.powHit_eval(null, CSigmaDslBuilder, 32, emptyBytes, emptyBytes, emptyBytes, 16)(evaluator)
          completed += 1
        }
      }
    }
    val elapsedNanos = System.nanoTime() - start
    val elapsedSeconds = elapsedNanos / 1e9
    val totalCost = accumulator.totalCost.value

    println(f"powHit: $completed calls before rejection in $elapsedSeconds%.3f seconds, cost $totalCost JIT")
    error shouldBe a[CostLimitException]
    completed shouldBe 5156
    totalCost shouldBe 80010855
    elapsedNanos should be < 10000000000L
  }

  test("Autolykos2CheckPowCost is 15515 JIT") {
    SHeaderMethods.checkPowMethod.costKind shouldBe sigma.ast.FixedCost(PowHitCostKind.Autolykos2CheckPowCost)
    PowHitCostKind.Autolykos2CheckPowCost.value shouldBe 15515
  }
}
