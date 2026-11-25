package sigmastate.eval

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import sigma.ast.{ErgoTree, JitCost, PerItemCost, SigmaPropBytes, SigmaPropConstant}
import sigma.crypto.CryptoConstants
import sigma.data.{CAND, COR, CTHRESHOLD, ProveDlog, SigmaBoolean}
import sigma.eval.EvalSettings
import sigma.serialization.SerializerException
import sigma.{Coll, ContractsTestkit, VersionContext}
import sigmastate.interpreter.CErgoTreeEvaluator.DefaultProfiler
import sigmastate.interpreter.{CErgoTreeEvaluator, CostAccumulator}

class SigmaBooleanSizeLimitTests extends AnyFunSuite with ContractsTestkit with Matchers {

  private def sharedAnd(depth: Int): SigmaBoolean = {
    var p: SigmaBoolean = ProveDlog(CryptoConstants.dlogGroup.generator)
    (1 to depth).foreach(_ => p = CAND(Seq(p, p)))
    p
  }

  private def sharedOr(depth: Int): SigmaBoolean = {
    var p: SigmaBoolean = ProveDlog(CryptoConstants.dlogGroup.generator)
    (1 to depth).foreach(_ => p = COR(Seq(p, p)))
    p
  }

  private def sharedThreshold(depth: Int): SigmaBoolean = {
    var p: SigmaBoolean = ProveDlog(CryptoConstants.dlogGroup.generator)
    (1 to depth).foreach(_ => p = CTHRESHOLD(1, Seq(p, p)))
    p
  }

  private def newEvaluator(): (CostAccumulator, CErgoTreeEvaluator) = {
    val evalSettings = EvalSettings(
      isMeasureOperationTime = false,
      isMeasureScriptTime = false,
      costTracingEnabled = false)
    val accumulator = new CostAccumulator(initialCost = JitCost(0), costLimit = None)
    val evaluator = new CErgoTreeEvaluator(
      context = null,
      constants = ErgoTree.EmptyConstants,
      coster = accumulator,
      DefaultProfiler,
      evalSettings)
    (accumulator, evaluator)
  }

  test("expanded sigma trees are accepted below the size limit and rejected above it") {
    val constructors = Seq(
      "CAND" -> sharedAnd _,
      "COR" -> sharedOr _,
      "CTHRESHOLD" -> sharedThreshold _
    )

    constructors.foreach { case (name, construct) =>
      withClue(name) {
        construct(9).size shouldBe 1023
        val error = the[SerializerException] thrownBy construct(10).size
        error.getMessage should include("exceeds limit 1024")
      }
    }
  }

  test("SigmaPropBytes returns for a boundary DAG and rejects the next level") {
    val boundary = SigmaPropBytes(SigmaPropConstant(sharedAnd(9)))
    val (accumulator, evaluator) = newEvaluator()
    val bytes = VersionContext.withVersions(VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion) {
      boundary.evalTo[Coll[Byte]](Map.empty)(evaluator)
    }

    bytes.length should be > 0
    accumulator.totalCost.value shouldBe 6178

    val (_, oversizedEvaluator) = newEvaluator()
    val error = the[SerializerException] thrownBy {
      val oversized = SigmaPropBytes(SigmaPropConstant(sharedAnd(10)))
      VersionContext.withVersions(VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion) {
        oversized.evalTo[Coll[Byte]](Map.empty)(oversizedEvaluator)
      }
    }
    error.getMessage should include("exceeds limit 1024")
  }

  test("oversized nodes are rejected before integer overflow") {
    val huge = new ProveDlog(CryptoConstants.dlogGroup.generator) {
      override def size: Int = Int.MaxValue
    }

    val error = the[SerializerException] thrownBy SigmaBoolean.totalSize(Seq(huge, huge))
    error.getMessage should include("exceeds limit 1024")
  }

  test("negative per-item counts are rejected while zero keeps its previous cost") {
    val costKind = PerItemCost(JitCost(35), JitCost(6), 1)

    costKind.cost(0).value shouldBe 35
    the[IllegalArgumentException] thrownBy costKind.cost(-1)
    the[IllegalArgumentException] thrownBy costKind.chunks(-1)
  }
}
