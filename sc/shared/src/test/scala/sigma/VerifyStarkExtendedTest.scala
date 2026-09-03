package sigmastate

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import sigma.ast.SCollection.SByteArray
import sigma.ast._
import sigma.exceptions.OpcodeUnavailableException
import sigma.serialization.{ConstantStore, SigmaSerializer, ValueSerializer}
import sigma.validation.ValidationRules
import sigmastate.interpreter.{CErgoTreeEvaluator, CostAccumulator}

class VerifyStarkExtendedTest extends AnyPropSpec with Matchers {

  private def createEvaluator(): CErgoTreeEvaluator = {
    val settings = CErgoTreeEvaluator.DefaultEvalSettings
    val accumulator = new CostAccumulator(
      initialCost = JitCost(0),
      costLimit = Some(JitCost.fromBlockCost(settings.scriptCostLimitInEvaluator))
    )
    new CErgoTreeEvaluator(
      context = null,
      constants = ErgoTree.EmptyConstants,
      coster = accumulator,
      profiler = CErgoTreeEvaluator.DefaultProfiler,
      settings = settings
    )
  }

  private def evalDirect(evaluator: CErgoTreeEvaluator,
                         node: Value[_ <: SType]): Any =
    CErgoTreeEvaluator.currentEvaluator.withValue(evaluator) {
      node.evalTo[Any](Map.empty)(evaluator)
    }

  private val chunks = ConcreteCollection[SByteArray](
    Vector(ByteArrayConstant(Array[Byte](1))),
    SByteArray
  ).asInstanceOf[Value[SCollection[SCollection[SByte.type]]]]

  private val inaccessibleChunks =
    ConstantPlaceholder(0, SCollection(SByteArray))
      .asInstanceOf[Value[SCollection[SCollection[SByte.type]]]]
  private val guardedNode = VerifyStark(
    inaccessibleChunks,
    ConstantPlaceholder(1, SByteArray),
    ConstantPlaceholder(2, SByteArray),
    ConstantPlaceholder(3, SByteArray)
  )

  property("VerifyStark preserves a constant-placeholder profile id") {
    val node = VerifyStark(
      chunks,
      ByteArrayConstant(Array[Byte](2)),
      ByteArrayConstant(Array[Byte](3)),
      ConstantPlaceholder(0, SByteArray)
    )
    val bytes = ValueSerializer.serialize(node)
    val reader = SigmaSerializer.startReader(
      bytes,
      new ConstantStore(IndexedSeq(ByteArrayConstant(Array[Byte](3)))),
      resolvePlaceholdersToConstants = false
    )
    ValueSerializer.deserialize(reader) shouldBe node
  }

  property("VerifyStark scaffold has no consensus charge metadata") {
    VerifyStark.costKind shouldBe FixedCost(JitCost(0))
  }

  property("unavailable VerifyStark fails before evaluating any child") {
    val evaluator = createEvaluator()
    val before = evaluator.getAccumulatedCost
    val error = the[OpcodeUnavailableException] thrownBy
      evalDirect(evaluator, guardedNode)

    error.opCode shouldBe 0xb9
    error.getMessage shouldBe "Opcode 0xB9 is unavailable before network activation"
    evaluator.getAccumulatedCost shouldBe before
  }

  property("negation cannot turn unavailable VerifyStark into success") {
    val evaluator = createEvaluator()
    val before = evaluator.getAccumulatedCost
    val error = the[OpcodeUnavailableException] thrownBy
      evalDirect(evaluator, LogicalNot(guardedNode))

    error.opCode shouldBe 0xb9
    evaluator.getAccumulatedCost shouldBe before
  }

  property("ordinary soft-fork handling does not swallow OpcodeUnavailable") {
    implicit val validationSettings = ValidationRules.coreSettings
    val evaluator = createEvaluator()

    val error = the[OpcodeUnavailableException] thrownBy
      ValidationRules.trySoftForkable(false) {
        evalDirect(evaluator, guardedNode)
        true
      }

    error.opCode shouldBe 0xb9
  }
}
