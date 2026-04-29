package sigmastate

import org.scalatest.propspec.AnyPropSpec
import org.scalatest.matchers.should.Matchers
import sigma.ast._
import sigma.ast.SCollection.SByteArray
import sigma.exceptions.CostLimitException
import sigma.serialization.{OpCodes, ValueSerializer}
import sigmastate.interpreter.{CostAccumulator, CErgoTreeEvaluator}

/**
 * Extended VerifyStark tests covering:
 * - Serialization edge cases (empty proof, multiple chunks, large imageId)
 * - OpCode assignment verification  
 * - Cost model boundary conditions
 * - Multiple sequential evaluations
 * - Security: the AOT cost formula is deterministic and reproducible
 */
class VerifyStarkExtendedTest extends AnyPropSpec with Matchers {

  private def createEvaluator(blockCostLimit: Int): CErgoTreeEvaluator = {
    val accumulator = new CostAccumulator(
      initialCost = JitCost(0),
      costLimit = Some(JitCost.fromBlockCost(blockCostLimit))
    )
    new CErgoTreeEvaluator(
      context = null,
      constants = ErgoTree.EmptyConstants,
      coster = accumulator,
      profiler = CErgoTreeEvaluator.DefaultProfiler,
      settings = CErgoTreeEvaluator.DefaultEvalSettings
    )
  }

  private def evalDirect(evaluator: CErgoTreeEvaluator, node: Value[_ <: SType]): Any = {
    CErgoTreeEvaluator.currentEvaluator.withValue(evaluator) {
      node.evalTo[Any](Map())(evaluator)
    }
  }

  private def mkVerifyStark(
    proofChunks: Seq[Array[Byte]] = Seq(Array[Byte](1, 2, 3)),
    pubInputs: Array[Byte] = Array[Byte](4, 5, 6),
    imageIdBytes: Array[Byte] = Array.fill[Byte](32)(0),
    vmTypeVal: Int = 0,
    q: Int = 35,
    d: Int = 16
  ): VerifyStark = {
    val chunks = ConcreteCollection[SByteArray](
      proofChunks.map(ByteArrayConstant(_)).toVector,
      SByteArray
    ).asInstanceOf[Value[SCollection[SCollection[SByte.type]]]]

    val pubIn = ByteArrayConstant(pubInputs)
    val imgId = ByteArrayConstant(imageIdBytes)
    val vt = IntConstant(vmTypeVal)
    val cp = ConcreteCollection[SInt.type](Vector(IntConstant(q), IntConstant(d)), SInt)

    VerifyStark(chunks, pubIn, imgId, vt, cp)
  }

  // =====================================================================
  // OpCode Verification
  // =====================================================================
  property("VerifyStarkCode is registered at slot 73") {
    val code = OpCodes.VerifyStarkCode
    // OpCode value = LastConstantCode + 73 = 112 + 73 = 185 (0xB9)
    // As signed byte: 185 - 256 = -71
    (code.toByte & 0xFF) shouldBe 185
  }

  // =====================================================================
  // Serialization Edge Cases
  // =====================================================================
  property("Serialization: empty proof chunks") {
    val node = mkVerifyStark(proofChunks = Seq(Array.emptyByteArray))
    val bytes = ValueSerializer.serialize(node)
    val deserialized = ValueSerializer.deserialize(bytes)
    deserialized shouldBe node
  }

  property("Serialization: multiple proof chunks") {
    val chunks = Seq(
      Array[Byte](1, 2, 3, 4, 5),
      Array[Byte](6, 7, 8, 9, 10),
      Array[Byte](11, 12, 13, 14, 15)
    )
    val node = mkVerifyStark(proofChunks = chunks)
    val bytes = ValueSerializer.serialize(node)
    val deserialized = ValueSerializer.deserialize(bytes)
    deserialized shouldBe node
  }

  property("Serialization: large imageId (32 bytes)") {
    val imageId = Array.tabulate[Byte](32)(i => i.toByte)
    val node = mkVerifyStark(imageIdBytes = imageId)
    val bytes = ValueSerializer.serialize(node)
    val deserialized = ValueSerializer.deserialize(bytes)
    deserialized shouldBe node
  }

  property("Serialization: different vmType values") {
    for (vt <- Seq(0, 1, 42, 255, Int.MaxValue)) {
      val node = mkVerifyStark(vmTypeVal = vt)
      val bytes = ValueSerializer.serialize(node)
      val deserialized = ValueSerializer.deserialize(bytes)
      deserialized shouldBe node
    }
  }

  property("Serialization: byte length is deterministic") {
    val node = mkVerifyStark()
    val bytes1 = ValueSerializer.serialize(node)
    val bytes2 = ValueSerializer.serialize(node)
    bytes1 shouldBe bytes2
  }

  // =====================================================================
  // Cost Model Boundary Tests
  // =====================================================================
  property("AOT Cost: Q=0, D=0 charges only BASE_COST (5000 JIT)") {
    val node = mkVerifyStark(q = 0, d = 0)
    val evaluator = createEvaluator(blockCostLimit = 1000000)
    val result = evalDirect(evaluator, node)
    result shouldBe false
    // With Q=0, D=0: cost = 5000 (BASE only) + byte ingestion overhead
    evaluator.getAccumulatedCost.value should be >= 5000
    evaluator.getAccumulatedCost.value should be < 10000 // reasonable upper bound
  }

  property("AOT Cost: exact threshold — just barely fits in budget") {
    // Q=35, D=16 → 12350 JIT → 1235 block cost
    // Set budget to exactly 1300 block cost → should pass
    val node = mkVerifyStark(q = 35, d = 16)
    val evaluator = createEvaluator(blockCostLimit = 1300)
    val result = evalDirect(evaluator, node)
    result shouldBe false // placeholder, but should not throw
  }

  property("AOT Cost: exact threshold — just barely exceeds budget") {
    // Q=35, D=16 → 12350 JIT → 1235 block cost + byte ingestion
    // Set budget to exactly 1200 block cost → should throw
    val node = mkVerifyStark(q = 35, d = 16)
    val evaluator = createEvaluator(blockCostLimit = 1200)
    val ex = the[CostLimitException] thrownBy {
      evalDirect(evaluator, node)
    }
    ex.estimatedCost should be > 0L
  }

  property("AOT Cost: large Q overflows budget") {
    // Q=10000, D=100 → cost = 5000 + 10000*50 + 10000*100*10 = 10_505_000
    // 10.5M JIT → block cost ≈ 1,050,000 → exceeds 1M budget
    val node = mkVerifyStark(q = 10000, d = 100)
    val evaluator = createEvaluator(blockCostLimit = 1000000)
    the[CostLimitException] thrownBy {
      evalDirect(evaluator, node)
    }
  }

  property("AOT Cost: large D overflows budget") {
    // Q=100, D=10000 → cost = 5000 + 100*50 + 100*10000*10 = 10_010_000
    // 10M JIT → block cost ≈ 1,001,000 → exceeds 1M budget
    val node = mkVerifyStark(q = 100, d = 10000)
    val evaluator = createEvaluator(blockCostLimit = 1000000)
    the[CostLimitException] thrownBy {
      evalDirect(evaluator, node)
    }
  }

  // =====================================================================
  // Security: Graceful Degradation
  // =====================================================================
  property("Unknown vmType returns false (not throw)") {
    val node = mkVerifyStark(vmTypeVal = 999)
    val evaluator = createEvaluator(blockCostLimit = 1000000)
    val result = evalDirect(evaluator, node)
    result shouldBe false
  }

  property("Both Q and D negative returns false") {
    val node = mkVerifyStark(q = -100, d = -200)
    val evaluator = createEvaluator(blockCostLimit = 1000000)
    val result = evalDirect(evaluator, node)
    result shouldBe false
  }

  property("Zero-length publicInputs returns false (not throw)") {
    val node = mkVerifyStark(pubInputs = Array.emptyByteArray)
    val evaluator = createEvaluator(blockCostLimit = 1000000)
    val result = evalDirect(evaluator, node)
    result shouldBe false
  }

  // =====================================================================
  // Determinism: Same inputs → same cost
  // =====================================================================
  property("Cost is deterministic across evaluations") {
    val node = mkVerifyStark(q = 35, d = 16)

    val eval1 = createEvaluator(blockCostLimit = 1000000)
    evalDirect(eval1, node)
    val cost1 = eval1.getAccumulatedCost.value

    val eval2 = createEvaluator(blockCostLimit = 1000000)
    evalDirect(eval2, node)
    val cost2 = eval2.getAccumulatedCost.value

    cost1 shouldBe cost2
  }
}
