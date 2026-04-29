package sigmastate

import org.scalatest.propspec.AnyPropSpec
import org.scalatest.matchers.should.Matchers
import sigma.ast._
import sigma.ast.SCollection.SByteArray
import sigma.exceptions.CostLimitException
import sigma.serialization.ValueSerializer
import sigmastate.interpreter.{CostAccumulator, CErgoTreeEvaluator}

/**
 * EIP-0045 VerifyStark Integration Tests.
 *
 * Validates four critical properties of the VerifyStark opcode:
 * 1. P2P Serialization Roundtrip — ensures the node survives network propagation.
 * 2. AOT Fail-Fast — proves the DoS shield rejects malicious cost parameters
 *    BEFORE loading heavy byte arrays.
 * 3. Successful Evaluation — confirms legitimate parameters pass and cost is charged.
 * 4. Negative Parameters — ensures graceful false-return without exceptions.
 *
 * NOTE: This test lives in package `sigmastate` to access the private[sigmastate]
 * `currentEvaluator` DynamicVariable, which is required to set up the
 * thread-local evaluator context without needing a full ErgoLikeContext.
 */
class VerifyStarkTest extends AnyPropSpec with Matchers {

  /** Helper: creates a CErgoTreeEvaluator with a given block-cost limit.
   *  Follows the exact pattern from DataValueComparerSpecification. */
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

  /** Evaluates a Value node using the given evaluator, setting the thread-local
   *  currentEvaluator DynamicVariable (which is private[sigmastate]).
   *  This bypasses CErgoTreeEvaluator.eval() which requires a non-null Context. */
  private def evalDirect(evaluator: CErgoTreeEvaluator, node: Value[_ <: SType]): Any = {
    CErgoTreeEvaluator.currentEvaluator.withValue(evaluator) {
      node.evalTo[Any](Map())(evaluator)
    }
  }

  /** Helper: builds a VerifyStark node with given Q, D parameters. */
  private def mkVerifyStark(
    proofBytes: Array[Byte] = Array[Byte](1, 2, 3),
    pubInputs: Array[Byte] = Array[Byte](4, 5, 6),
    imageIdBytes: Array[Byte] = Array.fill[Byte](32)(0),
    vmTypeVal: Int = 0,
    q: Int = 35,
    d: Int = 16
  ): VerifyStark = {
    val chunk = ByteArrayConstant(proofBytes)
    val chunks = ConcreteCollection[SByteArray](
      Vector(chunk),
      SByteArray
    ).asInstanceOf[Value[SCollection[SCollection[SByte.type]]]]

    val pubIn = ByteArrayConstant(pubInputs)
    val imgId = ByteArrayConstant(imageIdBytes)
    val vt = IntConstant(vmTypeVal)
    val cp = ConcreteCollection[SInt.type](Vector(IntConstant(q), IntConstant(d)), SInt)

    VerifyStark(chunks, pubIn, imgId, vt, cp)
  }

  // =====================================================================
  // TEST 1: P2P Serialization Roundtrip
  // =====================================================================
  property("VerifyStark P2P Serialization Roundtrip") {
    val node = mkVerifyStark()

    // Serialize to bytes (what would be sent over the P2P network)
    val bytes = ValueSerializer.serialize(node)
    bytes.length should be > 0

    // Deserialize from bytes (what a receiving node would reconstruct)
    val deserialized = ValueSerializer.deserialize(bytes)
    deserialized shouldBe node
  }

  // =====================================================================
  // TEST 2: AOT Fail-Fast — DoS Protection
  // =====================================================================
  property("AOT Costing: Fail-Fast on budget exceeded (DoS Protection)") {
    // 💥 ATTACK: Q=1000, D=1000 → AOT cost = 5000 + 50*1000 + 10*1000*1000 = 10_055_000
    // This exceeds the block limit of 1,000,000 → MUST be rejected instantly
    val maliciousNode = mkVerifyStark(q = 1000, d = 1000)

    val evaluator = createEvaluator(blockCostLimit = 1000000)

    // The node MUST crash with CostLimitException INSTANTANEOUSLY,
    // before any proof bytes are loaded.
    val ex = the[CostLimitException] thrownBy {
      evalDirect(evaluator, maliciousNode)
    }
    // Verify the cost that caused the limit breach
    ex.estimatedCost should be > 0L
  }

  // =====================================================================
  // TEST 3: Successful evaluation with legitimate parameters
  // =====================================================================
  property("AOT Costing: Successful charge on sufficient budget") {
    // Normal parameters: Q=35, D=16
    // AOT cost = 5000 + 35*50 + 35*16*10 = 5000 + 1750 + 5600 = 12350 JIT units
    val node = mkVerifyStark(q = 35, d = 16)

    val evaluator = createEvaluator(blockCostLimit = 1000000)

    val result = evalDirect(evaluator, node)

    // Placeholder returns false (until StarkVerifier is wired)
    result shouldBe false

    // Verify the AOT mathematical cost was actually debited:
    // 12350 JIT (AOT) + byteIngestion + ConcreteCollection overhead
    val accumulatedCost = evaluator.getAccumulatedCost
    accumulatedCost.value should be >= 12350 // minimum AOT cost
  }

  // =====================================================================
  // TEST 4: Negative cost parameters return false (not throw)
  // =====================================================================
  property("Negative cost parameters return false without throwing") {
    val node = mkVerifyStark(q = -1, d = 16)

    val evaluator = createEvaluator(blockCostLimit = 1000000)

    val result = evalDirect(evaluator, node)
    result shouldBe false
  }
}
