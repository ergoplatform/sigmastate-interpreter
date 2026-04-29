package sigma.ast

import sigma.Coll
import sigma.ast.SCollection.SByteArray
import sigma.eval.ErgoTreeEvaluator
import sigma.eval.ErgoTreeEvaluator.DataEnv
import sigma.exceptions.CostLimitException
import sigma.serialization.OpCodes
import sigma.serialization.ValueCodes.OpCode

/**
 * Native STARK Proof Verifier Node (EIP-0045).
 *
 * Verifies a STARK proof on-chain, enabling zero-knowledge virtual machine
 * execution verification as a first-class L1 operation.
 *
 * The AOT (Ahead-Of-Time) costing model evaluates `costParams` and `vmType`
 * BEFORE touching the heavy byte arrays, providing a fail-fast guarantee
 * against block budget exhaustion without instantiating proof data.
 *
 * @param proofChunks  chunked proof data (avoids SigmaSerializer OOM on large proofs)
 * @param publicInputs public inputs to the verified computation
 * @param imageId      hash identifying the zkVM program image
 * @param vmType       AIR registry identifier (0 = Fibonacci test, future: RISC-V, etc.)
 * @param costParams   [Q, D, ...] parameters for AOT cost derivation:
 *                     Q = number of FRI queries, D = Merkle tree depth
 */
case class VerifyStark(
  proofChunks: Value[SCollection[SCollection[SByte.type]]],
  publicInputs: Value[SByteArray],
  imageId: Value[SByteArray],
  vmType: Value[SInt.type],
  costParams: Value[SCollection[SInt.type]]
) extends Value[SBoolean.type] with NotReadyValueBoolean {

  override def companion = VerifyStark

  override def opType: SFunc = VerifyStark.OpType

  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    // ================================================================
    // PHASE 1: AOT PREEMPTIVE COST — Evaluate O(1) scalars FIRST
    // ================================================================
    val vmTypeV = vmType.evalTo[Int](env)
    val costsV = costParams.evalTo[Coll[Int]](env)

    // Fail-fast on corrupted/malicious parameters
    if (costsV.length < 2 || costsV(0) < 0 || costsV(1) < 0 || vmTypeV < 0) {
      return false
    }

    val numQueries = costsV(0)   // Q: number of FRI queries
    val merkleDepth = costsV(1)  // D: Merkle tree depth

    // Compute AOT cost from proof metadata (O(1), no heavy data touched)
    val aotCostValue = VerifyStark.BASE_COST +
      (numQueries * VerifyStark.PER_QUERY_COST) +
      (numQueries * merkleDepth * VerifyStark.PER_MERKLE_LAYER_COST)

    // FAIL-FAST: charge the full mathematical cost upfront
    // This throws CostLimitException if block budget is exceeded
    E.addCost(FixedCost(JitCost(aotCostValue)), VerifyStark.opDesc)

    // ================================================================
    // PHASE 2: Evaluate heavy byte arrays (only after cost is secured)
    // ================================================================
    val chunksV = proofChunks.evalTo[Coll[Coll[Byte]]](env)
    val pubInputsV = publicInputs.evalTo[Coll[Byte]](env)
    val imageIdV = imageId.evalTo[Coll[Byte]](env)

    // Compute total byte count for ingestion cost
    var totalBytes = pubInputsV.length + imageIdV.length
    var i = 0
    while (i < chunksV.length) {
      totalBytes += chunksV(i).length
      i += 1
    }

    // ================================================================
    // PHASE 3: Charge byte processing cost, then verify
    // ================================================================
    addSeqCost(VerifyStark.byteIngestionCost, totalBytes) { () =>
      try {
        // TODO (Phase 5): Wire to StarkVerifier.verify(...)
        // val proof = StarkProofDeserializer.deserialize(chunksV, vmTypeV)
        // val publicSeed = pubInputsV.toArray ++ imageIdV.toArray
        // StarkVerifier.verify(proof, publicSeed)
        false // Placeholder — returns false until verifier is wired
      } catch {
        case e: CostLimitException => throw e // MUST propagate — AOT fail-fast guarantee
        case _: Throwable => false // Soft-fork: invalid proof = false, never throw
      }
    }
  }
}

object VerifyStark extends ValueCompanion {
  override def opCode: OpCode = OpCodes.VerifyStarkCode

  /** Type signature: (Coll[Coll[Byte]], Coll[Byte], Coll[Byte], Int, Coll[Int]) => Boolean */
  val OpType: SFunc = SFunc(
    IndexedSeq(
      SCollection(SCollection(SByte)),  // proofChunks
      SByteArray,                       // publicInputs
      SByteArray,                       // imageId
      SInt,                             // vmType
      SCollection(SInt)                 // costParams
    ),
    SBoolean
  )

  // ======================================================================
  // AOT Cost Constants (preliminary — to be calibrated via JMH benchmarks)
  // ======================================================================

  /** Fixed overhead: transcript setup, AIR constraint check, OOD derivation, DEEP-ALI */
  val BASE_COST: Int = 5000

  /** Per-query cost: iFFT-8 folding + Horner evaluation per query per round */
  val PER_QUERY_COST: Int = 50

  /** Per-query-per-Merkle-layer: hash verification for each query's Merkle path */
  val PER_MERKLE_LAYER_COST: Int = 10

  /** Byte ingestion cost: protects against padding/spam attacks */
  val byteIngestionCost: PerItemCost = PerItemCost(
    baseCost = JitCost(10),
    perChunkCost = JitCost(1),
    chunkSize = 1024
  )

  /** The overall costKind reported to the framework.
   *  The actual dynamic cost is computed in eval() from costParams. */
  override val costKind: FixedCost = FixedCost(JitCost(BASE_COST))
}
