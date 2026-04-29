package org.ergo.stark.protocol

import org.ergo.stark.field.{BabyBearField, QuadraticTower}
import org.ergo.stark.field.QuadraticTower.Ext16
import org.ergo.stark.protocol.air.AirRegistry

/**
 * Complete STARK proof structure.
 *
 * This is the deserialized proof blob that the EIP-0045 opcode receives.
 */
case class StarkProof(
  vmType: Int,                          // AIR identifier (0 = Fibonacci)
  domainSize: Int,                      // Execution domain size (power of 2)
  numQueries: Int,                      // Q = number of FRI queries
  traceRoot: Array[Int],                // Merkle root of trace columns
  quotientRoot: Array[Int],             // Merkle root of quotient polynomial
  traceOod: Array[Ext16],              // T_i(z) evaluations at OOD point
  nextTraceOod: Array[Ext16],          // T_i(z·g) evaluations
  quotientOod: Ext16,                  // H(z) evaluation at OOD point
  traceOpenings: Array[Array[Ext16]],  // Per-query trace column evals: traceOpenings(q)(col)
  traceProof: Array[Array[Int]],       // Batch Merkle siblings for trace openings
  quotientOpenings: Array[Ext16],      // Per-query quotient evals: H(x_q)
  quotientProof: Array[Array[Int]],    // Batch Merkle siblings for quotient openings
  friRounds: Array[FriRound],          // FRI folding rounds
  remainderCoeffs: Array[Ext16]        // Final remainder polynomial
)

/**
 * Top-level STARK Verifier: the function that sigmastate-interpreter will call.
 *
 * Orchestrates the full verification pipeline:
 * 1. Transcript setup & commitment absorption
 * 2. OOD point derivation
 * 3. AIR constraint check at OOD
 * 4. DEEP-ALI composition
 * 5. FRI verification
 *
 * This is the candidate implementation for the EIP-0045 opcode.
 */
object StarkVerifier {

  /**
   * Verify a STARK proof.
   *
   * @param proof      the deserialized proof structure
   * @param publicSeed public randomness seed (for Merkle tweakable hashing)
   * @return true if the proof is valid
   */
  def verify(proof: StarkProof, publicSeed: Array[Int]): Boolean = {
    val air = AirRegistry.getEvaluator(proof.vmType)
    val numCols = air.numColumns
    val Q = proof.numQueries

    // ══════════════════════════════════════════
    // 1. Initialize Fiat-Shamir transcript
    // ══════════════════════════════════════════
    val transcript = FiatShamirTranscript("eip-0045-stark")

    // Absorb public commitments
    transcript.absorbFieldElements(proof.traceRoot)
    transcript.absorbFieldElements(proof.quotientRoot)

    // ══════════════════════════════════════════
    // 2. Derive OOD evaluation point z
    // ══════════════════════════════════════════
    val z = transcript.squeezeExt16()
    val domainGenerator = BabyBearField.rootOfUnity(
      Integer.numberOfTrailingZeros(proof.domainSize)
    )
    val zg = QuadraticTower.ext16ScalarMul(domainGenerator, z)

    // Absorb OOD evaluations
    proof.traceOod.foreach(e => transcript.absorbFieldElements(QuadraticTower.toFlat(e)))
    proof.nextTraceOod.foreach(e => transcript.absorbFieldElements(QuadraticTower.toFlat(e)))
    transcript.absorbFieldElements(QuadraticTower.toFlat(proof.quotientOod))

    // ══════════════════════════════════════════
    // 3. AIR constraint verification at OOD
    // ══════════════════════════════════════════
    val airValid = air.evaluateConstraints(
      z, proof.traceOod, proof.nextTraceOod, proof.quotientOod, proof.domainSize
    )
    if (!airValid) return false

    // ══════════════════════════════════════════
    // 4. Derive query indices
    // ══════════════════════════════════════════
    val friDomainSize = proof.domainSize  // FRI operates on the full evaluation domain
    val queryIndices = transcript.squeezeQueryIndices(Q, friDomainSize)

    // ══════════════════════════════════════════
    // 5. Verify Merkle openings (Trace + Quotient)
    // ══════════════════════════════════════════

    // Hash trace openings into leaf digests
    val traceLeafDigests = proof.traceOpenings.map { cols =>
      val flat = new Array[Int](cols.length * 16)
      var c = 0
      while (c < cols.length) {
        val cf = QuadraticTower.toFlat(cols(c))
        System.arraycopy(cf, 0, flat, c * 16, 16)
        c += 1
      }
      org.ergo.stark.hash.Poseidon1BabyBear.hash(flat, 13)
    }

    val traceDepth = Integer.numberOfTrailingZeros(friDomainSize)
    val traceMerkleValid = MerkleVerifier.verifyBatch(
      proof.traceRoot, traceDepth, queryIndices, traceLeafDigests,
      proof.traceProof, publicSeed
    )
    if (!traceMerkleValid) return false

    // Hash quotient openings into leaf digests
    val quotientLeafDigests = proof.quotientOpenings.map { qEval =>
      org.ergo.stark.hash.Poseidon1BabyBear.hash(QuadraticTower.toFlat(qEval), 13)
    }

    val quotientMerkleValid = MerkleVerifier.verifyBatch(
      proof.quotientRoot, traceDepth, queryIndices, quotientLeafDigests,
      proof.quotientProof, publicSeed
    )
    if (!quotientMerkleValid) return false

    // ══════════════════════════════════════════
    // 6. DEEP-ALI composition
    // ══════════════════════════════════════════
    val queryPoints = queryIndices.map(idx =>
      BabyBearField.pow(domainGenerator, idx)
    )

    val deepEvals = DeepAliVerifier.composeDeepValues(
      transcript, numCols,
      z, zg,
      proof.traceOod, proof.nextTraceOod, proof.quotientOod,
      queryPoints,
      proof.traceOpenings,
      proof.quotientOpenings
    )

    // ══════════════════════════════════════════
    // 7. FRI verification
    // ══════════════════════════════════════════
    val friGenerator = domainGenerator  // FRI domain uses same generator
    FriVerifier.verify(
      transcript,
      friDomainSize, friGenerator,
      queryIndices, deepEvals,
      proof.friRounds,
      proof.remainderCoeffs, publicSeed
    )
  }
}
