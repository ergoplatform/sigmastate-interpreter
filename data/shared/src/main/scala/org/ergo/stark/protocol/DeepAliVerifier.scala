package org.ergo.stark.protocol

import org.ergo.stark.field.{BabyBearField, QuadraticTower}
import org.ergo.stark.field.QuadraticTower.Ext16

/**
 * DEEP-ALI Verifier (Domain Extension for Eliminating Pretenders).
 *
 * Bridges the trace polynomial evaluations and the FRI verifier by:
 * 1. Verifying the AIR constraints at the OOD point z.
 * 2. Computing the DEEP composition polynomial evaluations at each
 *    query point x_q, which become the initial `queryEvals` for FRI.
 *
 * The DEEP composition formula (for each query x_q):
 *
 *   E(x_q) = Σ αᵢ · (Tᵢ(x_q) - Tᵢ(z)) / (x_q - z)
 *          + Σ βᵢ · (Tᵢ(x_q) - Tᵢ(z·g)) / (x_q - z·g)
 *          + γ  · (H(x_q) - H(z)) / (x_q - z)
 *
 * All 2Q+Q denominators are batch-inverted using ext16BatchInv
 * (Montgomery's trick: 1 true Ext16 inversion for all queries).
 */
object DeepAliVerifier {

  /**
   * Compute the DEEP composition polynomial values at each query point.
   *
   * @param transcript    Fiat-Shamir transcript (squeezes composition weights)
   * @param numColumns    number of trace columns
   * @param z             OOD evaluation point (Ext16)
   * @param zg            z * g (next-row OOD point)
   * @param traceOod      trace column evaluations at z: T_i(z)
   * @param nextTraceOod  trace column evaluations at z·g: T_i(z·g)
   * @param quotientOod   quotient polynomial evaluation H(z)
   * @param queryPoints   BabyBear evaluation points x_q for each query
   * @param traceAtQuery  trace column evaluations at each query: traceAtQuery(q)(i) = T_i(x_q)
   * @param quotientAtQuery quotient evaluations at each query: H(x_q)
   * @return array of DEEP composition values E(x_q), one per query
   */
  def composeDeepValues(
    transcript: FiatShamirTranscript,
    numColumns: Int,
    z: Ext16,
    zg: Ext16,
    traceOod: Array[Ext16],
    nextTraceOod: Array[Ext16],
    quotientOod: Ext16,
    queryPoints: Array[Int],
    traceAtQuery: Array[Array[Ext16]],
    quotientAtQuery: Array[Ext16]
  ): Array[Ext16] = {
    val Q = queryPoints.length

    // Squeeze composition weights from transcript
    val alphas = new Array[Ext16](numColumns)   // weights for (T_i(x) - T_i(z)) / (x - z)
    val betas = new Array[Ext16](numColumns)    // weights for (T_i(x) - T_i(z·g)) / (x - z·g)
    var i = 0
    while (i < numColumns) {
      alphas(i) = transcript.squeezeExt16()
      i += 1
    }
    i = 0
    while (i < numColumns) {
      betas(i) = transcript.squeezeExt16()
      i += 1
    }
    val gamma = transcript.squeezeExt16()       // weight for quotient term

    // ── Batch invert all denominators ──
    // For each query q: (x_q - z) and (x_q - z·g)
    val denominators = new Array[Ext16](2 * Q)
    var q = 0
    while (q < Q) {
      val xExt = QuadraticTower.fromScalar(queryPoints(q))
      denominators(q) = QuadraticTower.ext16Sub(xExt, z)        // x_q - z
      denominators(Q + q) = QuadraticTower.ext16Sub(xExt, zg)   // x_q - z·g
      q += 1
    }
    val invDenoms = QuadraticTower.ext16BatchInv(denominators)

    // ── Compose DEEP values ──
    val result = new Array[Ext16](Q)
    q = 0
    while (q < Q) {
      val invXZ = invDenoms(q)       // 1 / (x_q - z)
      val invXZG = invDenoms(Q + q)  // 1 / (x_q - z·g)

      var acc = QuadraticTower.EXT16_ZERO

      // Trace columns: α_i · (T_i(x_q) - T_i(z)) / (x_q - z)
      i = 0
      while (i < numColumns) {
        val diff = QuadraticTower.ext16Sub(traceAtQuery(q)(i), traceOod(i))
        val term = QuadraticTower.ext16Mul(alphas(i), QuadraticTower.ext16Mul(diff, invXZ))
        acc = QuadraticTower.ext16Add(acc, term)
        i += 1
      }

      // Next-row trace: β_i · (T_i(x_q) - T_i(z·g)) / (x_q - z·g)
      i = 0
      while (i < numColumns) {
        val diff = QuadraticTower.ext16Sub(traceAtQuery(q)(i), nextTraceOod(i))
        val term = QuadraticTower.ext16Mul(betas(i), QuadraticTower.ext16Mul(diff, invXZG))
        acc = QuadraticTower.ext16Add(acc, term)
        i += 1
      }

      // Quotient: γ · (H(x_q) - H(z)) / (x_q - z)
      val qDiff = QuadraticTower.ext16Sub(quotientAtQuery(q), quotientOod)
      val qTerm = QuadraticTower.ext16Mul(gamma, QuadraticTower.ext16Mul(qDiff, invXZ))
      acc = QuadraticTower.ext16Add(acc, qTerm)

      result(q) = acc
      q += 1
    }

    result
  }
}
