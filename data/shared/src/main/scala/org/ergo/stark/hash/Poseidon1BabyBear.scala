package org.ergo.stark.hash

import org.ergo.stark.field.BabyBearField
import org.ergo.stark.field.BabyBearField.{mul => bMul, add => bAdd, inv => bInv, P}

/**
 * Poseidon1 hash function over BabyBear field.
 *
 * Parameters (from EIP-0045):
 *   - State width:     t = 24
 *   - Capacity:        c = 13
 *   - Rate:            r = 11
 *   - S-box:           α = 7 (x^7)
 *   - Full rounds:     R_f = 8 (4 before + 4 after partial rounds)
 *   - Partial rounds:  R_p = 22
 *   - MDS matrix:      Dense Cauchy over BabyBear
 *
 * This is Poseidon1 with dense MDS (NOT Poseidon2 with sparse matrix,
 * which is vulnerable to subspace trail attacks per Grassi et al. 2025).
 *
 * Security invariant: The Squeeze phase NEVER reads from the capacity
 * portion of the state (indices R..T-1). If more than R output elements
 * are needed, additional permutations are applied between reads.
 */
object Poseidon1BabyBear {

  /** State width */
  val T: Int = 24

  /** Capacity (security parameter) */
  val C: Int = 13

  /** Rate (absorbable elements per permutation) */
  val R: Int = 11

  /** S-box exponent */
  val ALPHA: Int = 7

  /** Full rounds (split: R_F/2 before partial, R_F/2 after) */
  val R_F: Int = 8

  /** Partial rounds */
  val R_P: Int = 22

  /** Total rounds */
  val TOTAL_ROUNDS: Int = R_F + R_P  // 30

  // ═══════════════════════════════════════════════════════════════
  // MDS Matrix: Dense Cauchy Construction
  //
  // M[i][j] = 1 / (x_i + y_j) mod p
  // where x_i = i, y_j = T + j  (disjoint sets ensure non-singularity)
  // ═══════════════════════════════════════════════════════════════

  /** The T×T Cauchy MDS matrix, precomputed */
  val MDS: Array[Array[Int]] = {
    val m = Array.ofDim[Int](T, T)
    var i = 0
    while (i < T) {
      var j = 0
      while (j < T) {
        m(i)(j) = bInv(bAdd(i, T + j))
        j += 1
      }
      i += 1
    }
    m
  }

  // ═══════════════════════════════════════════════════════════════
  // Round Constants — NUMS derivation via SHA-256
  // ═══════════════════════════════════════════════════════════════

  /** Round constants: TOTAL_ROUNDS × T matrix */
  val ROUND_CONSTANTS: Array[Array[Int]] = generateRoundConstants()

  private def generateRoundConstants(): Array[Array[Int]] = {
    val seed = "Poseidon1-BabyBear-t24-alpha7-Rf8-Rp22"
    val md = java.security.MessageDigest.getInstance("SHA-256")
    // Note: Using SHA-256 as Blake2b is not in default JVM providers.
    // When integrating into sigmastate-interpreter (which has Blake2b),
    // this should be switched to Blake2b-256 for consistency with EIP-0045.

    val constants = Array.ofDim[Int](TOTAL_ROUNDS, T)
    var counter = 0

    var round = 0
    while (round < TOTAL_ROUNDS) {
      var elem = 0
      while (elem < T) {
        md.reset()
        md.update(seed.getBytes("UTF-8"))
        md.update(((counter >> 24) & 0xFF).toByte)
        md.update(((counter >> 16) & 0xFF).toByte)
        md.update(((counter >> 8) & 0xFF).toByte)
        md.update((counter & 0xFF).toByte)

        val hash = md.digest()
        val value = ((hash(0) & 0xFF).toLong << 24) |
                    ((hash(1) & 0xFF).toLong << 16) |
                    ((hash(2) & 0xFF).toLong << 8) |
                    (hash(3) & 0xFF).toLong
        constants(round)(elem) = BabyBearField.reduce(value)

        counter += 1
        elem += 1
      }
      round += 1
    }
    constants
  }

  // ═══════════════════════════════════════════════════════════════
  // Core Permutation — Zero-allocation inner loop
  //
  // All round functions mutate `state` in-place using a single
  // pre-allocated `temp` buffer. No heap allocation per round.
  // ═══════════════════════════════════════════════════════════════

  /** S-box: x^7 = x^4 · x^2 · x (3 multiplications) */
  @inline
  private def sbox(x: Int): Int = {
    val x2 = bMul(x, x)
    val x4 = bMul(x2, x2)
    bMul(bMul(x4, x2), x)
  }

  /** MDS matrix-vector product, computed in-place via temp buffer */
  private def mdsMultiply(state: Array[Int], temp: Array[Int]): Unit = {
    var i = 0
    while (i < T) {
      var acc = 0L
      var j = 0
      while (j < T) {
        acc += state(j).toLong * MDS(i)(j).toLong % P
        j += 1
      }
      temp(i) = BabyBearField.reduce(acc)
      i += 1
    }
    System.arraycopy(temp, 0, state, 0, T)
  }

  /** Add round constants to state in-place */
  private def addRoundConstants(state: Array[Int], round: Int): Unit = {
    var i = 0
    while (i < T) {
      state(i) = bAdd(state(i), ROUND_CONSTANTS(round)(i))
      i += 1
    }
  }

  /** Full round: ARC → S-box on ALL elements → MDS (in-place) */
  private def fullRound(state: Array[Int], temp: Array[Int], round: Int): Unit = {
    addRoundConstants(state, round)
    var i = 0
    while (i < T) {
      state(i) = sbox(state(i))
      i += 1
    }
    mdsMultiply(state, temp)
  }

  /** Partial round: ARC → S-box on FIRST element → MDS (in-place) */
  private def partialRound(state: Array[Int], temp: Array[Int], round: Int): Unit = {
    addRoundConstants(state, round)
    state(0) = sbox(state(0))
    mdsMultiply(state, temp)
  }

  /**
   * The Poseidon1 permutation: transforms state IN-PLACE.
   *
   * Structure (ARC-first): R_F/2 full → R_P partial → R_F/2 full
   * Single temp buffer allocation for all 30 rounds.
   */
  def permute(state: Array[Int]): Unit = {
    require(state.length == T, s"State must have $T elements, got ${state.length}")

    val temp = new Array[Int](T)  // Single allocation per permutation
    var round = 0
    val halfF = R_F / 2

    var i = 0
    while (i < halfF) {
      fullRound(state, temp, round); round += 1; i += 1
    }

    i = 0
    while (i < R_P) {
      partialRound(state, temp, round); round += 1; i += 1
    }

    i = 0
    while (i < halfF) {
      fullRound(state, temp, round); round += 1; i += 1
    }
  }

  // ═══════════════════════════════════════════════════════════════
  // Sponge Construction
  //
  // SECURITY INVARIANT: Squeeze NEVER reads from capacity (state[R..T-1]).
  // If outputLen > R, additional permutations are interleaved.
  // Default output = C (13 elements = 403 bits) for ≥128-bit PQ collision.
  // ═══════════════════════════════════════════════════════════════

  /**
   * Hash a variable-length sequence of BabyBear field elements.
   *
   * @param outputLen defaults to C (13) for 128-bit PQ collision resistance.
   *                  Must be > 0. Squeeze reads ONLY from rate portion.
   */
  def hash(input: Array[Int], outputLen: Int = C): Array[Int] = {
    require(outputLen > 0, s"outputLen must be positive, got $outputLen")

    val state = new Array[Int](T)
    // Domain separation: encode input length in first capacity slot
    state(R) = input.length

    // ── Absorb ──
    var offset = 0
    while (offset < input.length) {
      val chunkSize = math.min(R, input.length - offset)
      var i = 0
      while (i < chunkSize) {
        state(i) = bAdd(state(i), input(offset + i))
        i += 1
      }
      permute(state)
      offset += chunkSize
    }

    if (input.length == 0) {
      permute(state)
    }

    // ── Squeeze (capacity-safe) ──
    // Read at most R elements per permutation, from rate only (indices 0..R-1).
    // If more output is needed, re-permute before reading the next chunk.
    val out = new Array[Int](outputLen)
    var outOffset = 0
    while (outOffset < outputLen) {
      val chunkSize = math.min(R, outputLen - outOffset)
      System.arraycopy(state, 0, out, outOffset, chunkSize)
      outOffset += chunkSize

      if (outOffset < outputLen) {
        permute(state)
      }
    }
    out
  }

  /**
   * Hash two digests for Merkle tree internal nodes.
   * @return C-element (13) digest for 128-bit PQ collision resistance
   */
  def hashTwo(left: Array[Int], right: Array[Int]): Array[Int] = {
    hash(left ++ right, C)
  }
}
