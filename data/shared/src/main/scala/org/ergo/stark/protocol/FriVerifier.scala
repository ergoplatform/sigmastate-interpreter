package org.ergo.stark.protocol

import org.ergo.stark.field.{BabyBearField, QuadraticTower}
import org.ergo.stark.field.QuadraticTower.Ext16
import org.ergo.stark.hash.Poseidon1BabyBear

/**
 * A single FRI round's proof data.
 *
 * @param commitment    Merkle root of the folded polynomial evaluations
 * @param cosets        Per-query cosets: Q arrays of 8 Ext16 evaluations each
 * @param merkleSiblings Deduplicated Merkle siblings for batch verification
 */
case class FriRound(
  commitment: Array[Int],
  cosets: Array[Array[Ext16]],
  merkleSiblings: Array[Array[Int]]
)

/**
 * FRI Verifier with Radix-8 folding.
 *
 * The folding uses an iFFT-8 (Inverse Fast Fourier Transform of size 8)
 * followed by Horner evaluation. This avoids all Ext16 inversions and
 * full Ext16×Ext16 multiplications during interpolation.
 *
 * Each fold reduces the domain by a factor of 8:
 *   - 8 evaluation points form a multiplicative coset
 *   - iFFT-8 interpolates the degree-7 polynomial through them
 *   - Horner evaluates P(α) at the Fiat-Shamir challenge α
 *
 * Cost per fold per query: ~800 base field muls (vs ~3000 with Lagrange).
 */
object FriVerifier {

  /** ω₈ = primitive 8th root of unity in BabyBear */
  val OMEGA_8: Int = BabyBearField.rootOfUnity(3)

  /** ω₈⁻¹ */
  val OMEGA_8_INV: Int = BabyBearField.inv(OMEGA_8)

  /** 8⁻¹ mod p (scaling factor for iFFT normalization) */
  val INV_8: Int = BabyBearField.inv(8)

  /** Precomputed twiddle factors for iFFT-8: ω₈^{-k} for k=0..7 */
  private val ZETAS: Array[Int] = {
    val z = new Array[Int](8)
    z(0) = BabyBearField.ONE
    var i = 1
    while (i < 8) {
      z(i) = BabyBearField.mul(z(i - 1), OMEGA_8_INV)
      i += 1
    }
    z
  }

  /**
   * Radix-8 FRI fold via iFFT-8 + Horner evaluation.
   *
   * Given 8 evaluations of a polynomial on a coset {x₀, x₀·ω, ..., x₀·ω⁷},
   * interpolates the unique degree-7 polynomial P and evaluates P(α).
   *
   * All butterfly operations use ext16ScalarMul (Ext16 × BabyBear scalar),
   * NOT ext16Mul (Ext16 × Ext16). This is the critical performance win.
   *
   * @param x0       generator of the coset (BabyBear element)
   * @param coset    8 Ext16 evaluations at {x₀·ω^k} for k=0..7
   * @param challenge Fiat-Shamir challenge α (Ext16 element)
   * @return P(α) as an Ext16 element
   */
  def foldRadix8(x0: Int, coset: Array[Ext16], challenge: Ext16): Ext16 = {
    require(coset.length == 8, s"Coset must have 8 elements, got ${coset.length}")

    val A = new Array[Ext16](8)

    // Bit-reverse permutation input (standard FFT ordering)
    A(0) = coset(0); A(1) = coset(4); A(2) = coset(2); A(3) = coset(6)
    A(4) = coset(1); A(5) = coset(5); A(6) = coset(3); A(7) = coset(7)

    // ── Stage 1: Length-2 butterflies ──
    var i = 0
    while (i < 8) {
      val u = A(i)
      val v = A(i + 1)
      A(i) = QuadraticTower.ext16Add(u, v)
      A(i + 1) = QuadraticTower.ext16Sub(u, v)
      i += 2
    }

    // ── Stage 2: Length-4 butterflies ──
    i = 0
    while (i < 8) {
      var j = 0
      while (j < 2) {
        val u = A(i + j)
        val v = QuadraticTower.ext16ScalarMul(ZETAS(j * 2), A(i + j + 2))
        A(i + j) = QuadraticTower.ext16Add(u, v)
        A(i + j + 2) = QuadraticTower.ext16Sub(u, v)
        j += 1
      }
      i += 4
    }

    // ── Stage 3: Length-8 butterflies ──
    var j = 0
    while (j < 4) {
      val u = A(j)
      val v = QuadraticTower.ext16ScalarMul(ZETAS(j), A(j + 4))
      A(j) = QuadraticTower.ext16Add(u, v)
      A(j + 4) = QuadraticTower.ext16Sub(u, v)
      j += 1
    }

    // ── Normalize: multiply all coefficients by 1/8 ──
    i = 0
    while (i < 8) {
      A(i) = QuadraticTower.ext16ScalarMul(INV_8, A(i))
      i += 1
    }

    // A now contains the 8 coefficients of P(X) in the coset basis.
    // Evaluate P(challenge) via Horner with z = challenge / x₀:
    //   P(α) = c₀ + c₁·z + c₂·z² + ... + c₇·z⁷
    //        = c₀ + z·(c₁ + z·(c₂ + ... + z·c₇))

    val x0Inv = BabyBearField.inv(x0)
    val z = QuadraticTower.ext16ScalarMul(x0Inv, challenge)

    var result = A(7)
    i = 6
    while (i >= 0) {
      result = QuadraticTower.ext16Add(A(i), QuadraticTower.ext16Mul(result, z))
      i -= 1
    }
    result
  }

  /**
   * Hash a coset of 8 Ext16 elements (128 BabyBear elements)
   * into a Merkle leaf digest.
   */
  def hashCoset(coset: Array[Ext16]): Array[Int] = {
    val flat = new Array[Int](128)
    var i = 0
    while (i < 8) {
      val arr = QuadraticTower.toFlat(coset(i))
      System.arraycopy(arr, 0, flat, i * 16, 16)
      i += 1
    }
    Poseidon1BabyBear.hash(flat, Poseidon1BabyBear.C)
  }

  /**
   * Full FRI verification.
   *
   * @param transcript       Fiat-Shamir transcript (absorbs commitments, squeezes challenges)
   * @param initialDomainSize size of the initial evaluation domain (power of 2)
   * @param initialGenerator  generator of the initial domain
   * @param queries          Q query indices into the initial domain
   * @param queryEvals       Q initial evaluations (from trace polynomial)
   * @param rounds           array of FRI round proof data
   * @param remainderCoeffs  coefficients of the final remainder polynomial
   * @param publicSeed       seed for tweakable Merkle hashing
   * @return true if the FRI proof verifies
   */
  def verify(
    transcript: FiatShamirTranscript,
    initialDomainSize: Int,
    initialGenerator: Int,
    queries: Array[Int],
    queryEvals: Array[Ext16],
    rounds: Array[FriRound],
    remainderCoeffs: Array[Ext16],
    publicSeed: Array[Int]
  ): Boolean = {
    val Q = queries.length
    var currentDomainSize = initialDomainSize
    var currentGenerator = initialGenerator
    var currentQueries = queries.clone()
    var currentEvals = queryEvals.clone()

    var r = 0
    while (r < rounds.length) {
      val round = rounds(r)
      val nextDomainSize = currentDomainSize / 8

      // Absorb commitment and derive folding challenge
      transcript.absorbFieldElements(round.commitment)
      val alpha = transcript.squeezeExt16()

      val cosetIndices = new Array[Int](Q)
      val leafDigests = new Array[Array[Int]](Q)

      var q = 0
      while (q < Q) {
        val idx = currentQueries(q)

        // Interleaved domain: cosetIdx identifies which coset, posInCoset is position within
        val cosetIdx = idx % nextDomainSize
        val posInCoset = idx / nextDomainSize

        cosetIndices(q) = cosetIdx

        val coset = round.cosets(q)

        // Verify the claimed evaluation matches the coset at the correct position
        if (coset(posInCoset) != currentEvals(q)) return false

        leafDigests(q) = hashCoset(coset)
        q += 1
      }

      // Batch Merkle verification of all coset commitments
      val uniqueMap = scala.collection.mutable.TreeMap.empty[Int, Array[Int]]
      q = 0
      while (q < Q) {
        uniqueMap(cosetIndices(q)) = leafDigests(q)
        q += 1
      }
      val sortedIndices = uniqueMap.keys.toArray
      val sortedDigests = uniqueMap.values.toArray
      val depth = Integer.numberOfTrailingZeros(nextDomainSize)

      val merkleValid = MerkleVerifier.verifyBatch(
        round.commitment, depth, sortedIndices, sortedDigests,
        round.merkleSiblings, publicSeed
      )
      if (!merkleValid) return false

      // Fold each query via iFFT-8 + Horner
      val nextQueries = new Array[Int](Q)
      val nextEvals = new Array[Ext16](Q)

      q = 0
      while (q < Q) {
        val x0 = BabyBearField.pow(currentGenerator, cosetIndices(q))
        nextQueries(q) = cosetIndices(q)
        nextEvals(q) = foldRadix8(x0, round.cosets(q), alpha)
        q += 1
      }

      currentQueries = nextQueries
      currentEvals = nextEvals
      currentDomainSize = nextDomainSize
      currentGenerator = BabyBearField.pow(currentGenerator, 8)
      r += 1
    }

    // ── Remainder check: evaluate the final polynomial at each query point ──
    if (remainderCoeffs.length > currentDomainSize) return false

    var q = 0
    while (q < Q) {
      val x = BabyBearField.pow(currentGenerator, currentQueries(q))
      val xExt = QuadraticTower.fromScalar(x)

      // Horner evaluation of remainder polynomial
      var eval = QuadraticTower.EXT16_ZERO
      if (remainderCoeffs.nonEmpty) {
        eval = remainderCoeffs(remainderCoeffs.length - 1)
        var i = remainderCoeffs.length - 2
        while (i >= 0) {
          eval = QuadraticTower.ext16Add(remainderCoeffs(i), QuadraticTower.ext16Mul(eval, xExt))
          i -= 1
        }
      }

      if (eval != currentEvals(q)) return false
      q += 1
    }

    true
  }
}
