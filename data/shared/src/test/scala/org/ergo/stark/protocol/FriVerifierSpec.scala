package org.ergo.stark.protocol

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.ergo.stark.field.{BabyBearField, QuadraticTower}
import org.ergo.stark.field.QuadraticTower.Ext16

class FriVerifierSpec extends AnyFlatSpec with Matchers {

  import BabyBearField.{mul => bMul, add => bAdd, pow => bPow, inv => bInv}

  val omega8: Int = FriVerifier.OMEGA_8

  // ══════════════════════════════════════════
  // Helper: evaluate a polynomial (in monomial basis) at a point
  // ══════════════════════════════════════════

  /** Evaluate polynomial with Ext16 coefficients at a BabyBear scalar point via Horner */
  private def evalPoly(coeffs: Array[Ext16], x: Int): Ext16 = {
    if (coeffs.isEmpty) return QuadraticTower.EXT16_ZERO
    val xExt = QuadraticTower.fromScalar(x)
    var result = coeffs(coeffs.length - 1)
    var i = coeffs.length - 2
    while (i >= 0) {
      result = QuadraticTower.ext16Add(coeffs(i), QuadraticTower.ext16Mul(result, xExt))
      i -= 1
    }
    result
  }

  /** Build coset evaluations: P(x₀·ω⁰), P(x₀·ω¹), ..., P(x₀·ω⁷) */
  private def buildCoset(coeffs: Array[Ext16], x0: Int): Array[Ext16] = {
    val coset = new Array[Ext16](8)
    var k = 0
    while (k < 8) {
      val point = bMul(x0, bPow(omega8, k))
      coset(k) = evalPoly(coeffs, point)
      k += 1
    }
    coset
  }

  // ══════════════════════════════════════════
  // iFFT-8 Correctness
  // ══════════════════════════════════════════

  "foldRadix8" should "correctly interpolate a constant polynomial" in {
    // P(X) = c₀ (constant)
    val c0 = QuadraticTower.fromScalar(42)
    val coeffs = Array(c0) ++ Array.fill(7)(QuadraticTower.EXT16_ZERO)

    val x0 = 7  // arbitrary coset generator
    val coset = buildCoset(coeffs, x0)

    // For any challenge α, P(α) should be c₀
    val alpha = QuadraticTower.fromScalar(1337)
    val result = FriVerifier.foldRadix8(x0, coset, alpha)

    result shouldBe c0
  }

  it should "correctly interpolate a linear polynomial" in {
    // P(X) = 5 + 3X
    val c0 = QuadraticTower.fromScalar(5)
    val c1 = QuadraticTower.fromScalar(3)
    val coeffs = Array(c0, c1) ++ Array.fill(6)(QuadraticTower.EXT16_ZERO)

    val x0 = 11
    val coset = buildCoset(coeffs, x0)
    val alpha = QuadraticTower.fromScalar(100)

    val result = FriVerifier.foldRadix8(x0, coset, alpha)
    val expected = evalPoly(coeffs, 100)

    result shouldBe expected
  }

  it should "correctly interpolate a degree-7 polynomial" in {
    // P(X) = 1 + 2X + 3X² + 4X³ + 5X⁴ + 6X⁵ + 7X⁶ + 8X⁷
    val coeffs = (1 to 8).map(i => QuadraticTower.fromScalar(i)).toArray

    val x0 = 13
    val coset = buildCoset(coeffs, x0)
    val alpha = QuadraticTower.fromScalar(777)

    val result = FriVerifier.foldRadix8(x0, coset, alpha)
    val expected = evalPoly(coeffs, 777)

    result shouldBe expected
  }

  it should "work with Ext16 coefficients (not just scalars)" in {
    // Build coefficients as non-trivial Ext16 elements
    val coeffs = new Array[Ext16](8)
    var i = 0
    while (i < 8) {
      val flat = new Array[Int](16)
      var j = 0
      while (j < 16) {
        flat(j) = BabyBearField.mul(i + 1, j + 1)  // deterministic non-zero
        j += 1
      }
      coeffs(i) = QuadraticTower.fromFlat(flat)
      i += 1
    }

    val x0 = 17
    val coset = buildCoset(coeffs, x0)

    // Use an Ext16 challenge (not a scalar)
    val challengeFlat = Array.tabulate(16)(k => BabyBearField.mul(42, k + 1))
    val alpha = QuadraticTower.fromFlat(challengeFlat)

    val result = FriVerifier.foldRadix8(x0, coset, alpha)

    // Directly evaluate P(alpha) via Horner with Ext16 arithmetic
    var expected = coeffs(7)
    i = 6
    while (i >= 0) {
      expected = QuadraticTower.ext16Add(coeffs(i), QuadraticTower.ext16Mul(expected, alpha))
      i -= 1
    }

    result shouldBe expected
  }

  it should "work with x0 = 1 (the trivial coset)" in {
    val coeffs = Array.tabulate(8)(i => QuadraticTower.fromScalar(i * 10 + 1))
    val x0 = 1
    val coset = buildCoset(coeffs, x0)
    val alpha = QuadraticTower.fromScalar(999)

    FriVerifier.foldRadix8(x0, coset, alpha) shouldBe evalPoly(coeffs, 999)
  }

  // ══════════════════════════════════════════
  // Coset Hashing
  // ══════════════════════════════════════════

  "hashCoset" should "produce a C-element digest" in {
    val coset = Array.tabulate(8)(i => QuadraticTower.fromScalar(i + 1))
    val digest = FriVerifier.hashCoset(coset)

    digest.length shouldBe 13
    digest.foreach(e => BabyBearField.isValid(e) shouldBe true)
  }

  it should "be deterministic" in {
    val coset = Array.tabulate(8)(i => QuadraticTower.fromScalar(i * 7))
    FriVerifier.hashCoset(coset) shouldBe FriVerifier.hashCoset(coset)
  }

  it should "be sensitive to coset content" in {
    val coset1 = Array.tabulate(8)(i => QuadraticTower.fromScalar(i + 1))
    val coset2 = Array.tabulate(8)(i => QuadraticTower.fromScalar(i + 2))
    FriVerifier.hashCoset(coset1) should not be FriVerifier.hashCoset(coset2)
  }

  // ══════════════════════════════════════════
  // Twiddle Factors
  // ══════════════════════════════════════════

  "OMEGA_8" should "be a primitive 8th root of unity" in {
    BabyBearField.pow(omega8, 8) shouldBe BabyBearField.ONE
    BabyBearField.pow(omega8, 4) should not be BabyBearField.ONE
  }

  "INV_8" should "satisfy 8 * INV_8 = 1" in {
    BabyBearField.mul(8, FriVerifier.INV_8) shouldBe BabyBearField.ONE
  }

  // ══════════════════════════════════════════
  // Multi-round Integration (synthetic proof)
  // ══════════════════════════════════════════

  "FRI verify" should "accept a valid single-round synthetic proof" in {
    // Build a synthetic FRI proof:
    // - Start with a degree-7 polynomial P(X) over a domain of size 64
    // - One FRI round folds it to domain size 8
    // - Remainder is a degree-0 polynomial (constant)

    val domainSize = 64
    val generator = BabyBearField.rootOfUnity(6) // 2^6 = 64

    // Our polynomial: P(X) = 1 + 2X + 3X² + ... + 8X⁷
    val polyCoeffs = (1 to 8).map(i => QuadraticTower.fromScalar(i)).toArray

    // Build the folded domain (8 cosets of size 8)
    val foldedDomainSize = domainSize / 8  // 8
    val foldedGenerator = BabyBearField.pow(generator, 8)

    // Compute all coset leaf digests for the Merkle tree
    val allCosets = new Array[Array[Ext16]](foldedDomainSize)
    var ci = 0
    while (ci < foldedDomainSize) {
      val x0 = BabyBearField.pow(generator, ci)
      allCosets(ci) = buildCoset(polyCoeffs, x0)
      ci += 1
    }
    val allDigests = allCosets.map(FriVerifier.hashCoset)

    // Build Merkle tree
    val seed = Array.fill(8)(42)
    val root = MerkleVerifier.computeRoot(allDigests, seed)

    // Select queries (use small indices for simplicity)
    val queryIndices = Array(2, 17, 45) // in [0, 64)

    // For each query, compute initial eval
    val queryEvals = queryIndices.map { idx =>
      val x = BabyBearField.pow(generator, idx)
      evalPoly(polyCoeffs, x)
    }

    val queryCosets = queryIndices.map { idx =>
      val cosetIdx = idx % foldedDomainSize
      allCosets(cosetIdx)
    }

    // Generate batch Merkle proof
    val cosetIndicesForMerkle = queryIndices.map(_ % foldedDomainSize).distinct.sorted
    val (_, merkleDigests, merkleSiblings) = MerkleVerifier.generateBatchProof(
      allDigests, cosetIndicesForMerkle, seed
    )

    // Derive alpha using a transcript that matches what verify() will do internally:
    // verify() absorbs round.commitment, then squeezes alpha.
    // We replicate that to compute the folded values.
    val alphaTranscript = FiatShamirTranscript("fri-test")
    alphaTranscript.absorbFieldElements(root)
    val alpha = alphaTranscript.squeezeExt16()

    // Compute folded values (remainder polynomial evaluations)
    val foldedEvals = queryIndices.map { idx =>
      val cosetIdx = idx % foldedDomainSize
      val x0 = BabyBearField.pow(generator, cosetIdx)
      FriVerifier.foldRadix8(x0, allCosets(cosetIdx), alpha)
    }

    // For a degree-7 poly folded by factor 8, the result is degree 0 (constant).
    // All folded evaluations at different query points should be the same.
    // Build the remainder polynomial as that constant.
    val remainderCoeffs = Array(foldedEvals(0))

    // Verify they all agree (sanity check)
    foldedEvals.foreach(_ shouldBe foldedEvals(0))

    val friRound = FriRound(
      commitment = root,
      cosets = queryCosets,
      merkleSiblings = merkleSiblings
    )

    // Verify with a fresh transcript (verify() will absorb internally)
    val verifyTranscript = FiatShamirTranscript("fri-test")

    val valid = FriVerifier.verify(
      verifyTranscript,
      domainSize, generator,
      queryIndices, queryEvals,
      Array(friRound),
      remainderCoeffs, seed
    )

    valid shouldBe true
  }
}
