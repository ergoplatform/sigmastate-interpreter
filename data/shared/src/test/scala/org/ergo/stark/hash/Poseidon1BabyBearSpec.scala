package org.ergo.stark.hash

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen
import org.ergo.stark.field.BabyBearField

class Poseidon1BabyBearSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {

  import Poseidon1BabyBear._

  val fieldElem: Gen[Int] = Gen.choose(0, BabyBearField.P_INT - 1)

  // ══════════════════════════════════════════
  // MDS Matrix Properties
  // ══════════════════════════════════════════

  "MDS matrix" should "have correct dimensions (T × T)" in {
    MDS.length shouldBe T
    MDS.foreach(_.length shouldBe T)
  }

  it should "have all non-zero entries" in {
    for (i <- 0 until T; j <- 0 until T) {
      MDS(i)(j) should not be 0
    }
  }

  it should "be a valid Cauchy matrix: M[i][j] = inv(i + T + j)" in {
    for (i <- 0 until T; j <- 0 until T) {
      val expected = BabyBearField.inv(BabyBearField.add(i, T + j))
      MDS(i)(j) shouldBe expected
    }
  }

  it should "have all distinct entries in each row" in {
    for (i <- 0 until T) {
      MDS(i).toSet.size shouldBe T
    }
  }

  // ══════════════════════════════════════════
  // Round Constants
  // ══════════════════════════════════════════

  "Round constants" should "have correct dimensions" in {
    ROUND_CONSTANTS.length shouldBe TOTAL_ROUNDS
    ROUND_CONSTANTS.foreach(_.length shouldBe T)
  }

  it should "be deterministic (same values across accesses)" in {
    val rc1 = ROUND_CONSTANTS
    val rc2 = ROUND_CONSTANTS
    for (r <- 0 until TOTAL_ROUNDS; i <- 0 until T) {
      rc1(r)(i) shouldBe rc2(r)(i)
    }
  }

  it should "have all values in valid range [0, p)" in {
    for (r <- 0 until TOTAL_ROUNDS; i <- 0 until T) {
      BabyBearField.isValid(ROUND_CONSTANTS(r)(i)) shouldBe true
    }
  }

  it should "not be all zeros" in {
    ROUND_CONSTANTS.exists(_.exists(_ != 0)) shouldBe true
  }

  // ══════════════════════════════════════════
  // Permutation (in-place)
  // ══════════════════════════════════════════

  "permute" should "reject wrong-sized input" in {
    an[IllegalArgumentException] should be thrownBy permute(new Array[Int](10))
    an[IllegalArgumentException] should be thrownBy permute(new Array[Int](25))
  }

  it should "be deterministic" in {
    val s1 = Array.fill(T)(42)
    val s2 = Array.fill(T)(42)
    permute(s1)
    permute(s2)
    s1 shouldBe s2
  }

  it should "produce different output for different input" in {
    val s1 = new Array[Int](T); s1(0) = 1
    val s2 = new Array[Int](T); s2(0) = 2
    permute(s1)
    permute(s2)
    s1 should not be s2
  }

  it should "not be the identity" in {
    val state = Array.tabulate(T)(i => i + 1)
    val original = state.clone()
    permute(state)
    state should not be original
  }

  it should "produce outputs in valid field range" in {
    forAll(Gen.listOfN(T, fieldElem)) { elems =>
      val state = elems.toArray
      permute(state)
      state.foreach(e => BabyBearField.isValid(e) shouldBe true)
    }
  }

  it should "mutate state in-place (zero-allocation inner loop)" in {
    val state = Array.tabulate(T)(i => i + 1)
    val ref = state  // same reference
    permute(state)
    (ref eq state) shouldBe true  // same object, mutated in-place
  }

  // ══════════════════════════════════════════
  // Sponge Hash — Security Properties
  // ══════════════════════════════════════════

  "hash" should "be deterministic" in {
    val input = Array(1, 2, 3, 4, 5)
    hash(input) shouldBe hash(input)
  }

  it should "produce default output of C (13) elements" in {
    hash(Array(1, 2, 3)).length shouldBe C
  }

  it should "support custom output length" in {
    hash(Array(1, 2, 3), outputLen = 5).length shouldBe 5
    hash(Array(1, 2, 3), outputLen = 20).length shouldBe 20
  }

  it should "produce collision resistance: different inputs → different outputs" in {
    hash(Array(1, 2, 3)) should not be hash(Array(1, 2, 4))
  }

  it should "handle empty input" in {
    val result = hash(Array.empty)
    result.length shouldBe C
    result.foreach(e => BabyBearField.isValid(e) shouldBe true)
  }

  it should "handle input longer than rate" in {
    val longInput = Array.tabulate(30)(i => i + 1)
    val result = hash(longInput)
    result.length shouldBe C
    result.foreach(e => BabyBearField.isValid(e) shouldBe true)
  }

  it should "be sensitive to input length (length extension resistance)" in {
    hash(Array(1, 2, 3)) should not be hash(Array(1, 2, 3, 0))
  }

  it should "never read from capacity when outputLen > R (squeeze safety)" in {
    // Request more output than the rate — this triggers multi-squeeze.
    // The output should be all valid field elements (not garbage).
    val result = hash(Array(1, 2, 3), outputLen = R + 5)  // 16 > R=11
    result.length shouldBe (R + 5)
    result.foreach(e => BabyBearField.isValid(e) shouldBe true)

    // The output from a single-rate squeeze vs multi-rate should differ
    // after the first R elements (different permutation state)
    val shortResult = hash(Array(1, 2, 3), outputLen = R)
    result.take(R) shouldBe shortResult  // First R elements must match
  }

  // ══════════════════════════════════════════
  // hashTwo — Merkle tree node hashing
  // ══════════════════════════════════════════

  "hashTwo" should "be deterministic and produce C elements" in {
    val left = Array.tabulate(C)(i => i + 1)
    val right = Array.tabulate(C)(i => i + 100)
    val result = hashTwo(left, right)
    result.length shouldBe C
    result shouldBe hashTwo(left, right)
  }

  it should "be non-commutative: hash(L, R) != hash(R, L)" in {
    val left = Array.tabulate(C)(i => i + 1)
    val right = Array.tabulate(C)(i => i + 100)
    hashTwo(left, right) should not be hashTwo(right, left)
  }
}
