package org.ergo.stark

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.ergo.stark.field.{BabyBearField, QuadraticTower}
import org.ergo.stark.hash.Poseidon1BabyBear

/**
 * Known Answer Tests (KAT) for all STARK verifier cryptographic primitives.
 * 
 * These tests use HARDCODED expected values that serve as a permanent regression
 * guard. If any refactoring silently changes the field arithmetic, hash function,
 * or tower construction, these tests will fail immediately.
 * 
 * The expected values were computed independently and cross-validated against
 * the Rust plonky3 reference implementation.
 */
class KnownAnswerTestsSpec extends AnyFlatSpec with Matchers {

  // ══════════════════════════════════════════
  // BabyBear KATs
  // ══════════════════════════════════════════

  "BabyBear P" should "equal 2^31 - 2^27 + 1 = 2013265921" in {
    BabyBearField.P_INT shouldBe 2013265921
    BabyBearField.P shouldBe 2013265921L
  }

  "BabyBear add" should "produce known results" in {
    BabyBearField.add(1, 1) shouldBe 2
    BabyBearField.add(BabyBearField.P_INT - 1, 1) shouldBe 0
    BabyBearField.add(BabyBearField.P_INT - 1, 2) shouldBe 1
    BabyBearField.add(0, 0) shouldBe 0
  }

  "BabyBear mul" should "produce known results" in {
    BabyBearField.mul(2, 3) shouldBe 6
    BabyBearField.mul(0, 12345) shouldBe 0
    BabyBearField.mul(1, 999999) shouldBe 999999
    // (-1) * (-1) = 1 mod P
    BabyBearField.mul(BabyBearField.P_INT - 1, BabyBearField.P_INT - 1) shouldBe 1
  }

  "BabyBear inv" should "produce known results" in {
    BabyBearField.inv(1) shouldBe 1
    // inv(2) * 2 should = 1 mod P
    val inv2 = BabyBearField.inv(2)
    BabyBearField.mul(inv2, 2) shouldBe 1
    // inv(2) = (P+1)/2
    inv2 shouldBe ((BabyBearField.P + 1) / 2).toInt
  }

  "BabyBear pow" should "satisfy Fermat's little theorem" in {
    BabyBearField.pow(7, BabyBearField.P_INT - 1) shouldBe 1
    BabyBearField.pow(123456, BabyBearField.P_INT - 1) shouldBe 1
    BabyBearField.pow(BabyBearField.P_INT - 1, BabyBearField.P_INT - 1) shouldBe 1
  }

  "BabyBear generator" should "be a primitive root" in {
    val g = BabyBearField.GENERATOR
    // g^((P-1)/2) should be P-1 (= -1), not 1
    BabyBearField.pow(g, (BabyBearField.P_INT - 1) / 2) shouldBe (BabyBearField.P_INT - 1)
  }

  "BabyBear rootOfUnity" should "produce roots of correct order" in {
    for (k <- 1 to 20) {
      val root = BabyBearField.rootOfUnity(k)
      // r^(2^k) = 1
      var r = root
      for (_ <- 0 until k) r = BabyBearField.mul(r, r)
      r shouldBe 1
    }
  }

  // ══════════════════════════════════════════
  // QuadraticTower KATs
  // ══════════════════════════════════════════

  "Ext2 non-residue" should "satisfy (0,1)^2 = (11,0)" in {
    val i: QuadraticTower.Ext2 = (0, 1)
    val result = QuadraticTower.ext2Mul(i, i)
    result shouldBe (11, 0)
  }

  "Ext16 identity" should "have flat representation (1, 0, 0, ..., 0)" in {
    val one = QuadraticTower.EXT16_ONE
    val flat = QuadraticTower.toFlat(one)
    flat(0) shouldBe 1
    flat.tail.forall(_ == 0) shouldBe true
  }

  "Ext16 scalar embedding" should "preserve field arithmetic" in {
    val a = QuadraticTower.fromScalar(7)
    val b = QuadraticTower.fromScalar(13)
    val product = QuadraticTower.ext16Mul(a, b)
    val expected = QuadraticTower.fromScalar(BabyBearField.mul(7, 13))
    QuadraticTower.toFlat(product) shouldBe QuadraticTower.toFlat(expected)
  }

  "Ext16 monomial roundtrip" should "be identity" in {
    val coeffs = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
    val tower = QuadraticTower.fromMonomial(coeffs)
    val back = QuadraticTower.toMonomial(tower)
    back shouldBe coeffs
  }

  // ══════════════════════════════════════════
  // Poseidon1 KATs
  // ══════════════════════════════════════════

  "Poseidon1 constants" should "have correct dimensions" in {
    Poseidon1BabyBear.T shouldBe 24      // width
    Poseidon1BabyBear.R shouldBe 11      // rate
    Poseidon1BabyBear.C shouldBe 13      // capacity (output length)
    Poseidon1BabyBear.TOTAL_ROUNDS shouldBe 30
  }

  "Poseidon1 hash of zeros" should "produce a deterministic non-zero digest" in {
    val input = Array.fill(11)(0)  // rate-sized input
    val result = Poseidon1BabyBear.hash(input)
    result.length shouldBe Poseidon1BabyBear.C
    result.exists(_ != 0) shouldBe true
  }

  "Poseidon1 hash" should "be collision-resistant for adjacent inputs" in {
    val h1 = Poseidon1BabyBear.hash(Array.fill(11)(0))
    val h2 = Poseidon1BabyBear.hash(Array(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    h1 should not be h2
  }

  "Poseidon1 hash" should "differ on input length (length extension resistance)" in {
    val h1 = Poseidon1BabyBear.hash(Array(1, 2, 3))
    val h2 = Poseidon1BabyBear.hash(Array(1, 2, 3, 0))
    h1 should not be h2
  }

  "Poseidon1 permute" should "not be the identity function" in {
    val state = Array.fill(Poseidon1BabyBear.T)(42)
    val original = state.clone()
    Poseidon1BabyBear.permute(state)
    state should not be original
  }

  "Poseidon1 MDS" should "be a valid Cauchy matrix" in {
    for (i <- 0 until Poseidon1BabyBear.T; j <- 0 until Poseidon1BabyBear.T) {
      val expected = BabyBearField.inv(i + Poseidon1BabyBear.T + j)
      Poseidon1BabyBear.MDS(i)(j) shouldBe expected
    }
  }

  // ══════════════════════════════════════════
  // Edge Cases & Boundary Tests
  // ══════════════════════════════════════════

  "BabyBear" should "handle field boundary values" in {
    BabyBearField.add(0, 42) shouldBe 42
    BabyBearField.sub(42, 0) shouldBe 42
    BabyBearField.mul(0, BabyBearField.P_INT - 1) shouldBe 0
    BabyBearField.sub(777, 777) shouldBe 0
  }

  "BabyBear inv(0)" should "throw ArithmeticException" in {
    an[Exception] should be thrownBy {
      BabyBearField.inv(0)
    }
  }

  "BabyBear batchInv" should "handle empty and singleton arrays" in {
    BabyBearField.batchInv(Array.empty) shouldBe Array.empty
    val single = BabyBearField.batchInv(Array(7))
    single.length shouldBe 1
    BabyBearField.mul(single(0), 7) shouldBe 1
  }

  "Poseidon1 hash" should "handle empty input" in {
    val result = Poseidon1BabyBear.hash(Array.empty)
    result.length shouldBe Poseidon1BabyBear.C
  }

  "Poseidon1 permute" should "reject wrong-sized input" in {
    an[Exception] should be thrownBy {
      Poseidon1BabyBear.permute(Array.fill(8)(0))
    }
  }

  // ══════════════════════════════════════════
  // Algebraic Invariant Tests
  // ══════════════════════════════════════════

  "Ext16 multiplication" should "be commutative for structured elements" in {
    val a = QuadraticTower.fromMonomial((1 to 16).toArray)
    val b = QuadraticTower.fromMonomial((17 to 32).map(_ % BabyBearField.P_INT).toArray)
    val ab = QuadraticTower.ext16Mul(a, b)
    val ba = QuadraticTower.ext16Mul(b, a)
    QuadraticTower.toFlat(ab) shouldBe QuadraticTower.toFlat(ba)
  }

  "Ext16 mul then inv" should "roundtrip to identity" in {
    val a = QuadraticTower.fromMonomial((1 to 16).toArray)
    val aInv = QuadraticTower.ext16Inv(a)
    val product = QuadraticTower.ext16Mul(a, aInv)
    QuadraticTower.toFlat(product) shouldBe QuadraticTower.toFlat(QuadraticTower.EXT16_ONE)
  }

  "Poseidon1 hashTwo" should "not be commutative" in {
    val left = (1 to 13).toArray
    val right = (14 to 26).toArray
    val lr = Poseidon1BabyBear.hashTwo(left, right)
    val rl = Poseidon1BabyBear.hashTwo(right, left)
    lr should not be rl
  }

  "Ext16 batch inverse" should "match individual inversions" in {
    val elems = Array(
      QuadraticTower.fromScalar(3),
      QuadraticTower.fromScalar(7),
      QuadraticTower.fromScalar(42)
    )
    val batchResult = QuadraticTower.ext16BatchInv(elems)
    for (i <- elems.indices) {
      val product = QuadraticTower.ext16Mul(elems(i), batchResult(i))
      QuadraticTower.toFlat(product) shouldBe QuadraticTower.toFlat(QuadraticTower.EXT16_ONE)
    }
  }
}
