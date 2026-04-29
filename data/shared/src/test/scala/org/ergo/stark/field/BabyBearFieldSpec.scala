package org.ergo.stark.field

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen

class BabyBearFieldSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {

  import BabyBearField._

  // Generator for valid BabyBear field elements
  val fieldElem: Gen[Int] = Gen.choose(0, P_INT - 1)
  val nonZeroElem: Gen[Int] = Gen.choose(1, P_INT - 1)

  // ══════════════════════════════════════════
  // Field Axioms
  // ══════════════════════════════════════════

  "BabyBear addition" should "be commutative: a + b == b + a" in {
    forAll(fieldElem, fieldElem) { (a, b) =>
      add(a, b) shouldBe add(b, a)
    }
  }

  it should "be associative: (a + b) + c == a + (b + c)" in {
    forAll(fieldElem, fieldElem, fieldElem) { (a, b, c) =>
      add(add(a, b), c) shouldBe add(a, add(b, c))
    }
  }

  it should "have zero as identity: a + 0 == a" in {
    forAll(fieldElem) { a =>
      add(a, ZERO) shouldBe a
    }
  }

  "BabyBear subtraction" should "be the inverse of addition: (a + b) - b == a" in {
    forAll(fieldElem, fieldElem) { (a, b) =>
      sub(add(a, b), b) shouldBe a
    }
  }

  "BabyBear negation" should "satisfy a + (-a) == 0" in {
    forAll(fieldElem) { a =>
      add(a, neg(a)) shouldBe ZERO
    }
  }

  "BabyBear multiplication" should "be commutative: a * b == b * a" in {
    forAll(fieldElem, fieldElem) { (a, b) =>
      mul(a, b) shouldBe mul(b, a)
    }
  }

  it should "be associative: (a * b) * c == a * (b * c)" in {
    forAll(fieldElem, fieldElem, fieldElem) { (a, b, c) =>
      mul(mul(a, b), c) shouldBe mul(a, mul(b, c))
    }
  }

  it should "have one as identity: a * 1 == a" in {
    forAll(fieldElem) { a =>
      mul(a, ONE) shouldBe a
    }
  }

  it should "distribute over addition: a * (b + c) == a*b + a*c" in {
    forAll(fieldElem, fieldElem, fieldElem) { (a, b, c) =>
      mul(a, add(b, c)) shouldBe add(mul(a, b), mul(a, c))
    }
  }

  "BabyBear inverse" should "satisfy a * a^(-1) == 1 for non-zero a" in {
    forAll(nonZeroElem) { a =>
      mul(a, inv(a)) shouldBe ONE
    }
  }

  it should "throw on zero" in {
    an[IllegalArgumentException] should be thrownBy inv(ZERO)
  }

  "BabyBear division" should "satisfy (a * b) / b == a" in {
    forAll(fieldElem, nonZeroElem) { (a, b) =>
      div(mul(a, b), b) shouldBe a
    }
  }

  // ══════════════════════════════════════════
  // Specific Known Values
  // ══════════════════════════════════════════

  "BabyBear constants" should "have correct P" in {
    P shouldBe 2013265921L
    // Verify: 2^31 - 2^27 + 1
    P shouldBe ((1L << 31) - (1L << 27) + 1)
  }

  it should "have P - 1 divisible by 2^27" in {
    ((P - 1) % (1L << 27)) shouldBe 0
    ((P - 1) / (1L << 27)) shouldBe 15
  }

  "BabyBear pow" should "compute small powers correctly" in {
    pow(2, 10) shouldBe 1024
    pow(3, 5) shouldBe 243
    pow(7, 0) shouldBe 1
    pow(0, 100) shouldBe 0
  }

  it should "satisfy Fermat's little theorem: a^(p-1) == 1 for a != 0" in {
    forAll(nonZeroElem) { a =>
      pow(a, P - 1) shouldBe ONE
    }
  }

  // ══════════════════════════════════════════
  // Batch Inversion
  // ══════════════════════════════════════════

  "batchInv" should "produce same results as individual inversions" in {
    forAll(Gen.listOfN(10, nonZeroElem)) { elems =>
      val arr = elems.toArray
      val batched = batchInv(arr)
      val individual = arr.map(inv)
      batched shouldBe individual
    }
  }

  it should "handle single element" in {
    forAll(nonZeroElem) { a =>
      batchInv(Array(a)) shouldBe Array(inv(a))
    }
  }

  it should "handle empty array" in {
    batchInv(Array.empty) shouldBe Array.empty
  }

  // ══════════════════════════════════════════
  // Roots of Unity
  // ══════════════════════════════════════════

  "rootOfUnity" should "be an actual root: r^(2^k) == 1" in {
    for (k <- 1 to 10) {
      val r = rootOfUnity(k)
      pow(r, 1L << k) shouldBe ONE
    }
  }

  it should "be a PRIMITIVE root: r^(2^(k-1)) != 1" in {
    for (k <- 1 to 10) {
      val r = rootOfUnity(k)
      pow(r, 1L << (k - 1)) should not be ONE
    }
  }
}
