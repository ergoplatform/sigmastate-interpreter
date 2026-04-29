package org.ergo.stark.field

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen

class QuadraticTowerSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {

  import BabyBearField.{P_INT, ONE, ZERO, mul => bMul}
  import QuadraticTower._

  // Generators
  val fieldElem: Gen[Int] = Gen.choose(0, P_INT - 1)
  val nonZeroElem: Gen[Int] = Gen.choose(1, P_INT - 1)

  val ext2Elem: Gen[Ext2] = for { a <- fieldElem; b <- fieldElem } yield (a, b)
  val nonZeroExt2: Gen[Ext2] = ext2Elem.suchThat(e => e._1 != 0 || e._2 != 0)

  val ext16Elem: Gen[Ext16] = for {
    coeffs <- Gen.listOfN(16, fieldElem)
  } yield fromFlat(coeffs.toArray)

  val nonZeroExt16: Gen[Ext16] = ext16Elem.suchThat(_ != EXT16_ZERO)

  // ══════════════════════════════════════════
  // Ext2 Field Axioms
  // ══════════════════════════════════════════

  "Ext2 multiplication" should "be commutative" in {
    forAll(ext2Elem, ext2Elem) { (a, b) =>
      ext2Mul(a, b) shouldBe ext2Mul(b, a)
    }
  }

  it should "be associative" in {
    forAll(ext2Elem, ext2Elem, ext2Elem) { (a, b, c) =>
      ext2Mul(ext2Mul(a, b), c) shouldBe ext2Mul(a, ext2Mul(b, c))
    }
  }

  it should "have ONE as identity" in {
    forAll(ext2Elem) { a =>
      ext2Mul(a, EXT2_ONE) shouldBe a
    }
  }

  it should "distribute over addition" in {
    forAll(ext2Elem, ext2Elem, ext2Elem) { (a, b, c) =>
      ext2Mul(a, ext2Add(b, c)) shouldBe ext2Add(ext2Mul(a, b), ext2Mul(a, c))
    }
  }

  "Ext2 inverse" should "satisfy a * a^(-1) == 1" in {
    forAll(nonZeroExt2) { a =>
      ext2Mul(a, ext2Inv(a)) shouldBe EXT2_ONE
    }
  }

  // ══════════════════════════════════════════
  // Ext2 specific checks
  // ══════════════════════════════════════════

  "Ext2" should "embed BabyBear correctly: (a, 0) * (b, 0) == (a*b, 0)" in {
    forAll(fieldElem, fieldElem) { (a, b) =>
      val result = ext2Mul((a, ZERO), (b, ZERO))
      result shouldBe ((bMul(a, b), ZERO))
    }
  }

  it should "satisfy x² = 11: (0,1) * (0,1) = (11, 0)" in {
    val result = ext2Mul((ZERO, ONE), (ZERO, ONE))
    result shouldBe ((EXT2_W, ZERO))
  }

  // ══════════════════════════════════════════
  // MulByNonResidue correctness
  // ══════════════════════════════════════════

  "ext2MulByNonResidue" should "match full ext2Mul by (0,1)" in {
    forAll(ext2Elem) { a =>
      ext2MulByNonResidue(a) shouldBe ext2Mul(a, (ZERO, ONE))
    }
  }

  "ext4MulByNonResidue" should "match full ext4Mul by the non-residue" in {
    // The Ext4 non-residue is ((0,0), (1,0)) — i.e. "y" in Ext4
    val nr: Ext4 = (EXT2_ZERO, EXT2_ONE)
    forAll(ext2Elem, ext2Elem) { (a, b) =>
      val e: Ext4 = (a, b)
      ext4MulByNonResidue(e) shouldBe ext4Mul(e, nr)
    }
  }

  // ══════════════════════════════════════════
  // Ext16 Field Axioms
  // ══════════════════════════════════════════

  "Ext16 multiplication" should "be commutative" in {
    forAll(ext16Elem, ext16Elem) { (a, b) =>
      ext16Mul(a, b) shouldBe ext16Mul(b, a)
    }
  }

  it should "have ONE as identity" in {
    forAll(ext16Elem) { a =>
      ext16Mul(a, EXT16_ONE) shouldBe a
    }
  }

  it should "distribute over addition" in {
    forAll(ext16Elem, ext16Elem, ext16Elem) { (a, b, c) =>
      ext16Mul(a, ext16Add(b, c)) shouldBe ext16Add(ext16Mul(a, b), ext16Mul(a, c))
    }
  }

  "Ext16 inverse" should "satisfy a * a^(-1) == 1" in {
    forAll(nonZeroExt16) { a =>
      ext16Mul(a, ext16Inv(a)) shouldBe EXT16_ONE
    }
  }

  // ══════════════════════════════════════════
  // Flat ↔ Tower roundtrip
  // ══════════════════════════════════════════

  "fromFlat/toFlat" should "roundtrip correctly" in {
    forAll(Gen.listOfN(16, fieldElem)) { coeffs =>
      val arr = coeffs.toArray
      toFlat(fromFlat(arr)) shouldBe arr
    }
  }

  "fromScalar" should "embed correctly" in {
    forAll(fieldElem) { s =>
      val e = fromScalar(s)
      val flat = toFlat(e)
      flat(0) shouldBe s
      flat.drop(1).forall(_ == 0) shouldBe true
    }
  }

  "fromScalar multiplication" should "match base field: fromScalar(a) * fromScalar(b) == fromScalar(a*b)" in {
    forAll(fieldElem, fieldElem) { (a, b) =>
      ext16Mul(fromScalar(a), fromScalar(b)) shouldBe fromScalar(bMul(a, b))
    }
  }

  // ══════════════════════════════════════════
  // Monomial ↔ Tower interop (Bit-Reversal)
  // ══════════════════════════════════════════

  "fromMonomial/toMonomial" should "roundtrip correctly" in {
    forAll(Gen.listOfN(16, fieldElem)) { coeffs =>
      val arr = coeffs.toArray
      toMonomial(fromMonomial(arr)) shouldBe arr
    }
  }

  it should "preserve scalar embedding (T^0 is same in both)" in {
    forAll(fieldElem) { s =>
      val mono = new Array[Int](16)
      mono(0) = s
      fromMonomial(mono) shouldBe fromScalar(s)
    }
  }

  it should "differ from fromFlat for non-scalar elements" in {
    // T^1 in monomial basis should NOT equal tower index 1
    // fromMonomial puts T^1 coeff at tower index 8 (bitrev of 1 = 8)
    val mono = new Array[Int](16)
    mono(1) = 42  // coefficient of T^1
    val e = fromMonomial(mono)
    val flat = toFlat(e)
    flat(1) shouldBe 0   // tower index 1 = T^8, not T^1
    flat(8) shouldBe 42  // tower index 8 = T^1 (via bitrev: 1000 -> 0001)
  }
}
