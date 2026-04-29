package org.ergo.stark.protocol

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen
import org.ergo.stark.field.BabyBearField

class FiatShamirTranscriptSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {

  // ══════════════════════════════════════════
  // Determinism
  // ══════════════════════════════════════════

  "FiatShamirTranscript" should "be deterministic: same inputs → same outputs" in {
    val t1 = FiatShamirTranscript("test-protocol")
    val t2 = FiatShamirTranscript("test-protocol")

    t1.absorb("hello".getBytes("UTF-8"))
    t2.absorb("hello".getBytes("UTF-8"))

    t1.squeezeFieldElement() shouldBe t2.squeezeFieldElement()
  }

  it should "produce different outputs for different labels" in {
    val t1 = FiatShamirTranscript("protocol-A")
    val t2 = FiatShamirTranscript("protocol-B")

    t1.squeezeFieldElement() should not be t2.squeezeFieldElement()
  }

  it should "produce different outputs for different absorbed data" in {
    val t1 = FiatShamirTranscript("test")
    val t2 = FiatShamirTranscript("test")

    t1.absorb(Array[Byte](1, 2, 3))
    t2.absorb(Array[Byte](1, 2, 4))

    t1.squeezeFieldElement() should not be t2.squeezeFieldElement()
  }

  // ══════════════════════════════════════════
  // Field element squeeze
  // ══════════════════════════════════════════

  "squeezeFieldElement" should "always produce valid BabyBear elements" in {
    val t = FiatShamirTranscript("validity-test")
    (0 until 100).foreach { _ =>
      val e = t.squeezeFieldElement()
      BabyBearField.isValid(e) shouldBe true
    }
  }

  it should "produce distinct values (not stuck in a loop)" in {
    val t = FiatShamirTranscript("distinct-test")
    val values = (0 until 50).map(_ => t.squeezeFieldElement()).toSet
    values.size should be > 40  // At least 40 distinct out of 50
  }

  // ══════════════════════════════════════════
  // Ext16 squeeze
  // ══════════════════════════════════════════

  "squeezeExt16" should "produce a valid Ext16 element" in {
    val t = FiatShamirTranscript("ext16-test")
    val ext = t.squeezeExt16()
    // ext is a nested tuple structure — just verify it's not trivially zero
    // by converting back to flat and checking
    val flat = org.ergo.stark.field.QuadraticTower.toFlat(ext)
    flat.length shouldBe 16
    flat.foreach(e => BabyBearField.isValid(e) shouldBe true)
    // Should not be all zeros (astronomically unlikely)
    flat.exists(_ != 0) shouldBe true
  }

  // ══════════════════════════════════════════
  // Query indices
  // ══════════════════════════════════════════

  "squeezeQueryIndices" should "produce exactly Q unique sorted indices" in {
    val t = FiatShamirTranscript("query-test")
    val indices = t.squeezeQueryIndices(35, 1024)
    indices.length shouldBe 35
    indices.toSet.size shouldBe 35  // All unique
    indices shouldBe indices.sorted  // Sorted
  }

  it should "produce indices within [0, domainSize)" in {
    val t = FiatShamirTranscript("range-test")
    val domainSize = 2048
    val indices = t.squeezeQueryIndices(35, domainSize)
    indices.foreach { idx =>
      idx should be >= 0
      idx should be < domainSize
    }
  }

  it should "reject invalid parameters" in {
    val t = FiatShamirTranscript("reject-test")
    an[IllegalArgumentException] should be thrownBy t.squeezeQueryIndices(0, 1024)
    an[IllegalArgumentException] should be thrownBy t.squeezeQueryIndices(35, 30)
    an[IllegalArgumentException] should be thrownBy t.squeezeQueryIndices(35, 1000)  // Not power of 2
  }

  // ══════════════════════════════════════════
  // Absorb then squeeze ordering
  // ══════════════════════════════════════════

  "absorb ordering" should "matter: different order → different output" in {
    val t1 = FiatShamirTranscript("order-test")
    val t2 = FiatShamirTranscript("order-test")

    t1.absorb(Array[Byte](1)); t1.absorb(Array[Byte](2))
    t2.absorb(Array[Byte](2)); t2.absorb(Array[Byte](1))

    t1.squeezeFieldElement() should not be t2.squeezeFieldElement()
  }

  "absorbFieldElements" should "produce same result as individual absorbs" in {
    val t1 = FiatShamirTranscript("batch-test")
    val t2 = FiatShamirTranscript("batch-test")

    val elems = Array(42, 1337, 999)
    t1.absorbFieldElements(elems)
    // t2 absorbs same data as a single byte array
    val buf = new Array[Byte](12)
    for (i <- elems.indices) {
      buf(i * 4) = ((elems(i) >> 24) & 0xFF).toByte
      buf(i * 4 + 1) = ((elems(i) >> 16) & 0xFF).toByte
      buf(i * 4 + 2) = ((elems(i) >> 8) & 0xFF).toByte
      buf(i * 4 + 3) = (elems(i) & 0xFF).toByte
    }
    t2.absorb(buf)

    t1.squeezeFieldElement() shouldBe t2.squeezeFieldElement()
  }
}
