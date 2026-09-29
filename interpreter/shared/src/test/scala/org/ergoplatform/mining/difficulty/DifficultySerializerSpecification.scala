package org.ergoplatform.mining.difficulty

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class DifficultySerializerSpecification extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with Matchers {

  import DifficultySerializer._

  // Reference test vectors for the nBits compact encoding, taken from the
  // documentation of the reference implementation in the Ergo node codebase
  // (org.ergoplatform.mining.difficulty.DifficultySerializer):
  //   0x1234560000 is compact 0x05123456
  //   0xc0de000000 is compact 0x0600c0de
  //   compact 0x05c0de00 is -0x40de000000

  property("decodeCompactBits: reference test vectors") {
    decodeCompactBits(0x05123456L) shouldBe BigInt(0x1234560000L)
    decodeCompactBits(0x0600c0deL) shouldBe BigInt(0xc0de000000L)
    decodeCompactBits(0x05c0de00L) shouldBe BigInt(-0x40de000000L)
    decodeCompactBits(0L) shouldBe BigInt(0)
  }

  property("encodeCompactBits: reference test vectors") {
    encodeCompactBits(BigInt(0x1234560000L)) shouldBe 0x05123456L
    encodeCompactBits(BigInt(0xc0de000000L)) shouldBe 0x0600c0deL
    encodeCompactBits(BigInt(0)) shouldBe 0x01000000L
  }

  property("fromNBits/toNBits convert like decode/encodeCompactBits") {
    fromNBits(0x05123456L) shouldBe decodeCompactBits(0x05123456L)
    fromNBits(0x0600c0deL) shouldBe BigInt(0xc0de000000L)
    toNBits(BigInt(0x1234560000L)) shouldBe encodeCompactBits(BigInt(0x1234560000L))
    toNBits(BigInt(0xc0de000000L)) shouldBe 0x0600c0deL
  }

  property("toNBits(fromNBits(nBits)) is identity on valid encodings") {
    forAll(Gen.choose(BigInt(1), BigInt(2).pow(256) - 1)) { difficulty =>
      val nBits = toNBits(difficulty)
      toNBits(fromNBits(nBits)) shouldBe nBits
    }
  }

  property("fromNBits(toNBits(d)) normalizes positive difficulties") {
    forAll(Gen.choose(BigInt(1), BigInt(2).pow(256) - 1)) { difficulty =>
      val nBits = toNBits(difficulty)
      val back = fromNBits(nBits)
      // encoding keeps the most significant bytes only, so decoding
      // yields a value less than or equal to the original difficulty
      back should be <= difficulty
      back should be > BigInt(0)
      // re-encoding the normalized value is stable
      toNBits(back) shouldBe nBits
    }
  }

  property("decodeCompactBits handles the Bitcoin genesis nBits") {
    // 0x1d00ffff is the well-known genesis block compact difficulty: 0x00ffff * 256^26
    decodeCompactBits(0x1d00ffffL) shouldBe (BigInt(0xFFFF) * BigInt(256).pow(26))
    toNBits(fromNBits(0x1d00ffffL)) shouldBe 0x1d00ffffL
  }
}
