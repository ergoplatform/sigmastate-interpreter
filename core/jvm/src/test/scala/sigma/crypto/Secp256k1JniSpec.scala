package sigma.crypto

import org.bitcoin.Secp256k1Context
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.math.BigInteger

class Secp256k1JniSpec extends AnyFunSuite with Matchers {
  private val ctx = CryptoFacade.createCryptoContext()
  private val G = ctx.generator
  private val order = ctx.order
  private val inf = ctx.infinity()

  test("Secp256k1Context is loaded and enabled by default on supported JVM platforms") {
    Secp256k1Context.isEnabled() shouldBe true
    Platform.isSecp256k1Enabled shouldBe true
  }

  test("BouncyCastle fallback can be toggled via withSecp256k1Enabled and system property") {
    Platform.withSecp256k1Enabled(false) {
      Platform.isSecp256k1Enabled shouldBe false
    }
    Platform.withSecp256k1Enabled(true) {
      Platform.isSecp256k1Enabled shouldBe true
    }

    val prev = System.getProperty("DISABLE_SECP256K1")
    try {
      System.setProperty("DISABLE_SECP256K1", "true")
      Platform.isSecp256k1Enabled shouldBe false
      System.setProperty("DISABLE_SECP256K1", "false")
      Platform.isSecp256k1Enabled shouldBe true
    } finally {
      if (prev == null) System.clearProperty("DISABLE_SECP256K1")
      else System.setProperty("DISABLE_SECP256K1", prev)
    }
  }

  test("exponentiatePoint matches BouncyCastle fallback for generator and arbitrary points across edge cases") {
    val nonGenerator = Platform.withSecp256k1Enabled(false) {
      CryptoFacade.exponentiatePoint(G, BigInteger.valueOf(7))
    }
    val bases = Seq(G, nonGenerator, inf)
    val scalars = Seq(
      BigInteger.ZERO,
      BigInteger.ONE,
      BigInteger.ONE.negate(),
      BigInteger.valueOf(2),
      BigInteger.valueOf(-2),
      BigInteger.valueOf(42),
      BigInteger.valueOf(-42),
      order.subtract(BigInteger.ONE),
      order,
      order.add(BigInteger.ONE),
      order.negate(),
      order.negate().subtract(BigInteger.ONE),
      order.multiply(BigInteger.valueOf(2)).add(BigInteger.valueOf(13)),
      new BigInteger("109749205800194830127901595352600384558037183218698112947062497909408298157746"),
      new BigInteger("-109749205800194830127901595352600384558037183218698112947062497909408298157746")
    )

    for (base <- bases; k <- scalars) {
      val viaJni = Platform.withSecp256k1Enabled(true) {
        CryptoFacade.exponentiatePoint(base, k)
      }
      val viaBc = Platform.withSecp256k1Enabled(false) {
        CryptoFacade.exponentiatePoint(base, k)
      }
      viaJni shouldBe viaBc
      CryptoFacade.getASN1Encoding(viaJni, compressed = true) shouldEqual
        CryptoFacade.getASN1Encoding(viaBc, compressed = true)
    }
  }

  test("multiplyPoints matches BouncyCastle fallback including identity, doubling, and inverse cancellation") {
    val p1 = Platform.withSecp256k1Enabled(false) {
      CryptoFacade.exponentiatePoint(G, BigInteger.valueOf(123456789L))
    }
    val p2 = Platform.withSecp256k1Enabled(false) {
      CryptoFacade.exponentiatePoint(G, BigInteger.valueOf(987654321L))
    }
    val negP1 = CryptoFacade.negatePoint(p1)

    val pairs = Seq(
      (p1, p2),
      (p2, p1),
      (p1, p1),
      (p1, negP1),
      (negP1, p1),
      (p1, inf),
      (inf, p2),
      (inf, inf)
    )

    for ((a, b) <- pairs) {
      val viaJni = Platform.withSecp256k1Enabled(true) {
        CryptoFacade.multiplyPoints(a, b)
      }
      val viaBc = Platform.withSecp256k1Enabled(false) {
        CryptoFacade.multiplyPoints(a, b)
      }
      viaJni shouldBe viaBc
      CryptoFacade.getASN1Encoding(viaJni, compressed = true) shouldEqual
        CryptoFacade.getASN1Encoding(viaBc, compressed = true)
    }
  }

  test("decodePoint matches BouncyCastle fallback for compressed, uncompressed, infinity, and invalid encodings") {
    val p = Platform.withSecp256k1Enabled(false) {
      CryptoFacade.exponentiatePoint(G, BigInteger.valueOf(314159265L))
    }
    val compressed = CryptoFacade.getASN1Encoding(p, compressed = true)
    val uncompressed = CryptoFacade.getASN1Encoding(p, compressed = false)
    val infBytes = Array[Byte](0)

    for (encoded <- Seq(compressed, uncompressed, infBytes)) {
      val viaJni = Platform.withSecp256k1Enabled(true) {
        ctx.decodePoint(encoded)
      }
      val viaBc = Platform.withSecp256k1Enabled(false) {
        ctx.decodePoint(encoded)
      }
      viaJni shouldBe viaBc
    }

    val invalidPrefix = compressed.clone()
    invalidPrefix(0) = 0x05.toByte

    // x = 5 has x^3 + 7 = 132, which is not a quadratic residue mod p on secp256k1
    val offCurveCompressed = new Array[Byte](33)
    offCurveCompressed(0) = 0x02.toByte
    offCurveCompressed(32) = 0x05.toByte

    val tamperedUncompressed = uncompressed.clone()
    tamperedUncompressed(64) = (tamperedUncompressed(64) ^ 0x01).toByte

    for (invalid <- Seq(invalidPrefix, offCurveCompressed, tamperedUncompressed)) {
      an[IllegalArgumentException] should be thrownBy {
        Platform.withSecp256k1Enabled(true) {
          ctx.decodePoint(invalid)
        }
      }
      an[IllegalArgumentException] should be thrownBy {
        Platform.withSecp256k1Enabled(false) {
          ctx.decodePoint(invalid)
        }
      }
    }
  }

  test("points produced by secp256k1-jni exponentiatePoint and decodePoint are normalized affine points") {
    Platform.withSecp256k1Enabled(true) {
      val p1 = CryptoFacade.exponentiatePoint(G, BigInteger.valueOf(123456789L))
      val p2 = CryptoFacade.exponentiatePoint(p1, BigInteger.valueOf(987654321L))
      val encoded = CryptoFacade.getASN1Encoding(p2, compressed = true)
      val decoded = ctx.decodePoint(encoded)

      // getAffineXCoord / getAffineYCoord throw IllegalStateException if point is not normalized
      CryptoFacade.getAffineXCoord(p1) shouldBe CryptoFacade.getXCoord(CryptoFacade.normalizePoint(p1))
      CryptoFacade.getAffineYCoord(p1) shouldBe CryptoFacade.getYCoord(CryptoFacade.normalizePoint(p1))
      CryptoFacade.getAffineXCoord(p2) shouldBe CryptoFacade.getAffineXCoord(decoded)
      CryptoFacade.getAffineYCoord(p2) shouldBe CryptoFacade.getAffineYCoord(decoded)
    }
  }
}
