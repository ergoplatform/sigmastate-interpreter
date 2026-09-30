package sigma.crypto

import org.bouncycastle.math.ec.ECPoint
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.math.BigInteger
import java.util.concurrent.{Callable, Executors, TimeUnit}
import scala.collection.JavaConverters._

/** Checks that EC scalar multiplication routed through native libsecp256k1
  * ([[SecP256K1Native]]) is indistinguishable from the BouncyCastle reference
  * implementation, for random inputs and for every edge case of the fallback logic.
  *
  * `Platform.exponentiatePoint` is consensus-critical: any input on which the two
  * backends disagree would fork the network, so the comparison is on encoded bytes.
  */
class SecP256K1NativeSpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers {

  private val ctx: CryptoContextJvm = Platform.createContext().asInstanceOf[CryptoContextJvm]
  private val order: BigInteger = ctx.order
  private val G: ECPoint = ctx.generator.value
  private val infinity: ECPoint = ctx.infinity().value
  private val two256: BigInteger = BigInteger.ONE.shiftLeft(256)

  /** Pure BouncyCastle reference (what every node computed before this change). */
  private def reference(p: ECPoint, n: BigInteger): ECPoint = p.multiply(n)

  private def encode(p: ECPoint): Seq[Byte] = p.getEncoded(true).toSeq

  /** Both backends must agree on the point at infinity and on the compressed encoding. */
  private def assertSamePoint(actual: ECPoint, expected: ECPoint, clue: => String): Unit = {
    withClue(clue) {
      actual.isInfinity shouldBe expected.isInfinity
      if (!expected.isInfinity) {
        actual.isValid shouldBe true
        encode(actual) shouldBe encode(expected)
      }
    }
  }

  private def checkAgainstReference(p: ECPoint, n: BigInteger): Unit = {
    val expected = reference(p, n)
    val viaPlatform = Platform.exponentiatePoint(Platform.Ecp(p), n).value
    assertSamePoint(viaPlatform, expected, s"Platform.exponentiatePoint, n=$n")

    if (SecP256K1Native.isEnabled && !p.isInfinity) {
      SecP256K1Native.multiplyPointByScalar(p, n) match {
        case Some(native) => assertSamePoint(native, expected, s"native path, n=$n")
        case None =>
          // The native path may only decline when the result is infinity (n ≡ 0 mod order);
          // everything else must be handled natively, otherwise the backend is not exercised.
          withClue(s"native path returned None for n=$n, but reference result is not infinity") {
            expected.isInfinity shouldBe true
          }
      }
    }
  }

  // ----- generators -----------------------------------------------------------------------

  private val bytes32: Gen[Array[Byte]] = Gen.containerOfN[Array, Byte](32, Gen.choose(Byte.MinValue, Byte.MaxValue))

  /** Uniform scalar in [1, order-1]: the range accepted by libsecp256k1. */
  private val inRangeScalar: Gen[BigInteger] =
    bytes32.map(b => new BigInteger(1, b).mod(order)).suchThat(_.signum != 0)

  /** Uniform signed 256-bit integer: the value range of ErgoTree's BigInt type. */
  private val signed256: Gen[BigInteger] = bytes32.map(b => new BigInteger(b))

  private val smallScalar: Gen[BigInteger] = Gen.choose(-1000L, 1000L).map(BigInteger.valueOf)

  /** Scalars outside [1, order-1] that libsecp256k1 rejects and the fallback must handle. */
  private val outOfRangeScalar: Gen[BigInteger] = Gen.oneOf(
    Gen.choose(0L, 1000L).map(k => order.add(BigInteger.valueOf(k))),
    Gen.choose(0L, 1000L).map(k => order.subtract(BigInteger.valueOf(k)).negate()),
    Gen.choose(1L, 1000L).map(k => order.multiply(BigInteger.valueOf(k))),
    Gen.choose(0L, 1000L).map(k => two256.add(BigInteger.valueOf(k))),
    inRangeScalar.map(k => order.multiply(BigInteger.valueOf(7)).add(k)),
    inRangeScalar.map(k => two256.multiply(BigInteger.valueOf(3)).add(k).negate())
  )

  private val anyScalar: Gen[BigInteger] = Gen.frequency(
    4 -> inRangeScalar, 3 -> signed256, 2 -> smallScalar, 2 -> outOfRangeScalar
  )

  /** Random curve point, produced by BC so it is independent of the code under test. */
  private val randomPoint: Gen[ECPoint] = inRangeScalar.map(k => G.multiply(k))

  private val anyPoint: Gen[ECPoint] = Gen.frequency(
    6 -> randomPoint,
    1 -> Gen.const(G),
    1 -> Gen.const(G.negate()),
    1 -> randomPoint.map(_.negate()),
    1 -> Gen.const(infinity)
  )

  private val minSuccessful = MinSuccessful(500)

  // ----- properties -----------------------------------------------------------------------

  property("native library status is reported") {
    info(s"native libsecp256k1 enabled: ${SecP256K1Native.isEnabled}")
    // Not an assertion: on platforms without a bundled native lib the BC fallback is used
    // and every test in this suite still verifies the fallback against the reference.
  }

  property("exponentiatePoint agrees with BouncyCastle for random points and scalars") {
    forAll(anyPoint, anyScalar, minSuccessful) { (p, n) => checkAgainstReference(p, n) }
  }

  property("exponentiatePoint agrees with BouncyCastle on the full ErgoTree BigInt range") {
    forAll(randomPoint, signed256, minSuccessful) { (p, n) => checkAgainstReference(p, n) }
  }

  property("native path is taken for every scalar in [1, order-1]") {
    assume(SecP256K1Native.isEnabled, "native libsecp256k1 not available on this platform")
    forAll(randomPoint, inRangeScalar, minSuccessful) { (p, n) =>
      val res = SecP256K1Native.multiplyPointByScalar(p, n)
      res shouldBe defined
      assertSamePoint(res.get, reference(p, n), s"n=$n")
    }
  }

  property("exponentiation is a group homomorphism (p^(a+b) == p^a * p^b, p^(ab) == (p^a)^b)") {
    forAll(randomPoint, inRangeScalar, inRangeScalar, MinSuccessful(100)) { (p, a, b) =>
      def exp(q: ECPoint, k: BigInteger) = Platform.exponentiatePoint(Platform.Ecp(q), k).value
      assertSamePoint(exp(p, a.add(b)), exp(p, a).add(exp(p, b)), "additive")
      assertSamePoint(exp(p, a.multiply(b)), exp(exp(p, a), b), "multiplicative")
    }
  }

  property("accepts non-normalized (projective) input points") {
    forAll(randomPoint, inRangeScalar, inRangeScalar, MinSuccessful(100)) { (p, a, b) =>
      val projective = p.multiply(a)   // BC leaves the result in Jacobian coordinates
      projective.isNormalized shouldBe false
      checkAgainstReference(projective, b)
    }
  }

  // ----- edge cases -----------------------------------------------------------------------

  private val edgeScalars: Seq[(String, BigInteger)] = Seq(
    "0"            -> BigInteger.ZERO,
    "1"            -> BigInteger.ONE,
    "2"            -> BigInteger.valueOf(2),
    "-1"           -> BigInteger.ONE.negate(),
    "-2"           -> BigInteger.valueOf(-2),
    "order-1"      -> order.subtract(BigInteger.ONE),
    "order"        -> order,
    "order+1"      -> order.add(BigInteger.ONE),
    "-(order-1)"   -> order.subtract(BigInteger.ONE).negate(),
    "-order"       -> order.negate(),
    "-(order+1)"   -> order.add(BigInteger.ONE).negate(),
    "2*order"      -> order.shiftLeft(1),
    "2*order+1"    -> order.shiftLeft(1).add(BigInteger.ONE),
    "2^255"        -> BigInteger.ONE.shiftLeft(255),          // 32 bytes, high bit set
    "2^255-1"      -> BigInteger.ONE.shiftLeft(255).subtract(BigInteger.ONE), // max ErgoTree BigInt
    "-2^255"       -> BigInteger.ONE.shiftLeft(255).negate(), // min ErgoTree BigInt
    "2^256-1"      -> two256.subtract(BigInteger.ONE),        // max unsigned 32-byte value
    "2^256"        -> two256,                                 // 33 significant bytes
    "2^256+1"      -> two256.add(BigInteger.ONE),
    "2^256+order"  -> two256.add(order),
    "-(2^256+1)"   -> two256.add(BigInteger.ONE).negate(),
    "2^300+12345"  -> BigInteger.ONE.shiftLeft(300).add(BigInteger.valueOf(12345)),
    "2^1000"       -> BigInteger.ONE.shiftLeft(1000),
    "order^2"      -> order.multiply(order),
    "order^2+1"    -> order.multiply(order).add(BigInteger.ONE)
  )

  private val edgePoints: Seq[(String, ECPoint)] = Seq(
    "G"        -> G,
    "-G"       -> G.negate(),
    "2G"       -> G.twice(),
    "(order-1)G" -> G.multiply(order.subtract(BigInteger.ONE)),
    "infinity" -> infinity
  )

  property("edge-case scalars agree with BouncyCastle on edge-case points") {
    for ((pName, p) <- edgePoints; (nName, n) <- edgeScalars) {
      withClue(s"point=$pName scalar=$nName: ") { checkAgainstReference(p, n) }
    }
  }

  property("scalar 0 and multiples of the order give the point at infinity") {
    for (n <- Seq(BigInteger.ZERO, order, order.negate(), order.shiftLeft(1), order.multiply(order))) {
      Platform.isInfinityPoint(Platform.exponentiatePoint(Platform.Ecp(G), n)) shouldBe true
      if (SecP256K1Native.isEnabled) {
        SecP256K1Native.multiplyPointByScalar(G, n) shouldBe None
      }
    }
  }

  property("exponentiating the point at infinity gives infinity for any scalar") {
    for ((_, n) <- edgeScalars) {
      Platform.isInfinityPoint(Platform.exponentiatePoint(Platform.Ecp(infinity), n)) shouldBe true
    }
  }

  property("scalar 1 is the identity and -1 is negation") {
    forAll(randomPoint, MinSuccessful(50)) { p =>
      assertSamePoint(Platform.exponentiatePoint(Platform.Ecp(p), BigInteger.ONE).value, p, "p^1")
      assertSamePoint(Platform.exponentiatePoint(Platform.Ecp(p), BigInteger.ONE.negate()).value, p.negate(), "p^-1")
      assertSamePoint(Platform.exponentiatePoint(Platform.Ecp(p), order.subtract(BigInteger.ONE)).value, p.negate(), "p^(order-1)")
    }
  }

  property("negative scalars and scalars beyond the order reduce modulo the order") {
    forAll(randomPoint, inRangeScalar, Gen.choose(-5L, 5L), MinSuccessful(100)) { (p, k, m) =>
      val n = k.add(order.multiply(BigInteger.valueOf(m)))
      assertSamePoint(
        Platform.exponentiatePoint(Platform.Ecp(p), n).value,
        Platform.exponentiatePoint(Platform.Ecp(p), k).value,
        s"k=$k m=$m")
    }
  }

  property("known-answer: small multiples of the generator") {
    // Compressed encodings of 2G and 3G from the secp256k1 test vectors.
    val twoG   = "02c6047f9441ed7d6d3045406e95c07cd85c778e4b8cef3ca7abac09b95c709ee5"
    val threeG = "02f9308a019258c31049344f85f89d5229b531c845836f99b08601f113bce036f9"
    def hex(p: ECPoint) = p.getEncoded(true).map("%02x".format(_)).mkString
    hex(Platform.exponentiatePoint(Platform.Ecp(G), BigInteger.valueOf(2)).value) shouldBe twoG
    hex(Platform.exponentiatePoint(Platform.Ecp(G), BigInteger.valueOf(3)).value) shouldBe threeG
  }

  property("results are consistent under concurrent use from many threads") {
    val nThreads = 16
    val perThread = 200
    val inputs: Seq[(ECPoint, BigInteger)] = {
      val rnd = new scala.util.Random(0xC0FFEE)
      Seq.fill(nThreads * perThread) {
        val b = new Array[Byte](32); rnd.nextBytes(b)
        val k = new BigInteger(1, b).mod(order).add(BigInteger.ONE)
        val pb = new Array[Byte](32); rnd.nextBytes(pb)
        (G.multiply(new BigInteger(1, pb).mod(order).add(BigInteger.ONE)), k)
      }
    }
    val expected = inputs.map { case (p, n) => encode(reference(p, n)) }

    val pool = Executors.newFixedThreadPool(nThreads)
    try {
      val tasks = inputs.grouped(perThread).map { chunk =>
        new Callable[Seq[Seq[Byte]]] {
          def call(): Seq[Seq[Byte]] =
            chunk.map { case (p, n) => encode(Platform.exponentiatePoint(Platform.Ecp(p), n).value) }
        }
      }.toList
      val futures = pool.invokeAll(tasks.asJava)
      val actual = futures.asScala.flatMap(_.get(60, TimeUnit.SECONDS)).toSeq
      actual shouldBe expected
    } finally {
      pool.shutdownNow()
    }
  }
}
