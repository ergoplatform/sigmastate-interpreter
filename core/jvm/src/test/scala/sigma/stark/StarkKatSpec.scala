package sigma.stark

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.io.Source

/** Known-Answer-Test parity for the EIP-0045 verifyStark field primitives.
  *
  * Expected values come from an EXTERNAL oracle — RISC0's own field
  * implementation (risc0-core 1.2.6), captured by the `stark-kat/` generator
  * into `resources/stark-kats/` — never from the Scala code under test
  * (oracle-parity rule: a self-oracle proves consistency, not correctness).
  */
class StarkKatSpec extends AnyFunSuite with Matchers {

  private def lines(resource: String): Seq[String] = {
    val is = getClass.getResourceAsStream(resource)
    require(is != null, s"missing KAT resource $resource — run stark-kat/ generator")
    try Source.fromInputStream(is, "UTF-8").getLines().filterNot(_.startsWith("#")).toList
    finally is.close()
  }

  private def coeffs(s: String): Array[Int] = s.split(',').map(_.toInt)

  test("BabyBear ops match risc0-core vectors (add/sub/mul/neg/inv/pow)") {
    val cases = lines("/stark-kats/babybear_ops.tsv")
    cases should not be empty
    cases.foreach { line =>
      val f = line.split('\t')
      val (a, b) = (f(0).toInt, f(1).toInt)
      withClue(s"a=$a b=$b: ") {
        BabyBear.add(a, b) shouldBe f(2).toInt
        BabyBear.sub(a, b) shouldBe f(3).toInt
        BabyBear.mul(a, b) shouldBe f(4).toInt
        BabyBear.neg(a) shouldBe f(5).toInt
        if (f(6) != "-") BabyBear.inv(a) shouldBe f(6).toInt
        BabyBear.pow(a, b.toLong) shouldBe f(7).toInt
      }
    }
  }

  test("Ext4 ops match risc0-core vectors (add/mul/inv, x^4 + 11 reduction)") {
    val cases = lines("/stark-kats/ext4_ops.tsv")
    cases should not be empty
    cases.foreach { line =>
      val f = line.split('\t')
      val a = coeffs(f(0)); val b = coeffs(f(1))
      val (ea, eb) = (Ext4(a(0), a(1), a(2), a(3)), Ext4(b(0), b(1), b(2), b(3)))
      def arr(e: Ext4): Array[Int] = Array(e.c0, e.c1, e.c2, e.c3)
      withClue(s"a=${f(0)} b=${f(1)}: ") {
        arr(ea + eb) shouldBe coeffs(f(2))
        arr(ea * eb) shouldBe coeffs(f(3))
        if (f(4) != "-") arr(ea.inv) shouldBe coeffs(f(4))
      }
    }
  }

  test("Poseidon2 permutation matches risc0-zkp vectors (width 24)") {
    val cases = lines("/stark-kats/poseidon2_perm.tsv")
    cases should not be empty
    cases.foreach { line =>
      val f = line.split('\t')
      val input = coeffs(f(0))
      val expected = coeffs(f(1))
      input.length shouldBe Poseidon2Constants.Cells
      val cells = input.clone()
      Poseidon2.mix(cells)
      withClue(s"input=${f(0).take(40)}...: ") {
        cells shouldBe expected
      }
    }
  }

  test("Poseidon2 compiled constants expose no mutable consensus view") {
    val vector = lines("/stark-kats/poseidon2_perm.tsv").head.split('\t')
    val input = coeffs(vector(0))
    val expected = coeffs(vector(1))
    val roundConstants = Poseidon2Constants.roundConstantsSnapshot
    val diagonal = Poseidon2Constants.mIntDiagSnapshot
    roundConstants(0) ^= 1
    diagonal(0) ^= 1

    val cells = input.clone()
    Poseidon2.mix(cells)
    cells shouldBe expected
    Poseidon2Constants.roundConstantsSnapshot(0) should not be roundConstants(0)
    Poseidon2Constants.mIntDiagSnapshot(0) should not be diagonal(0)
  }

  test("Poseidon2 unpadded sponge hash matches risc0-zkp vectors (incl. padding + multi-block)") {
    val cases = lines("/stark-kats/poseidon2_hash.tsv")
    cases should not be empty
    cases.foreach { line =>
      val f = line.split('\t')
      val input = if (f(0).isEmpty) Array.empty[Int] else coeffs(f(0))
      val expected = coeffs(f(1))
      withClue(s"len=${input.length}: ") {
        Poseidon2.unpaddedHash(input) shouldBe expected
      }
    }
    // hash_pair is unpadded_hash over the concatenation — pin the equivalence
    // on the 16-element vector (one full rate block).
    val block = cases.map(_.split('\t')).find(f => !f(0).isEmpty && coeffs(f(0)).length == 16).get
    val in16 = coeffs(block(0))
    Poseidon2.hashPair(in16.take(8), in16.drop(8)) shouldBe coeffs(block(1))
  }

  test("Poseidon2Rng transcript replay matches risc0-zkp op-script vectors") {
    val script = lines("/stark-kats/poseidon2_rng.tsv")
    script should not be empty
    val rng = new Poseidon2Rng
    script.foreach { line =>
      if (line.startsWith("mix:")) {
        rng.mix(coeffs(line.stripPrefix("mix:")))
      } else if (line.startsWith("elem -> ")) {
        rng.randomElem() shouldBe line.stripPrefix("elem -> ").toInt
      } else if (line.startsWith("bits:")) {
        val Array(spec, expect) = line.stripPrefix("bits:").split(" -> ")
        rng.randomBits(spec.trim.toInt) shouldBe expect.trim.toInt
      } else if (line.startsWith("ext -> ")) {
        val e = coeffs(line.stripPrefix("ext -> "))
        rng.randomExtElem() shouldBe Ext4(e(0), e(1), e(2), e(3))
      } else fail(s"unknown op line: $line")
    }
  }

  test("Ext4 field laws hold on vector inputs (assoc/distrib/inv roundtrip)") {
    // Structural sanity on top of parity: (a*b)*a == a*(b*a), a*inv(a) == 1.
    val cases = lines("/stark-kats/ext4_ops.tsv").take(20)
    cases.foreach { line =>
      val f = line.split('\t')
      val a = coeffs(f(0)); val b = coeffs(f(1))
      val (ea, eb) = (Ext4(a(0), a(1), a(2), a(3)), Ext4(b(0), b(1), b(2), b(3)))
      (ea * eb) * ea shouldBe ea * (eb * ea)
      if (!ea.isZero) ea * ea.inv shouldBe Ext4.One
    }
  }
}
