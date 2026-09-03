package sigma.stark.profile

import java.io.ByteArrayOutputStream
import java.security.MessageDigest

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import sigma.stark.FriVerifier
import sigma.stark.circuit.{CircuitTapSet, PolyExtOp, PolyExtTable}
import sigma.stark.profile.Risc0RawSealVerifier._

import scala.collection.mutable
import scala.io.Source

/** End-to-end interoperability tests for the direct EIP-0045 RISC Zero
  * raw-seal verifier. The positive fixture was emitted by RISC Zero's own
  * prover at the pinned commit recorded in `fixture-manifest.json`; no host
  * receipt codec or bincode parser participates in these tests.
  */
class Risc0RawSealVerifierE2ESpec extends AnyFunSuite with Matchers {

  private val FixtureRoot = "/stark-kats/eip0045-direct/"
  private val ChunkLengths = Array(65535, 65535, 65535, 26063)

  private def resourceBytes(path: String): Array[Byte] = {
    val in = getClass.getResourceAsStream(path)
    require(in != null, s"missing test resource $path")
    val out = new ByteArrayOutputStream()
    val buffer = new Array[Byte](8192)
    try {
      var read = in.read(buffer)
      while (read >= 0) {
        if (read > 0) out.write(buffer, 0, read)
        read = in.read(buffer)
      }
      out.toByteArray
    } finally {
      in.close()
      out.close()
    }
  }

  private def resourceLines(path: String): Array[String] = {
    val in = getClass.getResourceAsStream(path)
    require(in != null, s"missing test resource $path")
    try Source.fromInputStream(in, "UTF-8").getLines().toArray
    finally in.close()
  }

  private def sha256Hex(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(bytes)
      .map(b => f"${b & 0xff}%02x").mkString

  private def hex(value: String): Array[Byte] = {
    require((value.length & 1) == 0, "hex input must have even length")
    value.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
  }

  private def manifestResource(path: String): String =
    if (path.startsWith("../")) "/stark-kats/" + path.substring(3)
    else FixtureRoot + path

  private def load[A](label: String, value: Either[String, A]): A = value match {
    case Right(result) => result
    case Left(detail)  => fail(s"$label rejected pinned oracle data: $detail")
  }

  private lazy val oracleLines = resourceLines(FixtureRoot + "profile-oracle.tsv")
    .filter(line => line.nonEmpty && !line.startsWith("#"))

  private lazy val parameters: Map[String, String] = oracleLines
    .filter(_.startsWith("param\t"))
    .map(_.split("\t", -1))
    .map(fields => fields(1) -> fields(2))
    .toMap

  private lazy val innerControlRoot: Array[Byte] = oracleLines
    .find(_.startsWith("inner_control_root\t"))
    .map(_.split("\t", -1)(1)).map(hex)
    .getOrElse(fail("profile oracle has no inner control root"))

  private lazy val controls: IndexedSeq[(Int, Int, Array[Byte])] = oracleLines
    .filter(_.startsWith("control\t"))
    .map(_.split("\t", -1))
    .map(fields => (fields(1).toInt, fields(2).toInt, hex(fields(3))))
    .toIndexedSeq

  private lazy val taps: CircuitTapSet =
    load("tap table", CircuitTapSet.parse(
      resourceLines("/stark-kats/circuit_taps.tsv").iterator))

  private lazy val polyExt: PolyExtTable =
    load("poly-ext table", PolyExtTable.parse(
      resourceLines("/stark-kats/circuit_polyext_ops.tsv").iterator))

  private def parametersWith(program: PolyExtTable): VerifierParameters =
    new VerifierParameters(
      parameters("proof_system_info"),
      parameters("circuit_info"),
      parameters("output_size").toInt,
      parameters("mix_size").toInt,
      parameters("queries").toInt,
      parameters("inv_rate").toInt,
      parameters("ext_size").toInt,
      parameters("check_size").toInt,
      parameters("fri_fold").toInt,
      parameters("fri_fold_po2").toInt,
      parameters("fri_min_degree").toInt,
      taps,
      program)

  private lazy val verifierParameters = parametersWith(polyExt)

  private def profile(
      root: Array[Byte],
      controlOverrides: Map[(Int, Int), Array[Byte]]): RawSealProfile = {
    val entries = controls.map { case (kind, parameter, controlId) =>
      new TerminalControl(
        kind,
        parameter,
        controlOverrides.getOrElse((kind, parameter), controlId))
    }
    new RawSealProfile(parameters("outer_po2").toInt, root, entries)
  }

  private def verifier(
      root: Array[Byte] = innerControlRoot,
      controlOverrides: Map[(Int, Int), Array[Byte]] = Map.empty): Risc0RawSealVerifier =
    new Risc0RawSealVerifier(verifierParameters, profile(root, controlOverrides))

  private def verifierWith(program: PolyExtTable): Risc0RawSealVerifier =
    new Risc0RawSealVerifier(
      parametersWith(program),
      profile(innerControlRoot, Map.empty))

  private def stockTable(ops: Array[PolyExtOp]): PolyExtTable =
    PolyExtTable.fromValidatedBinary(
      ops,
      polyExt.ret,
      polyExt.fpVars,
      polyExt.mixVars,
      polyExt.opcodeHistogram)

  private def assertStockShape(table: PolyExtTable): Unit = {
    table.opsCount shouldBe polyExt.opsCount
    table.ret shouldBe polyExt.ret
    table.fpVars shouldBe polyExt.fpVars
    table.mixVars shouldBe polyExt.mixVars
    table.opcodeHistogram shouldBe polyExt.opcodeHistogram
    table.ops.iterator.map(_.getClass).toSeq shouldBe
      polyExt.ops.iterator.map(_.getClass).toSeq
  }

  private lazy val rawSeal = resourceBytes(FixtureRoot + "po2-15-raw-seal.bin")
  private lazy val expectedClaim = resourceBytes(FixtureRoot + "po2-15-claim-digest.bin")
  private lazy val polyExtOracleSeal =
    resourceBytes("/stark-kats/eip0045-arkadia-independent/raw-seal.bin")
  private lazy val polyExtOracleClaim =
    resourceBytes("/stark-kats/eip0045-arkadia-independent/claim-digest.bin")

  private val PolyExtCheckpointLabels =
    Set("out", "mix", "poly_mix", "eval_u", "result", "check_value")

  private lazy val polyExtOracle: Map[String, Array[Int]] = {
    val rows = resourceLines("/stark-kats/polyext_transcript_oracle.tsv").iterator
      .filter(_.startsWith("ck\t"))
      .map(_.split("\t", -1))
      .toArray
    rows.length shouldBe PolyExtCheckpointLabels.size
    rows.map(_(1)).toSet shouldBe PolyExtCheckpointLabels
    rows.iterator.map { fields =>
      val values = fields.last.split(",", -1)
        .map(java.lang.Long.parseLong)
        .map(_.toInt)
      if (fields(1) == "eval_u") {
        fields.length shouldBe 4
        fields(2).toInt shouldBe values.length / 4
      } else {
        fields.length shouldBe 3
      }
      fields(1) -> values
    }.toMap
  }

  private def canonicalChunks(bytes: Array[Byte]): Array[Array[Byte]] = {
    require(bytes.length == ChunkLengths.sum, "fixture does not have the canonical raw-seal size")
    val chunks = new Array[Array[Byte]](ChunkLengths.length)
    var offset = 0
    var i = 0
    while (i < chunks.length) {
      chunks(i) = java.util.Arrays.copyOfRange(bytes, offset, offset + ChunkLengths(i))
      offset += ChunkLengths(i)
      i += 1
    }
    chunks
  }

  private final class QueryProbe extends Probe {
    var queries: Int = 0
    override def onCheckpoint(label: String, values: Array[Int]): Unit =
      if (label == "query") queries += 1
  }

  private final class PolyExtCheckpointProbe extends Probe {
    val captured: mutable.Map[String, Vector[Array[Int]]] = mutable.Map.empty

    override def onCheckpoint(label: String, values: Array[Int]): Unit = {
      if (PolyExtCheckpointLabels.contains(label)) {
        val observations = captured.getOrElse(label, Vector.empty)
        captured.update(label, observations :+ values.clone())
      }
    }
  }

  test("fixture manifest pins public provenance, exact sizes, and SHA-256 digests") {
    val manifestText = new String(
      resourceBytes(FixtureRoot + "fixture-manifest.json"),
      "UTF-8")
    manifestText should include("\"formatVersion\": 1")
    manifestText should include("\"candidateStatus\": \"non-final-pre-B3-test-evidence\"")
    manifestText should include("8eb06ab020a92dc5b63ba6dd0836d432aba6d890")

    val fileEntry =
      """(?s)\{\s*"path"\s*:\s*"([^"]+)"\s*,\s*"encoding"\s*:\s*"([^"]+)"\s*,\s*"length"\s*:\s*([0-9]+)\s*,\s*"sha256"\s*:\s*"([0-9a-f]{64})"\s*\}""".r
    val entries = fileEntry.findAllMatchIn(manifestText).map { matched =>
      (matched.group(1), matched.group(2), matched.group(3).toLong, matched.group(4))
    }.toVector
    entries.length shouldBe 5

    entries.foreach { case (path, encoding, expectedLength, expectedDigest) =>
      val bytes = resourceBytes(manifestResource(path))
      withClue(path + ": ") {
        bytes.length.toLong shouldBe expectedLength
        sha256Hex(bytes) shouldBe expectedDigest
        encoding match {
          case "raw-binary" => ()
          case "utf8-lf-text" =>
            bytes should not contain 0x0d.toByte
            new String(bytes, "UTF-8").getBytes("UTF-8") shouldBe bytes
          case other => fail(s"unknown fixture encoding $other")
        }
      }
    }

    manifestText should not include ("D:\\")
    manifestText should not include ("C:\\")
  }

  test("stock verifier accepts the real po2=15 seal through exactly four canonical chunks") {
    val chunks = canonicalChunks(rawSeal)
    chunks.map(_.length).toSeq shouldBe ChunkLengths.toSeq
    verifier().verify(chunks, expectedClaim) shouldBe Right(Verified(1, 15))
  }

  test("production verifier reproduces every PolyExt oracle checkpoint exactly once") {
    val probe = new PolyExtCheckpointProbe
    verifier().verify(
      canonicalChunks(polyExtOracleSeal),
      polyExtOracleClaim,
      probe) shouldBe Right(Verified(1, 16))

    probe.captured.keySet shouldBe PolyExtCheckpointLabels
    PolyExtCheckpointLabels.foreach { label =>
      withClue(label + ": ") {
        probe.captured(label) should have length 1
        probe.captured(label).head.toSeq shouldBe polyExtOracle(label).toSeq
      }
    }
  }

  test("verifier construction rejects a stock-shaped PolyExt tap outside the frozen tap set") {
    val invalidTapOps = polyExt.ops
    val getIndex = invalidTapOps.indexWhere(_.isInstanceOf[PolyExtOp.Get])
    getIndex should be >= 0
    invalidTapOps(getIndex) = PolyExtOp.Get(taps.tapSize)
    val invalidTapTable = stockTable(invalidTapOps)
    assertStockShape(invalidTapTable)
    val invalidTap = the[ProfileInvariantException] thrownBy verifierWith(invalidTapTable)
    invalidTap.getMessage shouldBe "constraint tap is out of range"
  }

  test("verifier construction rejects a stock-shaped PolyExt forward fp reference") {
    val forwardFpOps = polyExt.ops
    val arithmeticIndex = forwardFpOps.indexWhere {
      case _: PolyExtOp.Add | _: PolyExtOp.Sub | _: PolyExtOp.Mul => true
      case _                                                      => false
    }
    arithmeticIndex should be >= 0
    val currentFp = forwardFpOps.iterator.take(arithmeticIndex).count {
      case PolyExtOp.True | _: PolyExtOp.AndEqz | _: PolyExtOp.AndCond => false
      case _                                                           => true
    }
    forwardFpOps(arithmeticIndex) = forwardFpOps(arithmeticIndex) match {
      case PolyExtOp.Add(_, b) => PolyExtOp.Add(currentFp, b)
      case PolyExtOp.Sub(_, b) => PolyExtOp.Sub(currentFp, b)
      case PolyExtOp.Mul(_, b) => PolyExtOp.Mul(currentFp, b)
      case other               => fail(s"expected arithmetic op, got $other")
    }
    val forwardFpTable = stockTable(forwardFpOps)
    assertStockShape(forwardFpTable)
    val forwardFp = the[ProfileInvariantException] thrownBy verifierWith(forwardFpTable)
    forwardFp.getMessage shouldBe "constraint program fp operand is out of range"
  }

  test("claim mismatch is rejected only after all 50 proof queries") {
    val wrongClaim = expectedClaim.clone()
    wrongClaim(0) = (wrongClaim(0) ^ 1).toByte
    val probe = new QueryProbe
    verifier().verify(canonicalChunks(rawSeal), wrongClaim, probe) shouldBe Left(ClaimMismatch)
    probe.queries shouldBe FriVerifier.Queries
  }

  test("a reconstructed control ID outside the fixed allowlist is rejected") {
    val disallowed = Array.fill[Byte](DigestBytes)(0)
    verifier(controlOverrides = Map((1, 15) -> disallowed))
      .verify(canonicalChunks(rawSeal), expectedClaim) shouldBe Left(ControlIdNotAllowed)
  }

  test("inner control-root mismatch is rejected only after full cryptographic verification") {
    val wrongRoot = Array.fill[Byte](DigestBytes)(0)
    val probe = new QueryProbe
    verifier(root = wrongRoot)
      .verify(canonicalChunks(rawSeal), expectedClaim, probe) shouldBe Left(InnerControlRootMismatch)
    probe.queries shouldBe FriVerifier.Queries
  }

  test("cryptographic mutations in the middle and final proof word cannot verify") {
    Seq(27000, RawSealV1Decoder.WordCount - 1).foreach { wordIndex =>
      val mutated = rawSeal.clone()
      val offset = wordIndex * 4
      mutated(offset) = (mutated(offset) ^ 1).toByte
      withClue(s"word $wordIndex: ") {
        verifier().verify(canonicalChunks(mutated), expectedClaim) match {
          case Left(MalformedProof("fri", detail)) => detail should include("root path mismatch")
          case Left(other) => fail(s"mutation reached an unexpected rejection branch: $other")
          case Right(value) => fail(s"mutation verified as $value")
        }
      }
    }
  }

  test("transport partition and literal outerPo2 are consensus-strict") {
    val shifted = Array(
      java.util.Arrays.copyOfRange(rawSeal, 0, 65534),
      java.util.Arrays.copyOfRange(rawSeal, 65534, 131070),
      java.util.Arrays.copyOfRange(rawSeal, 131070, 196605),
      java.util.Arrays.copyOfRange(rawSeal, 196605, rawSeal.length))
    verifier().verify(shifted, expectedClaim) shouldBe Left(TransportRejected(
      RawSealV1Decoder.WrongChunkLength(0, 65535, 65534)))

    val wrongOuterPo2 = rawSeal.clone()
    wrongOuterPo2(32 * 4) = 17
    wrongOuterPo2(32 * 4 + 1) = 0
    wrongOuterPo2(32 * 4 + 2) = 0
    wrongOuterPo2(32 * 4 + 3) = 0
    verifier().verify(canonicalChunks(wrongOuterPo2), expectedClaim) shouldBe Left(TransportRejected(
      RawSealV1Decoder.WrongOuterPo2(RawSealV1Decoder.ExpectedOuterPo2, 17L)))
  }
}
