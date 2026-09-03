/*
 * SPDX-License-Identifier: MIT AND Apache-2.0
 *
 * Copyright 2026 A. Shannon.
 */
package sigma.stark.profile

import sigma.stark.{BabyBear, FriVerifier, Poseidon2Constants}
import sigma.stark.circuit.{CircuitTap, CircuitTapSet, PolyExtOp, PolyExtTable, TapRegister}
import sigma.stark.profile.Risc0RawSealVerifier.{RawSealProfile, TerminalControl, VerifierParameters}

/** Authenticates and decodes one activated EIP-0045 stock-profile package.
  *
  * This is the only public construction boundary for a production
  * [[Risc0RawSealVerifier]]. It snapshots every caller-owned array, validates
  * the exact 458-byte manifest and its network-selected `profileId`, binds B1
  * and B2 through their domain-separated artifact envelopes, then strictly
  * decodes B2 before constructing the verifier. No classpath resource or TSV
  * parser participates in production assembly.
  *
  * The loader is intentionally bounded: Manifest V1 and B2 have exact sizes,
  * and the normative ASCII B1 artifact is capped before allocation or hashing.
  */
object Risc0ProfilePackageLoader {
  final val ManifestBytes: Int = 458
  final val BinaryDataBytes: Int = 65119
  final val ProfileIdBytes: Int = 32
  final val MaxAlgorithmBytes: Int = 1024 * 1024

  private final val ManifestVersion = 1
  private final val ControlCount = 10
  private final val AlgorithmKind = 1
  private final val BinaryDataKind = 2
  private final val StockPayloadBytes = 16384
  private final val LiftCount = 8
  private final val FirstSegmentPo2 = 15

  private final val HeaderBytes = 87
  private final val RootCount = 28
  private final val StoredRoundConstants = 213
  private final val ExpandedRoundConstants = 29 * 24
  private final val DiagonalConstants = 24
  private final val TapCount = 643
  private final val GroupCount = 3
  private final val ComboCount = 5
  private final val TotalComboBacks = 20
  private final val PolyExtOps = 12359
  private final val PolyExtFpVars = 11130
  private final val PolyExtMixVars = 1229
  private final val PolyExtReturn = 1228
  private final val RegisterCount = 163
  private final val TapTableOffset = 1147
  private final val PolyExtOffset = 4362

  private val GroupNames = Array("accum", "code", "data")
  private val GroupSizes = Array(12, 23, 128)
  private val ExpectedGroupBegin = Array(0, 16, 39, 643)
  private val ExpectedHistogram = Array(284, 0, 669, 52, 4061, 1385, 4679, 1, 1076, 152)

  private val ArtifactDomain = ascii("Ergo.StarkProfileArtifact.v1")
  private val ProfileIdDomain = ascii("Ergo.StarkProfileId.v1")

  /** A profile package which was fully authenticated before construction.
    * Array-valued identity data is returned by copy.
    */
  final class LoadedProfile private[profile] (
      val verifier: Risc0RawSealVerifier,
      val exactProofBytes: Int,
      val maxApplicationPayloadBytes: Int,
      val outerPo2: Int,
      profileIdBytes: Array[Byte]) {
    private val profileIdSnapshot = profileIdBytes.clone()
    def profileId: Array[Byte] = profileIdSnapshot.clone()
  }

  /** Stable startup rejection taxonomy. These failures describe immutable
    * activation/package data, never proof-controlled bytes.
    */
  sealed trait Failure extends Product with Serializable {
    def code: String
  }

  final case class NullInput(name: String) extends Failure {
    override val code: String = "profile-package-null-input"
  }
  final case class WrongInputLength(name: String, expected: Int, actual: Int) extends Failure {
    override val code: String = "profile-package-wrong-input-length"
  }
  final case class AlgorithmTooLarge(actual: Int, maximum: Int) extends Failure {
    override val code: String = "profile-package-algorithm-too-large"
  }
  final case class AlgorithmEncodingRejected(offset: Int, value: Int) extends Failure {
    override val code: String = "profile-package-algorithm-encoding-rejected"
  }
  final case class ManifestRejected(field: String, detail: String) extends Failure {
    override val code: String = "profile-package-manifest-rejected"
  }
  case object ProfileIdMismatch extends Failure {
    override val code: String = "profile-package-profile-id-mismatch"
  }
  final case class ArtifactLengthMismatch(kind: Int, expected: Long, actual: Int) extends Failure {
    override val code: String = "profile-package-artifact-length-mismatch"
  }
  final case class ArtifactDigestMismatch(kind: Int) extends Failure {
    override val code: String = "profile-package-artifact-digest-mismatch"
  }
  final case class BinaryDataRejected(offset: Int, detail: String) extends Failure {
    override val code: String = "profile-package-binary-data-rejected"
  }
  final case class CompiledImplementationMismatch(component: String, index: Int) extends Failure {
    override val code: String = "profile-package-compiled-implementation-mismatch"
  }
  final case class VerifierConstructionRejected(detail: String) extends Failure {
    override val code: String = "profile-package-verifier-construction-rejected"
  }

  /** Load one package selected by a network activation record.
    *
    * The expected profile ID is an activation input, not read from the
    * package itself. All four arrays are snapshotted before authentication so
    * concurrent caller mutation cannot create a check/use split.
    */
  def load(
      rawManifest: Array[Byte],
      algorithmBytes: Array[Byte],
      binaryDataBytes: Array[Byte],
      expectedProfileId: Array[Byte]): Either[Failure, LoadedProfile] = {
    if (rawManifest == null) return Left(NullInput("manifest"))
    if (algorithmBytes == null) return Left(NullInput("algorithm"))
    if (binaryDataBytes == null) return Left(NullInput("binary-data"))
    if (expectedProfileId == null) return Left(NullInput("expected-profile-id"))
    if (rawManifest.length != ManifestBytes)
      return Left(WrongInputLength("manifest", ManifestBytes, rawManifest.length))
    if (expectedProfileId.length != ProfileIdBytes)
      return Left(WrongInputLength("expected-profile-id", ProfileIdBytes, expectedProfileId.length))
    if (binaryDataBytes.length != BinaryDataBytes)
      return Left(WrongInputLength("binary-data", BinaryDataBytes, binaryDataBytes.length))
    if (algorithmBytes.length > MaxAlgorithmBytes)
      return Left(AlgorithmTooLarge(algorithmBytes.length, MaxAlgorithmBytes))

    val manifestSnapshot = rawManifest.clone()
    val algorithmSnapshot = algorithmBytes.clone()
    val binarySnapshot = binaryDataBytes.clone()
    val expectedIdSnapshot = expectedProfileId.clone()

    try {
      Right(loadChecked(
        manifestSnapshot,
        algorithmSnapshot,
        binarySnapshot,
        expectedIdSnapshot))
    } catch {
      case rejected: Rejected => Left(rejected.failure)
    }
  }

  /** Domain-separated artifact digest, exposed package-wide for KATs and
    * activation tooling which shares this exact codec.
    */
  private[profile] def artifactDigest(kind: Int, bytes: Array[Byte]): Array[Byte] = {
    if (kind < 0 || kind > 0xffff) throw new IllegalArgumentException("artifact kind is not u16")
    if (bytes == null) throw new NullPointerException("artifact bytes")
    if (bytes.length > MaxAlgorithmBytes && kind == AlgorithmKind)
      throw new IllegalArgumentException("algorithm artifact exceeds loader bound")
    val preimage = new Array[Byte](ArtifactDomain.length + 1 + 2 + 4 + bytes.length)
    var pos = 0
    pos = copy(ArtifactDomain, preimage, pos)
    preimage(pos) = 0
    pos += 1
    writeU16Le(preimage, pos, kind)
    pos += 2
    writeU32Le(preimage, pos, bytes.length.toLong)
    pos += 4
    copy(bytes, preimage, pos)
    ProfileBlake2b256.hash(preimage)
  }

  private[profile] def profileIdDigest(manifest: Array[Byte]): Array[Byte] = {
    if (manifest == null || manifest.length != ManifestBytes)
      throw new IllegalArgumentException("profile ID requires exactly 458 manifest bytes")
    val preimage = new Array[Byte](ProfileIdDomain.length + 1 + 4 + ManifestBytes)
    var pos = 0
    pos = copy(ProfileIdDomain, preimage, pos)
    preimage(pos) = 0
    pos += 1
    writeU32Le(preimage, pos, ManifestBytes.toLong)
    pos += 4
    copy(manifest, preimage, pos)
    ProfileBlake2b256.hash(preimage)
  }

  private def loadChecked(
      rawManifest: Array[Byte],
      algorithmBytes: Array[Byte],
      binaryDataBytes: Array[Byte],
      expectedProfileId: Array[Byte]): LoadedProfile = {
    val manifest = decodeManifest(rawManifest)
    val derivedProfileId = profileIdDigest(rawManifest)
    if (!constantTimeEqual(derivedProfileId, expectedProfileId)) reject(ProfileIdMismatch)

    validateArtifact(manifest.algorithmArtifact, algorithmBytes)
    validateArtifact(manifest.binaryArtifact, binaryDataBytes)
    validateAlgorithm(algorithmBytes)

    val binary = decodeBinaryData(binaryDataBytes)
    validateCompiledImplementation(binary)

    val parameters = new VerifierParameters(
      binary.proofSystemInfo,
      binary.circuitInfo,
      binary.outputSize,
      binary.mixSize,
      binary.queries,
      binary.invRate,
      binary.extSize,
      binary.checkSize,
      binary.friFold,
      binary.friFoldPo2,
      binary.friMinDegree,
      binary.taps,
      binary.polyExt)

    val controls = manifest.controls.map { control =>
      new TerminalControl(control.kind, control.parameter, control.controlId)
    }.toIndexedSeq
    val profile = new RawSealProfile(
      manifest.outerPo2,
      manifest.innerControlRoot,
      controls)
    val verifier = try new Risc0RawSealVerifier(parameters, profile)
    catch {
      case invalid: Risc0RawSealVerifier.ProfileInvariantException =>
        reject(VerifierConstructionRejected(invalid.getMessage))
    }
    new LoadedProfile(
      verifier,
      manifest.exactProofBytes.toInt,
      manifest.maxApplicationPayloadBytes.toInt,
      manifest.outerPo2,
      derivedProfileId)
  }

  // ------------------------------------------------------------------
  // Manifest V1.
  // ------------------------------------------------------------------

  private final case class ArtifactReference(kind: Int, length: Long, digest: Array[Byte])
  private final case class Control(kind: Int, parameter: Int, controlId: Array[Byte])
  private final case class Manifest(
      exactProofBytes: Long,
      maxApplicationPayloadBytes: Long,
      outerPo2: Int,
      innerControlRoot: Array[Byte],
      controls: Array[Control],
      algorithmArtifact: ArtifactReference,
      binaryArtifact: ArtifactReference)

  private def decodeManifest(bytes: Array[Byte]): Manifest = {
    val cursor = new Cursor(bytes, "manifest")
    val version = cursor.u8()
    if (version != ManifestVersion)
      manifestReject("formatVersion", "expected 1, got " + version)
    val exactProofBytes = cursor.u32()
    val maxPayloadBytes = cursor.u32()
    val outerPo2 = cursor.u8()
    val innerRoot = cursor.bytes(ProfileIdBytes)
    val controls = new Array[Control](ControlCount)
    var i = 0
    while (i < controls.length) {
      controls(i) = Control(cursor.u8(), cursor.u8(), cursor.bytes(ProfileIdBytes))
      i += 1
    }
    val algorithm = ArtifactReference(cursor.u16(), cursor.u32(), cursor.bytes(ProfileIdBytes))
    val binary = ArtifactReference(cursor.u16(), cursor.u32(), cursor.bytes(ProfileIdBytes))
    if (cursor.position != ManifestBytes)
      manifestReject("layout", "decoder did not reach byte 458")

    if (exactProofBytes == 0) manifestReject("exactProofBytes", "must be nonzero")
    if (exactProofBytes != RawSealV1Decoder.ByteCount.toLong)
      manifestReject("exactProofBytes", "stock raw-seal verifier requires 222668")
    if (maxPayloadBytes != StockPayloadBytes.toLong)
      manifestReject("maxApplicationPayloadBytes", "stock profile requires 16384")
    if (outerPo2 != RawSealV1Decoder.ExpectedOuterPo2)
      manifestReject("outerPo2", "stock raw-seal verifier requires 18")
    validateDigestWords(innerRoot, "innerControlRoot")

    i = 0
    while (i < controls.length) {
      val expectedKind =
        if (i < LiftCount) Risc0RawSealVerifier.NormalLiftControlKind
        else if (i == LiftCount) Risc0RawSealVerifier.JoinControlKind
        else Risc0RawSealVerifier.ResolveControlKind
      val expectedParameter = if (i < LiftCount) FirstSegmentPo2 + i else 0
      if (controls(i).kind != expectedKind)
        manifestReject("controls[" + i + "].kind", "expected " + expectedKind)
      if (controls(i).parameter != expectedParameter)
        manifestReject("controls[" + i + "].parameter", "expected " + expectedParameter)
      validateDigestWords(controls(i).controlId, "controls[" + i + "].controlId")
      var earlier = 0
      while (earlier < i) {
        if (constantTimeEqual(controls(earlier).controlId, controls(i).controlId))
          manifestReject("controls[" + i + "].controlId", "duplicate control ID")
        earlier += 1
      }
      i += 1
    }
    if (algorithm.kind != AlgorithmKind)
      manifestReject("algorithm.kind", "expected artifact kind 1")
    if (binary.kind != BinaryDataKind)
      manifestReject("binaryData.kind", "expected artifact kind 2")
    if (algorithm.length == 0)
      manifestReject("algorithm.length", "must be nonzero")
    if (algorithm.length > MaxAlgorithmBytes.toLong)
      manifestReject("algorithm.length", "exceeds loader bound")
    if (binary.length != BinaryDataBytes.toLong)
      manifestReject("binaryData.length", "stock B2 must contain exactly 65119 bytes")

    val decoded = Manifest(
      exactProofBytes,
      maxPayloadBytes,
      outerPo2,
      innerRoot,
      controls,
      algorithm,
      binary)
    if (!java.util.Arrays.equals(encodeManifest(decoded), bytes))
      manifestReject("layout", "decode/re-encode identity failed")
    decoded
  }

  private def encodeManifest(manifest: Manifest): Array[Byte] = {
    val output = new Array[Byte](ManifestBytes)
    val writer = new Writer(output)
    writer.u8(ManifestVersion)
    writer.u32(manifest.exactProofBytes)
    writer.u32(manifest.maxApplicationPayloadBytes)
    writer.u8(manifest.outerPo2)
    writer.bytes(manifest.innerControlRoot)
    var i = 0
    while (i < manifest.controls.length) {
      writer.u8(manifest.controls(i).kind)
      writer.u8(manifest.controls(i).parameter)
      writer.bytes(manifest.controls(i).controlId)
      i += 1
    }
    encodeArtifactReference(writer, manifest.algorithmArtifact)
    encodeArtifactReference(writer, manifest.binaryArtifact)
    if (writer.position != ManifestBytes)
      throw new IllegalStateException("internal manifest encoder length")
    output
  }

  private def encodeArtifactReference(writer: Writer, reference: ArtifactReference): Unit = {
    writer.u16(reference.kind)
    writer.u32(reference.length)
    writer.bytes(reference.digest)
  }

  private def validateArtifact(reference: ArtifactReference, bytes: Array[Byte]): Unit = {
    if (reference.length != bytes.length.toLong)
      reject(ArtifactLengthMismatch(reference.kind, reference.length, bytes.length))
    val actual = artifactDigest(reference.kind, bytes)
    if (!constantTimeEqual(reference.digest, actual))
      reject(ArtifactDigestMismatch(reference.kind))
  }

  private def validateAlgorithm(bytes: Array[Byte]): Unit = {
    if (bytes.length == 0) reject(AlgorithmEncodingRejected(0, -1))
    var i = 0
    while (i < bytes.length) {
      val value = bytes(i) & 0xff
      if (value != 0x09 && value != 0x0a && (value < 0x20 || value > 0x7e))
        reject(AlgorithmEncodingRejected(i, value))
      i += 1
    }
    if ((bytes(bytes.length - 1) & 0xff) != 0x0a)
      reject(AlgorithmEncodingRejected(bytes.length - 1, bytes(bytes.length - 1) & 0xff))
    if (bytes.length >= 2 && (bytes(bytes.length - 2) & 0xff) == 0x0a)
      reject(AlgorithmEncodingRejected(bytes.length - 2, 0x0a))
  }

  private def validateDigestWords(bytes: Array[Byte], field: String): Unit = {
    var offset = 0
    while (offset < bytes.length) {
      if (readU32Le(bytes, offset) >= BabyBear.P.toLong)
        manifestReject(field, "contains a non-canonical BabyBear word")
      offset += 4
    }
  }

  // ------------------------------------------------------------------
  // B2 stock-profile binary data.
  // ------------------------------------------------------------------

  private final class BinaryProfile(
      val reverseRoots: Array[Int],
      val storedRoundConstants: Array[Int],
      val expandedRoundConstants: Array[Int],
      val diagonals: Array[Int],
      val tapRows: Array[CircuitTap],
      val taps: CircuitTapSet,
      val ops: Array[PolyExtOp],
      val polyExt: PolyExtTable,
      val proofSystemInfo: String,
      val circuitInfo: String,
      val outputSize: Int,
      val mixSize: Int,
      val queries: Int,
      val invRate: Int,
      val extSize: Int,
      val checkSize: Int,
      val friFold: Int,
      val friFoldPo2: Int,
      val friMinDegree: Int)

  private def decodeBinaryData(bytes: Array[Byte]): BinaryProfile = {
    val cursor = new Cursor(bytes, "binary-data")

    expectU32(cursor, BabyBear.P.toLong, "BabyBear modulus")
    val extSize = expectU8(cursor, 4, "extension degree")
    expectU32(cursor, 11, "extension polynomial beta")
    expectU8(cursor, 27, "maximum root exponent")
    expectU8(cursor, 24, "Poseidon2 cells")
    expectU8(cursor, 16, "Poseidon2 rate")
    expectU8(cursor, 8, "Poseidon2 output")
    expectU8(cursor, 4, "Poseidon2 half-full rounds")
    expectU8(cursor, 21, "Poseidon2 partial rounds")
    expectU8(cursor, 7, "Poseidon2 S-box degree")
    expectU16(cursor, StoredRoundConstants, "stored Poseidon2 constants")
    expectU8(cursor, DiagonalConstants, "Poseidon2 diagonal constants")
    val queries = expectU8(cursor, 50, "STARK queries")
    val invRate = expectU8(cursor, 4, "inverse Reed-Solomon rate")
    val friFold = expectU8(cursor, 16, "FRI fold")
    val friFoldPo2 = expectU8(cursor, 4, "FRI fold exponent")
    val friMinDegree = expectU16(cursor, 256, "FRI minimum degree")
    val outputSize = expectU8(cursor, 32, "recursion output size")
    val mixSize = expectU8(cursor, 20, "recursion mix size")
    val checkSize = expectU8(cursor, 16, "check size")
    expectU16(cursor, TapCount, "tap count")
    expectU8(cursor, GroupCount, "register-group count")
    var i = 0
    while (i < GroupSizes.length) {
      expectU8(cursor, GroupSizes(i), "group size " + i)
      i += 1
    }
    expectU8(cursor, ComboCount, "combination count")
    expectU8(cursor, TotalComboBacks, "total combination backs")
    expectU16(cursor, PolyExtOps, "PolyExt instruction count")
    expectU16(cursor, PolyExtFpVars, "PolyExt field-variable count")
    expectU16(cursor, PolyExtMixVars, "PolyExt mix-variable count")
    expectU16(cursor, PolyExtReturn, "returned mix-variable index")
    val expectedRemap = Array(0, 2, 1, 3)
    i = 0
    while (i < expectedRemap.length) {
      expectU8(cursor, expectedRemap(i), "extension coefficient remap " + i)
      i += 1
    }
    i = 0
    while (i < GroupCount) {
      expectU8(cursor, i, "register-group identifier " + i)
      i += 1
    }
    expectU8(cursor, 16, "proof-system info length")
    expectU8(cursor, 16, "circuit info length")
    expectU8(cursor, 5, "tap-record width")
    expectU8(cursor, 3, "extension challenge scale")
    val proofSystemInfo = cursor.ascii(16)
    val circuitInfo = cursor.ascii(16)
    if (proofSystemInfo != "RISC0_STARK:v1__")
      binaryReject(cursor.position - 32, "proof-system info does not match stock profile")
    if (circuitInfo != "RECURSION:rev1v1")
      binaryReject(cursor.position - 16, "circuit info does not match stock profile")
    if (cursor.position != HeaderBytes)
      binaryReject(cursor.position, "fixed header did not end at byte 87")

    val reverseRoots = new Array[Int](RootCount)
    i = 0
    while (i < reverseRoots.length) {
      reverseRoots(i) = cursor.fieldElement("reverse root " + i)
      i += 1
    }

    val storedConstants = new Array[Int](StoredRoundConstants)
    val expandedConstants = new Array[Int](ExpandedRoundConstants)
    var stored = 0
    var round = 0
    while (round < 29) {
      val full = round < 4 || round >= 25
      val count = if (full) 24 else 1
      var cell = 0
      while (cell < count) {
        val value = cursor.fieldElement("Poseidon2 round " + round + " cell " + cell)
        storedConstants(stored) = value
        expandedConstants(round * 24 + cell) = value
        stored += 1
        cell += 1
      }
      round += 1
    }
    if (stored != StoredRoundConstants)
      binaryReject(cursor.position, "Poseidon2 stored-constant census mismatch")

    val diagonals = new Array[Int](DiagonalConstants)
    i = 0
    while (i < diagonals.length) {
      diagonals(i) = cursor.fieldElement("Poseidon2 diagonal " + i)
      i += 1
    }
    if (cursor.position != TapTableOffset)
      binaryReject(cursor.position, "numeric tables did not end at byte 1147")

    val tapRows = new Array[CircuitTap](TapCount)
    i = 0
    while (i < tapRows.length) {
      tapRows(i) = CircuitTap(cursor.u8(), cursor.u8(), cursor.u8(), cursor.u8(), cursor.u8())
      i += 1
    }
    if (cursor.position != PolyExtOffset)
      binaryReject(cursor.position, "tap table did not end at byte 4362")
    val taps = validateAndBuildTaps(tapRows)

    val ops = new Array[PolyExtOp](PolyExtOps)
    val histogram = Array.fill(10)(0)
    var fpCount = 0
    var mixCount = 0
    i = 0
    while (i < ops.length) {
      val opOffset = cursor.position
      val tag = cursor.u8()
      if (tag < 0 || tag >= histogram.length)
        binaryReject(opOffset, "unknown PolyExt tag " + tag)
      val op: PolyExtOp = tag match {
        case 0 =>
          val value = cursor.u32()
          if (value >= BabyBear.P.toLong)
            binaryReject(opOffset, "Const is not a canonical BabyBear value")
          fpCount += 1
          PolyExtOp.Const(value)
        case 1 =>
          val values = Array(cursor.u32(), cursor.u32(), cursor.u32(), cursor.u32())
          var component = 0
          while (component < values.length) {
            if (values(component) >= BabyBear.P.toLong)
              binaryReject(opOffset, "ConstExt is not a canonical BabyBear value")
            component += 1
          }
          fpCount += 1
          PolyExtOp.ConstExt(values(0), values(1), values(2), values(3))
        case 2 =>
          val tap = cursor.u16()
          if (tap >= TapCount) binaryReject(opOffset, "Get tap is out of range")
          fpCount += 1
          PolyExtOp.Get(tap)
        case 3 =>
          val arg = cursor.u16()
          val offset = cursor.u16()
          val bound = if (arg == 0) outputSize else if (arg == 1) mixSize else -1
          if (bound < 0 || offset >= bound)
            binaryReject(opOffset, "GetGlobal operand is out of range")
          fpCount += 1
          PolyExtOp.GetGlobal(arg, offset)
        case 4 =>
          val left = cursor.u16()
          val right = cursor.u16()
          requireFpRef(opOffset, left, fpCount)
          requireFpRef(opOffset, right, fpCount)
          fpCount += 1
          PolyExtOp.Add(left, right)
        case 5 =>
          val left = cursor.u16()
          val right = cursor.u16()
          requireFpRef(opOffset, left, fpCount)
          requireFpRef(opOffset, right, fpCount)
          fpCount += 1
          PolyExtOp.Sub(left, right)
        case 6 =>
          val left = cursor.u16()
          val right = cursor.u16()
          requireFpRef(opOffset, left, fpCount)
          requireFpRef(opOffset, right, fpCount)
          fpCount += 1
          PolyExtOp.Mul(left, right)
        case 7 =>
          mixCount += 1
          PolyExtOp.True
        case 8 =>
          val chain = cursor.u16()
          val inner = cursor.u16()
          requireMixRef(opOffset, chain, mixCount)
          requireFpRef(opOffset, inner, fpCount)
          mixCount += 1
          PolyExtOp.AndEqz(chain, inner)
        case 9 =>
          val chain = cursor.u16()
          val condition = cursor.u16()
          val inner = cursor.u16()
          requireMixRef(opOffset, chain, mixCount)
          requireFpRef(opOffset, condition, fpCount)
          requireMixRef(opOffset, inner, mixCount)
          mixCount += 1
          PolyExtOp.AndCond(chain, condition, inner)
      }
      histogram(tag) += 1
      ops(i) = op
      i += 1
    }
    if (cursor.position != BinaryDataBytes)
      binaryReject(cursor.position, "strict EOF failed")
    if (fpCount != PolyExtFpVars)
      binaryReject(cursor.position, "PolyExt field-variable census mismatch")
    if (mixCount != PolyExtMixVars || PolyExtReturn != mixCount - 1)
      binaryReject(cursor.position, "PolyExt mix-variable/return census mismatch")
    i = 0
    while (i < histogram.length) {
      if (histogram(i) != ExpectedHistogram(i))
        binaryReject(cursor.position, "PolyExt tag " + i + " census mismatch")
      i += 1
    }
    val histogramMap = Map(
      "Const" -> histogram(0),
      "ConstExt" -> histogram(1),
      "Get" -> histogram(2),
      "GetGlobal" -> histogram(3),
      "Add" -> histogram(4),
      "Sub" -> histogram(5),
      "Mul" -> histogram(6),
      "True" -> histogram(7),
      "AndEqz" -> histogram(8),
      "AndCond" -> histogram(9))
    val polyExt = PolyExtTable.fromValidatedBinary(
      ops,
      PolyExtReturn,
      PolyExtFpVars,
      PolyExtMixVars,
      histogramMap)

    val decoded = new BinaryProfile(
      reverseRoots,
      storedConstants,
      expandedConstants,
      diagonals,
      tapRows,
      taps,
      ops,
      polyExt,
      proofSystemInfo,
      circuitInfo,
      outputSize,
      mixSize,
      queries,
      invRate,
      extSize,
      checkSize,
      friFold,
      friFoldPo2,
      friMinDegree)
    if (!java.util.Arrays.equals(encodeBinaryData(decoded), bytes))
      binaryReject(0, "decode/re-encode identity failed")
    decoded
  }

  private def validateAndBuildTaps(rows: Array[CircuitTap]): CircuitTapSet = {
    val registers = scala.collection.mutable.ArrayBuffer.empty[TapRegister]
    val comboBacks = new Array[Array[Int]](ComboCount)
    val groupBegin = new Array[Int](GroupCount + 1)
    var expectedGroup = 0
    var expectedOffset = 0
    var index = 0
    while (index < rows.length) {
      val head = rows(index)
      val rowOffset = TapTableOffset + index * 5
      if (expectedGroup >= GroupCount)
        binaryReject(rowOffset, "tap records exceed complete group/offset coverage")
      if (head.group != expectedGroup || head.offset != expectedOffset)
        binaryReject(rowOffset, "registers are not in exact group/offset lexicographic order")
      if (head.combo < 0 || head.combo >= ComboCount)
        binaryReject(rowOffset + 3, "tap combo is out of range")
      if (head.skip <= 0 || index + head.skip > rows.length)
        binaryReject(rowOffset + 4, "tap skip is zero or overruns the table")
      val backs = new Array[Int](head.skip)
      var j = 0
      while (j < head.skip) {
        val row = rows(index + j)
        if (row.group != head.group || row.offset != head.offset ||
            row.combo != head.combo || row.skip != head.skip)
          binaryReject(TapTableOffset + (index + j) * 5,
            "tap run changes group, offset, combo, or skip")
        if (j > 0 && row.back <= backs(j - 1))
          binaryReject(TapTableOffset + (index + j) * 5 + 2,
            "register backs are not unique and strictly increasing")
        backs(j) = row.back
        j += 1
      }
      if (comboBacks(head.combo) == null) comboBacks(head.combo) = backs.clone()
      else if (!java.util.Arrays.equals(comboBacks(head.combo), backs))
        binaryReject(rowOffset, "register back list differs from its combo")
      registers += new TapRegister(head.group, head.offset, head.combo, backs)
      index += head.skip
      expectedOffset += 1
      if (expectedOffset == GroupSizes(expectedGroup)) {
        groupBegin(expectedGroup + 1) = index
        expectedGroup += 1
        expectedOffset = 0
      }
    }
    if (expectedGroup != GroupCount || expectedOffset != 0)
      binaryReject(TapTableOffset, "register coverage is incomplete")
    if (registers.length != RegisterCount)
      binaryReject(TapTableOffset, "expected exactly 163 registers")
    if (!java.util.Arrays.equals(groupBegin, ExpectedGroupBegin))
      binaryReject(TapTableOffset, "derived group boundaries do not match the stock circuit")

    var combo = 0
    var totalBacks = 0
    while (combo < comboBacks.length) {
      if (comboBacks(combo) == null)
        binaryReject(TapTableOffset, "not every combo occurs")
      totalBacks += comboBacks(combo).length
      var other = 0
      while (other < combo) {
        if (java.util.Arrays.equals(comboBacks(other), comboBacks(combo)))
          binaryReject(TapTableOffset, "combo back lists are not pairwise distinct")
        other += 1
      }
      combo += 1
    }
    if (totalBacks != TotalComboBacks)
      binaryReject(TapTableOffset, "concatenated combo backs do not contain exactly 20 values")

    val comboBegin = new Array[Int](ComboCount + 1)
    val comboTaps = new Array[Int](TotalComboBacks)
    var out = 0
    combo = 0
    while (combo < ComboCount) {
      comboBegin(combo) = out
      var j = 0
      while (j < comboBacks(combo).length) {
        comboTaps(out) = comboBacks(combo)(j)
        out += 1
        j += 1
      }
      combo += 1
    }
    comboBegin(ComboCount) = out

    new CircuitTapSet(
      GroupNames.clone(),
      groupBegin,
      GroupSizes.clone(),
      registers.length,
      ComboCount,
      comboBegin,
      comboTaps,
      TotalComboBacks,
      rows.clone(),
      registers.toArray)
  }

  private def requireFpRef(offset: Int, value: Int, count: Int): Unit =
    if (value < 0 || value >= count)
      binaryReject(offset, "PolyExt field operand " + value + " is not earlier than " + count)

  private def requireMixRef(offset: Int, value: Int, count: Int): Unit =
    if (value < 0 || value >= count)
      binaryReject(offset, "PolyExt mix operand " + value + " is not earlier than " + count)

  private def encodeBinaryData(binary: BinaryProfile): Array[Byte] = {
    val output = new Array[Byte](BinaryDataBytes)
    val writer = new Writer(output)
    writer.u32(BabyBear.P.toLong)
    writer.u8(4)
    writer.u32(11)
    writer.u8(27)
    writer.u8(24)
    writer.u8(16)
    writer.u8(8)
    writer.u8(4)
    writer.u8(21)
    writer.u8(7)
    writer.u16(StoredRoundConstants)
    writer.u8(DiagonalConstants)
    writer.u8(binary.queries)
    writer.u8(binary.invRate)
    writer.u8(binary.friFold)
    writer.u8(binary.friFoldPo2)
    writer.u16(binary.friMinDegree)
    writer.u8(binary.outputSize)
    writer.u8(binary.mixSize)
    writer.u8(binary.checkSize)
    writer.u16(TapCount)
    writer.u8(GroupCount)
    var i = 0
    while (i < GroupSizes.length) {
      writer.u8(GroupSizes(i))
      i += 1
    }
    writer.u8(ComboCount)
    writer.u8(TotalComboBacks)
    writer.u16(PolyExtOps)
    writer.u16(PolyExtFpVars)
    writer.u16(PolyExtMixVars)
    writer.u16(PolyExtReturn)
    val remap = Array(0, 2, 1, 3)
    i = 0
    while (i < remap.length) {
      writer.u8(remap(i))
      i += 1
    }
    i = 0
    while (i < GroupCount) {
      writer.u8(i)
      i += 1
    }
    writer.u8(16)
    writer.u8(16)
    writer.u8(5)
    writer.u8(3)
    writer.ascii(binary.proofSystemInfo)
    writer.ascii(binary.circuitInfo)
    i = 0
    while (i < binary.reverseRoots.length) {
      writer.u32(binary.reverseRoots(i).toLong)
      i += 1
    }
    i = 0
    while (i < binary.storedRoundConstants.length) {
      writer.u32(binary.storedRoundConstants(i).toLong)
      i += 1
    }
    i = 0
    while (i < binary.diagonals.length) {
      writer.u32(binary.diagonals(i).toLong)
      i += 1
    }
    i = 0
    while (i < binary.tapRows.length) {
      val row = binary.tapRows(i)
      writer.u8(row.group)
      writer.u8(row.offset)
      writer.u8(row.back)
      writer.u8(row.combo)
      writer.u8(row.skip)
      i += 1
    }
    i = 0
    while (i < binary.ops.length) {
      binary.ops(i) match {
        case PolyExtOp.Const(value) =>
          writer.u8(0); writer.u32(value)
        case PolyExtOp.ConstExt(a, b, c, d) =>
          writer.u8(1); writer.u32(a); writer.u32(b); writer.u32(c); writer.u32(d)
        case PolyExtOp.Get(tap) =>
          writer.u8(2); writer.u16(tap)
        case PolyExtOp.GetGlobal(arg, offset) =>
          writer.u8(3); writer.u16(arg); writer.u16(offset)
        case PolyExtOp.Add(left, right) =>
          writer.u8(4); writer.u16(left); writer.u16(right)
        case PolyExtOp.Sub(left, right) =>
          writer.u8(5); writer.u16(left); writer.u16(right)
        case PolyExtOp.Mul(left, right) =>
          writer.u8(6); writer.u16(left); writer.u16(right)
        case PolyExtOp.True =>
          writer.u8(7)
        case PolyExtOp.AndEqz(chain, inner) =>
          writer.u8(8); writer.u16(chain); writer.u16(inner)
        case PolyExtOp.AndCond(chain, condition, inner) =>
          writer.u8(9); writer.u16(chain); writer.u16(condition); writer.u16(inner)
      }
      i += 1
    }
    if (writer.position != BinaryDataBytes)
      throw new IllegalStateException("internal B2 encoder length " + writer.position)
    output
  }

  /** B2 is authoritative. These checks prove that the compiled arithmetic
    * implementation is a byte-for-byte-compatible optimization of the
    * authenticated B2 values before it can be used.
    */
  private def validateCompiledImplementation(binary: BinaryProfile): Unit = {
    if (FriVerifier.rouRevLength != binary.reverseRoots.length)
      reject(CompiledImplementationMismatch("reverse-roots-length", -1))
    var i = 0
    while (i < binary.reverseRoots.length) {
      if (FriVerifier.RouRev(i) != binary.reverseRoots(i))
        reject(CompiledImplementationMismatch("reverse-root", i))
      val derivedForward = BabyBear.inv(binary.reverseRoots(i))
      if (FriVerifier.rouFwdLength <= i || FriVerifier.RouFwd(i) != derivedForward)
        reject(CompiledImplementationMismatch("forward-root", i))
      i += 1
    }
    if (Poseidon2Constants.roundConstantsLength != binary.expandedRoundConstants.length)
      reject(CompiledImplementationMismatch("poseidon2-round-constants-length", -1))
    i = 0
    while (i < binary.expandedRoundConstants.length) {
      if (Poseidon2Constants.roundConstant(i) != binary.expandedRoundConstants(i))
        reject(CompiledImplementationMismatch("poseidon2-round-constant", i))
      i += 1
    }
    if (Poseidon2Constants.mIntDiagLength != binary.diagonals.length)
      reject(CompiledImplementationMismatch("poseidon2-diagonal-length", -1))
    i = 0
    while (i < binary.diagonals.length) {
      if (Poseidon2Constants.mIntDiag(i) != binary.diagonals(i))
        reject(CompiledImplementationMismatch("poseidon2-diagonal", i))
      i += 1
    }
  }

  // ------------------------------------------------------------------
  // Small total codecs and rejection helpers.
  // ------------------------------------------------------------------

  private final class Cursor(input: Array[Byte], label: String) {
    private var pos = 0
    def position: Int = pos

    def u8(): Int = {
      ensure(1)
      val value = input(pos) & 0xff
      pos += 1
      value
    }

    def u16(): Int = {
      ensure(2)
      val value = (input(pos) & 0xff) | ((input(pos + 1) & 0xff) << 8)
      pos += 2
      value
    }

    def u32(): Long = {
      ensure(4)
      val value = readU32Le(input, pos)
      pos += 4
      value
    }

    def bytes(length: Int): Array[Byte] = {
      ensure(length)
      val result = java.util.Arrays.copyOfRange(input, pos, pos + length)
      pos += length
      result
    }

    def ascii(length: Int): String = {
      ensure(length)
      val builder = new java.lang.StringBuilder(length)
      var i = 0
      while (i < length) {
        val value = input(pos + i) & 0xff
        if (value > 0x7f)
          binaryReject(pos + i, "protocol-info byte is not ASCII")
        builder.append(value.toChar)
        i += 1
      }
      pos += length
      builder.toString
    }

    def fieldElement(name: String): Int = {
      val offset = pos
      val value = u32()
      if (value >= BabyBear.P.toLong)
        binaryReject(offset, name + " is not a canonical BabyBear value")
      value.toInt
    }

    private def ensure(length: Int): Unit = {
      if (length < 0 || pos > input.length - length) {
        if (label == "manifest") manifestReject("layout", "truncated at byte " + pos)
        else binaryReject(pos, "truncated input")
      }
    }
  }

  private final class Writer(output: Array[Byte]) {
    private var pos = 0
    def position: Int = pos
    def u8(value: Int): Unit = {
      output(pos) = value.toByte
      pos += 1
    }
    def u16(value: Int): Unit = {
      writeU16Le(output, pos, value)
      pos += 2
    }
    def u32(value: Long): Unit = {
      writeU32Le(output, pos, value)
      pos += 4
    }
    def bytes(value: Array[Byte]): Unit = pos = copy(value, output, pos)
    def ascii(value: String): Unit = {
      var i = 0
      while (i < value.length) {
        output(pos) = value.charAt(i).toByte
        pos += 1
        i += 1
      }
    }
  }

  private def expectU8(cursor: Cursor, expected: Int, field: String): Int = {
    val offset = cursor.position
    val value = cursor.u8()
    if (value != expected) binaryReject(offset, field + ": expected " + expected + ", got " + value)
    value
  }

  private def expectU16(cursor: Cursor, expected: Int, field: String): Int = {
    val offset = cursor.position
    val value = cursor.u16()
    if (value != expected) binaryReject(offset, field + ": expected " + expected + ", got " + value)
    value
  }

  private def expectU32(cursor: Cursor, expected: Long, field: String): Long = {
    val offset = cursor.position
    val value = cursor.u32()
    if (value != expected) binaryReject(offset, field + ": expected " + expected + ", got " + value)
    value
  }

  private def manifestReject(field: String, detail: String): Nothing =
    reject(ManifestRejected(field, detail))

  private def binaryReject(offset: Int, detail: String): Nothing =
    reject(BinaryDataRejected(offset, detail))

  private def reject(failure: Failure): Nothing = throw new Rejected(failure)

  private final class Rejected(val failure: Failure) extends RuntimeException {
    override def fillInStackTrace(): Throwable = this
  }

  private def constantTimeEqual(left: Array[Byte], right: Array[Byte]): Boolean = {
    if (left == null || right == null || left.length != right.length) return false
    var difference = 0
    var i = 0
    while (i < left.length) {
      difference |= (left(i) ^ right(i)) & 0xff
      i += 1
    }
    difference == 0
  }

  private def ascii(value: String): Array[Byte] = {
    val result = new Array[Byte](value.length)
    var i = 0
    while (i < value.length) {
      val ch = value.charAt(i)
      if (ch > 0x7f) throw new IllegalArgumentException("non-ASCII domain")
      result(i) = ch.toByte
      i += 1
    }
    result
  }

  private def copy(source: Array[Byte], destination: Array[Byte], offset: Int): Int = {
    System.arraycopy(source, 0, destination, offset, source.length)
    offset + source.length
  }

  private def readU32Le(bytes: Array[Byte], offset: Int): Long =
    (bytes(offset) & 0xffL) |
      ((bytes(offset + 1) & 0xffL) << 8) |
      ((bytes(offset + 2) & 0xffL) << 16) |
      ((bytes(offset + 3) & 0xffL) << 24)

  private def writeU16Le(bytes: Array[Byte], offset: Int, value: Int): Unit = {
    bytes(offset) = value.toByte
    bytes(offset + 1) = (value >>> 8).toByte
  }

  private def writeU32Le(bytes: Array[Byte], offset: Int, value: Long): Unit = {
    bytes(offset) = value.toByte
    bytes(offset + 1) = (value >>> 8).toByte
    bytes(offset + 2) = (value >>> 16).toByte
    bytes(offset + 3) = (value >>> 24).toByte
  }
}
