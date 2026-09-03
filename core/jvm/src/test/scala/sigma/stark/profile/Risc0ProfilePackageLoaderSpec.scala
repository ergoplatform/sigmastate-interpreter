/*
 * SPDX-License-Identifier: MIT AND Apache-2.0
 *
 * Copyright 2026 A. Shannon.
 */
package sigma.stark.profile

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import sigma.stark.profile.Risc0ProfilePackageLoader._
import sigma.stark.profile.Risc0RawSealVerifier.Verified

import scala.io.Source

class Risc0ProfilePackageLoaderSpec extends AnyFunSuite with Matchers {
  private val PackageRoot = "/stark-kats/eip0045-profile-package/"
  private val DirectRoot = "/stark-kats/eip0045-direct/"
  private val ChunkLengths = Array(65535, 65535, 65535, 26063)

  private def resourceBytes(path: String): Array[Byte] = {
    val in = getClass.getResourceAsStream(path)
    require(in != null, "missing test resource " + path)
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
    require(in != null, "missing test resource " + path)
    try Source.fromInputStream(in, "UTF-8").getLines().toArray
    finally in.close()
  }

  private def hex(value: String): Array[Byte] =
    value.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray

  private def hex(bytes: Array[Byte]): String =
    bytes.map(b => f"${b & 0xff}%02x").mkString

  private def sha256Hex(bytes: Array[Byte]): String =
    hex(MessageDigest.getInstance("SHA-256").digest(bytes))

  private lazy val algorithm = resourceBytes(PackageRoot + "algorithm.txt")
  private lazy val constants = resourceBytes(PackageRoot + "constants.bin")
  private lazy val finalManifest = resourceBytes(PackageRoot + "manifest.bin")
  private lazy val finalProfileId = resourceBytes(PackageRoot + "profile-id.bin")

  private lazy val oracleLines = resourceLines(DirectRoot + "profile-oracle.tsv")
    .filter(line => line.nonEmpty && !line.startsWith("#"))

  private lazy val innerControlRoot: Array[Byte] = oracleLines
    .find(_.startsWith("inner_control_root\t"))
    .map(_.split("\t", -1)(1)).map(hex)
    .getOrElse(fail("profile oracle has no inner control root"))

  private lazy val controls: Array[(Int, Int, Array[Byte])] = oracleLines
    .filter(_.startsWith("control\t"))
    .map(_.split("\t", -1))
    .map(fields => (fields(1).toInt, fields(2).toInt, hex(fields(3))))

  private def writeU16(bytes: Array[Byte], offset: Int, value: Int): Unit = {
    bytes(offset) = value.toByte
    bytes(offset + 1) = (value >>> 8).toByte
  }

  private def writeU32(bytes: Array[Byte], offset: Int, value: Long): Unit = {
    bytes(offset) = value.toByte
    bytes(offset + 1) = (value >>> 8).toByte
    bytes(offset + 2) = (value >>> 16).toByte
    bytes(offset + 3) = (value >>> 24).toByte
  }

  private def buildManifest(
      algorithmBytes: Array[Byte],
      binaryBytes: Array[Byte]): Array[Byte] = {
    val manifest = new Array[Byte](ManifestBytes)
    manifest(0) = 1
    writeU32(manifest, 1, RawSealV1Decoder.ByteCount)
    writeU32(manifest, 5, 16384)
    manifest(9) = RawSealV1Decoder.ExpectedOuterPo2.toByte
    System.arraycopy(innerControlRoot, 0, manifest, 10, 32)
    var offset = 42
    controls.foreach { case (kind, parameter, controlId) =>
      manifest(offset) = kind.toByte
      manifest(offset + 1) = parameter.toByte
      System.arraycopy(controlId, 0, manifest, offset + 2, 32)
      offset += 34
    }
    writeU16(manifest, 382, 1)
    writeU32(manifest, 384, algorithmBytes.length)
    System.arraycopy(artifactDigest(1, algorithmBytes), 0, manifest, 388, 32)
    writeU16(manifest, 420, 2)
    writeU32(manifest, 422, binaryBytes.length)
    System.arraycopy(artifactDigest(2, binaryBytes), 0, manifest, 426, 32)
    manifest
  }

  private def load(
      manifest: Array[Byte],
      algorithmBytes: Array[Byte] = algorithm,
      binaryBytes: Array[Byte] = constants,
      expectedProfileId: Array[Byte] = null): Either[Failure, LoadedProfile] = {
    val expected = if (expectedProfileId == null) profileIdDigest(manifest) else expectedProfileId
    Risc0ProfilePackageLoader.load(manifest, algorithmBytes, binaryBytes, expected)
  }

  private def canonicalChunks(bytes: Array[Byte]): Array[Array[Byte]] = {
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

  test("BLAKE2b-256 and the kind-1 abc artifact envelope match independent KATs") {
    val abc = "abc".getBytes(StandardCharsets.US_ASCII)
    hex(ProfileBlake2b256.hash(abc)) shouldBe
      "bddd813c634239723171ef3fee98579b94964e3bb1cb3e427262c8c068d52319"
    hex(artifactDigest(1, abc)) shouldBe
      "a16874031c15d7bf7f5a3fbe5a14e1a34bb4d57b02d33344ab5e9d9caa7d024c"
    hex(profileIdDigest(new Array[Byte](ManifestBytes))) shouldBe
      "24537f0d2c05444666acfee843fe041aae1136e0b97e039863bd26f27d042ee9"
  }

  test("authenticated B1/B2 fixtures construct a verifier which accepts the real seal") {
    algorithm.length shouldBe 29773
    constants.length shouldBe BinaryDataBytes
    val manifest = buildManifest(algorithm, constants)
    val loaded = load(manifest) match {
      case Right(value) => value
      case Left(reason) => fail("valid profile package rejected: " + reason)
    }
    loaded.exactProofBytes shouldBe RawSealV1Decoder.ByteCount
    loaded.maxApplicationPayloadBytes shouldBe 16384
    loaded.outerPo2 shouldBe RawSealV1Decoder.ExpectedOuterPo2
    loaded.profileId.toSeq shouldBe profileIdDigest(manifest).toSeq

    val seal = resourceBytes(DirectRoot + "po2-15-raw-seal.bin")
    val claim = resourceBytes(DirectRoot + "po2-15-claim-digest.bin")
    loaded.verifier.verify(canonicalChunks(seal), claim) shouldBe Right(Verified(1, 15))
  }

  test("frozen B3 manifest and profile ID match the independent fixture bytes") {
    finalManifest.length shouldBe ManifestBytes
    finalProfileId.length shouldBe ProfileIdBytes
    finalManifest.toSeq shouldBe buildManifest(algorithm, constants).toSeq
    hex(artifactDigest(1, algorithm)) shouldBe
      "6ed8a807a7b55177fa664de51c1d6f0daad81daf879e651da32367fed9d171c4"
    hex(artifactDigest(2, constants)) shouldBe
      "dd8528a8621edc8dd24aadeed7bd7a2f0c1afd88dd563c5ec8f51cc7f75df0b1"
    hex(finalProfileId) shouldBe
      "23c4a123ffb33a1c8db89436fe0e7972bd8e4e289459ee5fd71be5440607d383"
    sha256Hex(algorithm) shouldBe
      "90a884da420a09f2c1108d7388c2ac74db8dbdb195de704206e2bf8ec1ad0bee"
    sha256Hex(constants) shouldBe
      "8c4a92b7d354890481eefdef233d4ca43f6bcd9f7cb00e4dd9e709da47789ef3"
    sha256Hex(finalManifest) shouldBe
      "deffb2cb231f98a348cbd166d5f1c43315661ccd8bd212099f16f238d0fe8946"
    sha256Hex(finalProfileId) shouldBe
      "aa144c74a0cb52b3c5a9827f10a264f320820190da14a9bf82dcf3466f41aae1"
    profileIdDigest(finalManifest).toSeq shouldBe finalProfileId.toSeq

    Risc0ProfilePackageLoader.load(
      finalManifest,
      algorithm,
      constants,
      finalProfileId) match {
      case Right(loaded) => loaded.profileId.toSeq shouldBe finalProfileId.toSeq
      case Left(reason)  => fail("frozen B3 package rejected: " + reason)
    }
  }

  test("profileId is independently authenticated before either artifact") {
    val manifest = buildManifest(algorithm, constants)
    val wrong = profileIdDigest(manifest)
    wrong(0) = (wrong(0) ^ 1).toByte
    load(manifest, expectedProfileId = wrong) shouldBe Left(ProfileIdMismatch)
  }

  test("one malformed manifest field is rejected even under its recomputed profileId") {
    val wrongVersion = buildManifest(algorithm, constants)
    wrongVersion(0) = 2
    load(wrongVersion) match {
      case Left(ManifestRejected("formatVersion", _)) => succeed
      case other => fail("unexpected result: " + other)
    }

    val duplicateControl = buildManifest(algorithm, constants)
    System.arraycopy(duplicateControl, 44, duplicateControl, 78, 32)
    load(duplicateControl) match {
      case Left(ManifestRejected("controls[1].controlId", _)) => succeed
      case other => fail("unexpected result: " + other)
    }

    val wrongLiftKind = buildManifest(algorithm, constants)
    wrongLiftKind(42) = 2
    load(wrongLiftKind) match {
      case Left(ManifestRejected("controls[0].kind", _)) => succeed
      case other => fail("unexpected result: " + other)
    }

    val wrongJoinParameter = buildManifest(algorithm, constants)
    wrongJoinParameter(42 + 8 * 34 + 1) = 1
    load(wrongJoinParameter) match {
      case Left(ManifestRejected("controls[8].parameter", _)) => succeed
      case other => fail("unexpected result: " + other)
    }

    val wrongResolveKind = buildManifest(algorithm, constants)
    wrongResolveKind(42 + 9 * 34) = 2
    load(wrongResolveKind) match {
      case Left(ManifestRejected("controls[9].kind", _)) => succeed
      case other => fail("unexpected result: " + other)
    }

    val wrongArtifactKind = buildManifest(algorithm, constants)
    writeU16(wrongArtifactKind, 382, 2)
    load(wrongArtifactKind) match {
      case Left(ManifestRejected("algorithm.kind", _)) => succeed
      case other => fail("unexpected result: " + other)
    }
  }

  test("the superseded 382-byte candidate cannot enter the Manifest V1 decoder") {
    val short = java.util.Arrays.copyOf(buildManifest(algorithm, constants), 382)
    load(short, expectedProfileId = new Array[Byte](ProfileIdBytes)) shouldBe
      Left(WrongInputLength("manifest", ManifestBytes, 382))
  }

  test("artifact length and digest mismatches are isolated after manifest authentication") {
    val manifest = buildManifest(algorithm, constants)
    val longerAlgorithm = algorithm ++ Array('\n'.toByte)
    load(manifest, algorithmBytes = longerAlgorithm) shouldBe
      Left(ArtifactLengthMismatch(1, algorithm.length.toLong, longerAlgorithm.length))

    val changedAlgorithm = algorithm.clone()
    changedAlgorithm(0) = (if (changedAlgorithm(0) == 'A') 'B' else 'A').toByte
    load(manifest, algorithmBytes = changedAlgorithm) shouldBe Left(ArtifactDigestMismatch(1))

    val changedBinary = constants.clone()
    changedBinary(0) = (changedBinary(0) ^ 1).toByte
    load(manifest, binaryBytes = changedBinary) shouldBe Left(ArtifactDigestMismatch(2))
  }

  test("authenticated algorithm bytes must remain bounded canonical ASCII text") {
    val changedAlgorithm = algorithm.clone()
    changedAlgorithm(10) = 0
    val manifest = buildManifest(changedAlgorithm, constants)
    load(manifest, algorithmBytes = changedAlgorithm) shouldBe
      Left(AlgorithmEncodingRejected(10, 0))
  }

  test("authenticated B1 without its one required terminal LF is rejected") {
    val missingTerminalLf = algorithm.dropRight(1)
    val manifest = buildManifest(missingTerminalLf, constants)
    val finalOffset = missingTerminalLf.length - 1

    load(manifest, algorithmBytes = missingTerminalLf) shouldBe
      Left(AlgorithmEncodingRejected(finalOffset, missingTerminalLf(finalOffset) & 0xff))
  }

  test("authenticated B1 with two trailing LF bytes is rejected") {
    val duplicateTerminalLf = algorithm ++ Array(0x0a.toByte)
    val manifest = buildManifest(duplicateTerminalLf, constants)

    load(manifest, algorithmBytes = duplicateTerminalLf) shouldBe
      Left(AlgorithmEncodingRejected(duplicateTerminalLf.length - 2, 0x0a))
  }

  test("authenticated B1 may retain internal blank lines") {
    val firstLf = algorithm.indexOf(0x0a.toByte)
    firstLf should be >= 0
    val internalBlankLine =
      algorithm.take(firstLf + 1) ++ Array(0x0a.toByte) ++ algorithm.drop(firstLf + 1)
    val manifest = buildManifest(internalBlankLine, constants)

    load(manifest, algorithmBytes = internalBlankLine).isRight shouldBe true
  }

  test("one authenticated B2 header field mutation reaches the strict decoder") {
    val changed = constants.clone()
    changed(0) = (changed(0) ^ 1).toByte
    val manifest = buildManifest(algorithm, changed)
    load(manifest, binaryBytes = changed) match {
      case Left(BinaryDataRejected(0, detail)) => detail should include("BabyBear modulus")
      case other => fail("unexpected result: " + other)
    }
  }

  test("one authenticated B2 tap-run mutation is rejected at its exact byte") {
    val changed = constants.clone()
    changed(1147 + 4) = 0
    val manifest = buildManifest(algorithm, changed)
    load(manifest, binaryBytes = changed) match {
      case Left(BinaryDataRejected(1151, detail)) => detail should include("skip")
      case other => fail("unexpected result: " + other)
    }
  }

  test("one authenticated B2 PolyExt-tag mutation is rejected at stream start") {
    val changed = constants.clone()
    changed(4362) = 10
    val manifest = buildManifest(algorithm, changed)
    load(manifest, binaryBytes = changed) match {
      case Left(BinaryDataRejected(4362, detail)) => detail should include("unknown PolyExt tag")
      case other => fail("unexpected result: " + other)
    }
  }

  test("authenticated B2 is authority and a compiled numeric mismatch blocks startup") {
    val changed = constants.clone()
    val root = (changed(87) & 0xff) |
      ((changed(88) & 0xff) << 8) |
      ((changed(89) & 0xff) << 16) |
      ((changed(90) & 0xff) << 24)
    writeU32(changed, 87, (root.toLong & 0xffffffffL) ^ 1L)
    val manifest = buildManifest(algorithm, changed)
    load(manifest, binaryBytes = changed) shouldBe
      Left(CompiledImplementationMismatch("reverse-root", 0))
  }

  test("caller mutation after load cannot alter the loaded identity or verifier profile") {
    val algorithmInput = algorithm.clone()
    val binaryInput = constants.clone()
    val manifestInput = buildManifest(algorithmInput, binaryInput)
    val expectedInput = profileIdDigest(manifestInput)
    val expectedSnapshot = expectedInput.clone()
    val loaded = Risc0ProfilePackageLoader.load(
      manifestInput,
      algorithmInput,
      binaryInput,
      expectedInput) match {
      case Right(value) => value
      case Left(reason) => fail("valid profile package rejected: " + reason)
    }
    java.util.Arrays.fill(manifestInput, 0.toByte)
    java.util.Arrays.fill(algorithmInput, 0.toByte)
    java.util.Arrays.fill(binaryInput, 0.toByte)
    java.util.Arrays.fill(expectedInput, 0.toByte)
    loaded.profileId.toSeq shouldBe expectedSnapshot.toSeq

    val exposed = loaded.profileId
    exposed(0) = (exposed(0) ^ 1).toByte
    loaded.profileId.toSeq shouldBe expectedSnapshot.toSeq
  }
}
