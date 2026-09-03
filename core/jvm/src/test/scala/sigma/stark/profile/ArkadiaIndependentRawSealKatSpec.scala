package sigma.stark.profile

import java.io.ByteArrayOutputStream
import java.security.MessageDigest

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import sigma.stark.profile.Risc0RawSealVerifier.Verified

/** Minimal cross-implementation KAT for the raw-seal consensus boundary.
  *
  * This is a non-B4 interoperability KAT: its independently generated receipt
  * uses a different guest image and journal from the B4 reference statement. Only
  * its raw seal and claim inputs are retained here; no bincode receipt parser or
  * implementation-specific transcript participates in verification.
  */
class ArkadiaIndependentRawSealKatSpec extends AnyFunSuite with Matchers {
  private val FixtureRoot = "/stark-kats/eip0045-arkadia-independent/"
  private val ProfileRoot = "/stark-kats/eip0045-profile-package/"

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

  private def sha256Hex(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(bytes)
      .map(byte => f"${byte & 0xff}%02x").mkString

  private def canonicalChunks(rawSeal: Array[Byte]): Array[Array[Byte]] = {
    val lengths = RawSealV1Decoder.canonicalChunkLengths
    rawSeal.length shouldBe lengths.sum
    val chunks = new Array[Array[Byte]](lengths.length)
    var offset = 0
    var i = 0
    while (i < lengths.length) {
      chunks(i) = java.util.Arrays.copyOfRange(rawSeal, offset, offset + lengths(i))
      offset += lengths(i)
      i += 1
    }
    chunks
  }

  private lazy val loadedProfile = Risc0ProfilePackageLoader.load(
    resourceBytes(ProfileRoot + "manifest.bin"),
    resourceBytes(ProfileRoot + "algorithm.txt"),
    resourceBytes(ProfileRoot + "constants.bin"),
    resourceBytes(ProfileRoot + "profile-id.bin")) match {
    case Right(profile) => profile
    case Left(failure)  => fail("B3-frozen profile package rejected: " + failure)
  }

  test("manifest pins the independent source and every retained byte string") {
    val manifest = new String(resourceBytes(FixtureRoot + "fixture-manifest.json"), "UTF-8")
    manifest should include("a2da7834efd84e0e25433c78584a5872d1fa0458")
    manifest should include("\"rawSealOffset\": 12")
    manifest should include("\"terminalParameter\": 16")

    val expected = Seq(
      ("raw-seal.bin", 222668, "d7bdef7d0b3759a6d8ba43c9b531b017112b07e42af2761fbe654a596d759d79"),
      ("claim-digest.bin", 32, "e8b4b5217ae717e000f8fb3e36a510aeef5ed5c79d04f2c600b74997b5858cfc"),
      ("image-id.bin", 32, "0e3a24e2345c1d8e4c3ef2e769aeb3c15465df56d9355c377c4de54cec97fa69"),
      ("journal.bin", 67, "be21ccbca9266d302b484d9a4dd01247f6d2cba5e4dc30474a7238f2033ba2e8"))

    expected.foreach { case (name, length, digest) =>
      val bytes = resourceBytes(FixtureRoot + name)
      withClue(name + ": ") {
        bytes.length shouldBe length
        sha256Hex(bytes) shouldBe digest
      }
    }
  }

  test("independent journal and image ID derive the independently recorded claim") {
    val claim = Risc0ClaimBuilder.deriveOkClaimDigests(
      resourceBytes(FixtureRoot + "image-id.bin"),
      resourceBytes(FixtureRoot + "journal.bin")) match {
      case Right(value) => value
      case Left(failure) => fail("independent claim inputs rejected: " + failure)
    }

    claim.expectedClaim shouldBe resourceBytes(FixtureRoot + "claim-digest.bin")
  }

  test("current direct verifier accepts the independent real seal as normal lift po2 16") {
    val rawSeal = resourceBytes(FixtureRoot + "raw-seal.bin")
    val claim = resourceBytes(FixtureRoot + "claim-digest.bin")
    loadedProfile.verifier.verify(canonicalChunks(rawSeal), claim) shouldBe
      Right(Verified(Risc0RawSealVerifier.NormalLiftControlKind, 16))
  }
}
