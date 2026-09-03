package sigma.stark.profile

import sigma.BaseTests
import sigma.stark.profile.Risc0ClaimBuilder._

class ErgoStarkStatementSpec extends BaseTests {
  private val SyntheticPayload = ascii("EIP-0045 synthetic payload; non-final")
  private val SyntheticContract = decodeHex(
    "96f2898a7d4a18a164943804351e489c38af0f2f6cd897b31e73437fb26f0e7d")

  test("ErgoStatementV1 matches the current independent Rust KAT byte for byte") {
    val binding = right(build(
      ascending32(0),
      ascending32(32),
      ascending32(64),
      SyntheticContract,
      SyntheticPayload,
      16384))

    binding.statement.length shouldBe 196
    binding.statement.sameElements(decodeHex(
      "4572676f2e566572696679537461726b2e53746174656d656e7401000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f96f2898a7d4a18a164943804351e489c38af0f2f6cd897b31e73437fb26f0e7d250000004549502d303034352073796e746865746963207061796c6f61643b206e6f6e2d66696e616c")) shouldBe true
  }

  test("OK ReceiptClaim chain matches the current independent Rust KAT") {
    val digests = claimRight(deriveOkClaimDigests(
      ascending32(0),
      ascii("EIP-0045 independent claim oracle KAT")))

    hex(digests.journalDigest) shouldBe
      "67261f6e1ed8c95ccb856bab6331a9a3ea4bb16ba53d1993a2840b0a2f5dfb25"
    hex(digests.postDigest) shouldBe
      "a3acc27117418996340b84e5a90f3ef4c49d22c79e44aad822ec9c313e1eb8e2"
    hex(digests.outputDigest) shouldBe
      "63b627d856c66a3a8ed83ba5e1f28b172234e1c7b15d3370977bf157019d38f2"
    hex(digests.expectedClaim) shouldBe
      "14fd85f19032d53ce5ae85f10dab8c6c3f12ae9e11a9c6b778e26cd751170e16"
  }

  test("B3 profile ID and candidate B4 foundation contract produce the independent 191-byte KAT") {
    val binding = right(build(
      decodeHex("b0244dfc267baca974a4caee06120321562784303a8a688976ae56170e4d175b"),
      decodeHex("23c4a123ffb33a1c8db89436fe0e7972bd8e4e289459ee5fd71be5440607d383"),
      decodeHex("9490a07414919c7eca0176d4ff9614523beecc8746ae7ffd4916f29b2edb9fe5"),
      decodeHex("f3582418f41ba6920c83758e56ac4475bf6084a039f91a762388e774258a6c61"),
      Array.tabulate[Byte](32)(_.toByte),
      16384))

    binding.statement.length shouldBe 191
    hex(binding.statement) shouldBe
      "4572676f2e566572696679537461726b2e53746174656d656e7401b0244dfc267baca974a4caee06120321562784303a8a688976ae56170e4d175b23c4a123ffb33a1c8db89436fe0e7972bd8e4e289459ee5fd71be5440607d3839490a07414919c7eca0176d4ff9614523beecc8746ae7ffd4916f29b2edb9fe5f3582418f41ba6920c83758e56ac4475bf6084a039f91a762388e774258a6c6120000000000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
    hex(binding.journalDigest) shouldBe
      "fc8064e54129958bcbdfa292b90251c1536d7cd8ce5e641a326e7cbd78b452ef"
    hex(binding.postDigest) shouldBe
      "a3acc27117418996340b84e5a90f3ef4c49d22c79e44aad822ec9c313e1eb8e2"
    hex(binding.outputDigest) shouldBe
      "d40c2944d972b0f440870aef261eca1ac8b0cd167f940cde7457d790cf1e9624"
    hex(binding.expectedClaim) shouldBe
      "2ea54a883cfb8b2252937074a3b9d453264cf3e87ae6760afc3680c1da6fc92d"
  }

  test("statement boundaries are exactly 159 and 16543 bytes") {
    val empty = right(build(
      new Array[Byte](32),
      new Array[Byte](32),
      new Array[Byte](32),
      new Array[Byte](32),
      Array.empty[Byte],
      16384))
    empty.statement.length shouldBe 159
    empty.statement.slice(155, 159).sameElements(Array[Byte](0, 0, 0, 0)) shouldBe true

    val maximum = right(build(
      new Array[Byte](32),
      new Array[Byte](32),
      new Array[Byte](32),
      new Array[Byte](32),
      new Array[Byte](16384),
      16384))
    maximum.statement.length shouldBe 16543
    maximum.statement.slice(155, 159).sameElements(
      Array[Byte](0x00, 0x40, 0x00, 0x00)) shouldBe true

    MaxApplicationPayloadBytes shouldBe Int.MaxValue - StatementPrefixBytes
    right(build(
      new Array[Byte](32),
      new Array[Byte](32),
      new Array[Byte](32),
      new Array[Byte](32),
      Array.empty[Byte],
      MaxApplicationPayloadBytes)).statement.length shouldBe StatementPrefixBytes
  }

  test("manifest-owned payload maximum is enforced before construction") {
    left(build(
      new Array[Byte](32),
      new Array[Byte](32),
      new Array[Byte](32),
      new Array[Byte](32),
      new Array[Byte](16385),
      16384)) shouldBe ApplicationPayloadTooLarge(16385, 16384)

    left(build(
      new Array[Byte](32),
      new Array[Byte](32),
      new Array[Byte](32),
      new Array[Byte](32),
      Array.empty[Byte],
      -1)) shouldBe InvalidPayloadMaximum(-1)

    left(build(
      new Array[Byte](32),
      new Array[Byte](32),
      new Array[Byte](32),
      new Array[Byte](32),
      Array.empty[Byte],
      MaxApplicationPayloadBytes + 1)) shouldBe
      InvalidPayloadMaximum(MaxApplicationPayloadBytes + 1)

    left(build(
      new Array[Byte](32),
      new Array[Byte](32),
      new Array[Byte](32),
      new Array[Byte](32),
      Array[Byte](1),
      0)) shouldBe ApplicationPayloadTooLarge(1, 0)
  }

  test("digest identities, version, payload length, and EOF occupy fixed positions") {
    val payload = Array.tabulate[Byte](258)(i => (i * 3).toByte)
    val chain = ascending32(0)
    val profile = ascending32(32)
    val program = ascending32(64)
    val contract = ascending32(96)
    val statement = right(build(chain, profile, program, contract, payload, 16384)).statement

    statement.slice(0, 26).sameElements(ascii("Ergo.VerifyStark.Statement")) shouldBe true
    statement(26) shouldBe 1.toByte
    statement.slice(27, 59).sameElements(chain) shouldBe true
    statement.slice(59, 91).sameElements(profile) shouldBe true
    statement.slice(91, 123).sameElements(program) shouldBe true
    statement.slice(123, 155).sameElements(contract) shouldBe true
    statement.slice(155, 159).sameElements(Array[Byte](0x02, 0x01, 0x00, 0x00)) shouldBe true
    statement.drop(159).sameElements(payload) shouldBe true
    statement.length shouldBe 159 + payload.length
  }

  test("caller inputs and returned arrays cannot mutate the owned binding") {
    val chain = ascending32(0)
    val profile = ascending32(32)
    val program = ascending32(64)
    val contract = ascending32(96)
    val payload = ascii("snapshot")
    val binding = right(build(chain, profile, program, contract, payload, 16384))
    val statement = binding.statement
    val claim = binding.expectedClaim

    chain(0) = 99
    profile(0) = 99
    program(0) = 99
    contract(0) = 99
    payload(0) = 99
    statement(0) = 99
    claim(0) = 99

    binding.statement.sameElements(statement) shouldBe false
    binding.expectedClaim.sameElements(claim) shouldBe false
    binding.statement(0) shouldBe 'E'.toByte
    hex(binding.expectedClaim) shouldBe
      "6a9ba93030eec7fb086242c020a90b8395ae259118dea85a905823e451740aff"
  }

  test("mutating any bound identity or the payload changes the expected claim") {
    val base = Array(
      ascending32(0),
      ascending32(32),
      ascending32(64),
      ascending32(96),
      ascii("bound-payload"))
    val baseline = right(build(base(0), base(1), base(2), base(3), base(4), 16384))

    var field = 0
    while (field < base.length) {
      val changed = base.map(_.clone())
      changed(field)(0) = (changed(field)(0) ^ 1).toByte
      val candidate = right(build(
        changed(0), changed(1), changed(2), changed(3), changed(4), 16384))
      candidate.statement.sameElements(baseline.statement) shouldBe false
      candidate.expectedClaim.sameElements(baseline.expectedClaim) shouldBe false
      field += 1
    }
  }

  test("appending a u16le data count produces the known noncanonical digest") {
    val tagDigest = ProfileSha256.hash(ascii("eip0045.test.Tag"))
    val child = ascending32(0)
    val wrong = new Array[Byte](32 + 32 + 8 + 2 + 2)
    var offset = copy(tagDigest, wrong, 0)
    offset = copy(child, wrong, offset)
    putU32Le(wrong, offset, 0x01020304)
    offset += 4
    putU32Le(wrong, offset, 0xa1b2c3d4)
    offset += 4
    putU16Le(wrong, offset, 1)
    offset += 2
    // Non-canonical suffix: m=2. Canonical RISC0 encodes only n=1 above.
    putU16Le(wrong, offset, 2)

    hex(ProfileSha256.hash(wrong)) shouldBe
      "d4b37c48388216599a16f5db0dd6fa24db49f64c334bb4a5812e1b03ba72bb9f"
    hex(ProfileSha256.hash(wrong)) should not be
      "cee52364c4b492833a14d1258fac5bcd6747dae84658dac401de1385b5ad6a9b"
  }

  test("nulls and every non-32-byte digest are typed host failures") {
    val digest = new Array[Byte](32)
    left(build(null, digest, digest, digest, Array.empty[Byte], 0)) shouldBe
      NullInput("chain-domain-id")
    left(build(digest, null, digest, digest, Array.empty[Byte], 0)) shouldBe
      NullInput("authenticated-profile-id")
    left(build(digest, digest, null, digest, Array.empty[Byte], 0)) shouldBe
      NullInput("program-id")
    left(build(digest, digest, digest, null, Array.empty[Byte], 0)) shouldBe
      NullInput("contract-id")
    left(build(digest, digest, digest, digest, null, 0)) shouldBe
      NullInput("application-payload")

    Seq(31, 33).foreach { length =>
      left(build(new Array[Byte](length), digest, digest, digest, Array.empty[Byte], 0)) shouldBe
        WrongDigestLength("chain-domain-id", 32, length)
      left(build(digest, new Array[Byte](length), digest, digest, Array.empty[Byte], 0)) shouldBe
        WrongDigestLength("authenticated-profile-id", 32, length)
      left(build(digest, digest, new Array[Byte](length), digest, Array.empty[Byte], 0)) shouldBe
        WrongDigestLength("program-id", 32, length)
      left(build(digest, digest, digest, new Array[Byte](length), Array.empty[Byte], 0)) shouldBe
        WrongDigestLength("contract-id", 32, length)
    }
  }

  private def right(result: Either[Failure, Binding]): Binding = result match {
    case Right(value) => value
    case Left(error)  => fail("expected successful host binding, got " + error)
  }

  private def claimRight(result: Either[Failure, ClaimDigests]): ClaimDigests = result match {
    case Right(value) => value
    case Left(error)  => fail("expected successful claim derivation, got " + error)
  }

  private def left(result: Either[Failure, Binding]): Failure = result match {
    case Left(error) => error
    case Right(_)    => fail("expected a typed host failure")
  }

  private def ascending32(start: Int): Array[Byte] = {
    val output = new Array[Byte](32)
    var i = 0
    while (i < output.length) {
      output(i) = (start + i).toByte
      i += 1
    }
    output
  }

  private def ascii(value: String): Array[Byte] = {
    val output = new Array[Byte](value.length)
    var i = 0
    while (i < value.length) {
      output(i) = value.charAt(i).toByte
      i += 1
    }
    output
  }

  private def decodeHex(value: String): Array[Byte] = {
    val output = new Array[Byte](value.length / 2)
    var i = 0
    while (i < output.length) {
      output(i) = ((nibble(value.charAt(i * 2)) << 4) |
        nibble(value.charAt(i * 2 + 1))).toByte
      i += 1
    }
    output
  }

  private def nibble(char: Char): Int =
    if (char >= '0' && char <= '9') char - '0'
    else if (char >= 'a' && char <= 'f') char - 'a' + 10
    else throw new IllegalArgumentException("invalid test hex")

  private def hex(bytes: Array[Byte]): String = {
    val digits = "0123456789abcdef"
    val builder = new StringBuilder(bytes.length * 2)
    var i = 0
    while (i < bytes.length) {
      val value = bytes(i) & 0xff
      builder.append(digits.charAt(value >>> 4))
      builder.append(digits.charAt(value & 0x0f))
      i += 1
    }
    builder.toString()
  }

  private def copy(source: Array[Byte], target: Array[Byte], offset: Int): Int = {
    var i = 0
    while (i < source.length) {
      target(offset + i) = source(i)
      i += 1
    }
    offset + source.length
  }

  private def putU32Le(output: Array[Byte], offset: Int, value: Int): Unit = {
    output(offset) = value.toByte
    output(offset + 1) = (value >>> 8).toByte
    output(offset + 2) = (value >>> 16).toByte
    output(offset + 3) = (value >>> 24).toByte
  }

  private def putU16Le(output: Array[Byte], offset: Int, value: Int): Unit = {
    output(offset) = value.toByte
    output(offset + 1) = (value >>> 8).toByte
  }
}
