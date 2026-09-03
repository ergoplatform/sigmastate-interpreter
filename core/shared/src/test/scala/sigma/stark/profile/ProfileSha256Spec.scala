package sigma.stark.profile

import sigma.BaseTests

class ProfileSha256Spec extends BaseTests {
  test("FIPS and standard one-shot SHA-256 KATs") {
    hex(ProfileSha256.hash(Array.empty[Byte])) shouldBe
      "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    hex(ProfileSha256.hash(ascii("abc"))) shouldBe
      "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    hex(ProfileSha256.hash(ascii(
      "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))) shouldBe
      "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
  }

  test("padding boundaries 55, 56, 63, 64, and 65 bytes match independent KATs") {
    val expected = Seq(
      55 -> "463eb28e72f82e0a96c0a4cc53690c571281131f672aa229e0d45ae59b598b59",
      56 -> "da2ae4d6b36748f2a318f23e7ab1dfdf45acdc9d049bd80e59de82a60895f562",
      63 -> "29af2686fd53374a36b0846694cc342177e428d1647515f078784d69cdb9e488",
      64 -> "fdeab9acf3710362bd2658cdc9a29e8f9c757fcf9811603a8c447cd1d9151108",
      65 -> "4bfd2c8b6f1eec7a2afeb48b934ee4b2694182027e6d0fc075074f2fabb31781")

    expected.foreach { case (length, digest) =>
      val input = new Array[Byte](length)
      var i = 0
      while (i < input.length) {
        input(i) = i.toByte
        i += 1
      }
      hex(ProfileSha256.hash(input)) shouldBe digest
    }
  }

  test("hashing never mutates input and every result is independently owned") {
    val input = Array.tabulate[Byte](97)(i => (i * 17).toByte)
    val before = input.clone()
    val first = ProfileSha256.hash(input)
    input.sameElements(before) shouldBe true

    first(0) = (first(0) ^ 0xff).toByte
    val second = ProfileSha256.hash(input)
    hex(second) shouldBe "1be6fe91e250b16718b8597254c7d4f3f54eb3e4ce6fbc218e0a945d473b65ad"
    first.sameElements(second) shouldBe false
    input.sameElements(before) shouldBe true
  }

  private def ascii(value: String): Array[Byte] = {
    val bytes = new Array[Byte](value.length)
    var i = 0
    while (i < value.length) {
      bytes(i) = value.charAt(i).toByte
      i += 1
    }
    bytes
  }

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
}
