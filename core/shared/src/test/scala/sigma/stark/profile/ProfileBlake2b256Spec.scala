/*
 * SPDX-License-Identifier: MIT AND Apache-2.0
 *
 * Copyright 2026 A. Shannon.
 */
package sigma.stark.profile

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Cross-platform KATs around every BLAKE2b block-finalization boundary used
  * by the profile-package authenticator.
  */
class ProfileBlake2b256Spec extends AnyFunSuite with Matchers {
  private val vectors = Seq(
    0 -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
    1 -> "03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314",
    127 -> "f2fe67ff342e21b8f45e8f2e0bcd1d9243245d50ee6c78042e9c491388791c72",
    128 -> "c3582f71ebb2be66fa5dd750f80baae97554f3b015663c8be377cfcb2488c1d1",
    129 -> "f7f3c46ba2564ff4c4c162da1f5b605f9f1c4aa6a20652a9f9a337c1a2f5b9c9",
    255 -> "1d0850ee9bca0abc9601e9deabe1418fedec2fb6ac4150bd5302d2430f9be943",
    256 -> "39a7eb9fedc19aabc83425c6755dd90e6f9d0c804964a1f4aaeea3b9fb599835")

  private def input(length: Int): Array[Byte] =
    Array.tabulate(length)(i => (i & 0xff).toByte)

  private def decodeHex(value: String): Array[Byte] =
    value.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray

  private def hex(bytes: Array[Byte]): String = {
    val digits = "0123456789abcdef"
    val builder = new java.lang.StringBuilder(bytes.length * 2)
    var i = 0
    while (i < bytes.length) {
      val value = bytes(i) & 0xff
      builder.append(digits.charAt(value >>> 4))
      builder.append(digits.charAt(value & 0x0f))
      i += 1
    }
    builder.toString
  }

  vectors.foreach { case (length, expected) =>
    test("BLAKE2b-256 KAT at input length " + length) {
      val bytes = input(length)
      val snapshot = bytes.clone()
      val digest = ProfileBlake2b256.hash(bytes)
      hex(digest) shouldBe expected
      bytes.toSeq shouldBe snapshot.toSeq

      digest(0) = (digest(0) ^ 1).toByte
      hex(ProfileBlake2b256.hash(bytes)) shouldBe expected
      bytes.toSeq shouldBe snapshot.toSeq
    }
  }

  test("candidate B4 foundation proposition matches the contract generator's Scorex oracle") {
    val proposition = decodeHex(
      "1c53020e209490a07414919c7eca0176d4ff9614523beecc8746ae7ffd4916f29b2edb9fe5" +
        "0e2023c4a123ffb33a1c8db89436fe0e7972bd8e4e289459ee5fd71be5440607d383" +
        "d1b9e4e3001ae4e3010e73007301")

    proposition.length shouldBe 85
    hex(ProfileBlake2b256.hash(proposition)) shouldBe
      "f3582418f41ba6920c83758e56ac4475bf6084a039f91a762388e774258a6c61"
  }
}
