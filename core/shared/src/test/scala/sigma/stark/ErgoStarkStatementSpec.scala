package sigma.stark

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ErgoStarkStatementSpec extends AnyFunSuite with Matchers {

  private def digest(seed: Int): Array[Byte] =
    Array.tabulate[Byte](ErgoStarkStatement.DigestBytes)(i => ((seed + i) & 0xff).toByte)

  test("builds exact 159-byte prefix + little-endian payload length + payload") {
    val chain = digest(0x10)
    val profile = digest(0x30)
    val program = digest(0x50)
    val contract = digest(0x70)
    val payload = Array[Byte](0xde.toByte, 0xad.toByte, 0xbe.toByte, 0xef.toByte, 0x42)

    val Right(statement) = ErgoStarkStatement.build(chain, profile, program, contract, payload)
    statement.length shouldBe ErgoStarkStatement.StatementPrefixBytes + payload.length

    new String(statement.slice(0, 26), "US-ASCII") shouldBe "Ergo.VerifyStark.Statement"
    statement(26) shouldBe 0x01.toByte
    statement.slice(27, 59) shouldBe chain
    statement.slice(59, 91) shouldBe profile
    statement.slice(91, 123) shouldBe program
    statement.slice(123, 155) shouldBe contract
    statement.slice(155, 159) shouldBe Array[Byte](5, 0, 0, 0)
    statement.slice(159, 164) shouldBe payload
  }

  test("rejects null inputs, wrong digest lengths, and oversized payloads") {
    val d = digest(1)
    val empty = Array.emptyByteArray

    ErgoStarkStatement.build(null, d, d, d, empty) shouldBe Left(ErgoStarkStatement.NullInput("chain-domain-id"))
    ErgoStarkStatement.build(d, null, d, d, empty) shouldBe Left(ErgoStarkStatement.NullInput("profile-id"))
    ErgoStarkStatement.build(d, d, null, d, empty) shouldBe Left(ErgoStarkStatement.NullInput("program-id"))
    ErgoStarkStatement.build(d, d, d, null, empty) shouldBe Left(ErgoStarkStatement.NullInput("contract-id"))
    ErgoStarkStatement.build(d, d, d, d, null) shouldBe Left(ErgoStarkStatement.NullInput("application-payload"))

    ErgoStarkStatement.build(new Array[Byte](31), d, d, d, empty) shouldBe
      Left(ErgoStarkStatement.WrongDigestLength("chain-domain-id", 32, 31))
    ErgoStarkStatement.build(d, d, d, d, new Array[Byte](17), maxApplicationPayloadBytes = 16) shouldBe
      Left(ErgoStarkStatement.ApplicationPayloadTooLarge(17, 16))
    ErgoStarkStatement.build(d, d, d, d, empty, maxApplicationPayloadBytes = -1) shouldBe
      Left(ErgoStarkStatement.InvalidPayloadMaximum(-1))
  }
}
