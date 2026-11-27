/*
 * SPDX-License-Identifier: MIT
 *
 * Copyright 2026 A. Shannon.
 */
package sigma.stark

/** Profile-agnostic host construction of EIP-0045 `ErgoStatementV1`.
  *
  * Layout (exact bytes, strict EOF):
  * {{{
  *   ASCII("Ergo.VerifyStark.Statement")  // 26 bytes
  *   0x01                                 //  1 byte  (version)
  *   chainDomainId                        // 32 bytes (height-1 genesis Header.id)
  *   profileId                            // 32 bytes (immutable profile digest)
  *   programId                            // 32 bytes (guest program digest)
  *   contractId                           // 32 bytes (BLAKE2b-256(SELF.propositionBytes))
  *   u32le(applicationPayload.length)     //  4 bytes
  *   applicationPayload                   //  n bytes
  * }}}
  *
  * Every compiled `StarkProfileRuntime` receives the exact bytes
  * encoded by this builder so no profile can omit network, profile, program, or
  * contract domain separation.
  */
object ErgoStarkStatement {
  final val DigestBytes: Int = 32
  final val StatementVersion: Byte = 0x01
  final val StatementPrefixBytes: Int = 159

  /** Largest payload whose complete V1 statement length fits a signed 32-bit Int. */
  final val MaxApplicationPayloadBytes: Int = Int.MaxValue - StatementPrefixBytes

  private val StatementDomain: Array[Byte] = ascii("Ergo.VerifyStark.Statement")

  /** Stable rejection taxonomy for host-supplied statement inputs. */
  sealed trait Failure extends Product with Serializable {
    def code: String
  }

  final case class NullInput(name: String) extends Failure {
    override val code: String = "stark-statement-null-input"
  }

  final case class WrongDigestLength(name: String, expected: Int, actual: Int) extends Failure {
    override val code: String = "stark-statement-wrong-digest-length"
  }

  final case class InvalidPayloadMaximum(actual: Int) extends Failure {
    override val code: String = "stark-statement-invalid-payload-maximum"
  }

  final case class ApplicationPayloadTooLarge(actual: Int, maximum: Int) extends Failure {
    override val code: String = "stark-statement-application-payload-too-large"
  }

  /** Build canonical `ErgoStatementV1` bytes from validated host values. */
  def build(
      chainDomainId: Array[Byte],
      profileId: Array[Byte],
      programId: Array[Byte],
      contractId: Array[Byte],
      applicationPayload: Array[Byte],
      maxApplicationPayloadBytes: Int = MaxApplicationPayloadBytes): Either[Failure, Array[Byte]] = {
    validateDigest("chain-domain-id", chainDomainId) match {
      case Some(failure) => return Left(failure)
      case None          => ()
    }
    validateDigest("profile-id", profileId) match {
      case Some(failure) => return Left(failure)
      case None          => ()
    }
    validateDigest("program-id", programId) match {
      case Some(failure) => return Left(failure)
      case None          => ()
    }
    validateDigest("contract-id", contractId) match {
      case Some(failure) => return Left(failure)
      case None          => ()
    }
    if (applicationPayload == null)
      return Left(NullInput("application-payload"))
    if (maxApplicationPayloadBytes < 0 || maxApplicationPayloadBytes > MaxApplicationPayloadBytes)
      return Left(InvalidPayloadMaximum(maxApplicationPayloadBytes))
    if (applicationPayload.length > maxApplicationPayloadBytes)
      return Left(ApplicationPayloadTooLarge(applicationPayload.length, maxApplicationPayloadBytes))

    val statement = new Array[Byte](StatementPrefixBytes + applicationPayload.length)
    var offset = 0
    offset = copy(StatementDomain, statement, offset)
    statement(offset) = StatementVersion
    offset += 1
    offset = copy(chainDomainId, statement, offset)
    offset = copy(profileId, statement, offset)
    offset = copy(programId, statement, offset)
    offset = copy(contractId, statement, offset)
    putU32Le(statement, offset, applicationPayload.length)
    offset += 4
    offset = copy(applicationPayload, statement, offset)
    if (offset != statement.length)
      throw new IllegalStateException("internal ErgoStatementV1 length mismatch")
    Right(statement)
  }

  private def validateDigest(name: String, value: Array[Byte]): Option[Failure] =
    if (value == null) Some(NullInput(name))
    else if (value.length != DigestBytes) Some(WrongDigestLength(name, DigestBytes, value.length))
    else None

  private def ascii(value: String): Array[Byte] = {
    val output = new Array[Byte](value.length)
    var i = 0
    while (i < value.length) {
      val c = value.charAt(i)
      if (c > 0x7f) throw new IllegalArgumentException("internal tag is not ASCII")
      output(i) = c.toByte
      i += 1
    }
    output
  }

  private def copy(source: Array[Byte], target: Array[Byte], offset: Int): Int = {
    System.arraycopy(source, 0, target, offset, source.length)
    offset + source.length
  }

  private def putU32Le(output: Array[Byte], offset: Int, value: Int): Unit = {
    output(offset) = value.toByte
    output(offset + 1) = (value >>> 8).toByte
    output(offset + 2) = (value >>> 16).toByte
    output(offset + 3) = (value >>> 24).toByte
  }
}
