/*
 * SPDX-License-Identifier: MIT
 *
 * Copyright 2026 A. Shannon.
 */
package sigma.stark.profile

/** Package-bounded, payload-free census hook for work completed after the
  * opcode's program, payload and complete proof-shape guards. Production
  * evaluation passes `null`; observer exceptions intentionally propagate.
  */
private[sigma] trait StarkPreVerifierObserver {
  def onProofChunkMaterialized(): Unit
  def onProgramIdMaterialized(): Unit
  def onApplicationPayloadMaterialized(): Unit
  def onSelfPropositionBytesMaterialized(): Unit
  def onContractIdBuilt(): Unit
  def onStatementBuilt(): Unit
  def onJournalDigestBuilt(): Unit
  def onTaggedStructDigestBuilt(): Unit
  def onOkClaimBuilt(): Unit
  def onRawVerifierEntered(): Unit
}

/** Host-only construction of the statement and RISC0 OK ReceiptClaim bound by
  * the first EIP-0045 profile.
  *
  * The public entry point accepts profile-authenticated host values and derives
  * `expectedClaim` internally. No script value or caller-supplied claim digest
  * participates in this construction.
  */
object Risc0ClaimBuilder {
  final val DigestBytes: Int = 32
  final val StatementPrefixBytes: Int = 159
  /** Largest payload whose complete V1 statement length fits a signed Int. */
  final val MaxApplicationPayloadBytes: Int =
    Int.MaxValue - StatementPrefixBytes
  final val StatementVersion: Int = 1

  private val StatementDomain = ascii("Ergo.VerifyStark.Statement")
  private val SystemStateTag = ascii("risc0.SystemState")
  private val OutputTag = ascii("risc0.Output")
  private val ReceiptClaimTag = ascii("risc0.ReceiptClaim")
  private val ZeroDigest = new Array[Byte](DigestBytes)

  /** Stable rejection taxonomy for host inputs. */
  sealed trait Failure extends Product with Serializable {
    def code: String
  }

  final case class NullInput(name: String) extends Failure {
    override val code: String = "stark-claim-null-input"
  }

  final case class WrongDigestLength(name: String, expected: Int, actual: Int) extends Failure {
    override val code: String = "stark-claim-wrong-digest-length"
  }

  final case class InvalidPayloadMaximum(actual: Int) extends Failure {
    override val code: String = "stark-claim-invalid-payload-maximum"
  }

  final case class ApplicationPayloadTooLarge(actual: Int, maximum: Int) extends Failure {
    override val code: String = "stark-claim-application-payload-too-large"
  }

  final case class StatementLengthOverflow(payloadBytes: Int) extends Failure {
    override val code: String = "stark-claim-statement-length-overflow"
  }

  /** Complete host-derived binding. Every accessor returns a defensive copy. */
  final class Binding private[profile] (
      sourceStatement: Array[Byte],
      sourceJournalDigest: Array[Byte],
      sourcePostDigest: Array[Byte],
      sourceOutputDigest: Array[Byte],
      sourceExpectedClaim: Array[Byte]) {
    private val statementSnapshot = sourceStatement.clone()
    private val journalDigestSnapshot = sourceJournalDigest.clone()
    private val postDigestSnapshot = sourcePostDigest.clone()
    private val outputDigestSnapshot = sourceOutputDigest.clone()
    private val expectedClaimSnapshot = sourceExpectedClaim.clone()

    def statement: Array[Byte] = statementSnapshot.clone()
    def journalDigest: Array[Byte] = journalDigestSnapshot.clone()
    def postDigest: Array[Byte] = postDigestSnapshot.clone()
    def outputDigest: Array[Byte] = outputDigestSnapshot.clone()
    def expectedClaim: Array[Byte] = expectedClaimSnapshot.clone()
  }

  /** Intermediate claim chain used by package-local oracle tests. */
  final class ClaimDigests private[profile] (
      sourceJournalDigest: Array[Byte],
      sourcePostDigest: Array[Byte],
      sourceOutputDigest: Array[Byte],
      sourceExpectedClaim: Array[Byte]) {
    private[profile] val journalDigestSnapshot = sourceJournalDigest.clone()
    private[profile] val postDigestSnapshot = sourcePostDigest.clone()
    private[profile] val outputDigestSnapshot = sourceOutputDigest.clone()
    private[profile] val expectedClaimSnapshot = sourceExpectedClaim.clone()

    def journalDigest: Array[Byte] = journalDigestSnapshot.clone()
    def postDigest: Array[Byte] = postDigestSnapshot.clone()
    def outputDigest: Array[Byte] = outputDigestSnapshot.clone()
    def expectedClaim: Array[Byte] = expectedClaimSnapshot.clone()
  }

  /** Build exact `ErgoStatementV1` bytes and their RISC0 OK ReceiptClaim.
    *
    * `authenticatedProfileId` and `maxApplicationPayloadBytes` must come from
    * the same already-authenticated manifest package. All input arrays are
    * snapshotted before construction. The statement is exactly:
    *
    * `ASCII(domain) || 0x01 || chainDomainId || profileId || programId ||`
    * `contractId || payloadLength:u32le || applicationPayload || EOF`.
    */
  def build(
      loadedProfile: Risc0ProfilePackageLoader.LoadedProfile,
      chainDomainId: Array[Byte],
      programId: Array[Byte],
      contractId: Array[Byte],
      applicationPayload: Array[Byte]): Either[Failure, Binding] =
    buildObserved(
      loadedProfile,
      chainDomainId,
      programId,
      contractId,
      applicationPayload,
      null)

  /** Validation-only route through the authenticated-profile overload. */
  private[sigma] def buildObserved(
      loadedProfile: Risc0ProfilePackageLoader.LoadedProfile,
      chainDomainId: Array[Byte],
      programId: Array[Byte],
      contractId: Array[Byte],
      applicationPayload: Array[Byte],
      observer: StarkPreVerifierObserver): Either[Failure, Binding] = {
    if (loadedProfile == null) return Left(NullInput("loaded-profile"))
    buildObserved(
      chainDomainId,
      loadedProfile.profileId,
      programId,
      contractId,
      applicationPayload,
      loadedProfile.maxApplicationPayloadBytes,
      observer)
  }

  /** Package-local primitive used by the authenticated-profile overload and
    * exact construction KATs. Keeping this surface package-bounded prevents an
    * external caller from separating a profile ID from its manifest-owned
    * payload maximum.
    */
  private[profile] def build(
      chainDomainId: Array[Byte],
      authenticatedProfileId: Array[Byte],
      programId: Array[Byte],
      contractId: Array[Byte],
      applicationPayload: Array[Byte],
      maxApplicationPayloadBytes: Int): Either[Failure, Binding] =
    buildImpl(
      chainDomainId,
      authenticatedProfileId,
      programId,
      contractId,
      applicationPayload,
      maxApplicationPayloadBytes,
      null)

  private[sigma] def buildObserved(
      chainDomainId: Array[Byte],
      authenticatedProfileId: Array[Byte],
      programId: Array[Byte],
      contractId: Array[Byte],
      applicationPayload: Array[Byte],
      maxApplicationPayloadBytes: Int,
      observer: StarkPreVerifierObserver): Either[Failure, Binding] =
    buildImpl(
      chainDomainId,
      authenticatedProfileId,
      programId,
      contractId,
      applicationPayload,
      maxApplicationPayloadBytes,
      observer)

  private def buildImpl(
      chainDomainId: Array[Byte],
      authenticatedProfileId: Array[Byte],
      programId: Array[Byte],
      contractId: Array[Byte],
      applicationPayload: Array[Byte],
      maxApplicationPayloadBytes: Int,
      observer: StarkPreVerifierObserver): Either[Failure, Binding] = {
    validateDigest("chain-domain-id", chainDomainId) match {
      case Some(failure) => return Left(failure)
      case None          => ()
    }
    validateDigest("authenticated-profile-id", authenticatedProfileId) match {
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
    if (applicationPayload == null) return Left(NullInput("application-payload"))
    if (maxApplicationPayloadBytes < 0 ||
        maxApplicationPayloadBytes > MaxApplicationPayloadBytes)
      return Left(InvalidPayloadMaximum(maxApplicationPayloadBytes))
    if (applicationPayload.length > maxApplicationPayloadBytes)
      return Left(ApplicationPayloadTooLarge(
        applicationPayload.length,
        maxApplicationPayloadBytes))
    if (applicationPayload.length > Int.MaxValue - StatementPrefixBytes)
      return Left(StatementLengthOverflow(applicationPayload.length))

    val chainSnapshot = chainDomainId.clone()
    val profileSnapshot = authenticatedProfileId.clone()
    val programSnapshot = programId.clone()
    val contractSnapshot = contractId.clone()
    val payloadSnapshot = applicationPayload.clone()
    val statement = encodeStatement(
      chainSnapshot,
      profileSnapshot,
      programSnapshot,
      contractSnapshot,
      payloadSnapshot)
    if (observer ne null) observer.onStatementBuilt()
    val claim = deriveOkClaim(programSnapshot, statement, observer)
    if (observer ne null) observer.onOkClaimBuilt()

    Right(new Binding(
      statement,
      claim.journalDigestSnapshot,
      claim.postDigestSnapshot,
      claim.outputDigestSnapshot,
      claim.expectedClaimSnapshot))
  }

  /** Package-local oracle surface for the independent Rust claim KAT. */
  private[profile] def deriveOkClaimDigests(
      programId: Array[Byte],
      journal: Array[Byte]): Either[Failure, ClaimDigests] = {
    validateDigest("program-id", programId) match {
      case Some(failure) => return Left(failure)
      case None          => ()
    }
    if (journal == null) return Left(NullInput("journal"))
    Right(deriveOkClaim(programId.clone(), journal.clone(), null))
  }

  private def deriveOkClaim(
      programId: Array[Byte],
      journal: Array[Byte],
      observer: StarkPreVerifierObserver): ClaimDigests = {
    val journalDigest = ProfileSha256.hash(journal)
    if (observer ne null) observer.onJournalDigestBuilt()
    val post = taggedStructDigest(SystemStateTag, Array(ZeroDigest), Array(0), observer)
    val output = taggedStructDigest(
      OutputTag,
      Array(journalDigest, ZeroDigest),
      Array.empty[Int],
      observer)
    val expectedClaim = taggedStructDigest(
      ReceiptClaimTag,
      Array(ZeroDigest, programId, post, output),
      Array(0, 0),
      observer)
    new ClaimDigests(journalDigest, post, output, expectedClaim)
  }

  private def encodeStatement(
      chainDomainId: Array[Byte],
      profileId: Array[Byte],
      programId: Array[Byte],
      contractId: Array[Byte],
      payload: Array[Byte]): Array[Byte] = {
    val statement = new Array[Byte](StatementPrefixBytes + payload.length)
    var offset = 0
    offset = copy(StatementDomain, statement, offset)
    statement(offset) = StatementVersion.toByte
    offset += 1
    offset = copy(chainDomainId, statement, offset)
    offset = copy(profileId, statement, offset)
    offset = copy(programId, statement, offset)
    offset = copy(contractId, statement, offset)
    putU32Le(statement, offset, payload.length)
    offset += 4
    offset = copy(payload, statement, offset)
    if (offset != statement.length)
      throw new IllegalStateException("internal ErgoStatementV1 length mismatch")
    statement
  }

  /** RISC0 tagged-struct digest:
    * `SHA256(SHA256(tag) || down* || data:u32le* || u16le(down.length))`.
    * There is deliberately no encoded data-count suffix.
    */
  private def taggedStructDigest(
      tag: Array[Byte],
      down: Array[Array[Byte]],
      data: Array[Int],
      observer: StarkPreVerifierObserver): Array[Byte] = {
    val preimage = new Array[Byte](
      DigestBytes + down.length * DigestBytes + data.length * 4 + 2)
    var offset = copy(ProfileSha256.hash(tag), preimage, 0)
    var i = 0
    while (i < down.length) {
      offset = copy(down(i), preimage, offset)
      i += 1
    }
    i = 0
    while (i < data.length) {
      putU32Le(preimage, offset, data(i))
      offset += 4
      i += 1
    }
    putU16Le(preimage, offset, down.length)
    offset += 2
    if (offset != preimage.length)
      throw new IllegalStateException("internal tagged-struct length mismatch")
    val digest = ProfileSha256.hash(preimage)
    if (observer ne null) observer.onTaggedStructDigestBuilt()
    digest
  }

  private def validateDigest(name: String, value: Array[Byte]): Option[Failure] =
    if (value == null) Some(NullInput(name))
    else if (value.length != DigestBytes)
      Some(WrongDigestLength(name, DigestBytes, value.length))
    else None

  private def ascii(value: String): Array[Byte] = {
    val output = new Array[Byte](value.length)
    var i = 0
    while (i < value.length) {
      val char = value.charAt(i)
      if (char > 0x7f) throw new IllegalArgumentException("internal tag is not ASCII")
      output(i) = char.toByte
      i += 1
    }
    output
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
