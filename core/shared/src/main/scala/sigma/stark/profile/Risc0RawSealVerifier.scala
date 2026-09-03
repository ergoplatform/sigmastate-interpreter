/*
 * SPDX-License-Identifier: MIT AND Apache-2.0
 *
 * Contains material adapted from RISC Zero and the initial Scala port by arkadianet.
 * Copyright 2025 RISC Zero, Inc.
 * Copyright 2026 RISC Zero, Inc.
 * Modified for the EIP-0045 direct raw-seal ABI by A. Shannon in 2026.
 * See NOTICE and docs/eip-0045-risc0-source-map.json.
 */
package sigma.stark.profile

import sigma.stark.{BabyBear, Ext4, FriVerifier, MerkleVerifier, Poseidon2, ReadIop, VerifierOperationObserver}
import sigma.stark.circuit.{CircuitTapSet, PolyExtInterpreter, PolyExtOp, PolyExtTable}

/** Direct verifier for the EIP-0045 stock RISC0 raw-seal profile.
  *
  * The STARK, DEEP-ALI, and FRI flow is adapted from Arkadia's independently
  * implemented `SuccinctVerifier` at commit
  * `a2da7834efd84e0e25433c78584a5872d1fa0458`. That implementation mirrors
  * `risc0-zkp` 3.0.4's verifier. This entry point deliberately starts after
  * the EIP's fixed raw-seal transport decoder: it has no host receipt codec,
  * caller-selected control program, or control-set inclusion branch.
  *
  * The code-group Merkle root is reconstructed by the transcript itself and
  * must byte-match exactly one manifest-owned
  * `(controlKind, controlParameter, controlId)` entry. Normal lifts use their
  * segment exponent as the parameter; join and resolve use zero. The terminal
  * role is therefore derived from the proof, never supplied by the caller.
  * The recursion output's separately decoded inner root and claim are checked
  * after full proof verification, and strict ReadIOP EOF is part of
  * acceptance.
  *
  * This is deliberately an outermost-control policy. Child receipts consumed
  * by join or resolve are private witnesses, so their control IDs are not
  * reconstructible from the outer seal. In particular, RISC0 assumptions may
  * name an explicit control root; that root is committed by the conditional
  * guest claim consumed inside the recursive proof and interpreted according
  * to stock RISC0 semantics, not silently replaced by this allowlist.
  *
  * Verifier/profile construction errors throw [[ProfileInvariantException]];
  * proof-controlled rejection is always a typed [[Risc0RawSealVerifier.Failure]].
  * Fatal VM errors and instrumentation failures are not caught.
  */
final class Risc0RawSealVerifier private[profile] (
    parameters: Risc0RawSealVerifier.VerifierParameters,
    profile: Risc0RawSealVerifier.RawSealProfile) {
  import Risc0RawSealVerifier._

  if (parameters == null) invariant("verifier parameters are null")
  if (profile == null) invariant("raw-seal profile is null")

  private val profileSnapshot = ProfileSnapshot(profile)
  private val parameterSnapshot = ParameterSnapshot(parameters, profileSnapshot.outerPo2)

  private val proofSystemDigest = protocolInfoDigestRaw(parameterSnapshot.proofSystemInfo)
  private val circuitDigest = protocolInfoDigestRaw(parameterSnapshot.circuitInfo)

  /** Decode the canonical four-chunk transport and verify it against the
    * caller's expected 32-byte ReceiptClaim digest, constructed by the host
    * from the canonical ErgoStatementV1 and program ID.
    */
  def verify(
      proofChunks: Array[Array[Byte]],
      expectedClaim: Array[Byte]): Either[Failure, Verified] =
    RawSealV1Decoder.decode(proofChunks) match {
      case Left(reason)   => Left(TransportRejected(reason))
      case Right(decoded) => verifyDecoded(decoded, expectedClaim, NoProbe)
    }

  /** [[verify]] with transcript checkpoint instrumentation for KAT and
    * differential tests. The probe is verifier instrumentation, not proof
    * data; an exception raised by it is intentionally not converted to a
    * proof rejection.
    */
  private[profile] def verify(
      proofChunks: Array[Array[Byte]],
      expectedClaim: Array[Byte],
      probe: Probe): Either[Failure, Verified] =
    RawSealV1Decoder.decode(proofChunks) match {
      case Left(reason)   => Left(TransportRejected(reason))
      case Right(decoded) => verifyDecoded(decoded, expectedClaim, probe)
    }

  /** [[verify]] with only the payload-free operation IDs exposed to a
    * package-bounded validation observer. Checkpoint labels and values remain
    * inside this verifier.
    */
  private[sigma] def verifyObservedOperations(
      proofChunks: Array[Array[Byte]],
      expectedClaim: Array[Byte],
      observer: VerifierOperationObserver): Either[Failure, Verified] = {
    if (observer == null) invariant("verifier operation observer is null")
    verify(proofChunks, expectedClaim, new OperationOnlyProbe(observer))
  }

  /** Verify a decoder-produced word stream. This overload exists so the
    * opcode adapter can keep transport parsing and cryptographic execution as
    * explicit phases without copying the 222,668-byte seal.
    */
  private[profile] def verifyDecoded(
      decoded: RawSealV1Decoder.Decoded,
      expectedClaim: Array[Byte]): Either[Failure, Verified] =
    verifyDecoded(decoded, expectedClaim, NoProbe)

  private[profile] def verifyDecoded(
      decoded: RawSealV1Decoder.Decoded,
      expectedClaim: Array[Byte],
      probe: Probe): Either[Failure, Verified] = {
    if (decoded == null) return Left(NullDecodedSeal)
    if (expectedClaim == null) return Left(NullExpectedClaim)
    if (expectedClaim.length != DigestBytes)
      return Left(WrongExpectedClaimLength(expectedClaim.length))
    if (probe == null) invariant("verification probe is null")
    // Sigma values are immutable; the array form at this adapter boundary is
    // snapshotted so a non-consensus caller cannot change the late binding
    // check while cryptographic verification is running.
    val expectedClaimSnapshot = expectedClaim.clone()

    validateDecoded(decoded) match {
      case Some(failure) => return Left(failure)
      case None          => ()
    }

    starkVerify(decoded.wordsSnapshot, probe) match {
      case Left(failure) => Left(failure)
      case Right(result) =>
        if (!bytesEqual(decoded.innerControlRootSnapshot, profileSnapshot.innerControlRoot))
          Left(InnerControlRootMismatch)
        else if (!bytesEqual(decoded.claimDigestSnapshot, expectedClaimSnapshot))
          Left(ClaimMismatch)
        else
          Right(Verified(result.controlKind, result.controlParameter))
    }
  }

  private def validateDecoded(decoded: RawSealV1Decoder.Decoded): Option[Failure] = {
    val words = decoded.wordsSnapshot
    if (words == null) return Some(NullDecodedWords)
    if (words.length != RawSealV1Decoder.WordCount)
      return Some(WrongDecodedWordCount(RawSealV1Decoder.WordCount, words.length))
    if (decoded.innerControlRootSnapshot == null ||
        decoded.innerControlRootSnapshot.length != DigestBytes)
      return Some(WrongDecodedInnerRootLength(
        if (decoded.innerControlRootSnapshot == null) -1
        else decoded.innerControlRootSnapshot.length))
    if (decoded.claimDigestSnapshot == null || decoded.claimDigestSnapshot.length != DigestBytes)
      return Some(WrongDecodedClaimLength(
        if (decoded.claimDigestSnapshot == null) -1 else decoded.claimDigestSnapshot.length))

    var i = 0
    while (i < words.length) {
      val word = words(i)
      if (i == OuterPo2WordIndex) {
        if (word != profileSnapshot.outerPo2)
          return Some(WrongOuterPo2(profileSnapshot.outerPo2, unsigned(word)))
      } else if (word < 0 || word >= BabyBear.P) {
        return Some(UnreducedDecodedWord(i, unsigned(word)))
      }
      i += 1
    }
    None
  }

  // ------------------------------------------------------------------
  // STARK core: risc0-zkp verify + verify_validity.
  // ------------------------------------------------------------------

  private def starkVerify(
      seal: Array[Int],
      probe: Probe): Either[Failure, StarkResult] = {
    val p = parameterSnapshot
    val taps = p.taps
    val captureCheckpoints = shouldCaptureCheckpoints(probe)
    val iop = new ReadIop(seal)

    iop.commit(proofSystemDigest, probe.operationSinkOrNull)
    iop.commit(circuitDigest, probe.operationSinkOrNull)

    // read_slice_with_po2(OUTPUT_SIZE): the output elements are ordinary
    // field values; the final slot contains a literal raw u32 exponent.
    val slice = iop.readFieldElemSlice(p.outputSize + 1) match {
      case None    => return Left(MalformedProof("output", "truncated or unreduced output slice"))
      case Some(s) => s
    }
    iop.commit(
      Poseidon2.unpaddedHash(slice, probe.operationSinkOrNull).map(BabyBear.toRaw),
      probe.operationSinkOrNull)
    val out = java.util.Arrays.copyOfRange(slice, 0, p.outputSize)
    val outerPo2 = BabyBear.toRaw(slice(p.outputSize))
    if (outerPo2 != profileSnapshot.outerPo2)
      return Left(WrongOuterPo2(profileSnapshot.outerPo2, unsigned(outerPo2)))

    // Both values were derived and range-checked from immutable profile data
    // during construction. No proof-controlled value reaches a shift.
    val totCycles = p.totCycles
    val domain = p.domain
    if (captureCheckpoints) checkpoint(probe, "outer_po2", Array(outerPo2))
    if (captureCheckpoints) checkpoint(probe, "out", out)

    // Merkle group ids are 0=accum, 1=code, 2=data. Transcript creation
    // order is CODE, DATA, ACCUM; query-opening order is ACCUM, CODE, DATA.
    val codeMerkle = MerkleVerifier.create(
      iop,
      domain,
      taps.groupSize(1),
      p.queries,
      probe.operationSinkOrNull) match {
      case Left(detail) => return Left(MalformedProof("code-group", detail))
      case Right(tree)  => tree
    }
    if (captureCheckpoints)
      checkpoint(probe, "group_root_code", codeMerkle.rootRawOwned)

    val terminalControl = matchControlId(codeMerkle.rootRawOwned) match {
      case Left(failure) => return Left(failure)
      case Right(value)  => value
    }
    if (captureCheckpoints)
      checkpoint(
        probe,
        "derived_terminal_control",
        Array(terminalControl.kind, terminalControl.parameter))

    val dataMerkle = MerkleVerifier.create(
      iop,
      domain,
      taps.groupSize(2),
      p.queries,
      probe.operationSinkOrNull) match {
      case Left(detail) => return Left(MalformedProof("data-group", detail))
      case Right(tree)  => tree
    }
    if (captureCheckpoints)
      checkpoint(probe, "group_root_data", dataMerkle.rootRawOwned)

    val mixGlobals = new Array[Int](p.mixSize)
    var i = 0
    while (i < mixGlobals.length) {
      mixGlobals(i) = iop.randomElem(probe.operationSinkOrNull)
      i += 1
    }
    if (captureCheckpoints) checkpoint(probe, "mix", mixGlobals)

    val accumMerkle = MerkleVerifier.create(
      iop,
      domain,
      taps.groupSize(0),
      p.queries,
      probe.operationSinkOrNull) match {
      case Left(detail) => return Left(MalformedProof("accum-group", detail))
      case Right(tree)  => tree
    }
    if (captureCheckpoints)
      checkpoint(probe, "group_root_accum", accumMerkle.rootRawOwned)

    val polyMix = iop.randomExtElem(probe.operationSinkOrNull)
    if (captureCheckpoints) checkpoint(probe, "poly_mix", extWords(polyMix))

    val checkMerkle = MerkleVerifier.create(
      iop,
      domain,
      p.checkSize,
      p.queries,
      probe.operationSinkOrNull) match {
      case Left(detail) => return Left(MalformedProof("check-group", detail))
      case Right(tree)  => tree
    }

    val z = iop.randomExtElem(probe.operationSinkOrNull)
    if (captureCheckpoints) checkpoint(probe, "z", extWords(z))
    val backOne = FriVerifier.RouRev(outerPo2)

    val numTaps = taps.tapSize
    val coeffWords = iop.readFieldElemSlice(ExtSize * (numTaps + p.checkSize)) match {
      case None    => return Left(MalformedProof("coeff-u", "truncated or unreduced coefficient slice"))
      case Some(s) => s
    }
    iop.commit(
      Poseidon2.unpaddedHash(coeffWords, probe.operationSinkOrNull).map(BabyBear.toRaw),
      probe.operationSinkOrNull)
    if (captureCheckpoints) checkpoint(probe, "coeff_u", coeffWords)

    val coeffU = new Array[Ext4](numTaps + p.checkSize)
    i = 0
    while (i < coeffU.length) {
      coeffU(i) = Ext4(
        coeffWords(ExtSize * i),
        coeffWords(ExtSize * i + 1),
        coeffWords(ExtSize * i + 2),
        coeffWords(ExtSize * i + 3))
      i += 1
    }

    val evalU = new Array[Ext4](numTaps)
    var curPos = 0
    var evalPos = 0
    var registerIndex = 0
    while (registerIndex < taps.regs.length) {
      val register = taps.regs(registerIndex)
      i = 0
      while (i < register.backs.length) {
        val x = scale(z, BabyBear.pow(backOne, register.backs(i).toLong))
        evalU(evalPos) = polyEvalRange(coeffU, curPos, register.backs.length, x)
        evalPos += 1
        i += 1
      }
      curPos += register.backs.length
      registerIndex += 1
    }
    if (captureCheckpoints) checkpoint(probe, "eval_u", extArrayWords(evalU))

    val result = PolyExtInterpreter.runValidated(
      p.program.ops,
      p.program.ret,
      p.program.fpVars,
      p.program.mixVars,
      polyMix,
      evalU,
      Array(out, mixGlobals)).tot
    if (captureCheckpoints) checkpoint(probe, "result", extWords(result))

    // Four check-polynomial planes, with risc0-zkp's [0,2,1,3] remap.
    var check = Ext4.Zero
    var zi = Ext4.One
    i = 0
    while (i < ExtSize) {
      val remapped = CheckRemap(i)
      check = check +
        coeffU(numTaps + remapped) * zi +
        coeffU(numTaps + remapped + ExtSize) * zi * Basis1 +
        coeffU(numTaps + remapped + 2 * ExtSize) * zi * Basis2 +
        coeffU(numTaps + remapped + 3 * ExtSize) * zi * Basis3
      zi = zi * z
      i += 1
    }
    check = check * (scale(z, 3).pow(BigInt(totCycles)) - Ext4.One)
    if (captureCheckpoints) checkpoint(probe, "check_value", extWords(check))
    if (check != result) return Left(ConstraintCheckFailed)

    // DEEP-ALI batching.
    val friMix = iop.randomExtElem(probe.operationSinkOrNull)
    if (captureCheckpoints)
      checkpoint(probe, "fri_batch_mix", extWords(friMix))
    val comboU = Array.fill(taps.totComboBacks + 1)(Ext4.Zero)
    val tapMixPows = new Array[Ext4](taps.regs.length)
    val checkMixPows = new Array[Ext4](p.checkSize)
    var curMix = Ext4.One
    curPos = 0
    registerIndex = 0
    while (registerIndex < taps.regs.length) {
      val register = taps.regs(registerIndex)
      i = 0
      while (i < register.backs.length) {
        val index = taps.comboBegin(register.combo) + i
        comboU(index) = comboU(index) + curMix * coeffU(curPos + i)
        i += 1
      }
      tapMixPows(registerIndex) = curMix
      curMix = curMix * friMix
      curPos += register.backs.length
      registerIndex += 1
    }
    i = 0
    while (i < p.checkSize) {
      comboU(taps.totComboBacks) = comboU(taps.totComboBacks) + curMix * coeffU(curPos)
      curPos += 1
      checkMixPows(i) = curMix
      curMix = curMix * friMix
      i += 1
    }
    if (captureCheckpoints)
      checkpoint(probe, "combo_u", extArrayWords(comboU))

    val gen = FriVerifier.RouFwd(FriVerifier.log2Ceil(domain))
    var queryNumber = 0
    val inner: Int => Either[String, Ext4] = { index =>
      if (captureCheckpoints)
        checkpoint(probe, "query", Array(queryNumber, index))
      queryNumber += 1
      accumMerkle.verify(iop, index, probe.operationSinkOrNull) match {
        case Left(detail) => Left("accum row: " + detail)
        case Right(accumRow) =>
          codeMerkle.verify(iop, index, probe.operationSinkOrNull) match {
            case Left(detail) => Left("code row: " + detail)
            case Right(codeRow) =>
              dataMerkle.verify(iop, index, probe.operationSinkOrNull) match {
                case Left(detail) => Left("data row: " + detail)
                case Right(dataRow) =>
                  checkMerkle.verify(iop, index, probe.operationSinkOrNull) match {
                    case Left(detail) => Left("check row: " + detail)
                    case Right(checkRow) =>
                      Right(friEvalTaps(
                        comboU,
                        checkRow,
                        backOne,
                        BabyBear.pow(gen, index.toLong),
                        z,
                        Array(accumRow, codeRow, dataRow),
                        tapMixPows,
                        checkMixPows))
                  }
              }
          }
      }
    }

    FriVerifier.friVerify(
      iop,
      totCycles,
      p.queries,
      inner,
      FriVerifier.NoProbe,
      probe.operationSinkOrNull) match {
      case Left(detail) => return Left(MalformedProof("fri", detail))
      case Right(_)     => ()
    }

    if (!iop.verifyComplete) return Left(TrailingSealWords(iop.remaining))
    Right(StarkResult(out, terminalControl.kind, terminalControl.parameter))
  }

  /** Mirror of risc0-zkp `fri_eval_taps`. Zero inversion follows RISC0's
    * base/extension-field convention (`inv(0) = 0`).
    */
  private def friEvalTaps(
      comboU: Array[Ext4],
      checkRow: Array[Int],
      backOne: Int,
      xBase: Int,
      z: Ext4,
      rows: Array[Array[Int]],
      tapMixPows: Array[Ext4],
      checkMixPows: Array[Ext4]): Ext4 = {
    val p = parameterSnapshot
    val taps = p.taps
    val totals = Array.fill(taps.combosCount + 1)(Ext4.Zero)
    val x = Ext4.fromBase(xBase)

    var registerIndex = 0
    while (registerIndex < taps.regs.length) {
      val register = taps.regs(registerIndex)
      totals(register.combo) = totals(register.combo) +
        scale(tapMixPows(registerIndex), rows(register.group)(register.offset))
      registerIndex += 1
    }
    var i = 0
    while (i < p.checkSize) {
      totals(taps.combosCount) = totals(taps.combosCount) + scale(checkMixPows(i), checkRow(i))
      i += 1
    }

    var ret = Ext4.Zero
    var combo = 0
    while (combo < taps.combosCount) {
      val begin = taps.comboBegin(combo)
      val end = taps.comboBegin(combo + 1)
      val numerator = totals(combo) - polyEvalRange(comboU, begin, end - begin, x)
      var divisor = Ext4.One
      var tap = begin
      while (tap < end) {
        divisor = divisor * (x - scale(z, BabyBear.pow(backOne, taps.comboTaps(tap).toLong)))
        tap += 1
      }
      ret = ret + numerator * invOrZero(divisor)
      combo += 1
    }
    val checkNumerator = totals(taps.combosCount) - comboU(taps.totComboBacks)
    val checkDivisor = x - z.pow(BigInt(p.invRate))
    ret + checkNumerator * invOrZero(checkDivisor)
  }

  private def matchControlId(rootRaw: Array[Int]): Either[Failure, TerminalControlRole] = {
    val rootBytes = rawDigestBytes(rootRaw)
    var matchedKind = -1
    var matchedParameter = -1
    var matches = 0
    var i = 0
    while (i < profileSnapshot.controls.length) {
      val entry = profileSnapshot.controls(i)
      if (bytesEqual(rootBytes, entry.controlId)) {
        matchedKind = entry.kind
        matchedParameter = entry.parameter
        matches += 1
      }
      i += 1
    }
    if (matches == 0) Left(ControlIdNotAllowed)
    else if (matches == 1) Right(TerminalControlRole(matchedKind, matchedParameter))
    else invariant("validated profile produced an ambiguous control-ID match")
  }
}

object Risc0RawSealVerifier {
  final val DigestBytes: Int = 32
  private final val OuterPo2WordIndex = 32
  private final val ExtSize = 4
  private final val RequiredControlCount = 10
  private final val FirstSegmentPo2 = 15
  private final val LastSegmentPo2 = 22
  private[profile] final val NormalLiftControlKind = 1
  private[profile] final val JoinControlKind = 2
  private[profile] final val ResolveControlKind = 3
  private final val StockProofSystemInfo = "RISC0_STARK:v1__"
  private final val StockCircuitInfo = "RECURSION:rev1v1"
  private final val StockMixSize = 20
  private final val StockTapSize = 643
  private final val StockRegisterCount = 163
  private final val StockComboCount = 5
  private final val StockPolyExtOps = 12359
  private final val StockPolyExtRet = 1228
  private final val StockPolyExtFpVars = 11130
  private final val StockPolyExtMixVars = 1229
  private val StockGroupSize = Array(12, 23, 128)
  private val StockGroupBegin = Array(0, 16, 39, 643)
  private val StockComboBegin = Array(0, 1, 3, 9, 15, 20)
  private val StockComboTaps = Array(
    0,
    0, 1,
    0, 1, 2, 3, 4, 68,
    0, 1, 2, 7, 15, 16,
    0, 2, 7, 15, 16)

  /** Numeric/circuit inputs owned by the profile's algorithm and data
    * artifacts. [[Risc0ProfilePackageLoader]] is the public production
    * construction boundary: it authenticates the activated manifest and B2,
    * strictly decodes the tables, and only then creates this package-private
    * value. This shared verifier performs no classpath or test-resource
    * loading. Its constructor adds a second structural gate and snapshots
    * every consumed array.
    */
  final class VerifierParameters private[profile] (
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
      val friMinDegree: Int,
      val taps: CircuitTapSet,
      val polyExt: PolyExtTable)

  /** One manifest-owned terminal recursion program. Digest bytes use RISC0
    * `Digest::as_bytes()` order (eight little-endian raw BabyBear words).
    * Normal-lift entries carry their segment `po2`; join and resolve carry
    * parameter zero.
    */
  final class TerminalControl private[profile] (
      val kind: Int,
      val parameter: Int,
      controlIdBytes: Array[Byte]) {
    private[profile] val controlIdSnapshot: Array[Byte] =
      if (controlIdBytes == null) null else controlIdBytes.clone()

    def controlId: Array[Byte] =
      if (controlIdSnapshot == null) null else controlIdSnapshot.clone()
  }

  /** Manifest-owned inputs needed by the direct verifier. This intentionally
    * carries neither an activation identity nor a claimed final profile ID.
    */
  final class RawSealProfile private[profile] (
      val outerPo2: Int,
      innerControlRootBytes: Array[Byte],
      entries: IndexedSeq[TerminalControl]) {
    private[profile] val innerControlRootSnapshot: Array[Byte] =
      if (innerControlRootBytes == null) null else innerControlRootBytes.clone()
    private[profile] val entriesSnapshot: IndexedSeq[TerminalControl] =
      if (entries == null) null else entries.toVector

    def innerControlRoot: Array[Byte] =
      if (innerControlRootSnapshot == null) null else innerControlRootSnapshot.clone()

    def controls: IndexedSeq[TerminalControl] = entriesSnapshot
  }

  /** Successful verification result. Both fields were derived solely by
    * matching the reconstructed terminal code root against the immutable
    * allowlist. Kind 1 is normal lift, kind 2 is join, and kind 3 is resolve.
    */
  final case class Verified(controlKind: Int, controlParameter: Int)

  /** Stable proof-rejection taxonomy. Text from lower verifier stages is
    * diagnostic only; consensus callers branch on the failure type/code.
    */
  sealed trait Failure extends Product with Serializable {
    def code: String
  }

  final case class TransportRejected(reason: RawSealV1Decoder.Failure) extends Failure {
    override val code: String = "raw-seal-transport-rejected"
  }
  case object NullDecodedSeal extends Failure {
    override val code: String = "raw-seal-null-decoded"
  }
  case object NullDecodedWords extends Failure {
    override val code: String = "raw-seal-null-words"
  }
  final case class WrongDecodedWordCount(expected: Int, actual: Int) extends Failure {
    override val code: String = "raw-seal-wrong-word-count"
  }
  final case class WrongDecodedInnerRootLength(actual: Int) extends Failure {
    override val code: String = "raw-seal-wrong-decoded-inner-root-length"
  }
  final case class WrongDecodedClaimLength(actual: Int) extends Failure {
    override val code: String = "raw-seal-wrong-decoded-claim-length"
  }
  case object NullExpectedClaim extends Failure {
    override val code: String = "raw-seal-null-expected-claim"
  }
  final case class WrongExpectedClaimLength(actual: Int) extends Failure {
    override val code: String = "raw-seal-wrong-expected-claim-length"
  }
  final case class UnreducedDecodedWord(wordIndex: Int, value: Long) extends Failure {
    override val code: String = "raw-seal-unreduced-decoded-word"
  }
  final case class WrongOuterPo2(expected: Int, actual: Long) extends Failure {
    override val code: String = "raw-seal-wrong-outer-po2"
  }
  final case class MalformedProof(stage: String, detail: String) extends Failure {
    override val code: String = "raw-seal-malformed-proof"
  }
  case object ControlIdNotAllowed extends Failure {
    override val code: String = "raw-seal-control-id-not-allowed"
  }
  case object ConstraintCheckFailed extends Failure {
    override val code: String = "raw-seal-constraint-check-failed"
  }
  final case class TrailingSealWords(actual: Int) extends Failure {
    override val code: String = "raw-seal-trailing-words"
  }
  case object InnerControlRootMismatch extends Failure {
    override val code: String = "raw-seal-inner-control-root-mismatch"
  }
  case object ClaimMismatch extends Failure {
    override val code: String = "raw-seal-claim-mismatch"
  }

  /** Distinct construction-time failure for invalid immutable profile or
    * verifier artifacts. It is never used to reject proof-controlled bytes.
    */
  final class ProfileInvariantException(message: String)
      extends IllegalArgumentException(message)

  trait Probe {
    def onCheckpoint(label: String, values: Array[Int]): Unit = ()
    private[stark] def operationSinkOrNull: VerifierOperationObserver = null
  }
  object NoProbe extends Probe

  private[profile] final class OperationOnlyProbe(observer: VerifierOperationObserver)
      extends Probe {
    override final def onCheckpoint(label: String, values: Array[Int]): Unit = ()
    override private[stark] final def operationSinkOrNull: VerifierOperationObserver =
      observer
  }

  private final case class StarkResult(
      out: Array[Int],
      controlKind: Int,
      controlParameter: Int)
  private final case class TerminalControlRole(kind: Int, parameter: Int)
  private final case class TerminalControlSnapshot(
      kind: Int,
      parameter: Int,
      controlId: Array[Byte])

  private final class ProfileSnapshot(
      val outerPo2: Int,
      val innerControlRoot: Array[Byte],
      val controls: Array[TerminalControlSnapshot])

  private object ProfileSnapshot {
    def apply(source: RawSealProfile): ProfileSnapshot = {
      if (source.outerPo2 != RawSealV1Decoder.ExpectedOuterPo2)
        invariant("profile outerPo2 does not match the fixed raw-seal decoder")
      if (source.innerControlRootSnapshot == null ||
          source.innerControlRootSnapshot.length != DigestBytes)
        invariant("profile inner control root must be exactly 32 bytes")
      validateDigestWords(source.innerControlRootSnapshot, "profile inner control root")
      if (source.entriesSnapshot == null || source.entriesSnapshot.length != RequiredControlCount)
        invariant("profile must contain exactly ten terminal-control entries")

      val controls = new Array[TerminalControlSnapshot](source.entriesSnapshot.length)
      var i = 0
      while (i < controls.length) {
        val entry = source.entriesSnapshot(i)
        if (entry == null) invariant("profile contains a null terminal-control entry")
        if (i < 8) {
          if (entry.kind != NormalLiftControlKind ||
              entry.parameter != FirstSegmentPo2 + i ||
              entry.parameter > LastSegmentPo2)
            invariant("profile lift roles must be kind 1 with po2 15 through 22")
        } else if (i == 8) {
          if (entry.kind != JoinControlKind || entry.parameter != 0)
            invariant("profile join role must be kind 2 with parameter zero")
        } else if (entry.kind != ResolveControlKind || entry.parameter != 0) {
          invariant("profile resolve role must be kind 3 with parameter zero")
        }
        val controlId = entry.controlIdSnapshot
        if (controlId == null || controlId.length != DigestBytes)
          invariant("profile control ID must be exactly 32 bytes")
        validateDigestWords(controlId, "profile control ID")
        var earlier = 0
        while (earlier < i) {
          if (bytesEqual(controls(earlier).controlId, controlId))
            invariant("profile control IDs must be pairwise distinct")
          earlier += 1
        }
        controls(i) = TerminalControlSnapshot(entry.kind, entry.parameter, controlId.clone())
        i += 1
      }
      new ProfileSnapshot(source.outerPo2, source.innerControlRootSnapshot.clone(), controls)
    }
  }

  private final class RegisterSnapshot(
      val group: Int,
      val offset: Int,
      val combo: Int,
      val backs: Array[Int])

  private final class TapSnapshot(
      val groupSize: Array[Int],
      val tapSize: Int,
      val combosCount: Int,
      val comboBegin: Array[Int],
      val comboTaps: Array[Int],
      val totComboBacks: Int,
      val regs: Array[RegisterSnapshot])

  private object TapSnapshot {
    def apply(source: CircuitTapSet): TapSnapshot = {
      if (source == null) invariant("circuit taps are null")
      if (source.groupNames == null || source.groupNames.length != 3 ||
          source.groupNames(0) != "accum" ||
          source.groupNames(1) != "code" ||
          source.groupNames(2) != "data")
        invariant("circuit tap groups must be exactly accum,code,data")
      if (source.groupSize == null || source.groupSize.length != 3)
        invariant("circuit group sizes must contain three entries")
      val groupSize = source.groupSize.clone()
      var i = 0
      while (i < groupSize.length) {
        if (groupSize(i) <= 0) invariant("circuit group size must be positive")
        if (groupSize(i) != StockGroupSize(i))
          invariant("circuit group dimensions do not match the stock profile")
        i += 1
      }
      if (source.taps == null || source.groupBegin == null || source.groupBegin.length != 4)
        invariant("circuit tap/group arrays are malformed")
      if (source.groupBegin(0) != 0 || source.groupBegin(3) != source.taps.length)
        invariant("circuit group boundaries do not cover the tap table")
      if (!java.util.Arrays.equals(source.groupBegin, StockGroupBegin))
        invariant("circuit group boundaries do not match the stock profile")
      i = 1
      while (i < source.groupBegin.length) {
        if (source.groupBegin(i) < source.groupBegin(i - 1))
          invariant("circuit group boundaries are not monotone")
        i += 1
      }
      if (source.combosCount <= 0 || source.comboBegin == null ||
          source.comboBegin.length != source.combosCount + 1 || source.comboBegin(0) != 0)
        invariant("circuit combo boundaries are malformed")
      if (source.comboTaps == null ||
          source.comboBegin(source.combosCount) != source.comboTaps.length ||
          source.totComboBacks != source.comboTaps.length)
        invariant("circuit combo taps are malformed")
      if (source.totComboBacks == Int.MaxValue)
        invariant("circuit combo count overflows verifier allocation")
      val comboBegin = source.comboBegin.clone()
      val comboTaps = source.comboTaps.clone()
      i = 1
      while (i < comboBegin.length) {
        if (comboBegin(i) <= comboBegin(i - 1))
          invariant("every circuit combo must have a nonempty back list")
        i += 1
      }
      i = 0
      while (i < comboTaps.length) {
        if (comboTaps(i) < 0) invariant("circuit combo back is negative")
        i += 1
      }
      if (source.taps.length != StockTapSize ||
          source.regCount != StockRegisterCount ||
          source.combosCount != StockComboCount ||
          !java.util.Arrays.equals(comboBegin, StockComboBegin) ||
          !java.util.Arrays.equals(comboTaps, StockComboTaps))
        invariant("circuit tap dimensions do not match the stock profile")
      if (source.regs == null || source.regCount != source.regs.length)
        invariant("circuit register count is inconsistent")
      val regs = new Array[RegisterSnapshot](source.regs.length)
      var derivedTaps = 0
      var expectedGroup = 0
      var expectedOffset = 0
      i = 0
      while (i < regs.length) {
        val register = source.regs(i)
        if (register == null) invariant("circuit contains a null register")
        if (register.group < 0 || register.group >= groupSize.length)
          invariant("circuit register group is out of range")
        if (register.offset < 0 || register.offset >= groupSize(register.group))
          invariant("circuit register offset is out of range")
        if (register.group != expectedGroup || register.offset != expectedOffset)
          invariant("circuit registers must cover each group/offset exactly once in order")
        if (register.combo < 0 || register.combo >= source.combosCount)
          invariant("circuit register combo is out of range")
        if (register.size <= 0)
          invariant("circuit register must contain at least one back")
        val backs = new Array[Int](register.size)
        var j = 0
        while (j < backs.length) {
          backs(j) = register.back(j)
          if (backs(j) < 0) invariant("circuit register back is negative")
          if (j > 0 && backs(j) <= backs(j - 1))
            invariant("circuit register backs must be strictly increasing")
          val comboIndex = comboBegin(register.combo) + j
          if (comboIndex >= comboBegin(register.combo + 1) ||
              backs(j) != comboTaps(comboIndex))
            invariant("circuit register backs do not match its combo")
          j += 1
        }
        if (comboBegin(register.combo + 1) - comboBegin(register.combo) != backs.length)
          invariant("circuit register size does not match its combo")
        if (derivedTaps > Int.MaxValue - backs.length)
          invariant("circuit tap count overflows Int")
        derivedTaps += backs.length
        regs(i) = new RegisterSnapshot(register.group, register.offset, register.combo, backs)
        expectedOffset += 1
        if (expectedOffset == groupSize(expectedGroup)) {
          expectedGroup += 1
          expectedOffset = 0
        }
        i += 1
      }
      if (expectedGroup != groupSize.length || expectedOffset != 0)
        invariant("circuit register coverage is incomplete")
      if (derivedTaps != source.taps.length)
        invariant("circuit register walk does not cover every tap")

      i = 0
      while (i < source.combosCount) {
        var other = i + 1
        while (other < source.combosCount) {
          val leftBegin = comboBegin(i)
          val leftEnd = comboBegin(i + 1)
          val rightBegin = comboBegin(other)
          val rightEnd = comboBegin(other + 1)
          var equal = leftEnd - leftBegin == rightEnd - rightBegin
          var j = 0
          while (equal && j < leftEnd - leftBegin) {
            equal = comboTaps(leftBegin + j) == comboTaps(rightBegin + j)
            j += 1
          }
          if (equal) invariant("circuit combo back lists must be pairwise distinct")
          other += 1
        }
        i += 1
      }
      new TapSnapshot(
        groupSize,
        source.taps.length,
        source.combosCount,
        comboBegin,
        comboTaps,
        source.totComboBacks,
        regs)
    }
  }

  private final class ProgramSnapshot(
      val ops: Array[PolyExtOp],
      val ret: Int,
      val fpVars: Int,
      val mixVars: Int)

  private object ProgramSnapshot {
    def apply(source: PolyExtTable, taps: TapSnapshot, outputSize: Int, mixSize: Int): ProgramSnapshot = {
      if (source == null) invariant("constraint program is null")
      if (source.fpVars < 0 || source.mixVars <= 0 ||
          source.ret < 0 || source.ret >= source.mixVars)
        invariant("constraint program stack metadata is malformed")
      if (source.opsCount != StockPolyExtOps ||
          source.ret != StockPolyExtRet ||
          source.fpVars != StockPolyExtFpVars ||
          source.mixVars != StockPolyExtMixVars)
        invariant("constraint program dimensions do not match the stock profile")
      val ops = new Array[PolyExtOp](source.opsCount)
      var copyIndex = 0
      while (copyIndex < ops.length) {
        ops(copyIndex) = source.opAt(copyIndex)
        copyIndex += 1
      }
      var fpCount = 0
      var mixCount = 0
      var i = 0
      while (i < ops.length) {
        if (ops(i) == null) invariant("constraint program contains a null operation")
        def fp(index: Int): Unit =
          if (index < 0 || index >= fpCount) invariant("constraint program fp operand is out of range")
        def mix(index: Int): Unit =
          if (index < 0 || index >= mixCount) invariant("constraint program mix operand is out of range")
        ops(i) match {
          case PolyExtOp.Const(value) =>
            if (value < 0 || value >= BabyBear.P.toLong)
              invariant("constraint constant is not a canonical BabyBear value")
            fpCount += 1
          case PolyExtOp.ConstExt(a, b, c, d) =>
            val values = Array(a, b, c, d)
            var j = 0
            while (j < values.length) {
              if (values(j) < 0 || values(j) >= BabyBear.P.toLong)
                invariant("extension constraint constant is not a canonical BabyBear value")
              j += 1
            }
            fpCount += 1
          case PolyExtOp.Get(tap) =>
            if (tap < 0 || tap >= taps.tapSize) invariant("constraint tap is out of range")
            fpCount += 1
          case PolyExtOp.GetGlobal(arg, offset) =>
            val bound = if (arg == 0) outputSize else if (arg == 1) mixSize else -1
            if (bound < 0 || offset < 0 || offset >= bound)
              invariant("constraint global operand is out of range")
            fpCount += 1
          case PolyExtOp.Add(a, b) => fp(a); fp(b); fpCount += 1
          case PolyExtOp.Sub(a, b) => fp(a); fp(b); fpCount += 1
          case PolyExtOp.Mul(a, b) => fp(a); fp(b); fpCount += 1
          case PolyExtOp.True => mixCount += 1
          case PolyExtOp.AndEqz(chain, inner) => mix(chain); fp(inner); mixCount += 1
          case PolyExtOp.AndCond(chain, cond, inner) =>
            mix(chain); fp(cond); mix(inner); mixCount += 1
        }
        i += 1
      }
      if (fpCount != source.fpVars || mixCount != source.mixVars || source.ret != mixCount - 1)
        invariant("constraint program stack counts are inconsistent")
      new ProgramSnapshot(ops, source.ret, source.fpVars, source.mixVars)
    }
  }

  private final class ParameterSnapshot(
      val proofSystemInfo: String,
      val circuitInfo: String,
      val outputSize: Int,
      val mixSize: Int,
      val queries: Int,
      val invRate: Int,
      val checkSize: Int,
      val outerPo2: Int,
      val totCycles: Int,
      val domain: Int,
      val taps: TapSnapshot,
      val program: ProgramSnapshot)

  private object ParameterSnapshot {
    def apply(source: VerifierParameters, outerPo2: Int): ParameterSnapshot = {
      validateProtocolInfo(source.proofSystemInfo, "proof-system info")
      validateProtocolInfo(source.circuitInfo, "circuit info")
      if (source.proofSystemInfo != StockProofSystemInfo || source.circuitInfo != StockCircuitInfo)
        invariant("transcript protocol-info strings do not match the stock profile")
      if (source.outputSize != DigestBytes)
        invariant("stock recursion output size must be 32")
      if (source.mixSize != StockMixSize)
        invariant("accumulator mix size must be 20")
      if (source.queries != FriVerifier.Queries)
        invariant("query count does not match the stock FRI implementation")
      if (source.invRate != FriVerifier.InvRate)
        invariant("inverse rate does not match the stock FRI implementation")
      if (source.extSize != ExtSize)
        invariant("extension degree must be four")
      if (source.checkSize != ExtSize * ExtSize)
        invariant("check polynomial size must be 16")
      if (source.friFold != FriVerifier.FriFold ||
          source.friFoldPo2 != FriVerifier.FriFoldPo2 ||
          source.friMinDegree != FriVerifier.FriMinDegree)
        invariant("FRI parameters do not match the stock implementation")
      if (outerPo2 < 0 || outerPo2 >= FriVerifier.rouRevLength)
        invariant("outerPo2 is outside the BabyBear root table")
      val cycles = 1L << outerPo2
      val domain = cycles * source.invRate.toLong
      if (cycles > Int.MaxValue || domain <= 0 || domain > Int.MaxValue)
        invariant("outer domain does not fit the verifier's Int arithmetic")
      if ((domain & (domain - 1)) != 0)
        invariant("outer domain is not a power of two")
      val domainPo2 = FriVerifier.log2Ceil(domain.toInt)
      if (domainPo2 >= FriVerifier.rouFwdLength)
        invariant("outer domain is outside the BabyBear root table")
      val taps = TapSnapshot(source.taps)
      if (taps.tapSize > (Int.MaxValue / ExtSize) - source.checkSize)
        invariant("coefficient count overflows Int")
      val program = ProgramSnapshot(source.polyExt, taps, source.outputSize, source.mixSize)
      new ParameterSnapshot(
        source.proofSystemInfo,
        source.circuitInfo,
        source.outputSize,
        source.mixSize,
        source.queries,
        source.invRate,
        source.checkSize,
        outerPo2,
        cycles.toInt,
        domain.toInt,
        taps,
        program)
    }
  }

  private val CheckRemap: Array[Int] = Array(0, 2, 1, 3)
  private val Basis1 = Ext4(0, 1, 0, 0)
  private val Basis2 = Ext4(0, 0, 1, 0)
  private val Basis3 = Ext4(0, 0, 0, 1)

  private def validateProtocolInfo(value: String, label: String): Unit = {
    if (value == null || value.length != 16)
      invariant(label + " must contain exactly 16 ASCII bytes")
    var i = 0
    while (i < value.length) {
      if (value.charAt(i) > 0x7f) invariant(label + " contains a non-ASCII character")
      i += 1
    }
  }

  /** Digest of a 16-byte protocol-info string, encoded without any
    * platform charset API so this source remains Scala.js-neutral.
    */
  private def protocolInfoDigestRaw(info: String): Array[Int] = {
    val elements = new Array[Int](info.length)
    var i = 0
    while (i < info.length) {
      elements(i) = info.charAt(i).toInt
      i += 1
    }
    Poseidon2.unpaddedHash(elements).map(BabyBear.toRaw)
  }

  private def validateDigestWords(bytes: Array[Byte], label: String): Unit = {
    var offset = 0
    while (offset < bytes.length) {
      val word = readU32Le(bytes, offset)
      if (word >= BabyBear.P.toLong) invariant(label + " contains an unreduced word")
      offset += 4
    }
  }

  private def rawDigestBytes(words: Array[Int]): Array[Byte] = {
    if (words == null || words.length != Poseidon2.CellsOut)
      invariant("reconstructed digest does not contain eight words")
    val bytes = new Array[Byte](DigestBytes)
    var i = 0
    while (i < words.length) {
      val word = words(i)
      if (word < 0 || word >= BabyBear.P)
        invariant("reconstructed digest contains an unreduced word")
      val offset = i * 4
      bytes(offset) = word.toByte
      bytes(offset + 1) = (word >>> 8).toByte
      bytes(offset + 2) = (word >>> 16).toByte
      bytes(offset + 3) = (word >>> 24).toByte
      i += 1
    }
    bytes
  }

  private def readU32Le(bytes: Array[Byte], offset: Int): Long =
    (bytes(offset) & 0xffL) |
      ((bytes(offset + 1) & 0xffL) << 8) |
      ((bytes(offset + 2) & 0xffL) << 16) |
      ((bytes(offset + 3) & 0xffL) << 24)

  private def bytesEqual(a: Array[Byte], b: Array[Byte]): Boolean =
    java.util.Arrays.equals(a, b)

  private def scale(value: Ext4, scalar: Int): Ext4 =
    Ext4(
      BabyBear.mul(value.c0, scalar),
      BabyBear.mul(value.c1, scalar),
      BabyBear.mul(value.c2, scalar),
      BabyBear.mul(value.c3, scalar))

  private def invOrZero(value: Ext4): Ext4 =
    if (value.isZero) Ext4.Zero else value.inv

  private def polyEvalRange(
      coefficients: Array[Ext4],
      offset: Int,
      length: Int,
      x: Ext4): Ext4 = {
    var power = Ext4.One
    var total = Ext4.Zero
    var i = 0
    while (i < length) {
      total = total + coefficients(offset + i) * power
      power = power * x
      i += 1
    }
    total
  }

  private def extWords(value: Ext4): Array[Int] =
    Array(value.c0, value.c1, value.c2, value.c3)

  private def extArrayWords(values: Array[Ext4]): Array[Int] = {
    val words = new Array[Int](values.length * ExtSize)
    var i = 0
    while (i < values.length) {
      words(ExtSize * i) = values(i).c0
      words(ExtSize * i + 1) = values(i).c1
      words(ExtSize * i + 2) = values(i).c2
      words(ExtSize * i + 3) = values(i).c3
      i += 1
    }
    words
  }

  /** Checkpoint-disabled probes do not evaluate diagnostic payloads. Capturing
    * probes evaluate each payload once and receive a defensive clone.
    */
  private def shouldCaptureCheckpoints(probe: Probe): Boolean =
    (probe ne NoProbe) && !probe.isInstanceOf[OperationOnlyProbe]

  private[profile] def checkpoint(
      probe: Probe,
      label: String,
      values: => Array[Int]): Unit =
    if (shouldCaptureCheckpoints(probe))
      probe.onCheckpoint(label, values.clone())

  private def unsigned(value: Int): Long = value & 0xffffffffL

  private def invariant(message: String): Nothing =
    throw new ProfileInvariantException(message)
}
