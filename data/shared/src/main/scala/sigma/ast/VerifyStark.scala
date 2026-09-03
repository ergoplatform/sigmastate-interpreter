package sigma.ast

import sigma.Coll
import sigma.ast.SCollection.SByteArray
import sigma.eval.ErgoTreeEvaluator
import sigma.eval.ErgoTreeEvaluator.DataEnv
import sigma.eval.StarkVerificationCapability.{ActiveLifecycle, DispatchLookupObserver, QuarantinedLifecycle, Snapshot, Unavailable}
import sigma.eval.ObservedStarkProfileRuntime
import sigma.exceptions.{OpcodeUnavailableException, StarkProfileQuarantinedException}
import sigma.serialization.OpCodes
import sigma.serialization.ValueCodes.OpCode
import sigma.stark.profile.{ProfileBlake2b256, StarkPreVerifierObserver}

/** Package-bounded, payload-free observer for the complete evaluator route.
  * It composes the existing dispatch and post-guard observers without adding
  * state to the opcode, capability or runtime. Production evaluation passes
  * `null`; observer exceptions intentionally propagate.
  */
private[sigma] trait VerifyStarkEvaluationObserver
    extends StarkPreVerifierObserver with DispatchLookupObserver {
  def onProfileIdEvaluated(): Unit
  def onDispatchCharged(): Unit
  def onProfileIdValidated(): Unit
  def onProfileIdMaterialized(): Unit
  def onLookupCompleted(): Unit
  def onActiveLifecycleSelected(): Unit
  def onFixedCharged(): Unit
  def onProgramIdEvaluated(): Unit
  def onProgramIdValidated(): Unit
  def onApplicationPayloadEvaluated(): Unit
  def onApplicationPayloadValidated(): Unit
  def onProofChunksEvaluated(): Unit
  def onProofChunkCountValidated(): Unit
  def onProofChunkValidated(): Unit
}

/**
 * Native STARK proof verifier node (EIP-0045).
 *
 * The four children and their order are consensus-critical. The opcode has no
 * transaction-selected verifier or cost parameters; `profileId` selects an
 * immutable network profile.
 *
 * @param proofChunks chunked profile-defined canonical proof encoding
 *                    (the raw seal for the initial profile)
 * @param applicationPayload application-defined public payload
 * @param programId identifier of the program proven by the receipt
 * @param profileId identifier of the immutable verifier profile
 */
case class VerifyStark(
  proofChunks: Value[SCollection[SCollection[SByte.type]]],
  applicationPayload: Value[SByteArray],
  programId: Value[SByteArray],
  profileId: Value[SByteArray]
) extends Value[SBoolean.type] with NotReadyValueBoolean {

  override def companion = VerifyStark

  override def opType: SFunc = VerifyStark.OpType

  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val evaluationObserver = E.profiler match {
      case value: VerifyStarkEvaluationObserver => value
      case _                                    => null
    }
    evalImpl(env, evaluationObserver, evaluationObserver)
  }

  /** Validation-only route through the exact production evaluator body. */
  private[sigma] final def evalObserved(
      env: DataEnv,
      observer: StarkPreVerifierObserver)(implicit E: ErgoTreeEvaluator): Any = {
    val evaluationObserver = observer match {
      case value: VerifyStarkEvaluationObserver => value
      case _                                    => null
    }
    evalImpl(env, observer, evaluationObserver)
  }

  private def evalImpl(
      env: DataEnv,
      observer: StarkPreVerifierObserver,
      evaluationObserver: VerifyStarkEvaluationObserver)(
      implicit E: ErgoTreeEvaluator): Any = {
    // Opcode availability is first enforced by the whole-input preflight.
    // This evaluator guard is defense in depth and deliberately precedes every
    // child so no direct evaluator entry can make unavailable code true under
    // negation.
    E.starkVerificationCapability match {
      case Unavailable =>
        throw new OpcodeUnavailableException(
          VerifyStark.opCode.toByte & 0xff,
          "Opcode 0xB9 is unavailable before network activation")

      case snapshot: Snapshot =>
        // Normal invocation order begins here: profileId is the only script
        // child evaluated before trusted dispatch and lifecycle resolution.
        val evaluatedProfileId = profileId.evalTo[Coll[Byte]](env)
        if (evaluationObserver ne null) evaluationObserver.onProfileIdEvaluated()
        E.addCost(FixedCost(snapshot.dispatchJit), VerifyStark.opDesc)
        if (evaluationObserver ne null) evaluationObserver.onDispatchCharged()
        if (evaluatedProfileId.length != VerifyStark.DigestBytes) return false
        if (evaluationObserver ne null) evaluationObserver.onProfileIdValidated()

        val selectedProfileId = evaluatedProfileId.toArray
        if (evaluationObserver ne null) evaluationObserver.onProfileIdMaterialized()
        val selectedEntry =
          if (evaluationObserver eq null) snapshot.lookup(selectedProfileId)
          else snapshot.lookupObserved(selectedProfileId, evaluationObserver)
        if (evaluationObserver ne null) evaluationObserver.onLookupCompleted()
        selectedEntry match {
          case None => false

          case Some(entry) => entry.lifecycle match {
            case QuarantinedLifecycle =>
              val idHex = VerifyStark.toHex(selectedProfileId)
              throw new StarkProfileQuarantinedException(
                idHex,
                "STARK profile " + idHex + " is quarantined")

            case active: ActiveLifecycle =>
              if (evaluationObserver ne null)
                evaluationObserver.onActiveLifecycleSelected()
              // The complete profile charge precedes every heavy child,
              // transport allocation, statement hash and verifier operation.
              E.addCost(FixedCost(active.fixedJit), VerifyStark.opDesc)
              if (evaluationObserver ne null) evaluationObserver.onFixedCharged()

              val evaluatedProgramId = programId.evalTo[Coll[Byte]](env)
              if (evaluationObserver ne null) evaluationObserver.onProgramIdEvaluated()
              if (evaluatedProgramId.length != VerifyStark.DigestBytes) return false
              if (evaluationObserver ne null) evaluationObserver.onProgramIdValidated()

              val evaluatedPayload = applicationPayload.evalTo[Coll[Byte]](env)
              if (evaluationObserver ne null)
                evaluationObserver.onApplicationPayloadEvaluated()
              if (evaluatedPayload.length > active.maxApplicationPayloadBytes) return false
              if (evaluationObserver ne null)
                evaluationObserver.onApplicationPayloadValidated()

              val evaluatedChunks = proofChunks.evalTo[Coll[Coll[Byte]]](env)
              if (evaluationObserver ne null) evaluationObserver.onProofChunksEvaluated()
              val expectedChunkLengths = active.canonicalProofChunkLengths
              if (evaluatedChunks.length != expectedChunkLengths.length) return false
              if (evaluationObserver ne null)
                evaluationObserver.onProofChunkCountValidated()

              var i = 0
              while (i < expectedChunkLengths.length) {
                if (evaluatedChunks(i).length != expectedChunkLengths(i)) return false
                if (evaluationObserver ne null)
                  evaluationObserver.onProofChunkValidated()
                i += 1
              }

              val chunkArrays = new Array[Array[Byte]](expectedChunkLengths.length)
              i = 0
              while (i < chunkArrays.length) {
                chunkArrays(i) = evaluatedChunks(i).toArray
                if (observer ne null) observer.onProofChunkMaterialized()
                i += 1
              }
              val programIdBytes = evaluatedProgramId.toArray
              if (observer ne null) observer.onProgramIdMaterialized()
              val payloadBytes = evaluatedPayload.toArray
              if (observer ne null) observer.onApplicationPayloadMaterialized()
              val propositionBytes = E.context.SELF.propositionBytes.toArray
              if (observer ne null) observer.onSelfPropositionBytesMaterialized()
              val contractId = ProfileBlake2b256.hash(propositionBytes)
              if (observer ne null) observer.onContractIdBuilt()

              if (observer eq null)
                active.runtime.verify(
                  snapshot.chainDomainId,
                  programIdBytes,
                  contractId,
                  payloadBytes,
                  chunkArrays)
              else active.runtime match {
                case observedRuntime: ObservedStarkProfileRuntime =>
                  observedRuntime.verifyObserved(
                    snapshot.chainDomainId,
                    programIdBytes,
                    contractId,
                    payloadBytes,
                    chunkArrays,
                    observer)
                case _ =>
                  active.runtime.verify(
                    snapshot.chainDomainId,
                    programIdBytes,
                    contractId,
                    payloadBytes,
                    chunkArrays)
              }
          }
        }
    }
  }
}

object VerifyStark extends ValueCompanion {
  final val DigestBytes: Int = 32

  override def opCode: OpCode = OpCodes.VerifyStarkCode

  /**
   * (Coll[Coll[Byte]], Coll[Byte], Coll[Byte], Coll[Byte]) => Boolean
   */
  val OpType: SFunc = SFunc(
    IndexedSeq(
      SCollection(SCollection(SByte)),
      SByteArray,
      SByteArray,
      SByteArray
    ),
    SBoolean
  )

  // The profile-owned dispatch and fixed charges are added explicitly in the
  // mandated phased order. Static operation metadata remains zero so no
  // second implicit charge is applied by generic evaluator machinery.
  override val costKind: FixedCost = FixedCost(JitCost(0))

  private def toHex(bytes: Array[Byte]): String = {
    val alphabet = "0123456789abcdef"
    val output = new Array[Char](bytes.length * 2)
    var i = 0
    while (i < bytes.length) {
      val value = bytes(i) & 0xff
      output(2 * i) = alphabet.charAt(value >>> 4)
      output(2 * i + 1) = alphabet.charAt(value & 0x0f)
      i += 1
    }
    new String(output)
  }
}
