/*
 * SPDX-License-Identifier: MIT
 *
 * Copyright 2026 A. Shannon.
 */
package sigma.ast

import scorex.crypto.hash.Blake2b256
import sigma.Coll
import sigma.ast.SCollection.SByteArray
import sigma.eval.ErgoTreeEvaluator
import sigma.eval.ErgoTreeEvaluator.DataEnv
import sigma.eval.StarkVerificationCapability.{ActiveLifecycle, QuarantinedLifecycle, Snapshot, Unavailable}
import sigma.exceptions.{OpcodeUnavailableException, StarkProfileQuarantinedException, StarkProfileRuntimeException}
import sigma.serialization.OpCodes
import sigma.serialization.ValueCodes.OpCode
import sigma.stark.ErgoStarkStatement

/** Native STARK proof verifier node (EIP-0045, raw opcode `0xB9`).
  *
  * The four children and their wire order are consensus-critical. The opcode has
  * no transaction-selected verifier or cost parameters; `profileId` selects an
  * immutable network profile.
  *
  * @param proofChunks        canonically chunked proof bytes (65,535-byte non-final chunks)
  * @param applicationPayload application-defined public payload bound into `ErgoStatementV1`
  * @param programId          32-byte identifier of the proven program
  * @param profileId          32-byte identifier of the immutable verifier profile
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
        // Normal invocation order: evaluate profileId ONLY before dispatch and
        // lifecycle lookup.
        val evaluatedProfileId = profileId.evalTo[Coll[Byte]](env)
        E.addCost(FixedCost(snapshot.dispatchJit), VerifyStark.opDesc)
        if (evaluatedProfileId.length != VerifyStark.DigestBytes) return false

        val selectedProfileId = evaluatedProfileId.toArray
        snapshot.lookup(selectedProfileId) match {
          case None => false

          case Some(entry) => entry.lifecycle match {
            case QuarantinedLifecycle =>
              val idHex = VerifyStark.toHex(selectedProfileId)
              throw new StarkProfileQuarantinedException(
                idHex,
                "STARK profile " + idHex + " is quarantined")

            case active: ActiveLifecycle =>
              // Complete profile precharge precedes every heavy child,
              // allocation, statement hash, parsing, and cryptography.
              E.addCost(FixedCost(active.fixedJit), VerifyStark.opDesc)

              val evaluatedProgramId = programId.evalTo[Coll[Byte]](env)
              if (evaluatedProgramId.length != VerifyStark.DigestBytes) return false

              val evaluatedPayload = applicationPayload.evalTo[Coll[Byte]](env)
              if (evaluatedPayload.length > active.maxApplicationPayloadBytes) return false

              val evaluatedChunks = proofChunks.evalTo[Coll[Coll[Byte]]](env)
              val chunkCount = evaluatedChunks.length
              if (chunkCount <= 0 || chunkCount > active.maxChunkCount) return false
              var totalProofBytes = 0L
              val lastIdx = chunkCount - 1
              var i = 0
              while (i < lastIdx) {
                if (evaluatedChunks(i).length != sigma.eval.StarkVerificationCapability.MaxProofChunkBytes)
                  return false
                totalProofBytes += sigma.eval.StarkVerificationCapability.MaxProofChunkBytes.toLong
                i += 1
              }
              val lastLen = evaluatedChunks(lastIdx).length
              if (lastLen <= 0 || lastLen > sigma.eval.StarkVerificationCapability.MaxProofChunkBytes)
                return false
              totalProofBytes += lastLen.toLong
              if (totalProofBytes < active.minProofBytes.toLong || totalProofBytes > active.maxProofBytes.toLong)
                return false

              val chunkArrays = new Array[Array[Byte]](chunkCount)
              i = 0
              while (i < chunkCount) {
                chunkArrays(i) = evaluatedChunks(i).toArray
                i += 1
              }
              val programIdBytes = evaluatedProgramId.toArray
              val payloadBytes = evaluatedPayload.toArray
              val contractId = Blake2b256.hash(E.context.SELF.propositionBytes.toArray)

              val statement = ErgoStarkStatement.build(
                snapshot.chainDomainId,
                selectedProfileId,
                programIdBytes,
                contractId,
                payloadBytes,
                active.maxApplicationPayloadBytes
              ) match {
                case Left(failure) =>
                  throw new StarkProfileRuntimeException(
                    "Host failed to construct canonical ErgoStatementV1: " + failure.code)
                case Right(bytes) => bytes
              }

              active.runtime.verify(statement, programIdBytes, chunkArrays)
          }
        }
    }
  }
}

object VerifyStark extends ValueCompanion {
  final val DigestBytes: Int = 32

  override def opCode: OpCode = OpCodes.VerifyStarkCode

  /** (Coll[Coll[Byte]], Coll[Byte], Coll[Byte], Coll[Byte]) => Boolean */
  val OpType: SFunc = SFunc(
    IndexedSeq(
      SCollection(SCollection(SByte)),
      SByteArray,
      SByteArray,
      SByteArray
    ),
    SBoolean
  )

  // Profile-owned dispatch and fixed charges are added explicitly in the
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
