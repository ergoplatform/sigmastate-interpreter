package sigma.serialization

import sigma.ast._

/**
 * Serializer for the EIP-0045 four-child `VerifyStark` node.
 *
 * There is no EIP-specific arity byte. The four ordinary Sigma values are
 * encoded in this consensus-critical order:
 *
 *  1. `proofChunks: Coll[Coll[Byte]]`
 *  2. `applicationPayload: Coll[Byte]`
 *  3. `programId: Coll[Byte]`
 *  4. `profileId: Coll[Byte]`
 */
object VerifyStarkSerializer extends ValueSerializer[VerifyStark] {
  override def opDesc: ValueCompanion = VerifyStark

  override protected def getValueChildren(obj: VerifyStark): IndexedSeq[Value[SType]] =
    IndexedSeq(obj.proofChunks, obj.applicationPayload, obj.programId, obj.profileId)

  override protected def rebuildValueNode(
      obj: VerifyStark,
      children: IndexedSeq[Value[SType]]): Value[SType] =
    DeserializationSigmaBuilder.mkVerifyStark(children(0), children(1), children(2), children(3))

  override def serialize(obj: VerifyStark, w: SigmaByteWriter): Unit = {
    w.putValue(obj.proofChunks)
    w.putValue(obj.applicationPayload)
    w.putValue(obj.programId)
    w.putValue(obj.profileId)
  }

  override def parse(r: SigmaByteReader): Value[SBoolean.type] = {
    val proofChunks = r.getValue()
    val applicationPayload = r.getValue()
    val programId = r.getValue()
    val profileId = r.getValue()
    DeserializationSigmaBuilder.mkVerifyStark(
      proofChunks,
      applicationPayload,
      programId,
      profileId
    )
  }
}
