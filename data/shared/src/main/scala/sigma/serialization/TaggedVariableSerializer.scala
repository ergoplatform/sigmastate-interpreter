package sigma.serialization

import sigma.ast.SType
import sigma.ast._

/** Serializer for the legacy [[TaggedVariable]] node (opcode 0x71).
  *
  * Retained to preserve the consensus-level wire format for ErgoTrees with
  * `ergoTreeVersion <= V6SoftForkVersion`. The class is `@deprecated` to
  * signal the retirement plan:
  *   - Phase 1: types deprecated, wire format unchanged.
  *   - Phase 2 (version-gated): `parse` will reject opcode 0x71 when
  *     `VersionContext.current.isV7Activated`. The opcode slot is
  *     reserved and MUST NOT be reassigned to a new node.
  *
  * Behavior pinned by `TaggedVariableSerializerSpecification`.
  */
@deprecated("Scheduled for version-gated rejection in v7.0; do not reference outside the dispatch table.", since = "7.0.0")
@annotation.nowarn("cat=deprecation")
case class TaggedVariableSerializer(cons: (Byte, SType) => Value[SType])
  extends ValueSerializer[TaggedVariable[_ <: SType]] {
  override def opDesc = TaggedVariable

  override def serialize(obj: TaggedVariable[_ <: SType], w: SigmaByteWriter): Unit =
    w.put(obj.varId)
      .putType(obj.tpe)

  override def parse(r: SigmaByteReader): Value[SType] = {
    val varId = r.getByte()
    val tpe = r.getType()
    cons(varId, tpe)
  }
}
