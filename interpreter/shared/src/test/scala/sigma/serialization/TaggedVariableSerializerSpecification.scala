package sigma.serialization

import scala.annotation.nowarn

import sigma.VersionContext
import sigma.ast._
import sigma.serialization.OpCodes.TaggedVariableCode

/** Behavior pinning for the legacy `TaggedVariable` node (opcode 0x71).
  *
  * `TaggedVariable` is unreachable from the ErgoScript compiler (replaced by
  * `GetVar` / `ValUse`), but its opcode remains a consensus-visible part of the
  * ErgoTree serialization format. These tests freeze the current observable
  * behavior so that subsequent refactoring cannot silently change what bytes
  * are accepted or what those bytes decode to.
  *
  * Any change to these expectations MUST be deliberate and gated behind a
  * protocol version bump (see Phase 2 of the retirement plan).
  */
@nowarn("cat=deprecation")
class TaggedVariableSerializerSpecification extends SerializationSpecification {

  private val varIds: Seq[Byte]   = Seq(0.toByte, 1.toByte, 42.toByte, 127.toByte, -1.toByte)
  private val sampleTypes: Seq[SType] =
    Seq(SBoolean, SByte, SShort, SInt, SLong, SBigInt,
        SGroupElement, SSigmaProp, SBox, SAvlTree,
        SCollection.SByteArray, SCollection.SIntArray)

  /** All `(activatedVersion, ergoTreeVersion)` combos exercised by existing
    * version-switching specs. Listed explicitly so that adding a new protocol
    * version forces this test to be reviewed. */
  private val versionMatrix: Seq[(Byte, Byte)] =
    for {
      av <- (0: Byte) to VersionContext.MaxSupportedScriptVersion
      tv <- (0: Byte) to av
    } yield (av.toByte, tv.toByte)

  property("opcode constant value is stable (0x71)") {
    // Hard-coded to catch any accidental reshuffle of OpCodes.scala.
    TaggedVariableCode.toByte shouldBe 0x71.toByte
  }

  property("dispatch table still maps 0x71 to TaggedVariableSerializer") {
    val ser = ValueSerializer.getSerializer(TaggedVariableCode)
    ser shouldBe a[TaggedVariableSerializer]
    ser.opDesc shouldBe TaggedVariable
  }

  property("TaggedVariableNode: serializer round trip across version matrix") {
    for ((av, tv) <- versionMatrix; tpe <- sampleTypes; vid <- varIds) {
      VersionContext.withVersions(av, tv) {
        val node  = TaggedVariableNode(vid, tpe)
        val bytes = ValueSerializer.serialize(node)
        withClue(s"av=$av, tv=$tv, tpe=$tpe, varId=$vid: ") {
          // First byte must be the TaggedVariable opcode.
          bytes(0) shouldBe TaggedVariableCode.toByte
          // Second byte is the raw varId (per current wire format).
          bytes(1) shouldBe vid
          // Round-trip preserves structure.
          val parsed = ValueSerializer.deserialize(bytes)
          parsed shouldBe node
        }
      }
    }
  }

  property("TaggedVariableNode: golden bytes for a representative payload") {
    // Pinning a single hand-built byte sequence guards against accidental
    // changes in how `varId` / `tpe` are written. The full type-coverage matrix
    // is handled by the round-trip property above.
    VersionContext.withVersions(
      VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion) {
      val node  = TaggedVariableNode(1.toByte, SInt)
      val bytes = ValueSerializer.serialize(node)
      // opcode (0x71) || varId (0x01) || SInt.typeCode (0x04)
      bytes shouldBe Array[Byte](0x71.toByte, 0x01.toByte, 0x04.toByte)
    }
  }

  property("all currently-supported ErgoTree versions accept opcode 0x71") {
    // Sanity check the negative direction of the Phase 2 activation: until
    // the next protocol version that rejects 0x71 is activated, every
    // currently-supported `ergoTreeVersion` MUST accept the opcode.
    val acceptingVersions: Seq[Byte] =
      (0: Byte).to(VersionContext.MaxSupportedScriptVersion).toSeq.map(_.toByte)
    for (v <- acceptingVersions; tpe <- Seq[SType](SInt, SBox, SAvlTree)) {
      VersionContext.withVersions(v, v) {
        val node   = TaggedVariableNode(7.toByte, tpe)
        val bytes  = ValueSerializer.serialize(node)
        val parsed = ValueSerializer.deserialize(bytes)
        parsed shouldBe node
      }
    }
  }
}
