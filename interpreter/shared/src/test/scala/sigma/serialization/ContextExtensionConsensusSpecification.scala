package sigma.serialization

import sigma.ast.IntConstant
import sigma.interpreter.ContextExtension

/**
  * Consensus vectors for current ContextExtension serialization behavior.
  *
  * ContextExtension traversal order is consensus-sensitive because serialized
  * extensions are part of unsigned transaction bytes and therefore transaction
  * IDs. Existing nodes serialize entries in the traversal order of the current
  * scala.collection.Map representation. This behavior must be preserved by the
  * JVM implementation; other implementations such as sigma-rust should match it.
  *
  * See #1067 and #1122.
  */
class ContextExtensionConsensusSpecification extends SerializationSpecification {
  private def serialize(ce: ContextExtension): Array[Byte] = {
    val w = SigmaSerializer.startWriter()
    ContextExtension.serializer.serialize(ce, w)
    w.toBytes
  }

  private def bodyIds(bytes: Array[Byte]): Seq[Byte] = {
    val r = SigmaSerializer.startReader(bytes)
    val size = r.getUByte()
    (0 until size).map { _ =>
      val id = r.getByte()
      r.getValue()
      id
    }
  }

  property("preserve current JVM traversal order for 8 ContextExtension ids") {
    val ce = ContextExtension((0 to 7).map(i => i.toByte -> IntConstant(i)).toMap)
    val bytes = serialize(ce)

    val expectedIds = Seq[Byte](0, 5, 1, 6, 2, 7, 3, 4)
    bodyIds(bytes) shouldEqual expectedIds

    // Exact bytes for:
    // size=8, then entries in current JVM traversal order:
    // 0 -> IntConstant(0), 5 -> IntConstant(5), 1 -> IntConstant(1),
    // 6 -> IntConstant(6), 2 -> IntConstant(2), 7 -> IntConstant(7),
    // 3 -> IntConstant(3), 4 -> IntConstant(4).
    //
    // IntConstant uses SInt type byte 0x04 and ZigZag VLQ encoding.
    bytes shouldEqual Array[Byte](
      8,
      0, 4, 0,
      5, 4, 10,
      1, 4, 2,
      6, 4, 12,
      2, 4, 4,
      7, 4, 14,
      3, 4, 6,
      4, 4, 8
    )
  }
}
