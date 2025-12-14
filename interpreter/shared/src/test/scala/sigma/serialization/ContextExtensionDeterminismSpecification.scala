package sigma.serialization

import org.scalacheck.Gen
import sigma.ast._
import sigma.interpreter.{ContextExtension, SigmaMap}

/**
  * Consensus-critical tests.
  *
  * These tests enforce that ContextExtension preserves deterministic insertion order
  * across serialization and traversal.
  *
  * IMPORTANT: Do not change ContextExtension/SigmaMap to use scala.collection.Map
  * traversal. Scala Map iteration order is not guaranteed and is consensus-breaking.
  */
class ContextExtensionDeterminismSpecification extends SerializationSpecification {

  private def serialize(ce: ContextExtension): Array[Byte] = {
    val w = SigmaSerializer.startWriter()
    ContextExtension.serializer.serialize(ce, w)
    w.toBytes
  }

  private def parse(bytes: Array[Byte]): ContextExtension = {
    val r = SigmaSerializer.startReader(bytes)
    ContextExtension.serializer.parse(r)
  }

  private def bodyIds(bytes: Array[Byte]): Array[Byte] = {
    // format: [size:UByte] then repeated [id:Byte][value:...]
    // We decode by parsing, but also use the parsed object's traversal for expected.
    // For stronger checks we also extract ids by re-parsing values.
    val r = SigmaSerializer.startReader(bytes)
    val size = r.getUByte()
    val ids = new Array[Byte](size)
    var i = 0
    while (i < size) {
      ids(i) = r.getByte()
      r.getValue() // skip value
      i += 1
    }
    ids
  }

  property("SigmaMap traversal order equals insertion order (random insert)") {
    forAll(Gen.chooseNum(1, 200), Gen.listOf(Gen.chooseNum(0, 255))) { (n0, ks0) =>
      val n = math.max(1, math.min(200, n0))
      val keys = ks0.take(n).distinct.map(_.toByte)
      whenever(keys.nonEmpty) {
        val expected = keys
        val sm = keys.foldLeft(SigmaMap.empty[EvaluatedValue[_ <: SType]]) { (acc, k) =>
          acc.updated(k, IntConstant(k.toInt))
        }
        val got = sm.toSeq.map(_._1)
        got shouldEqual expected
      }
    }
  }

  property("ContextExtension serialization preserves insertion order") {
    forAll(Gen.chooseNum(1, 200), Gen.listOf(Gen.chooseNum(0, 255))) { (n0, ks0) =>
      val n = math.max(1, math.min(200, n0))
      val keys = ks0.take(n).distinct.map(_.toByte)
      whenever(keys.nonEmpty) {
        val ce = ContextExtension.fromSeq(keys.map(k => k -> IntConstant(k.toInt)))
        val bytes = serialize(ce)
        val idsInBytes = bodyIds(bytes)
        idsInBytes.toSeq shouldEqual keys

        val ce2 = parse(bytes)
        ce2.values.toSeq.map(_._1) shouldEqual keys
      }
    }
  }

  property("ContextExtension supports full byte key space (0..255) deterministically") {
    val keys = (0 to 255).map(_.toByte)
    val sm = SigmaMap.fromSeq(keys.map(k => k -> IntConstant(k.toInt)))
    sm.size shouldBe 256
    sm.toSeq.map(_._1) shouldEqual keys

    // NOTE: ContextExtension serialization length is encoded as UByte (0..255),
    // therefore 256 entries cannot be serialized.
    assertExceptionThrown(
      {
        val w = SigmaSerializer.startWriter()
        ContextExtension.serializer.serialize(ContextExtension(sm), w)
      },
      { _ => true }
    )
  }

  property("scala.collection.Map traversal is unsafe for consensus (demonstration)") {
    // This is a deterministic demonstration of why serializing via `Map.foreach`
    // is ill-defined: different Map implementations / construction orders can
    // traverse the same logical mapping in different orders.
    def unsafeSerializeMap(m: scala.collection.Map[Byte, EvaluatedValue[_ <: SType]]): Array[Byte] = {
      val w = SigmaSerializer.startWriter()
      w.putUByte(m.size)
      m.foreach { case (k, v) =>
        w.put(k)
        w.putValue(v)
      }
      w.toBytes
    }

    // Use SeqMap when available (2.13+), otherwise fall back to ListMap (2.11/2.12).
    val m1: scala.collection.immutable.Map[Byte, EvaluatedValue[_ <: SType]] = {
      val pairs = Seq(
        1.toByte -> IntConstant(1),
        2.toByte -> IntConstant(2),
        3.toByte -> IntConstant(3)
      )
      scala.collection.immutable.ListMap(pairs: _*)
    }
    val m2: scala.collection.immutable.Map[Byte, EvaluatedValue[_ <: SType]] = {
      val pairs = Seq(
        3.toByte -> IntConstant(3),
        2.toByte -> IntConstant(2),
        1.toByte -> IntConstant(1)
      )
      scala.collection.immutable.ListMap(pairs: _*)
    }

    // Same mapping, different traversal order => different bytes under unsafe serialization.
    m1.toMap shouldEqual m2.toMap
    unsafeSerializeMap(m1) should not equal unsafeSerializeMap(m2)
  }

  property("ContextExtension serializes 255 entries and preserves insertion order") {
    val keys = (0 to 254).map(_.toByte)
    val ce = ContextExtension.fromSeq(keys.map(k => k -> IntConstant(k.toInt)))
    val bytes = serialize(ce)
    bodyIds(bytes).toSeq shouldEqual keys
    parse(bytes).values.toSeq.map(_._1) shouldEqual keys
  }
}
