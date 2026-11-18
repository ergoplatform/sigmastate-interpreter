package sigmastate

import sigma.ast.{EvaluatedValue, IntConstant, SType}
import sigma.interpreter.{ContextExtension, SigmaMap}
import sigma.serialization.SigmaSerializer
import sigmastate.helpers.TestingCommons

class ContextExtensionSpecification extends TestingCommons {

  private def v(i: Int): EvaluatedValue[_ <: SType] = IntConstant(i)

  private def mk(keys: Byte*): ContextExtension = {
    val values = keys.map(k => k -> v(k.toInt)).toMap
    ContextExtension(SigmaMap(values))
  }

  property("ContextExtension.empty is an empty extension") {
    ContextExtension.empty.values.size shouldBe 0
    ContextExtension.empty.values.isEmpty shouldBe true
    ContextExtension.empty.get(0.toByte) shouldBe None
  }

  property("ContextExtension.get returns Some for present and None for absent/negative ids") {
    val ext = mk(5, 10, 20)
    ext.get(5) shouldBe Some(v(5))
    ext.get(10) shouldBe Some(v(10))
    ext.get(0) shouldBe None
    ext.get(-1) shouldBe None
    ext.get(Byte.MaxValue) shouldBe None
  }

  property("ContextExtension.add appends new bindings") {
    val ext = ContextExtension.empty
      .add((1.toByte, v(1)))
      .add((2.toByte, v(2)), (3.toByte, v(3)))
    ext.values.size shouldBe 3
    ext.get(1) shouldBe Some(v(1))
    ext.get(2) shouldBe Some(v(2))
    ext.get(3) shouldBe Some(v(3))
  }

  property("ContextExtension.add keeps first position and last value on duplicate id") {
    val ext = mk(5, 7)
      .add((7.toByte, v(77)))
      .add((5.toByte, v(55)))
    ext.values.size shouldBe 2
    ext.values.iterator.toList shouldBe List((5.toByte, v(55)), (7.toByte, v(77)))
    ext.get(5) shouldBe Some(v(55))
    ext.get(7) shouldBe Some(v(77))
  }

  property("ContextExtension serializer round-trip preserves order for representative sizes") {
    def roundtrip(ext: ContextExtension): Unit = {
      val bytes = ContextExtension.serializer.toBytes(ext)
      val parsed = ContextExtension.serializer.fromBytes(bytes)
      parsed shouldBe ext
      parsed.values.hashCode() shouldBe ext.values.hashCode()
      parsed.values.iterator.toList shouldBe ext.values.iterator.toList
    }

    roundtrip(ContextExtension.empty)
    roundtrip(mk(42))
    roundtrip(mk(73, 35, 31, 0))
    roundtrip(mk(10, 20, 30, 40, 50))
    // 127 entries is the maximum representable size
    val allKeys = (0 until Byte.MaxValue).map(_.toByte)
    roundtrip(ContextExtension(SigmaMap(allKeys.zip(allKeys.map(k => v(k.toInt))).toMap)))
  }

  property("ContextExtension serializer rejects more than Byte.MaxValue entries") {
    val allKeys = (0 to Byte.MaxValue).map(_.toByte)
    val tooBig = ContextExtension(SigmaMap(allKeys.zip(allKeys.map(k => v(k.toInt))).toMap))
    an[Exception] should be thrownBy ContextExtension.serializer.toBytes(tooBig)
  }

  property("ContextExtension serializer parse rejects negative size byte") {
    val w = SigmaSerializer.startWriter()
    w.putUByte(255) // -1 as signed byte
    an[Exception] should be thrownBy
      ContextExtension.serializer.parse(SigmaSerializer.startReader(w.toBytes))
  }

  property("ContextExtension serializer parse rejects negative key") {
    val w = SigmaSerializer.startWriter()
    w.putUByte(1)
    w.put((-1).toByte)
    w.putValue(v(1))
    an[Exception] should be thrownBy
      ContextExtension.serializer.parse(SigmaSerializer.startReader(w.toBytes))
  }

  property("ContextExtension serializer parse resolves duplicate keys to last value") {
    val w = SigmaSerializer.startWriter()
    w.putUByte(2)
    w.put(5.toByte).putValue(v(50))
    w.put(5.toByte).putValue(v(55))
    val parsed = ContextExtension.serializer.parse(SigmaSerializer.startReader(w.toBytes))
    parsed.values.size shouldBe 1
    parsed.get(5) shouldBe Some(v(55))
  }

  property("ContextExtension serializer parse preserves wire order as insertion order for small maps") {
    val w = SigmaSerializer.startWriter()
    w.putUByte(2)
    // keys are deliberately out of numeric order on the wire
    w.put(7.toByte).putValue(v(7))
    w.put(5.toByte).putValue(v(5))
    val parsed = ContextExtension.serializer.parse(SigmaSerializer.startReader(w.toBytes))
    parsed.values.size shouldBe 2
    // maps with <= 4 entries traverse in insertion (here: wire) order
    parsed.values.iterator.toList.map(_._1) shouldBe Seq[Byte](7, 5)
    parsed.get(5) shouldBe Some(v(5))
    parsed.get(7) shouldBe Some(v(7))
  }

  property("ContextExtension serializer parse normalizes large maps to hash-trie order") {
    val w = SigmaSerializer.startWriter()
    // write 5 entries in numeric ascending order on the wire
    val wireOrder = Array[Byte](10, 20, 30, 40, 50)
    w.putUByte(wireOrder.length)
    wireOrder.foreach(k => w.put(k).putValue(v(k.toInt)))
    val parsed = ContextExtension.serializer.parse(SigmaSerializer.startReader(w.toBytes))
    parsed.values.size shouldBe 5
    // for > 4 entries traversal is a pure function of the key set
    parsed.values.iterator.toList.map(_._1) shouldBe Seq[Byte](10, 20, 50, 40, 30)
  }
}
