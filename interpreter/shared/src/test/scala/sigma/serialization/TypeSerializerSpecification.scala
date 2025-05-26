package sigma.serialization

import org.scalacheck.Arbitrary._
import org.scalatest.Assertion
import sigma.ast._

class TypeSerializerSpecification extends SerializationSpecification {

  private def roundtrip[T <: SType](tpe: T, expected: Array[Byte]): Assertion = {
    val w = SigmaSerializer.startWriter()
        .putType(tpe)
    val bytes = w.toBytes
    bytes shouldBe expected
    roundtrip(tpe)
  }

  private def roundtrip[T <: SType](tpe: T): Assertion = {
    val w = SigmaSerializer.startWriter()
      .putType(tpe)
    val bytes = w.toBytes
    val r = SigmaSerializer.startReader(bytes, 0)
    val res = r.getType()
    res shouldBe tpe
    val randomPrefix = arrayGen[Byte].sample.get
    val r2 = SigmaSerializer.startReader(randomPrefix ++ bytes, randomPrefix.length)
    val res2 = r2.getType()
    res2 shouldBe tpe
  }

  property("Codes of embeddable types have correct order") {
    for (i <- TypeSerializer.embeddableIdToType.indices.drop(1))
      i shouldBe TypeSerializer.embeddableIdToType(i).typeCode
  }

  import SCollectionType._; import SOption._; import STuple._

  property("Embeddable type serialization roundtrip") {
    forAll { t: SPredefType =>
      whenever(t.isInstanceOf[SEmbeddable]) {
        val e = t.asInstanceOf[SEmbeddable]
        val tCode = t.typeCode
        roundtrip(t, Array[Byte](tCode))
        roundtrip(SCollection(t), Array[Byte](e.embedIn(CollectionTypeCode)))
        roundtrip(SCollection(SCollection(t)), Array[Byte](e.embedIn(NestedCollectionTypeCode)))
        roundtrip(SOption(t), Array[Byte](e.embedIn(OptionTypeCode)))
        roundtrip(SOption(SCollection(t)), Array[Byte](e.embedIn(OptionCollectionTypeCode)))
        roundtrip(STuple(t, t), Array[Byte](e.embedIn(PairSymmetricTypeCode)))
        if (t != SLong) {
          roundtrip(STuple(t, SBox), Array[Byte](e.embedIn(Pair1TypeCode), SBox.typeCode))
          roundtrip(STuple(SBox, t), Array[Byte](e.embedIn(Pair2TypeCode), SBox.typeCode))
        }
        roundtrip(STuple(SCollection(SLong), t), Array[Byte](e.embedIn(Pair2TypeCode), SLong.embedIn(CollectionTypeCode)))
      }
    }
  }

  property("Complex type serialization roundtrip") {
    forAll { t: SPredefType =>
      whenever(t.isInstanceOf[SEmbeddable]) {
        val e = t.asInstanceOf[SEmbeddable]
        val tupCode = e.embedIn(PairSymmetricTypeCode)
        roundtrip(SCollection(STuple(e, e)), Array[Byte](CollectionTypeCode, tupCode))
        roundtrip(SCollection(SOption(e)), Array[Byte](CollectionTypeCode, e.embedIn(OptionTypeCode)))
        roundtrip(SCollection(SCollection(STuple(e, e))), Array[Byte](CollectionTypeCode, CollectionTypeCode, tupCode))
        roundtrip(SCollection(SOption(STuple(e, e))), Array[Byte](CollectionTypeCode, OptionTypeCode, tupCode))
        roundtrip(SOption(STuple(e, e)), Array[Byte](OptionTypeCode, tupCode))
        roundtrip(SOption(SCollection(STuple(e, e))), Array[Byte](OptionTypeCode, CollectionTypeCode, tupCode))
      }
    }
  }

  property("Specific types serialization roundtrip") {
    roundtrip(SAny, Array[Byte](SAny.typeCode))
    roundtrip(SGlobal, Array[Byte](SGlobal.typeCode))
    roundtrip(STuple(SCollection(SLong), SCollection(SLong)),
      Array[Byte](Pair1TypeCode, SLong.embedIn(CollectionTypeCode), SLong.embedIn(CollectionTypeCode)))
    roundtrip(STuple(SCollection(SLong), SOption(SLong)),
      Array[Byte](Pair1TypeCode, SLong.embedIn(CollectionTypeCode), SLong.embedIn(OptionTypeCode)))

    roundtrip(STuple(SLong, SLong, SByte),
      Array[Byte](Pair2TypeCode, SLong.typeCode, SLong.typeCode, SByte.typeCode))
    roundtrip(STuple(SCollection(SLong), SLong, SByte),
      Array[Byte](Pair2TypeCode, SLong.embedIn(CollectionTypeCode), SLong.typeCode, SByte.typeCode))
    roundtrip(STuple(SCollection(SLong), SOption(SLong), SByte),
      Array[Byte](Pair2TypeCode, SLong.embedIn(CollectionTypeCode), SLong.embedIn(OptionTypeCode), SByte.typeCode))
    roundtrip(STuple(SCollection(SCollection(SLong)), SOption(SCollection(SLong)), SByte),
      Array[Byte](Pair2TypeCode, SLong.embedIn(NestedCollectionTypeCode), SLong.embedIn(OptionCollectionTypeCode), SByte.typeCode))

    roundtrip(STuple(SLong, SLong, SByte, SBoolean),
      Array[Byte](PairSymmetricTypeCode, SLong.typeCode, SLong.typeCode, SByte.typeCode, SBoolean.typeCode))

    roundtrip(STuple(SLong, SLong, SByte, SBoolean, SInt),
      Array[Byte](TupleTypeCode, 5, SLong.typeCode, SLong.typeCode, SByte.typeCode, SBoolean.typeCode, SInt.typeCode))
  }

  property("STypeIdent serialization roundtrip") {
    forAll(sTypeIdentGen) { ti =>
      roundtrip(ti)
    }
  }

  property("negative cases") {
    assertExceptionThrown(
      SigmaSerializer.startWriter().putType(STuple(SInt)),
      { t =>
        t.isInstanceOf[RuntimeException] &&
        t.getMessage.contains("Invalid Tuple type with less than 2 items")
      })
  }
}
