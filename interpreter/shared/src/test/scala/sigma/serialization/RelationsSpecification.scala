package sigma.serialization

import sigma.ast._
import sigma.serialization.OpCodes._
import sigma.serialization.ValueSerializer._
import scorex.util.encode.ZigZagEncoder.encodeZigZagLong
import sigma.ast.{SInt, SLong}

class RelationsSpecification extends TableSerializationSpecification {

  override val objects =
    Table(
      ("object", "bytes"),
      (LT(LongConstant(2), LongConstant(3)),
        Array[Byte](LtCode, SLong.typeCode, encodeZigZagLong(2).toByte, SLong.typeCode, encodeZigZagLong(3).toByte)),
      (LE(LongConstant(2), LongConstant(3)),
        Array[Byte](LeCode, SLong.typeCode, encodeZigZagLong(2).toByte, SLong.typeCode, encodeZigZagLong(3).toByte)),
      (GT(LongConstant(6), LongConstant(5)),
        Array[Byte](GtCode, SLong.typeCode, encodeZigZagLong(6).toByte, SLong.typeCode, encodeZigZagLong(5).toByte)),
      (GE(LongConstant(6), LongConstant(5)),
        Array[Byte](GeCode, SLong.typeCode, encodeZigZagLong(6).toByte, SLong.typeCode, encodeZigZagLong(5).toByte)),
      (EQ(TrueLeaf, TrueLeaf), Array[Byte](EqCode, ConcreteCollectionBooleanConstantCode, 3)),
      (NEQ(TrueLeaf, FalseLeaf), Array[Byte](NeqCode, ConcreteCollectionBooleanConstantCode, 1)),
      (EQ(EQ(LongConstant(1), LongConstant(1)), EQ(LongConstant(1), LongConstant(1))),
        Array[Byte](EqCode, EqCode, SLong.typeCode, encodeZigZagLong(1).toByte, SLong.typeCode, encodeZigZagLong(1).toByte, EqCode, SLong.typeCode, encodeZigZagLong(1).toByte, SLong.typeCode, encodeZigZagLong(1).toByte)),
      (EQ(LongConstant(1), LongConstant(0)),
        Array[Byte](EqCode, SLong.typeCode, encodeZigZagLong(1).toByte, SLong.typeCode, 0)),
      (NEQ(LongConstant(1), LongConstant(0)),
        Array[Byte](NeqCode, SLong.typeCode, encodeZigZagLong(1).toByte, SLong.typeCode, 0))
    )

  tableRoundTripTest("Relations: serializer round trip")
  tablePredefinedBytesTest("Relations: deserialize from predefined bytes")

  property("Relations: serialization LT(bool, bool) must fail") {
    assertThrows[Exception] {
      deserialize(Array[Byte](LtCode, 12, 13))
    }
  }

  property("TreeLookup: Serializer round trip") {
    forAll { im: TreeLookup =>
      roundTripTest(im)
    }
  }

  property("If: Serializer round trip") {
    forAll { i: If[SInt.type] =>
      roundTripTest(i)
    }
  }
}
