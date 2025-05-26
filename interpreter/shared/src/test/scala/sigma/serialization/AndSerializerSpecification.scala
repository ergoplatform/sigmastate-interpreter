package sigma.serialization

import sigma.ast.{AND, BooleanConstant, Constant, EQ, IntConstant, SBoolean, SCollection, SCollectionType, SInt}
import sigma.serialization.OpCodes._
import scorex.util.encode.ZigZagEncoder.encodeZigZagInt
import sigma.Extensions.ArrayOps
import sigma.ast.syntax.BooleanConstant

class AndSerializerSpecification extends TableSerializationSpecification {

  def boolConst(b: Boolean): BooleanConstant = BooleanConstant.fromBoolean(b)

  override def objects = Table(
    ("object", "bytes"),
    (AND(boolConst(true), boolConst(false)),
      Array[Byte](AndCode, ConcreteCollectionBooleanConstantCode, 2, 1)),
    (AND(Constant[SCollection[SBoolean.type]](Array[Boolean](false, true).toColl, SCollection(SBoolean))),
      Array[Byte](AndCode, SBoolean.embedIn(SCollectionType.CollectionTypeCode), 2, 2)),
    (AND(boolConst(true), EQ(IntConstant(1), IntConstant(1))),
      Array[Byte](AndCode, ConcreteCollectionCode, 2, SBoolean.typeCode, // collection type
        SBoolean.typeCode, 1,
        EqCode, SInt.typeCode, encodeZigZagInt(1).toByte,
        SInt.typeCode, encodeZigZagInt(1).toByte))
  )

  tableRoundTripTest("And: Serializer round trip on predefined values")
  tablePredefinedBytesTest("And: deserialize from predefined bytes")

  property("And: Serializer round trip") {
    forAll(logicalExprTreeNodeGen(Seq(AND.apply))) { tree =>
      roundTripTest(tree)
    }
  }

}
