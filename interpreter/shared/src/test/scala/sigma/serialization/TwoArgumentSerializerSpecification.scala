package sigma.serialization

import scorex.util.encode.ZigZagEncoder.encodeZigZagLong
import sigma.ast.ByteArrayConstant.ByteArrayTypeCode
import sigma.ast.syntax.{BigIntConstant, GroupElementConstant, LongConstant}
import sigma.ast._
import sigmastate._
import sigma.serialization.OpCodes._
import sigma.serialization.ValueCodes.OpCode

class TwoArgumentSerializerSpecification extends TableSerializationSpecification {

  override val objects =
    Table(
      ("object", "bytes"),
      (Minus(LongConstant(2), LongConstant(3)),
        Array[Byte](MinusCode, SLong.typeCode, encodeZigZagLong(2).toByte, SLong.typeCode, encodeZigZagLong(3).toByte)),
      (Plus(LongConstant(2), LongConstant(3)),
        Array[Byte](PlusCode, SLong.typeCode, encodeZigZagLong(2).toByte, SLong.typeCode, encodeZigZagLong(3).toByte)),
      (Multiply(LongConstant(2), LongConstant(3)),
        Array[Byte](MultiplyCode, SLong.typeCode, encodeZigZagLong(2).toByte, SLong.typeCode, encodeZigZagLong(3).toByte)),
      (Min(LongConstant(2), LongConstant(3)),
        Array[Byte](MinCode, SLong.typeCode, encodeZigZagLong(2).toByte, SLong.typeCode, encodeZigZagLong(3).toByte)),
      (Max(LongConstant(2), LongConstant(3)),
        Array[Byte](MaxCode, SLong.typeCode, encodeZigZagLong(2).toByte, SLong.typeCode, encodeZigZagLong(3).toByte)),
      (Xor(ByteArrayConstant(Array[Byte](1, 2, 3)), ByteArrayConstant(Array[Byte](3, 4))),
          Array[Byte](XorCode, ByteArrayTypeCode, 3, 1, 2, 3, ByteArrayTypeCode, 2, 3, 4)),
      (Append(ByteArrayConstant(Array[Byte](1, 2, 3)), ByteArrayConstant(Array[Byte](3, 4))),
          Array[Byte](AppendCode, ByteArrayTypeCode, 3, 1, 2, 3, ByteArrayTypeCode, 2, 3, 4))
    )

  tableRoundTripTest("TwoArguments: serializer round trip")
  tablePredefinedBytesTest("TwoArguments: deserialize from predefined bytes")

  property("ArithmeticOperations: Serializer round trip") {
    forAll(longConstGen, longConstGen, arithmeticCodeGen) { (x1: LongConstant, x2: LongConstant, opCode: OpCode) =>
      roundTripTest(ArithOp(x1, x2, opCode))
    }
  }

  property("MultiplyGroup: Serializer round trip") {
    forAll { (x1: GroupElementConstant, x2: GroupElementConstant) =>
      roundTripTest(MultiplyGroup(x1, x2))
    }
  }

  property("Exponentiate: Serializer round trip") {
    forAll { (x1: GroupElementConstant, x2: BigIntConstant) =>
      roundTripTest(Exponentiate(x1, x2))
    }
  }

}
