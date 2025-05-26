package sigma.serialization

import sigma.ast._

class UpcastOnDeserializationSpecification extends SerializationSpecification {
  import CheckingSigmaBuilder._

  property("Upcast deserialization round trip") {
    forAll(comparisonExprTreeNodeGen, minSuccessful(500)) { tree =>
      roundTripTest(tree)
    }
  }

  property("EQ: Upcast on deserialization") {
    val expr = mkEQ(Upcast(IntConstant(1), SLong), LongConstant(1))
    roundTripTest(expr)
  }

  property("GT: Upcast on deserialization") {
    val expr = mkGT(Upcast(IntConstant(1), SLong), LongConstant(1))
    roundTripTest(expr)
  }

  property("ByIndex: index upcast on deserialization") {
    val expr = ByIndex(Outputs, Upcast(ByteConstant(1), SInt))
    roundTripTest(expr)
  }
}
