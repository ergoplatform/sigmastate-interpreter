package sigma.serialization

import sigma.ast._
import sigmastate.CrossVersionProps

class UpcastOnDeserializationSpecification extends SerializationSpecification with CrossVersionProps {
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

  property("Upcast on deserialization in v3 trees") {
    val expr = Upcast(IntConstant(1), SLong)

    // in v3 tree, roundtrip holds
    roundTripTest(expr, Some(3))

    //in v2 tree serializer strips Upcast
    assertExceptionThrown(roundTripTest(expr, Some(2)), rootCause(_).getMessage.startsWith("IntConstant(1) did not equal Upcast(IntConstant(1)"))
  }


}
