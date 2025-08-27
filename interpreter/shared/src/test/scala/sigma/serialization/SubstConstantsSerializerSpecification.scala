package sigma.serialization

import sigma.VersionContext
import sigma.ast.{ConcreteCollection, EQ, IntArrayConstant, IntConstant, SInt, SNumericType, SubstConstants, Upcast, Value}
import sigma.ast.syntax.IntValue
import sigma.serialization.ErgoTreeSerializer.DefaultSerializer
import sigmastate.CrossVersionProps

class SubstConstantsSerializerSpecification extends SerializationSpecification
  with CrossVersionProps {

  property("SubstConstant deserialization round trip") {
    forAll(numExprTreeNodeGen) { propRaw =>
      val prop = if(VersionContext.current.isV3OrLaterErgoTreeVersion) {
        val p = VersionContext.withVersions(2, 2) {
          // roundtrip to add Upcast nodes needed for v3 trees
          ValueSerializer.deserialize(ValueSerializer.serialize(propRaw))
        }
        Upcast(p.asInstanceOf[Value[SNumericType]], SInt)
      } else {
        propRaw
      }
      val tree = mkTestErgoTree(EQ(prop, IntConstant(1)).toSigmaProp)
      val bytes = DefaultSerializer.serializeErgoTree(tree)
      val newVals = ConcreteCollection(Array[IntValue](1), SInt)
      val expr = SubstConstants(bytes, IntArrayConstant(Array(0)), newVals)
      roundTripTest(expr)
    }
  }

}