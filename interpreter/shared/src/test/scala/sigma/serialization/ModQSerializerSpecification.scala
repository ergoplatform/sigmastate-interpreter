package sigma.serialization

import sigma.ast.ModQ
import sigma.ast.syntax.BigIntConstant

class ModQSerializerSpecification extends SerializationSpecification {

  // TODO https://github.com/ScorexFoundation/sigmastate-interpreter/issues/327
  ignore("ModQ: Serializer round trip") {
    forAll(bigIntConstGen) { x: BigIntConstant =>
      roundTripTest(ModQ(x))
    }
  }

}
