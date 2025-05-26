package sigma.serialization

import sigma.data.ProveDlog
import sigma.eval.Extensions.SigmaBooleanOps

class ProveDlogSerializerSpec extends SerializationSpecification {

  property("ProveDlog: Serializer round trip") {
    forAll { pd: ProveDlog =>
      roundTripTest(pd.toSigmaPropValue)
    }
  }

}
