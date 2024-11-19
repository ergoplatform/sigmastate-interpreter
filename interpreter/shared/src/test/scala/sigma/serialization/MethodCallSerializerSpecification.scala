package sigma.serialization

import sigma.VersionContext
import sigma.ast.SCollection.SByteArray
import sigma.ast._
import sigma.validation.ValidationException

class MethodCallSerializerSpecification extends SerializationSpecification {

  property("MethodCall deserialization round trip") {
    val expr = MethodCall(Outputs,
      SCollectionMethods.FlatMapMethod.withConcreteTypes(Map(SCollection.tIV -> SBox, SCollection.tOV -> SByte)),
      Vector(FuncValue(1, SBox, ExtractScriptBytes(ValUse(1, SBox)))),
      Map()
    )
    roundTripTest(expr)
  }

  property("MethodCall deserialization round trip (non-generic method)") {
    val expr = MethodCall(Outputs,
      SCollectionMethods.SizeMethod.withConcreteTypes(Map(SCollection.tIV -> SBox)),
      Vector(),
      Map()
    )
    roundTripTest(expr)
  }

  property("MethodCall deserialization round trip for Header.checkPow") {
    def code = {
      val bi = HeaderConstant(headerGen.sample.get)
      val expr = MethodCall(bi,
        SHeaderMethods.checkPowMethod,
        Vector(),
        Map()
      )
      roundTripTest(expr)
    }

    VersionContext.withVersions(VersionContext.V6SoftForkVersion, 1) {
      code
    }

    an[SerializerException] should be thrownBy (
      VersionContext.withVersions((VersionContext.V6SoftForkVersion - 1).toByte, 1) {
        code
      }
      )
  }

  property("MethodCall deserialization round trip for Global.serialize") {
    def code = {
      val b = ByteArrayConstant(Array(1.toByte, 2.toByte, 3.toByte))
      val expr = MethodCall(Global,
        SGlobalMethods.serializeMethod.withConcreteTypes(Map(STypeVar("T") -> SByteArray)),
        Vector(b),
        Map()
      )
      roundTripTest(expr)
    }

    VersionContext.withVersions(VersionContext.V6SoftForkVersion, 1) {
      code
    }

    an[Exception] should be thrownBy (
      VersionContext.withVersions((VersionContext.V6SoftForkVersion - 1).toByte, 1) {
        code
      })
  }

  property("MethodCall deserialization round trip for Global.encodeNBits") {
    def code = {
      val bi = BigIntConstant(5)
      val expr = MethodCall(Global,
        SGlobalMethods.encodeNBitsMethod,
        Vector(bi),
        Map()
      )
      roundTripTest(expr)
    }

    // should be ok
    VersionContext.withVersions(VersionContext.V6SoftForkVersion, 1) {
      code
    }

    an[ValidationException] should be thrownBy (
      VersionContext.withVersions((VersionContext.V6SoftForkVersion - 1).toByte, 1) {
        code
      })
  }

  property("MethodCall deserialization round trip for Global.decodeNBits") {
    def code = {
      val l = LongConstant(5)
      val expr = MethodCall(Global,
        SGlobalMethods.decodeNBitsMethod,
        Vector(l),
        Map()
      )
      roundTripTest(expr)
    }

    VersionContext.withVersions(VersionContext.V6SoftForkVersion, 1) {
      code
    }

    an[ValidationException] should be thrownBy (
      VersionContext.withVersions((VersionContext.V6SoftForkVersion - 1).toByte, 1) {
        code
      }
      )
  }
}
