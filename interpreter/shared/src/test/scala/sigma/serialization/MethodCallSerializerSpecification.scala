package sigma.serialization

import scorex.utils.Ints
import sigma.VersionContext
import sigma.ast.SCollection.SByteArray
import sigma.ast.SType.tT
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

    VersionContext.withScriptVersion(VersionContext.V6SoftForkVersion) {
      code
    }

    a[SerializerException] should be thrownBy (
      VersionContext.withScriptVersion((VersionContext.V6SoftForkVersion - 1).toByte) {
        code
      }
      )
  }

  property("MethodCall deserialization round trip for Global.powHit") {
    val k = IntConstant(32)
    val msg = ByteArrayConstant(Array.fill(5)(1.toByte))
    val nonce = ByteArrayConstant(Array.fill(8)(2.toByte))
    val h = ByteArrayConstant(Ints.toByteArray(5))
    val N = IntConstant(1024 * 1024)

    def code = {
      val expr = MethodCall(Global,
        SGlobalMethods.powHitMethod,
        Vector(k, msg, nonce, h, N),
        Map()
      )
      roundTripTest(expr)
    }

    VersionContext.withScriptVersion(VersionContext.V6SoftForkVersion) {
      code
    }

    an[ValidationException] should be thrownBy (
      VersionContext.withScriptVersion((VersionContext.V6SoftForkVersion - 1).toByte) {
        code
      }
      )
  }

  property("MethodCall deserialization round trip for Global.serialize") {
    def code = {
      val b = ByteArrayConstant(Array(1.toByte, 2.toByte, 3.toByte))
      val expr = MethodCall(Global,
        SGlobalMethods.serializeMethod.withConcreteTypes(Map(tT -> SByteArray)),
        Vector(b),
        Map()
      )
      roundTripTest(expr)
    }

    VersionContext.withScriptVersion(VersionContext.V6SoftForkVersion) {
      code
    }

    an[ValidationException] should be thrownBy (
      VersionContext.withScriptVersion((VersionContext.V6SoftForkVersion - 1).toByte) {
        code
      }
    )
  }

  property("MethodCall deserialization round trip for Global.deserializeTo[]") {
    def code = {
      val h = HeaderConstant(headerGen.sample.get)
      val expr = MethodCall(h,
        SGlobalMethods.deserializeToMethod.withConcreteTypes(Map(tT -> SHeader)),
        Array(ByteArrayConstant(Array(1.toByte, 2.toByte, 3.toByte))), // wrong header bytes but ok for test
        Map(tT -> SHeader)
      )
      roundTripTest(expr)
    }

    println(SGlobalMethods.deserializeToMethod.hasExplicitTypeArgs)

    VersionContext.withScriptVersion(VersionContext.V6SoftForkVersion) {
      code
    }

    an[Exception] should be thrownBy (
      VersionContext.withScriptVersion((VersionContext.V6SoftForkVersion - 1).toByte) {
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
    VersionContext.withScriptVersion(VersionContext.V6SoftForkVersion) {
      code
    }

    an[ValidationException] should be thrownBy (
      VersionContext.withScriptVersion((VersionContext.V6SoftForkVersion - 1).toByte) {
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

    VersionContext.withScriptVersion(VersionContext.V6SoftForkVersion) {
      code
    }

    an[ValidationException] should be thrownBy (
      VersionContext.withScriptVersion((VersionContext.V6SoftForkVersion - 1).toByte) {
        code
      }
      )
  }
}
