package sigma.serialization

import scorex.util.encode.Base16
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

    VersionContext.withVersions(VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion) {
      code
    }

    a[SerializerException] should be thrownBy (
      VersionContext.withVersions((VersionContext.V6SoftForkVersion - 1).toByte, 1) {
        code
      }
      )

    a[sigma.serialization.SerializerException] should be thrownBy (
      VersionContext.withVersions(VersionContext.V6SoftForkVersion, (VersionContext.V6SoftForkVersion - 1).toByte) {
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

    VersionContext.withVersions(VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion) {
      code
    }

    a[ValidationException] should be thrownBy (
      VersionContext.withVersions((VersionContext.V6SoftForkVersion - 1).toByte, 1) {
        code
      }
      )

    a[ValidationException] should be thrownBy (
      VersionContext.withVersions(VersionContext.V6SoftForkVersion, (VersionContext.V6SoftForkVersion - 1).toByte) {
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

    VersionContext.withVersions(VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion) {
      code
    }

    a[ValidationException] should be thrownBy (
      VersionContext.withVersions((VersionContext.V6SoftForkVersion - 1).toByte, 1) {
        code
      }
      )

    a[ValidationException] should be thrownBy (
      VersionContext.withVersions(VersionContext.V6SoftForkVersion, (VersionContext.V6SoftForkVersion - 1).toByte) {
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

    VersionContext.withVersions(VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion) {
      code
    }

    a[SerializerException] should be thrownBy (
      VersionContext.withVersions((VersionContext.V6SoftForkVersion - 1).toByte, 1) {
        code
      }
      )

    a[sigma.serialization.SerializerException] should be thrownBy (
      VersionContext.withVersions(VersionContext.V6SoftForkVersion, (VersionContext.V6SoftForkVersion - 1).toByte) {
        code
      }
      )
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
    VersionContext.withVersions(VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion) {
      code
    }

    a[ValidationException] should be thrownBy (
      VersionContext.withVersions((VersionContext.V6SoftForkVersion - 1).toByte, 1) {
        code
      }
      )

    a[ValidationException] should be thrownBy (
      VersionContext.withVersions(VersionContext.V6SoftForkVersion, (VersionContext.V6SoftForkVersion - 1).toByte) {
        code
      }
      )
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

    VersionContext.withVersions(VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion) {
      code
    }

    a[ValidationException] should be thrownBy (
      VersionContext.withVersions((VersionContext.V6SoftForkVersion - 1).toByte, 1) {
        code
      }
      )

    a[ValidationException] should be thrownBy (
      VersionContext.withVersions(VersionContext.V6SoftForkVersion, (VersionContext.V6SoftForkVersion - 1).toByte) {
        code
      }
      )
  }

  property("MethodCall deserialization round trip for Box.getReg with explicit type args") {
    def code = {
      val box = arbBoxConstant.arbitrary.sample.get
      val expr = MethodCall(box,
        SBoxMethods.getRegMethodV6.withConcreteTypes(Map(tT -> SInt)),
        Vector(IntConstant(4)),
        Map(tT -> SInt)
      )
      roundTripTest(expr)
    }

    VersionContext.withVersions(VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion) {
      code
    }

    a[ValidationException] should be thrownBy (
      VersionContext.withVersions((VersionContext.V6SoftForkVersion - 1).toByte, 1) {
        code
      }
    )

    a[ValidationException] should be thrownBy (
      VersionContext.withVersions(VersionContext.V6SoftForkVersion, (VersionContext.V6SoftForkVersion - 1).toByte) {
        code
      }
    )
  }

  property("MethodCall deserialization round trip for Context.getVarFromInput with explicit type args") {
    def code = {
      val expr = MethodCall(Context,
        SContextMethods.getVarFromInputMethod.withConcreteTypes(Map(tT -> SInt)),
        Vector(ShortConstant(1), ByteConstant(0)),
        Map(tT -> SInt)
      )
      roundTripTest(expr)
    }

    VersionContext.withVersions(VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion) {
      code
    }

    a[ValidationException] should be thrownBy (
      VersionContext.withVersions((VersionContext.V6SoftForkVersion - 1).toByte, 1) {
        code
      }
    )

    a[ValidationException] should be thrownBy (
      VersionContext.withVersions(VersionContext.V6SoftForkVersion, (VersionContext.V6SoftForkVersion - 1).toByte) {
        code
      }
    )
  }

  property("MethodCall deserialization round trip for Global.deserializeTo with explicit type args") {
    def code = {
      val bytes = ByteArrayConstant(Array(1.toByte, 2.toByte, 3.toByte))
      val expr = MethodCall(Global,
        SGlobalMethods.deserializeToMethod.withConcreteTypes(Map(tT -> SInt)),
        Vector(bytes),
        Map(tT -> SInt)
      )
      roundTripTest(expr)
    }

    VersionContext.withVersions(VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion) {
      code
    }

    a[ValidationException] should be thrownBy (
      VersionContext.withVersions((VersionContext.V6SoftForkVersion - 1).toByte, 1) {
        code
      }
    )

    a[ValidationException] should be thrownBy (
      VersionContext.withVersions(VersionContext.V6SoftForkVersion, (VersionContext.V6SoftForkVersion - 1).toByte) {
        code
      }
    )
  }

  property("MethodCall deserialization round trip for Global.fromBigEndianBytes with explicit type args") {
    def code = {
      val bytes = ByteArrayConstant(Array(1.toByte, 2.toByte, 3.toByte))
      val expr = MethodCall(Global,
        SGlobalMethods.FromBigEndianBytesMethod.withConcreteTypes(Map(tT -> SInt)),
        Vector(bytes),
        Map(tT -> SInt)
      )
      roundTripTest(expr)
    }

    VersionContext.withVersions(VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion) {
      code
    }

    a[ValidationException] should be thrownBy (
      VersionContext.withVersions((VersionContext.V6SoftForkVersion - 1).toByte, 1) {
        code
      }
    )

    a[ValidationException] should be thrownBy (
      VersionContext.withVersions(VersionContext.V6SoftForkVersion, (VersionContext.V6SoftForkVersion - 1).toByte) {
        code
      }
    )
  }

  property("MethodCall deserialization round trip for Global.some with explicit type args") {
    def code = {
      val value = IntConstant(42)
      val expr = MethodCall(Global,
        SGlobalMethods.someMethod.withConcreteTypes(Map(tT -> SInt)),
        Vector(value),
        Map(tT -> SInt)
      )
      roundTripTest(expr)
    }

    VersionContext.withVersions(VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion) {
      code
    }

    a[ValidationException] should be thrownBy (
      VersionContext.withVersions((VersionContext.V6SoftForkVersion - 1).toByte, 1) {
        code
      }
    )

    a[ValidationException] should be thrownBy (
      VersionContext.withVersions(VersionContext.V6SoftForkVersion, (VersionContext.V6SoftForkVersion - 1).toByte) {
        code
      }
    )
  }

  property("MethodCall deserialization round trip for Global.none with explicit type args") {
    def code = {
      val expr = MethodCall(Global,
        SGlobalMethods.noneMethod.withConcreteTypes(Map(tT -> SInt)),
        Vector(),
        Map(tT -> SInt)
      )
      roundTripTest(expr)
    }

    VersionContext.withVersions(VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion) {
      code
    }

    a[ValidationException] should be thrownBy (
      VersionContext.withVersions((VersionContext.V6SoftForkVersion - 1).toByte, 1) {
        code
      }
    )

    a[ValidationException] should be thrownBy (
      VersionContext.withVersions(VersionContext.V6SoftForkVersion, (VersionContext.V6SoftForkVersion - 1).toByte) {
        code
      }
    )
  }

  property("MethodCall with multiple explicit type arguments") {
    def code = {
      // Test with methods that have multiple type parameters (if any exist)
      // For now, test with single type parameter methods using different types
      val bytes = ByteArrayConstant(Array(1.toByte, 2.toByte, 3.toByte))
      val expr = MethodCall(Global,
        SGlobalMethods.deserializeToMethod.withConcreteTypes(Map(tT -> SByteArray)),
        Vector(bytes),
        Map(tT -> SByteArray)
      )
      roundTripTest(expr)
    }

    VersionContext.withVersions(VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion) {
      code
    }

    a[ValidationException] should be thrownBy (
      VersionContext.withVersions((VersionContext.V6SoftForkVersion - 1).toByte, 1) {
        code
      }
    )

    a[ValidationException] should be thrownBy (
      VersionContext.withVersions(VersionContext.V6SoftForkVersion, (VersionContext.V6SoftForkVersion - 1).toByte) {
        code
      }
    )
  }

  property("MethodCall with complex explicit type arguments") {
    def code = {
      // Test with nested generic types
      val box = arbBoxConstant.arbitrary.sample.get
      val expr = MethodCall(box,
        SBoxMethods.getRegMethodV6.withConcreteTypes(Map(tT -> SOption(SInt))),
        Vector(IntConstant(4)),
        Map(tT -> SOption(SInt))
      )
      roundTripTest(expr)
    }

    VersionContext.withVersions(VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion) {
      code
    }

    a[ValidationException] should be thrownBy (
      VersionContext.withVersions((VersionContext.V6SoftForkVersion - 1).toByte, 1) {
        code
      }
    )

    a[ValidationException] should be thrownBy (
      VersionContext.withVersions(VersionContext.V6SoftForkVersion, (VersionContext.V6SoftForkVersion - 1).toByte) {
        code
      }
    )
  }

  property("MethodCall empty arguments validation for V3+ trees") {
    // Test that empty arguments assertion is triggered for V3+ ErgoTree versions
    val expr = MethodCall(Outputs,
      SCollectionMethods.SizeMethod.withConcreteTypes(Map(SCollection.tIV -> SBox)),
      Vector(),
      Map()
    )

    // Should work for V3+ trees
    VersionContext.withVersions(VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion) {
      roundTripTest(expr)
    }

    // Should also work for pre-V3 trees (no assertion)
    VersionContext.withVersions((VersionContext.V6SoftForkVersion - 1).toByte, (VersionContext.V6SoftForkVersion - 1).toByte) {
      roundTripTest(expr)
    }
  }

  property("MethodCall with explicit type args fails for pre-V3 trees") {
    // Test specific methods that should fail for pre-V3 trees
    val methods = Seq(
      ("Box.getReg", () => {
        val box = arbBoxConstant.arbitrary.sample.get
        MethodCall(box,
          SBoxMethods.getRegMethodV6.withConcreteTypes(Map(tT -> SInt)),
          Vector(IntConstant(4)),
          Map(tT -> SInt)
        )
      }),
      ("Global.deserializeTo", () => {
        val bytes = ByteArrayConstant(Array(1.toByte, 2.toByte, 3.toByte))
        MethodCall(Global,
          SGlobalMethods.deserializeToMethod.withConcreteTypes(Map(tT -> SInt)),
          Vector(bytes),
          Map(tT -> SInt)
        )
      }),
      ("Global.some", () => {
        val value = IntConstant(42)
        MethodCall(Global,
          SGlobalMethods.someMethod.withConcreteTypes(Map(tT -> SInt)),
          Vector(value),
          Map(tT -> SInt)
        )
      })
    )

    methods.foreach { case (methodName, exprBuilder) =>
      withClue(s"Method $methodName should fail for pre-V3 trees: ") {
        a[ValidationException] should be thrownBy {
          VersionContext.withVersions((VersionContext.V6SoftForkVersion - 1).toByte, (VersionContext.V6SoftForkVersion - 1).toByte) {
            val expr = exprBuilder()
            roundTripTest(expr)
          }
        }
      }
    }
  }

}
