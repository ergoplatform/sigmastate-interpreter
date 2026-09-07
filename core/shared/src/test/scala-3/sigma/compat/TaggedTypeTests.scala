package sigma.compat

import scala.compiletime.testing.typeCheckErrors
import sigma.{BaseTests, ByteType, Coll, Colls}
import sigma.ast.TypeCodes.TypeCode
import sigma.data.{Digest32Coll, SigmaPropCodes}

class TaggedTypeTests extends BaseTests {
  test("code tags retain byte values and operations") {
    val typeCode: TypeCode.Type = TypeCode @@ 7.toByte
    val propCode: SigmaPropCodes.SPCode = SigmaPropCodes.SPCode @@@ 8.toByte
    val underlying: Byte = typeCode

    assert(underlying == 7.toByte)
    assert(typeCode + 2 == 9)
    assert((propCode | 1) == 9)
    assert(TypeCode.raw(typeCode) == 7.toByte)
    assert(SigmaPropCodes.SPCode.raw(propCode) == 8.toByte)
  }

  test("different code tags and untagged bytes are not interchangeable") {
    val valid = typeCheckErrors("""
      import sigma.ast.TypeCodes.TypeCode
      import sigma.data.SigmaPropCodes.SPCode
      val typeCode: TypeCode.Type = TypeCode(1.toByte)
      val propCode: SPCode.Type = SPCode(1.toByte)
    """)
    val wrongPropCode = typeCheckErrors("""
      import sigma.ast.TypeCodes.TypeCode
      import sigma.data.SigmaPropCodes.SPCode
      val propCode: SPCode.Type = TypeCode(1.toByte)
    """)
    val wrongTypeCode = typeCheckErrors("""
      import sigma.ast.TypeCodes.TypeCode
      import sigma.data.SigmaPropCodes.SPCode
      val typeCode: TypeCode.Type = SPCode(1.toByte)
    """)
    val rawTypeCode = typeCheckErrors("""
      import sigma.ast.TypeCodes.TypeCode
      val typeCode: TypeCode.Type = 1.toByte
    """)
    val rawPropCode = typeCheckErrors("""
      import sigma.data.SigmaPropCodes.SPCode
      val propCode: SPCode.Type = 1.toByte
    """)

    assert(valid.isEmpty, valid.mkString("\n"))
    assert(wrongPropCode.nonEmpty)
    assert(wrongTypeCode.nonEmpty)
    assert(rawTypeCode.nonEmpty)
    assert(rawPropCode.nonEmpty)
  }

  test("digest tags retain the underlying collection instance and operations") {
    val bytes: Coll[Byte] = Colls.fromItems[Byte](1.toByte, 2.toByte)(ByteType)
    val tagged: Digest32Coll = Digest32Coll(bytes)
    val underlying: Coll[Byte] = tagged

    assert(tagged.length == 2)
    assert(tagged(0) == 1.toByte)
    assert(underlying eq bytes)
    assert(Digest32Coll.raw(tagged) eq bytes)
  }

  test("only reference-backed tags accept null") {
    val missingDigest: Digest32Coll = null
    val nullTypeCode = typeCheckErrors("""
      import sigma.ast.TypeCodes.TypeCode
      val typeCode: TypeCode.Type = null
    """)

    assert(missingDigest eq null)
    assert(nullTypeCode.nonEmpty)
  }
}
