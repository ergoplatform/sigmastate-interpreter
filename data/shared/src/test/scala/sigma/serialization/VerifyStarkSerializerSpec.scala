package sigma.serialization

import org.ergoplatform.validation.ValidationRules.CheckValidOpCode
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import sigma.VersionContext
import sigma.ast.SCollection.SByteArray
import sigma.ast._
import sigma.validation.ValidationException

class VerifyStarkSerializerSpec extends AnyPropSpec with Matchers {

  private val chunks = ConcreteCollection[SByteArray](
    Vector(ByteArrayConstant(Array[Byte](1, 2, 3))),
    SByteArray
  ).asInstanceOf[Value[SCollection[SCollection[SByte.type]]]]
  private val payload = ByteArrayConstant(Array[Byte](4, 5))
  private val programId = ByteArrayConstant(Array.fill[Byte](32)(6))
  private val profileId = ByteArrayConstant(Array.fill[Byte](32)(7))
  private val verifyStark = VerifyStark(chunks, payload, programId, profileId)

  private def v4SerializedNodeBytes(): Array[Byte] =
    VersionContext.withVersions(4, 4) {
      ValueSerializer.serialize(verifyStark)
    }

  property("VerifyStark roundtrips cleanly under ErgoTree v4 with exact raw opcode 0xB9") {
    val bytes = v4SerializedNodeBytes()
    (bytes(0) & 0xff) shouldBe 0xb9

    VersionContext.withVersions(4, 4) {
      ValueSerializer.deserialize(bytes) shouldBe verifyStark
    }

    val v4Tree = ErgoTree.fromProposition(
      ErgoTree.defaultHeaderWithVersion(VersionContext.StarkVerificationVersion),
      verifyStark.toSigmaProp)
    val encodedTree = ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(v4Tree)
    VersionContext.withVersions(4, 4) {
      val decoded = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(encodedTree)
      decoded.isRightParsed shouldBe true
      decoded shouldBe v4Tree
      decoded.toProposition(replaceConstants = true) shouldBe verifyStark.toSigmaProp
    }
  }

  property("0xB9 in v0-v3 ValueSerializer throws CheckValidOpCode ValidationException matching pre-EIP nodes") {
    val bytes = v4SerializedNodeBytes()
    (0 to 3).foreach { version =>
      VersionContext.withVersions(3, version.toByte) {
        val ve = the[ValidationException] thrownBy ValueSerializer.deserialize(bytes)
        ve.rule shouldBe CheckValidOpCode
        ve.args shouldBe Seq(OpCodes.VerifyStarkCode)
      }
    }
  }

  property("0xB9 cannot be serialized into v0-v3 ErgoTree versions") {
    (0 to 3).foreach { version =>
      VersionContext.withVersions(3, version.toByte) {
        val ve = the[ValidationException] thrownBy ValueSerializer.serialize(verifyStark)
        ve.rule shouldBe CheckValidOpCode
        ve.args shouldBe Seq(OpCodes.VerifyStarkCode)
      }
    }
  }

  property("0xB9 in a v0 output without size bit throws SerializerException (no unvoted split)") {
    val propBytes = VersionContext.withVersions(4, 4) {
      ValueSerializer.serialize(verifyStark.toSigmaProp)
    }
    // Header 0x00 = v0, no size bit, no constant segregation.
    val v0NoSizeBytes = Array[Byte](0x00) ++ propBytes

    VersionContext.withVersions(3, 0) {
      val ex = the[SerializerException] thrownBy
        ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(v0NoSizeBytes)
      ex.getCause shouldBe a[ValidationException]
      val ve = ex.getCause.asInstanceOf[ValidationException]
      ve.rule shouldBe CheckValidOpCode
      ve.args shouldBe Seq(OpCodes.VerifyStarkCode)
    }
  }

  property("0xB9 in a v1-v3 output with size bit yields UnparsedErgoTree matching pre-EIP nodes") {
    val propBytes = VersionContext.withVersions(4, 4) {
      ValueSerializer.serialize(verifyStark.toSigmaProp)
    }
    (1 to 3).foreach { version =>
      val header = (ErgoTree.SizeFlag | version.toByte).toByte
      val w = SigmaSerializer.startWriter()
      w.put(header)
      w.putUInt(propBytes.length.toLong)
      w.putBytes(propBytes)
      val encoded = w.toBytes

      VersionContext.withVersions(3, version.toByte) {
        val decoded = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(encoded)
        decoded.isRightParsed shouldBe false
        val Left(UnparsedErgoTree(_, ve)) = decoded.root
        ve.rule shouldBe CheckValidOpCode
        ve.args shouldBe Seq(OpCodes.VerifyStarkCode)
      }
    }
  }
}
