package sigma.serialization

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.util.encode.Base16
import sigma.ast.SCollection.SByteArray
import sigma.ast._
import sigma.exceptions.ConstraintFailed

class VerifyStarkSerializerSpec extends AnyPropSpec with Matchers {

  private val chunks = ConcreteCollection[SByteArray](
    Vector(
      ByteArrayConstant(Array[Byte](1)),
      ByteArrayConstant(Array[Byte](2, 3))
    ),
    SByteArray
  ).asInstanceOf[Value[SCollection[SCollection[SByte.type]]]]
  private val payload = ByteArrayConstant(Array[Byte](4, 5))
  private val programId = ByteArrayConstant(Array[Byte](6, 7, 8))
  private val profileId = ByteArrayConstant(Array[Byte](9, 10))
  private val node = VerifyStark(chunks, payload, programId, profileId)

  property("four-child encoding has opcode 0xB9 and canonical child order") {
    val bytes = ValueSerializer.serialize(node)
    (bytes.head & 0xff) shouldBe 0xb9
    bytes shouldBe (
      Array(0xb9.toByte) ++
      ValueSerializer.serialize(chunks) ++
      ValueSerializer.serialize(payload) ++
      ValueSerializer.serialize(programId) ++
      ValueSerializer.serialize(profileId)
    )
    ValueSerializer.deserialize(bytes) shouldBe node
  }

  property("four-child encoding matches the frozen golden bytes") {
    val bytes = ValueSerializer.serialize(node)
    bytes shouldBe Base16.decode(
      "b983020e0e01010e0202030e0204050e030607080e02090a").get
  }

  property("truncated four-child encodings are rejected") {
    val bytes = ValueSerializer.serialize(node)
    for (length <- 0 until bytes.length) {
      withClue(s"length $length: ") {
        an[Throwable] should be thrownBy ValueSerializer.deserialize(bytes.take(length))
      }
    }
  }

  property("deserialization rejects a wrong type in every child position") {
    val validChildren = IndexedSeq[Value[SType]](chunks, payload, programId, profileId)
    for (i <- validChildren.indices) {
      val children = validChildren.updated(i, IntConstant(i))
      val bytes = Array(0xb9.toByte) ++ children.flatMap(v => ValueSerializer.serialize(v))
      withClue(s"argument $i: ") {
        an[ConstraintFailed] should be thrownBy ValueSerializer.deserialize(bytes.toArray)
      }
    }
  }

  property("profileId may be a constant placeholder") {
    val placeholderNode = node.copy(profileId = ConstantPlaceholder(0, SByteArray))
    val bytes = ValueSerializer.serialize(placeholderNode)
    val reader = SigmaSerializer.startReader(
      bytes,
      new ConstantStore(IndexedSeq(profileId)),
      resolvePlaceholdersToConstants = false
    )
    ValueSerializer.deserialize(reader) shouldBe placeholderNode
  }
}
