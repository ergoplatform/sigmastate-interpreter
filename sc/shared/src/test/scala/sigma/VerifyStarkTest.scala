package sigmastate

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import sigma.ast.SCollection.SByteArray
import sigma.ast._
import sigma.exceptions.ConstraintFailed
import sigma.serialization.OpCodes

class VerifyStarkTest extends AnyPropSpec with Matchers {

  private val chunks = ConcreteCollection[SByteArray](
    Vector(ByteArrayConstant(Array[Byte](1, 2, 3))),
    SByteArray
  ).asInstanceOf[Value[SCollection[SCollection[SByte.type]]]]
  private val payload = ByteArrayConstant(Array[Byte](4, 5))
  private val programId = ByteArrayConstant(Array.fill[Byte](32)(6))
  private val profileId = ByteArrayConstant(Array.fill[Byte](32)(7))

  property("VerifyStark has the canonical four-child type and opcode") {
    VerifyStark.OpType.tDom shouldBe IndexedSeq(
      SCollection(SByteArray),
      SByteArray,
      SByteArray,
      SByteArray
    )
    VerifyStark.OpType.tRange shouldBe SBoolean
    (OpCodes.VerifyStarkCode.toByte & 0xff) shouldBe 0xb9
  }

  property("SigmaBuilder constructs children in canonical order") {
    StdSigmaBuilder.mkVerifyStark(chunks, payload, programId, profileId) shouldBe
      VerifyStark(chunks, payload, programId, profileId)
  }

  property("DeserializationSigmaBuilder rejects every wrong child type") {
    val valid = IndexedSeq[Value[SType]](chunks, payload, programId, profileId)
    for (i <- valid.indices) {
      val invalid = valid.updated(i, IntConstant(i))
      withClue(s"argument $i: ") {
        an[ConstraintFailed] should be thrownBy
          DeserializationSigmaBuilder.mkVerifyStark(
            invalid(0), invalid(1), invalid(2), invalid(3))
      }
    }
  }
}
