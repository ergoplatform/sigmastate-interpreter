package org.ergoplatform.validation

import sigma.validation.{ReplacedRule, RuleStatus}
import sigmastate.helpers.CompilerTestingCommons
import sigma.serialization.{SerializationSpecification, SigmaSerializer}

class RuleStatusSerializerSpec extends SerializationSpecification with CompilerTestingCommons {

  private def roundtrip(status: RuleStatus) = {
    implicit val ser: RuleStatusSerializer.type = RuleStatusSerializer
    roundTripTest(status)
    roundTripTestWithPos(status)
  }

  property("RuleStatusSerializer round trip") {
    forAll(statusGen, MinSuccessful(100)) { status =>
      roundtrip(status)
    }
  }

  property("RuleStatusSerializer parse unrecognized status") {
    val unknownCode = 100.toByte
    val someByte = 10.toByte
    val nextByte = 20.toByte
    val bytes = Array[Byte](1, unknownCode, someByte, nextByte)
    val r =  SigmaSerializer.startReader(bytes)
    val s = RuleStatusSerializer.parse(r)
    s shouldBe ReplacedRule(0)
    val b = r.getByte()
    b shouldBe nextByte
  }

}
