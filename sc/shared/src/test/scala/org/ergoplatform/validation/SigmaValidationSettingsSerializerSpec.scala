package org.ergoplatform.validation

import org.ergoplatform.validation.ValidationRules.currentSettings
import sigma.validation.{DisabledRule, SigmaValidationSettings}
import sigma.validation.ValidationRules.FirstRuleId
import sigmastate.helpers.CompilerTestingCommons
import sigma.serialization.SerializationSpecification

class SigmaValidationSettingsSerializerSpec extends SerializationSpecification with CompilerTestingCommons {

  private def roundtrip(settings: SigmaValidationSettings) = {
    implicit val set: SigmaValidationSettingsSerializer.type = SigmaValidationSettingsSerializer
    roundTripTest(settings)
    roundTripTestWithPos(settings)
  }

  property("ValidationRules.currentSettings round trip") {
    roundtrip(currentSettings)
  }

  property("SigmaValidationSettings round trip") {
    forAll(ruleIdGen, statusGen, MinSuccessful(100)) { (ruleId, status) =>
      val vs = currentSettings.updated(ruleId, status)
      roundtrip(vs)
    }
  }

  property("SigmaValidationSettings equality") {
    val vs = currentSettings
    val vs_copy = currentSettings.updated(FirstRuleId, currentSettings.getStatus(FirstRuleId).get)
    val vs2 = currentSettings.updated(FirstRuleId, DisabledRule)
    vs.equals(vs2) shouldBe false
    vs.equals(vs_copy) shouldBe true
  }
}