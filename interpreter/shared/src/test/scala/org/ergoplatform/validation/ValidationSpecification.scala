package org.ergoplatform.validation

import sigma.validation.SigmaValidationSettings

trait ValidationSpecification {
  implicit val vs: SigmaValidationSettings = ValidationRules.currentSettings
}
