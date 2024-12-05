package org.ergoplatform.validation

import sigma.validation.SigmaValidationSettings

trait ValidationSpecification {
  def vs: SigmaValidationSettings = ValidationRules.currentSettings
}
