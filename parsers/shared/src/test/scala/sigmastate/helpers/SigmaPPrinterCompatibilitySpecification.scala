package sigmastate.helpers

import org.scalatest.funsuite.AnyFunSuite

class SigmaPPrinterCompatibilitySpecification extends AnyFunSuite {
  private case class DisplayFixture(label: String, active: Boolean)

  test("nested strings retain Unicode escapes") {
    assert(SigmaPPrint(Some("é")).plainText == "Some(\"\\u00e9\")")
  }

  test("products retain positional arguments without field names") {
    assert(SigmaPPrint(DisplayFixture("value", true)).plainText == "DisplayFixture(\"value\", true)")
  }
}
