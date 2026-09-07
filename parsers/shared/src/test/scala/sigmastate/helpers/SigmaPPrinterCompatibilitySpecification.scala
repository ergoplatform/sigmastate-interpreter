package sigmastate.helpers

import org.scalatest.funsuite.AnyFunSuite
import sigma.ast.{FixedCost, FixedCostItem, JitCost, NamedDesc, PerItemCost, SeqCostItem}
import sigma.eval.GivenCost

class SigmaPPrinterCompatibilitySpecification extends AnyFunSuite {
  private case class DisplayFixture(label: String, active: Boolean)

  test("nested strings retain Unicode escapes") {
    assert(SigmaPPrint(Some("é")).plainText == "Some(\"\\u00e9\")")
  }

  test("products retain positional arguments without field names") {
    assert(SigmaPPrint(DisplayFixture("value", true)).plainText == "DisplayFixture(\"value\", true)")
  }

  test("fixed costs retain JitCost constructors in nested trace items") {
    val cost = FixedCost(JitCost(10))
    assert(SigmaPPrint(cost).plainText == "FixedCost(JitCost(10))")
    assert(SigmaPPrint(FixedCostItem(NamedDesc("test"), cost), width = 150).plainText ==
      "FixedCostItem(NamedDesc(\"test\"), FixedCost(JitCost(10)))")
  }

  test("per-item costs retain JitCost constructors in nested trace items") {
    val cost = PerItemCost(JitCost(10), JitCost(2), 3)
    assert(SigmaPPrint(cost).plainText == "PerItemCost(JitCost(10), JitCost(2), 3)")
    assert(SigmaPPrint(SeqCostItem(NamedDesc("test"), cost, 4), width = 150).plainText ==
      "SeqCostItem(NamedDesc(\"test\"), PerItemCost(JitCost(10), JitCost(2), 3), 4)")
  }

  test("given costs retain their JitCost constructor") {
    assert(SigmaPPrint(GivenCost(JitCost(10))).plainText == "GivenCost(JitCost(10), None)")
  }
}
