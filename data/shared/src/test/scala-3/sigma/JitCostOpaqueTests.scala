package sigma

import scala.compiletime.testing.typeCheckErrors
import sigma.ast.JitCost
import sigma.exceptions.CostLimitException

class JitCostOpaqueTests extends BaseTests {
  test("JitCost exposes its typed API without importing companion extensions") {
    val valid = typeCheckErrors("""
      import sigma.ast.JitCost
      val cost: JitCost = JitCost(30)
      val sum: JitCost = cost + JitCost(12)
      val product: JitCost = cost * 2
      val quotient: JitCost = cost / 2
      val greater: Boolean = cost > JitCost(10)
      val greaterOrEqual: Boolean = cost >= JitCost(30)
      val raw: Int = cost.value
      val block: Int = cost.toBlockCost
      val fromBlock: JitCost = JitCost.fromBlockCost(3)
      val copied: JitCost = cost.copy(value = 42)
      val extracted: Int = cost match { case JitCost(value) => value }
    """)
    val rawToCost = typeCheckErrors("""
      import sigma.ast.JitCost
      val cost: JitCost = 30
    """)
    val costToRaw = typeCheckErrors("""
      import sigma.ast.JitCost
      val raw: Int = JitCost(30)
    """)
    val mixedAddition = typeCheckErrors("""
      import sigma.ast.JitCost
      val cost = JitCost(30) + 1
    """)
    val costMultiplier = typeCheckErrors("""
      import sigma.ast.JitCost
      val cost = JitCost(30) * JitCost(2)
    """)

    assert(valid.isEmpty, valid.mkString("\n"))
    assert(rawToCost.nonEmpty)
    assert(costToRaw.nonEmpty)
    assert(mixedAddition.nonEmpty)
    assert(costMultiplier.nonEmpty)
  }

  test("addition and multiplication retain checked Int arithmetic at the boundaries") {
    (JitCost(Int.MaxValue) + JitCost(0)).value shouldBe Int.MaxValue
    (JitCost(Int.MinValue) + JitCost(0)).value shouldBe Int.MinValue
    (JitCost(Int.MaxValue) * 1).value shouldBe Int.MaxValue
    (JitCost(Int.MinValue) * 1).value shouldBe Int.MinValue
    intercept[ArithmeticException] { JitCost(Int.MaxValue) + JitCost(1) }
    intercept[ArithmeticException] { JitCost(Int.MinValue) + JitCost(-1) }
    intercept[ArithmeticException] { JitCost(Int.MaxValue) * 2 }
    intercept[ArithmeticException] { JitCost(Int.MinValue) * -1 }
  }

  test("division and comparisons retain normal signed Int behavior") {
    (JitCost(-11) / 2).value shouldBe -5
    (JitCost(11) / -2).value shouldBe -5
    (JitCost(Int.MinValue) / -1).value shouldBe Int.MinValue
    intercept[ArithmeticException] { JitCost(1) / 0 }
    (JitCost(Int.MaxValue) > JitCost(Int.MinValue)) shouldBe true
    (JitCost(-1) > JitCost(0)) shouldBe false
    (JitCost(-1) >= JitCost(-1)) shouldBe true
    (JitCost(-1) >= JitCost(0)) shouldBe false
  }

  test("block cost scaling checks multiplication and truncates division toward zero") {
    JitCost.fromBlockCost(214748364).value shouldBe 2147483640
    JitCost.fromBlockCost(-214748364).value shouldBe -2147483640
    JitCost.fromBlockCost(-123).toBlockCost shouldBe -123
    JitCost(19).toBlockCost shouldBe 1
    JitCost(-19).toBlockCost shouldBe -1
    intercept[ArithmeticException] { JitCost.fromBlockCost(214748365) }
    intercept[ArithmeticException] { JitCost.fromBlockCost(-214748365) }
  }

  test("extraction and copying preserve cost values") {
    val cost = JitCost(42)
    val extracted = cost match { case JitCost(value) => value }
    extracted shouldBe 42
    cost.copy() shouldBe cost
    cost.copy(value = -1).value shouldBe -1
    Set(cost, JitCost(42), JitCost(43)).size shouldBe 2
  }

  test("cost-limit diagnostics retain their existing public format") {
    CostLimitException.msgCostLimitError(JitCost(10000001), JitCost(10000000)) shouldBe
      "Estimated execution cost JitCost(10000001) exceeds the limit JitCost(10000000)"
  }
}
