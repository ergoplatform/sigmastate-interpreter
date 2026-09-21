package sigma.compiler.ir

import sigma.VersionContext
import sigma.VersionContext.V6SoftForkVersion
import sigma.ast._
import sigma.ast.SCollection.SByteArray
import sigma.ast.syntax.SValue
import sigma.serialization.OpCodes.{MaxCode, MinCode}
import sigmastate.helpers.CompilerTestingCommons
import sigmastate.helpers.SigmaPPrint

/** Pins the ErgoTree of the lowering rows that no language-specification script reaches, so the
  * rows are covered by a test that fails if they change. */
class LoweringRowsSpec extends CompilerTestingCommons {
  implicit lazy val IR: TestingIRContext = new TestingIRContext

  private def check(row: String, code: String, expected: SValue): Unit = withClue(s"$row: ") {
    val actual = VersionContext.withVersions(V6SoftForkVersion, V6SoftForkVersion)(compile(Map(), code))
    if (actual != expected) SigmaPPrint.pprintln(actual, width = 100)
    actual shouldBe expected
  }

  property("BigInt min and max (OpCallee(ArithOp) rows)") {
    val a = ValUse(1, SBigInt)
    val b = ValUse(2, SBigInt)
    check("min/max",
      "{ val a = getVar[BigInt](1).get; val b = getVar[BigInt](2).get; sigmaProp(min(a, b) == max(a, b)) }",
      BlockValue(
        Vector(
          ValDef(1, OptionGet(GetVar(1.toByte, SOption(SBigInt)))),
          ValDef(2, OptionGet(GetVar(2.toByte, SOption(SBigInt))))),
        BoolToSigmaProp(EQ(ArithOp(a, b, MinCode), ArithOp(a, b, MaxCode)))))
  }

  property("Global.xor (MethodCallee(xorMethod) row)") {
    check("Global.xor",
      "{ sigmaProp(Global.xor(getVar[Coll[Byte]](1).get, getVar[Coll[Byte]](2).get).size > 0) }",
      BoolToSigmaProp(GT(
        SizeOf(Xor(OptionGet(GetVar(1.toByte, SOption(SByteArray))), OptionGet(GetVar(2.toByte, SOption(SByteArray))))),
        IntConstant(0))))
  }
}
