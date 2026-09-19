package sigma.compiler.ir

import sigma.Colls
import sigma.ast._
import sigma.ast.syntax.SValue
import sigmastate.helpers.CompilerTestingCommons
import sigmastate.helpers.SigmaPPrint
import sigmastate.interpreter.Interpreter.ScriptEnv

/** Pins the ErgoTree produced when each IR rewrite rule that is reachable from ErgoScript
  * fires. A rule that stops firing changes the tree here before the language suites notice.
  * Rule locations refer to `sigma.compiler.ir.IRContext.rewriteDef` and
  * `sigma.compiler.ir.GraphBuilding.rewriteDef`.
  */
class IRRewriteRulesSpec extends CompilerTestingCommons {
  implicit lazy val IR: TestingIRContext = new TestingIRContext

  private def check(rule: String, code: String, expected: SValue, env: ScriptEnv = Map()): Unit = withClue(s"$rule: ") {
    val actual = compile(env, code)
    if (actual != expected) SigmaPPrint.pprintln(actual, width = 100)
    actual shouldBe expected
  }

  val xs = "getVar[Coll[Int]](1).get"
  val xsVar = OptionGet(GetVar(1.toByte, SOption(SCollection(SInt))))
  val pk = "getVar[SigmaProp](2).get"
  val pkVar = OptionGet(GetVar(2.toByte, SOption(SSigmaProp)))
  val heightGt1 = GT(Height, IntConstant(1))

  property("Coll rules (IRContext.rewriteDef)") {
    check("length(map(xs, f)) => length(xs)",
      s"{ $xs.map({ (x: Int) => x + 1 }).size > 0 }",
      GT(SizeOf(xsVar), IntConstant(0)))
    check("length(fromItems(items)) => items.length (then folded)",
      "{ Coll(1, 2, 3).size > 0 }",
      TrueLeaf)
    check("length(DslConst(coll)) => coll.length (then folded)",
      "{ xs.size > 0 }",
      TrueLeaf,
      env = Map("xs" -> Colls.fromItems(1, 2, 3)))
    check("map(xs, identity) => xs",
      s"{ $xs.map({ (x: Int) => x }).size > 0 }",
      GT(SizeOf(xsVar), IntConstant(0)))
    check("map(map(xs, f), g) => map(xs, g . f) (then length(map) => length)",
      s"{ $xs.map({ (x: Int) => x + 1 }).map({ (y: Int) => y * 2 }).size > 0 }",
      GT(SizeOf(xsVar), IntConstant(0)))
  }

  property("sigma rules (GraphBuilding.rewriteDef)") {
    // `pk && bool` is typed as BinAnd(SigmaPropIsProven(pk), bool) (SigmaTyper.scala:419), which is
    // the shape the isValid/sigmaProp rules consume; there is no explicit `.isProven` in ErgoScript.
    check("isValid(sigmaProp(b)) => b",
      "{ sigmaProp(HEIGHT > 1) && HEIGHT > 2 }",
      BinAnd(heightGt1, GT(Height, IntConstant(2))))
    check("sigmaProp(isValid(p)) => p",
      s"{ sigmaProp($pk && HEIGHT > 1) }",
      SigmaAnd(Seq(pkVar, BoolToSigmaProp(heightGt1))))
    check("isValid(l) && bool => (l && sigmaProp(bool)).isValid",
      s"{ $pk && HEIGHT > 1 }",
      SigmaAnd(Seq(pkVar, BoolToSigmaProp(heightGt1))))
    check("bool && isValid(r) => (sigmaProp(bool) && r).isValid",
      s"{ HEIGHT > 1 && $pk }",
      SigmaAnd(Seq(BoolToSigmaProp(heightGt1), pkVar)))
    check("isValid(l) || bool => (l || sigmaProp(bool)).isValid",
      s"{ $pk || HEIGHT > 1 }",
      SigmaOr(Seq(pkVar, BoolToSigmaProp(heightGt1))))
    check("bool || isValid(r) => (sigmaProp(bool) || r).isValid",
      s"{ HEIGHT > 1 || $pk }",
      SigmaOr(Seq(BoolToSigmaProp(heightGt1), pkVar)))
    check("allOf(single) => single",
      "{ allOf(Coll(HEIGHT > 1)) }",
      heightGt1)
    check("anyOf(single sigma) => anyZK(single) => single",
      s"{ anyOf(Coll($pk)) }",
      pkVar)
    check("allOf(single sigma) => allZK(single) => single",
      s"{ allOf(Coll($pk)) }",
      pkVar)
    check("allOf(consts) => folded const",
      "{ allOf(Coll(true, true)) }",
      TrueLeaf)
    check("anyOf(consts) => folded const",
      "{ anyOf(Coll(false, true)) }",
      TrueLeaf)
    check("allOf(bools ++ sigmas) => sigmaProp(allOf(bools)) && allZK(sigmas)",
      s"{ allOf(Coll($pk, HEIGHT > 1)) }",
      SigmaAnd(Seq(BoolToSigmaProp(heightGt1), pkVar)))
    check("anyOf(bools ++ sigmas) => sigmaProp(anyOf(bools)) || anyZK(sigmas)",
      s"{ anyOf(Coll($pk, HEIGHT > 1)) }",
      SigmaOr(Seq(BoolToSigmaProp(heightGt1), pkVar)))
  }
}
