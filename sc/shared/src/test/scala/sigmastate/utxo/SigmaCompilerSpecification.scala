package sigmastate.utxo

import sigma.ast.{GE, ModQ, SType}
import sigma.data.CAnyValue
import sigma.ast._
import sigma.ast.syntax.{GetVarInt, OptionValueOps}
import sigmastate.helpers.CompilerTestingCommons
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigma.ast.syntax._

/**
  * Specification for compile function
  */
class SigmaCompilerSpecification extends CompilerTestingCommons {
  implicit lazy val IR: TestingIRContext = new TestingIRContext

  private def compile(code: String, env: ScriptEnv = Map()): Value[SType] = compile(env, code)

  property(">= compile") {
    val elementId = 1: Byte
    val env = Map("elementId" -> CAnyValue(elementId))
    val propTree = GE(GetVarInt(elementId).get, IntConstant(120))
    val propComp = compile(env,
      """{
        |  getVar[Int](elementId).get >= 120
        |}""".stripMargin).asBoolValue
    propComp shouldBe propTree
  }

  // TODO https://github.com/ScorexFoundation/sigmastate-interpreter/issues/327
  ignore("modular arithmetic ops: ModQ") {
    compile("10.toBigInt.modQ") shouldEqual ModQ(BigIntConstant(10))
  }

}
