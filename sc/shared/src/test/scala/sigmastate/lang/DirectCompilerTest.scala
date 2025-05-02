package sigmastate.lang

import sigma.ast._
import sigma.ast.syntax.SValue
import sigma.compiler.CompilerSettings
import sigma.compiler.ir.IRContext
import sigma.exceptions.TyperException
import sigma.serialization.ValueCodes.OpCode
import sigma.serialization.generators.ObjectGenerators
import sigmastate._
import sigmastate.helpers.{CompilerTestingCommons, SigmaPPrint}
import sigmastate.interpreter.Interpreter

class DirectCompilerTest extends CompilerTestingCommons with LangTests with ObjectGenerators {

  override def compilerSettingsInTests: CompilerSettings =
    super.compilerSettingsInTests.copy(
      enableOptimizations = false
    )

  implicit lazy val IR: TestContext with IRContext =
    new TestContext with IRContext

  def checkEquals(input: String, expected: SValue): Unit = {
    val result = compiler.compile(Interpreter.emptyEnv, input).buildTree
    if (expected != result) {
      SigmaPPrint.pprintln(result, width = 100)
    }
    result shouldBe expected
  }

  property("compile simple expressions") {
    checkEquals("1", IntConstant(1))
    checkEquals("1 + 2", Plus(IntConstant(1), IntConstant(2)))
    checkEquals("1 + 2 * 3 + 4", Plus(Plus(IntConstant(1), Multiply(IntConstant(2), IntConstant(3))), IntConstant(4)))
    checkEquals("1 + 2 * (3 + 4)", Plus(IntConstant(1), Multiply(IntConstant(2), Plus(IntConstant(3), IntConstant(4)))))
  }

  property("compile simple expressions with variables") {
    checkEquals("{val x = 1; x}",
      BlockValue(Array(ValDef(1, List(), IntConstant(1))), ValUse(1, SInt)))
    checkEquals("{val x = 1; x + x}",
      BlockValue(
        Array(ValDef(1, List(), IntConstant(1))),
        ArithOp(ValUse(1, SInt), ValUse(1, SInt), OpCode @@ (-102.toByte))
      ))
    checkEquals("{val x = 1; val y = 2; x + y}",
      BlockValue(
        Array(ValDef(1, List(), IntConstant(1)), ValDef(2, List(), IntConstant(2))),
        ArithOp(ValUse(1, SInt), ValUse(2, SInt), OpCode @@ (-102.toByte))
      )
    )
  }

  property("compile simple expressions with variables and blocks") {
    checkEquals("{val x = 1; {val y = 2; x + y}}",
      BlockValue(
        Array(ValDef(1, List(), IntConstant(1))),
        BlockValue(
          Array(ValDef(2, List(), IntConstant(2))),
          ArithOp(ValUse(1, SInt), ValUse(2, SInt), OpCode @@ (-102.toByte))
        )
      )
    )
    checkEquals("{val x = 1; {val y = 2; x + y} + x}",
      BlockValue(
        Array(ValDef(1, List(), IntConstant(1))),
        ArithOp(
          BlockValue(
            Array(ValDef(2, List(), IntConstant(2))),
            ArithOp(ValUse(1, SInt), ValUse(2, SInt), OpCode @@ (-102.toByte))
          ),
          ValUse(1, SInt),
          OpCode @@ (-102.toByte)
        )
      )
    )
  }

  property("negative tests") {
    assertExceptionThrown(
      checkEquals("{val x = 1; {val x = 2; x + 1} + x}",
        null
      ),
      exceptionLike[TyperException]("Variable x already defined")
    )
  }

  property("compile simple expressions with functions") {
    checkEquals("{val f = {(x: Int) => x + 1}; f(1)}",
      BlockValue(
        Array(
          ValDef(1, List(),
            FuncValue(Array((1, SInt)), ArithOp(ValUse(1, SInt), IntConstant(1), OpCode @@ (-102.toByte)))
          )
        ),
        Apply(ValUse(1, SFunc(Array(SInt), SInt, List())), Array(IntConstant(1)))
      )
    )
    checkEquals("{val f = { (x: Int) => x + 1}; f(f(1))}",
      BlockValue(
        Array(
          ValDef(1, List(),
            FuncValue(Array((1, SInt)), ArithOp(ValUse(1, SInt), IntConstant(1), OpCode @@ (-102.toByte)))
          )
        ),
        Apply(
          ValUse(1, SFunc(Array(SInt), SInt, List())),
          Array(Apply(ValUse(1, SFunc(Array(SInt), SInt, List())), Array(IntConstant(1))))
        )
      )
    )
    checkEquals("{val f = { (x: Int) => {(y: Int) => x + y}}; f(1)(2) }",
      BlockValue(
        Array(
          ValDef( 1, List(),
            FuncValue(
              Array((1, SInt)),
              FuncValue(
                Array((2, SInt)),
                ArithOp(ValUse(1, SInt), ValUse(2, SInt), OpCode @@ (-102.toByte))
              )
            )
          )
        ),
        Apply(
          Apply(
            ValUse(1, SFunc(Array(SInt), SFunc(Array(SInt), SInt, List()), List())),
            Array(IntConstant(1))
          ),
          Array(IntConstant(2))
        )
      )
    )
  }
}
