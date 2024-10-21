package sigmastate.lang

import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.sdk.Parameter
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sigma.VersionContext
import sigma.ast.ErgoTree.HeaderType
import sigma.ast.{BinAnd, BoolToSigmaProp, ConstantPlaceholder, ErgoTree, FalseLeaf, GT, Height, IntConstant, LT, SBoolean, SInt, SLong, SString, TrueLeaf}
import sigma.compiler.SigmaTemplateCompiler
import sigma.exceptions.TyperException
import sigmastate.CompilerTestsBase
import sigmastate.interpreter.Interpreter.ScriptEnv

class SigmaTemplateCompilerTest extends AnyPropSpec with ScalaCheckPropertyChecks with CompilerTestsBase {
  val templateCompiler = SigmaTemplateCompiler(ErgoAddressEncoder.MainnetNetworkPrefix)

  property("compiles full contract template") {
    val source =
      """/** This is my contracts description.
        |* Here is another line describing what it does in more detail.
        |*
        |* @param p1 describe p1
        |* @param p2 description of the 2nd parameter
        |* which is pretty complex and on many
        |* lines to describe functions
        |* @param param3 the final parameter
        |* @return
        |*/
        |@contract def contractName(p1: Int = 5, p2: String = "default string", param3: Long) = {
        |  sigmaProp(true)
        |}""".stripMargin
    val template = templateCompiler.compile(source)

    template.name shouldBe "contractName"
    template.description shouldBe "This is my contracts description. Here is another line describing what it does in more detail."
    template.parameters should contain theSameElementsInOrderAs IndexedSeq(
      Parameter("p1", "describe p1", 0),
      Parameter("p2", "description of the 2nd parameter which is pretty complex and on many lines to describe functions", 1),
      Parameter("param3", "the final parameter", 2)
    )
    template.constTypes should contain theSameElementsInOrderAs Seq(SInt, SString, SLong)
    template.constValues.get should contain theSameElementsInOrderAs IndexedSeq(
      Some(5),
      Some("default string"),
      None
    )

    checkEquals(template.expressionTree, BoolToSigmaProp(TrueLeaf))
  }

  property("compiles contract template without braces") {
    val source =
      """/** This is my contracts description.
        |* Here is another line describing what it does in more detail.
        |*
        |* @param p1 describe p1
        |* @param p2 description of the 2nd parameter
        |* which is pretty complex and on many
        |
        |* lines to describe functions
        |* @return
        |*/
        |@contract def contractName(p1: Int = 5, p2: String = "default string") = sigmaProp(true)
        |""".stripMargin
    val template = templateCompiler.compile(source)

    template.name shouldBe "contractName"
    template.description shouldBe "This is my contracts description. Here is another line describing what it does in more detail."
    template.parameters should contain theSameElementsInOrderAs IndexedSeq(
      Parameter("p1", "describe p1", 0),
      Parameter("p2", "description of the 2nd parameter which is pretty complex and on many lines to describe functions", 1)
    )
    template.constTypes should contain theSameElementsInOrderAs Seq(SInt, SString)
    template.constValues.get should contain theSameElementsInOrderAs IndexedSeq(
      Some(5),
      Some("default string")
    )

    checkEquals(template.expressionTree, BoolToSigmaProp(TrueLeaf))
  }

  property("uses default value from parameter definition") {
    val source =
      """/**/
        |@contract def contractName(p: Boolean = true) = sigmaProp(p && HEIGHT > 1000)
        |""".stripMargin
    val template = templateCompiler.compile(source)

    template.parameters should contain theSameElementsInOrderAs IndexedSeq(
      Parameter("p", "", 0)
    )
    template.constTypes should contain theSameElementsInOrderAs Seq(SBoolean)
    template.constValues.get should contain theSameElementsInOrderAs IndexedSeq(
      Some(true)
    )

    val expectedExpr = BoolToSigmaProp(BinAnd(ConstantPlaceholder(0, SBoolean), GT(Height, IntConstant(1000))))
    checkEquals(template.expressionTree, expectedExpr)

    val expectedTree = new ErgoTree(
      HeaderType @@ 26.toByte,   // use ErgoTreeUtils to get explanation
      Vector(TrueLeaf),
      Right(BoolToSigmaProp(BinAnd(ConstantPlaceholder(0, SBoolean), GT(Height, IntConstant(1000))))))

    expectedTree.version shouldBe VersionContext.JitActivationVersion
    expectedTree.hasSize shouldBe true
    expectedTree.isConstantSegregation shouldBe true

    // apply using default values declared in the parameters
    checkEquals(
      template.applyTemplate(
        version = Some(VersionContext.JitActivationVersion),
        paramValues = Map.empty
      ),
      expectedTree
    )

    // apply overriding the default values
    checkEquals(
      template.applyTemplate(
        version = Some(VersionContext.JitActivationVersion),
        paramValues = Map("p" -> FalseLeaf)
      ),
      expectedTree.copy(constants = Vector(FalseLeaf))
    )
  }

  property("uses given environment when provided (overriding default value)") {
    val source =
      """/**/
        |@contract def contractName(low: Int = 0, high: Int) = sigmaProp(low < HEIGHT && HEIGHT < high)
        |""".stripMargin
    val template = templateCompiler.compile(source)

    template.parameters should contain theSameElementsInOrderAs IndexedSeq(
      Parameter("low", "", 0),
      Parameter("high", "", 1)
    )
    template.constTypes should contain theSameElementsInOrderAs Seq(SInt, SInt)
    // check parsed default values
    template.constValues.get should contain theSameElementsInOrderAs IndexedSeq(
      Some(0),
      None
    )
    checkEquals(
      template.expressionTree,
      BoolToSigmaProp(
        BinAnd(
          LT(ConstantPlaceholder(0, SInt), Height),
          LT(Height, ConstantPlaceholder(1, SInt))
        )
      )
    )

    // incomplete application (missing `high` parameter)
    assertExceptionThrown(
      template.applyTemplate(
        version = Some(VersionContext.JitActivationVersion),
        paramValues = Map.empty
      ),
      exceptionLike[IllegalArgumentException](
        "requirement failed: value for parameter `high` was not provided while it does not have a default value.")
    )

    val expectedTree = new ErgoTree(
      HeaderType @@ 26.toByte,
      Vector(IntConstant(0), IntConstant(100)),
      Right(
        BoolToSigmaProp(
          BinAnd(LT(ConstantPlaceholder(0, SInt), Height), LT(Height, ConstantPlaceholder(1, SInt)))
        )
      )
    )

    // apply providing the parameter without default value
    checkEquals(
      template.applyTemplate(
        version = Some(VersionContext.JitActivationVersion),
        paramValues = Map("high" -> IntConstant(100))
      ),
      expectedTree
    )

    // apply providing all parameters overriding the default values
    checkEquals(
      template.applyTemplate(
        version = Some(VersionContext.JitActivationVersion),
        paramValues = Map("low" -> IntConstant(10), "high" -> IntConstant(100))
      ),
      expectedTree.copy(constants = Vector(IntConstant(10), IntConstant(100)))
    )
  }

  property("fails when the parameter is not provided") {
    // NOTE: parameter `condition` is not provided */
    val source      =
      """/**/
       |@contract def contractName(low: Int = 0, high: Int) = sigmaProp(low < HEIGHT && HEIGHT < high) && condition
       |""".stripMargin
    assertExceptionThrown(
      templateCompiler.compile(source),
      exceptionLike[TyperException]("Cannot assign type for variable 'condition' because it is not found in env")
    )
  }

}


