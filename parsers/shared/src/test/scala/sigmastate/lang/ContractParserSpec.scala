package sigmastate.lang

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sigma.ast.SType.AnyOps
import sigma.ast._

class ContractParserSpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers {
  property("parses docstring") {
    val doc =
      """/** This is my contracts description.
        |* Here is another line describing what it does in more detail.
        |*
        |* @param p1 describe p1
        |* @param p2 description of the 2nd parameter
        |* which is pretty complex and on many
        |* lines to describe functions
        |* @param p3 the final parameter
        |* @return
        |*/""".stripMargin
    val contractDoc = ContractParser.Docs.parse(doc).get.value

    contractDoc.description shouldBe "This is my contracts description. Here is another line describing what it does in more detail."
    contractDoc.params should contain theSameElementsInOrderAs Seq(
      ParameterDoc("p1", "describe p1"),
      ParameterDoc("p2", "description of the 2nd parameter which is pretty complex and on many lines to describe functions"),
      ParameterDoc("p3", "the final parameter")
    )
  }

  property("parses contract signature") {
    val source = "@contract def contractName(p1: Int = 5, p2: String = \"default string\", param3: Long)"
    val parsed = ContractParser.Signature.parse(source).get.value

    parsed.name shouldBe "contractName"
    parsed.params should contain theSameElementsInOrderAs Seq(
      ContractParam("p1", SInt, Some(5.asWrappedType)),
      ContractParam("p2", SString, Some("default string".asWrappedType)),
      ContractParam("param3", SLong, None)
    )
  }

  property("parses contract signature no params") {
    val source = "@contract def contractName()"
    val parsed = ContractParser.Signature.parse(source).get.value

    parsed.name shouldBe "contractName"
    parsed.params should contain theSameElementsInOrderAs Seq()
  }
}
