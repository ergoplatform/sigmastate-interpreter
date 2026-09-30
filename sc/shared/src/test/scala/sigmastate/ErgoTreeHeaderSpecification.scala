package sigmastate

import scorex.crypto.hash.Blake2b256
import sigma.ast._
import sigma.ast.syntax._
import sigma.data.TrivialProp
import sigma.exceptions.TyperException
import sigma.interpreter.ContextExtension
import sigma.serialization.{ErgoTreeSerializer, ValueSerializer}
import sigmastate.helpers.{CompilerTestingCommons, ErgoLikeContextTesting, ErgoLikeTestInterpreter}
import sigmastate.helpers.TestingHelpers.createBox
import sigmastate.interpreter.Interpreter
import sigmastate.interpreter.Interpreter.ReductionResult

class ErgoTreeHeaderSpecification extends CompilerTestingCommons with CompilerCrossVersionProps {
  implicit lazy val IR: TestingIRContext = new TestingIRContext
  private lazy val interpreter = new ErgoLikeTestInterpreter

  private def expression(code: String): SValue = compile(Interpreter.emptyEnv, code)

  private def contract(condition: String): ErgoTree = {
    val tree = mkTestErgoTree(expression(s"sigmaProp($condition)").asSigmaProp)
    val restored = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(tree.bytes)
    restored shouldBe tree
    restored
  }

  private def reduce(tree: ErgoTree, values: Array[Byte]*): ReductionResult = {
    val bindings = values.zipWithIndex.map { case (bytes, index) =>
      (index + 1).toByte -> ByteArrayConstant(bytes)
    }.toMap
    val ctx = ErgoLikeContextTesting.dummy(createBox(0, TrueTree), activatedVersionInTests)
      .withExtension(ContextExtension(bindings))
      .withErgoTreeVersion(ergoTreeVersionInTests)
    interpreter.fullReduction(tree, ctx)
  }

  property("stripErgoTreeHeader lowers to the existing slice and preserves serialized bytes") {
    val inputs = Seq(
      "getVar[Coll[Byte]](1).get",
      "SELF.propositionBytes",
      "blake2b256(getVar[Coll[Byte]](1).get)")
    inputs.foreach { input =>
      val helper = expression(s"stripErgoTreeHeader($input)")
      val explicit = expression(s"{ val bytes = $input; bytes.slice(1, bytes.size) }")
      helper shouldBe explicit
      ValueSerializer.serialize(helper) shouldEqual ValueSerializer.serialize(explicit)
      ValueSerializer.deserialize(ValueSerializer.serialize(helper)) shouldBe helper
    }
  }

  property("stripErgoTreeHeader removes exactly one byte from runtime collections") {
    val tree = contract("stripErgoTreeHeader(getVar[Coll[Byte]](1).get) == getVar[Coll[Byte]](2).get")
    val cases = Seq(
      Array.emptyByteArray -> Array.emptyByteArray,
      Array[Byte](0) -> Array.emptyByteArray,
      Array[Byte](16, 1, 2, 3) -> Array[Byte](1, 2, 3),
      Array[Byte](-1, -128, 0, 127) -> Array[Byte](-128, 0, 127),
      // A size field or constant section after the first byte is not normalized away.
      Array[Byte](8, 2, 1, 2) -> Array[Byte](2, 1, 2))
    cases.foreach { case (input, expected) =>
      reduce(tree, input, expected).value shouldBe TrivialProp.TrueProp
    }
    reduce(tree, Array[Byte](0, 1, 2), Array[Byte](0, 1, 2)).value shouldBe TrivialProp.FalseProp
  }

  property("stripErgoTreeHeader comparisons ignore only the first byte") {
    val tree = contract(
      "stripErgoTreeHeader(getVar[Coll[Byte]](1).get) == stripErgoTreeHeader(getVar[Coll[Byte]](2).get)")
    reduce(tree, Array[Byte](0, 1, 2), Array[Byte](16, 1, 2)).value shouldBe TrivialProp.TrueProp
    reduce(tree, Array[Byte](0, 1, 2), Array[Byte](0, 1, 3)).value shouldBe TrivialProp.FalseProp
    reduce(tree, Array[Byte](0, 1, 2), Array[Byte](0, 1)).value shouldBe TrivialProp.FalseProp
  }

  property("stripErgoTreeHeader supports the runtime receipt hash use case") {
    val tree = contract(
      "blake2b256(stripErgoTreeHeader(getVar[Coll[Byte]](1).get)) == getVar[Coll[Byte]](2).get")
    val expectedHash = Blake2b256.hash(Array[Byte](1, 2, 3))
    reduce(tree, Array[Byte](0, 1, 2, 3), expectedHash).value shouldBe TrivialProp.TrueProp
    reduce(tree, Array[Byte](16, 1, 2, 3), expectedHash).value shouldBe TrivialProp.TrueProp
    reduce(tree, Array[Byte](0, 1, 2, 4), expectedHash).value shouldBe TrivialProp.FalseProp
    reduce(tree, Array[Byte](0), Blake2b256.hash(Array.emptyByteArray)).value shouldBe TrivialProp.TrueProp
  }

  property("stripErgoTreeHeader shares a computed argument like an explicit slice") {
    val helper = contract(
      "stripErgoTreeHeader(blake2b256(getVar[Coll[Byte]](1).get)) == getVar[Coll[Byte]](2).get")
    val explicit = contract(
      "{ val bytes = blake2b256(getVar[Coll[Byte]](1).get); bytes.slice(1, bytes.size) == getVar[Coll[Byte]](2).get }")
    helper.bytes shouldEqual explicit.bytes
    val input = Array[Byte](1, 2, 3)
    val expected = Blake2b256.hash(input).drop(1)
    val actual = reduce(helper, input, expected)
    actual.value shouldBe TrivialProp.TrueProp
    actual shouldBe reduce(explicit, input, expected)
  }

  property("stripErgoTreeHeader rejects incorrect argument types and arity") {
    Seq(
      "stripErgoTreeHeader()",
      "stripErgoTreeHeader(1)",
      "stripErgoTreeHeader(Coll(1, 2))",
      "stripErgoTreeHeader(Coll[Byte](), Coll[Byte]())"
    ).foreach { code =>
      an[TyperException] should be thrownBy expression(code)
    }
  }
}
