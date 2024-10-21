package sigmastate.eval

import org.ergoplatform.ErgoBox
import sigma.ast.{AND, ConcreteCollection, CreateProveDlog, DecodePoint, EQ, ErgoTree, IntArrayConstant, IntConstant, SSigmaProp, SigmaPropConstant, SigmaPropIsProven, SubstConstants}
import sigmastate.helpers.ContextEnrichingTestProvingInterpreter
import sigmastate.helpers.TestingHelpers._
import sigmastate.interpreter.Interpreter._
import scalan.BaseCtxTests
import sigma.ast.syntax.SigmaPropValue
import sigma.data.ProveDlog
import sigmastate.lang.LangTests
import sigma.util.BenchmarkUtil._
import sigmastate.crypto.DLogProtocol.DLogProverInput
import sigma.serialization.ErgoTreeSerializer.DefaultSerializer

class EvaluationTest extends BaseCtxTests
    with LangTests with ExampleContracts with ErgoScriptTestkit {

  test("constants") {
    val ctx = newErgoContext(height = 1, boxToSpend)
    reduce(emptyEnv, "one", "1", ctx,  1)
    reduce(emptyEnv, "oneL", "1L", ctx, 1L)
  }

  test("operations") {
    val ctx = newErgoContext(height = 1, boxToSpend)
    reduce(emptyEnv, "one+one", "1 + 1", ctx, 2)
    reduce(emptyEnv, "oneL+oneL", "1L - 1L", ctx, 0L)
    reduce(emptyEnv, "one_gt_one", "1 > 1", ctx, false)
    reduce(emptyEnv, "or", "1 > 1 || 2 < 1", ctx, false)
    reduce(emptyEnv, "or2", "1 > 1 || 2 < 1 || 2 > 1", ctx, true)
    reduce(emptyEnv, "or3", "OUTPUTS.size > 1 || OUTPUTS.size <= 1", ctx, true)
    reduce(emptyEnv, "and", "1 > 1 && 2 < 1", ctx, false)
    reduce(emptyEnv, "and2", "1 > 1 && 2 < 1 && 2 > 1", ctx, false)
    reduce(emptyEnv, "and3", "1 == 1 && (2 < 1 || 2 > 1)", ctx, true)
    reduce(emptyEnv, "and4", "OUTPUTS.size > 1 && OUTPUTS.size <= 1", ctx, false)
  }

  test("lazy logical ops") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val pk = prover.dlogSecrets.head.publicImage
    val self = testBox(1, ErgoTree.fromSigmaBoolean(pk), 0, additionalRegisters = Map(ErgoBox.R4 -> IntConstant(10)))
    val ctx = newErgoContext(height = 1, self)
    // guarded register access: existing reg
    reduce(emptyEnv, "lazy1", "SELF.R4[Int].isDefined && SELF.R4[Int].get == 10", ctx, true)
    reduce(emptyEnv, "lazy4", "SELF.R4[Int].isEmpty == false && SELF.R4[Int].get == 10", ctx, true)
    // guarded register access: non-existing reg
    reduce(emptyEnv, "lazy2", "SELF.R5[Int].isDefined && SELF.R5[Int].get == 10", ctx, false)
    reduce(emptyEnv, "lazy5", "SELF.R5[Int].isEmpty == false && SELF.R5[Int].get == 10", ctx, false)

    // guarded register access: reading register if it is defined and another one is undefined
    reduce(emptyEnv, "lazy3", "SELF.R4[Int].isDefined && (SELF.R5[Int].isDefined || SELF.R4[Int].get == 10)", ctx, true)
    reduce(emptyEnv, "lazy6", "SELF.R4[Int].isEmpty == false && (SELF.R5[Int].isEmpty == false || SELF.R4[Int].get == 10)", ctx, true)
  }

  test("context data") {
    val ctx = newErgoContext(height = 100, boxToSpend)
        .withTransaction(tx1)
    reduce(emptyEnv, "height1", "HEIGHT + 1L", ctx, 101)
    reduce(emptyEnv, "height2", "HEIGHT > 1L", ctx, true)
    reduce(emptyEnv, "size", "INPUTS.size + OUTPUTS.size", ctx, 3)
    reduce(emptyEnv, "value", "SELF.value + 1L", ctx, 11L)
  }

  test("lambdas") {
    val ctx = newErgoContext(height = 1, boxToSpend)
    reduce(emptyEnv, "lam3", "{ val f = { (out: Box) => out.value >= 0L }; f(SELF) }", ctx, true)

    // access R5 and call g only if f returns false
    reduce(emptyEnv, "lam4",
      """{
       |  def f(out: Box) = out.value >= 0L;
       |  def g(x: Int) = x < 0;
       |  f(SELF) || g(SELF.R5[Int].get)
       | }""".stripMargin, ctx, true)

    reduce(emptyEnv, "lam5",
      """{
       |  val f = { (out: Box) => out.value >= 0L };
       |  val g = { (xs: Coll[Int]) => xs.size > 0 };
       |  f(SELF) || g(SELF.R5[Coll[Int]].get)
       | }""".stripMargin, ctx, true)
  }
  
  test("SubstConst") {
    def script(pk: ProveDlog): SigmaPropValue =
      AND(EQ(IntConstant(1), IntConstant(1)), SigmaPropIsProven(SigmaPropConstant(pk))).toSigmaProp

    val pk1 = DLogProverInput.random().publicImage
    val pk2 = DLogProverInput.random().publicImage
    val script1 = script(pk1)
    val script2 = script(pk2)
    val inputBytes = DefaultSerializer.serializeErgoTree(mkTestErgoTree(script1))
    val positions = IntArrayConstant(Array[Int](2))
    // in ergo we have only byte array of a serialized group element
    val newVals = ConcreteCollection(Array[SigmaPropValue](CreateProveDlog(DecodePoint(pk2.pkBytes))), SSigmaProp)

    val expectedBytes = DefaultSerializer.serializeErgoTree(mkTestErgoTree(script2))
    val ctx = newErgoContext(height = 1, boxToSpend)
    reduce(emptyEnv, "SubstConst",
      EQ(SubstConstants(inputBytes, positions, newVals), expectedBytes),
      ctx,
      true)
  }
}
