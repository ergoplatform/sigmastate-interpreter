package sigmastate.eval

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import sigma.ast.{BigIntConstant, ErgoTree, Global, JitCost, MethodCall, SBigIntMethods, SGlobalMethods}
import sigma.crypto.SecP256K1Group
import sigma.data.{CBigInt, TrivialProp}
import sigma.eval.SigmaDsl
import sigma.util.Extensions.SigmaBooleanOps
import sigma.util.NBitsUtils

import java.math.BigInteger
import sigma.{ContractsTestkit, SigmaProp}
import sigmastate.interpreter.{CErgoTreeEvaluator, CostAccumulator}
import sigmastate.interpreter.CErgoTreeEvaluator.DefaultProfiler

import scala.language.implicitConversions

class BasicOpsTests extends AnyFunSuite with ContractsTestkit with Matchers {

  implicit def boolToSigma(b: Boolean): SigmaProp = TrivialProp(b).toSigmaProp

  test("atLeast") {
    val props = Colls.fromArray(Array[SigmaProp](false, true, true, false))

    // border cases
    SigmaDsl.atLeast(0, props).isValid shouldBe true
    SigmaDsl.atLeast(5, props).isValid shouldBe false

    // normal cases
    SigmaDsl.atLeast(1, props).isValid shouldBe true
    SigmaDsl.atLeast(2, props).isValid shouldBe true
    SigmaDsl.atLeast(3, props).isValid shouldBe false
  }

  // TODO this is valid for BigIntModQ type (https://github.com/ScorexFoundation/sigmastate-interpreter/issues/554)
  ignore("ByteArrayToBigInt should always produce a positive big int") {
    SigmaDsl.byteArrayToBigInt(collection[Byte](-1)).signum shouldBe 1
  }

  // TODO this is valid for BigIntModQ type (https://github.com/ScorexFoundation/sigmastate-interpreter/issues/554)
  ignore("ByteArrayToBigInt should always produce big int less than dlog group order") {
    val groupOrder = SecP256K1Group.ctx.order

    SigmaDsl.byteArrayToBigInt(
      Colls.fromArray(groupOrder.subtract(BigInteger.ONE).toByteArray)
    ).compareTo(SigmaDsl.BigInt(BigInteger.ONE)) shouldBe 1

    SigmaDsl.byteArrayToBigInt(
      Colls.fromArray(groupOrder.toByteArray)
    ).compareTo(SigmaDsl.BigInt(BigInteger.ONE)) shouldBe 1

    an [RuntimeException] should be thrownBy
      SigmaDsl.byteArrayToBigInt(Colls.fromArray(groupOrder.add(BigInteger.ONE).toByteArray))

    an [RuntimeException] should be thrownBy
      SigmaDsl.byteArrayToBigInt(Colls.fromArray(Array.fill[Byte](500)(1)))
  }

  test("Coll.append")  {
    val c1 = collection[Byte](1, 2)
    val c2 = collection[Byte](3, 4)
    c1.append(c2).toArray shouldBe Array[Byte](1, 2, 3, 4)
  }

  test("box.creationInfo._1 is Int") {
    val box = newAliceBox(100)
    box.creationInfo._1 shouldBe a [Integer]
  }

  /**
    * Checks BigInt.nbits evaluation for SigmaDSL as well as AST interpreter (MethodCall) layers
    */
  test("nbits evaluation") {
    SigmaDsl.encodeNbits(CBigInt(BigInteger.valueOf(0))) should be
      (NBitsUtils.encodeCompactBits(0))

    val es = CErgoTreeEvaluator.DefaultEvalSettings
    val accumulator = new CostAccumulator(
      initialCost = JitCost(0),
      costLimit = Some(JitCost.fromBlockCost(es.scriptCostLimitInEvaluator)))
    val evaluator = new CErgoTreeEvaluator(
      context = null,
      constants = ErgoTree.EmptyConstants,
      coster = accumulator, DefaultProfiler, es)

    val res = MethodCall(Global, SGlobalMethods.encodeNBitsMethod, IndexedSeq(BigIntConstant(BigInteger.valueOf(0))), Map.empty)
        .evalTo[Long](Map.empty)(evaluator)

    res should be (NBitsUtils.encodeCompactBits(0))

  }

}
