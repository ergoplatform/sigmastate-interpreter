package sigmastate.eval

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import sigma.ast.{BigIntConstant, ErgoTree, Global, JitCost, MethodCall, SGlobalMethods}
import scorex.util.encode.Base16
import sigma.Extensions.ArrayOps
import sigma.ast.{ByteArrayConstant, IntConstant}
import sigma.crypto.SecP256K1Group
import sigma.data.{CBigInt, CUnsignedBigInt, TrivialProp}
import sigma.eval.SigmaDsl
import sigma.util.Extensions.SigmaBooleanOps
import sigma.util.NBitsUtils

import java.math.BigInteger
import sigma.{ContractsTestkit, SigmaProp}
import sigmastate.interpreter.{CErgoTreeEvaluator, CostAccumulator}
import sigmastate.interpreter.CErgoTreeEvaluator.DefaultProfiler
import sigma.{Box, VersionContext}

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

  test("xor evaluation") {
    val es = CErgoTreeEvaluator.DefaultEvalSettings
    val accumulator = new CostAccumulator(
      initialCost = JitCost(0),
      costLimit = Some(JitCost.fromBlockCost(es.scriptCostLimitInEvaluator)))

    val context = new CContext(
      noInputs.toColl, noHeaders, dummyPreHeader,
      Array[Box]().toColl, Array[Box]().toColl, 0, null, 0, null,
      dummyPubkey.toColl, Colls.emptyColl, null, VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion)

    val evaluator = new CErgoTreeEvaluator(
      context = context,
      constants = ErgoTree.EmptyConstants,
      coster = accumulator, DefaultProfiler, es)

    val msg = Colls.fromArray(Base16.decode("0a101b8c6a4f2e").get)
    VersionContext.withVersions(VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion) {
      val res = MethodCall(Global, SGlobalMethods.xorMethod,
        IndexedSeq(ByteArrayConstant(msg), ByteArrayConstant(msg)), Map.empty)
        .evalTo[sigma.Coll[Byte]](Map.empty)(evaluator)

      res should be(Colls.fromArray(Base16.decode("00000000000000").get))
    }
  }

  /**
    * Checks BigInt.nbits evaluation for SigmaDSL as well as AST interpreter (MethodCall) layers
    */
  test("powHit evaluation") {
    val k = 32
    val msg = Colls.fromArray(Base16.decode("0a101b8c6a4f2e").get)
    val nonce = Colls.fromArray(Base16.decode("000000000000002c").get)
    val hbs = Colls.fromArray(Base16.decode("00000000").get)
    val N = 1024 * 1024

    SigmaDsl.powHit(k, msg, nonce, hbs, N) shouldBe CUnsignedBigInt(new BigInteger("326674862673836209462483453386286740270338859283019276168539876024851191344"))

    val es = CErgoTreeEvaluator.DefaultEvalSettings
    val accumulator = new CostAccumulator(
      initialCost = JitCost(0),
      costLimit = Some(JitCost.fromBlockCost(es.scriptCostLimitInEvaluator)))

    val context = new CContext(
      noInputs.toColl, noHeaders, dummyPreHeader,
      Array[Box]().toColl, Array[Box]().toColl, 0, null, 0, null,
      dummyPubkey.toColl, Colls.emptyColl, null, VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion)

    val evaluator = new CErgoTreeEvaluator(
      context = context,
      constants = ErgoTree.EmptyConstants,
      coster = accumulator, DefaultProfiler, es)

    VersionContext.withVersions(VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion) {
      val res = MethodCall(Global, SGlobalMethods.powHitMethod,
        IndexedSeq(IntConstant(k), ByteArrayConstant(msg), ByteArrayConstant(nonce),
          ByteArrayConstant(hbs), IntConstant(N)), Map.empty)
        .evalTo[sigma.UnsignedBigInt](Map.empty)(evaluator)

      res should be(CUnsignedBigInt(new BigInteger("326674862673836209462483453386286740270338859283019276168539876024851191344")))
    }
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
