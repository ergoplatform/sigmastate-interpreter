package sigmastate.eval

import scala.util.Success
import sigmastate.{SInt, AvlTreeData, SLong, SType}
import sigmastate.Values.{LongConstant, Constant, EvaluatedValue, SValue, TrueLeaf, SigmaPropConstant, IntConstant, BigIntArrayConstant}
import org.ergoplatform.{ErgoLikeContext, ErgoLikeTransaction, ErgoBox}
import sigmastate.utxo.CostTable
import special.sigma.{TestBox => DTestBox, ContractsTestkit, AnyValue, Box => DBox, TestContext => DTestContext, SigmaContract => DContract, Context => DContext}

import scalan.BaseCtxTests
import sigmastate.lang.{TransformingSigmaBuilder, LangTests}
import org.scalatest.TryValues._
import sigmastate.helpers.ErgoLikeProvingInterpreter
import sigmastate.interpreter.ContextExtension

trait ErgoScriptTestkit extends ContractsTestkit with LangTests { self: BaseCtxTests =>

  lazy val IR: TestContext with Evaluation = new TestContext with Evaluation {
    import TestSigmaDslBuilder._

    override val sigmaDslBuilder = RTestSigmaDslBuilder()
    override val builder = TransformingSigmaBuilder

    beginPass(new DefaultPass("mypass", Pass.defaultPassConfig.copy(constantPropagation = false)))

    override val sigmaDslBuilderValue = new special.sigma.TestSigmaDslBuilder()
    override val costedBuilderValue = new special.collection.ConcreteCostedBuilder()
    override val monoidBuilderValue = new special.collection.MonoidBuilderInst()
  }

  import IR._
  import Context._
  import Liftables._
  import WArray._
  import WOption._
  import ColBuilder._
  import Context._
  import Col._
  import SigmaProp._
  import CostedCol._
  import WBigInteger._
  import WECPoint._
  import ProveDlogEvidence._
  import ProveDHTEvidence._
  import sigmastate.serialization.OpCodes._
  import SType.AnyOps

  type EsEnv = Map[String, Any]

  val noEnv: EsEnv = Map()

  def newErgoContext(height: Long, boxToSpend: ErgoBox, extension: Map[Byte, EvaluatedValue[SType]] = Map()): ErgoLikeContext = {
    val tx1 = ErgoLikeTransaction(IndexedSeq(), IndexedSeq())
    val ergoCtx = ErgoLikeContext(
      currentHeight = height,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx1,
      self = boxToSpend,
      extension = ContextExtension(extension))
    ergoCtx
  }


  val boxA1 = newAliceBox(1, 100)
  val boxA2 = newAliceBox(2, 200)

  implicit class ErgoBoxOps(ebox: ErgoBox) {
    def toTestBox(isCost: Boolean): DBox = {
      val rs = regs(ebox.additionalRegisters.map {
        case (k, Constant(arr: Array[a], tpeA)) =>
          (k.number -> Cols.fromArray(arr))
        case (k,v) =>
          (k.number -> v)
      })
      new CostingBox(IR, Cols.fromArray(ebox.id), ebox.value, Cols.fromArray(ebox.propositionBytes), noBytes, noBytes, rs, isCost)
    }
  }

  implicit class ErgoLikeContextOps(ergoCtx: ErgoLikeContext) {
    def toTestContext(isCost: Boolean): DContext = {
      val inputs = ergoCtx.boxesToSpend.toArray.map(_.toTestBox(isCost))
      val outputs = ergoCtx.spendingTransaction.outputs.toArray.map(_.toTestBox(isCost))
      val vars = contextVars(ergoCtx.extension.values)
      new CostingDataContext(IR, inputs, outputs, ergoCtx.currentHeight, ergoCtx.self.toTestBox(isCost), emptyAvlTree, vars.arr, isCost)
    }
//    def withInputs(inputs: ErgoBox*) =
//      new ErgoLikeContext(
//        ergoCtx.currentHeight,
//        ergoCtx.lastBlockUtxoRoot,
//        ergoCtx.boxesToSpend, ergoCtx.spendingTransaction, ergoCtx.self, ergoCtx.extension)
//    def withOutputs(outputs: ErgoBox*) =
//      new ErgoLikeContext(
//        ergoCtx.currentHeight,
//        ergoCtx.lastBlockUtxoRoot,
//        ergoCtx.boxesToSpend, ergoCtx.spendingTransaction, ergoCtx.self, ergoCtx.extension)
  }

  def contract(canOpen: DContext => Boolean) = new NoEnvContract(canOpen)

  lazy val dsl = sigmaDslBuilder
  lazy val bigSym = liftConst(big)
  lazy val n1Sym = liftConst(n1)

  val timeout = 100L
  val minToRaise = 1000L
  val backerPubKeyId = 1.toByte
  val projectPubKeyId = 2.toByte
  val backerProver = new ErgoLikeProvingInterpreter
  val projectProver = new ErgoLikeProvingInterpreter
  val backerPubKey = backerProver.dlogSecrets.head.publicImage
  val projectPubKey = projectProver.dlogSecrets.head.publicImage
  val ctxVars = contextVars(Map(
    backerPubKeyId -> backerPubKey,
    projectPubKeyId -> projectPubKey,
    3.toByte -> bigIntArr1
  )).arr

  val boxToSpend = ErgoBox(10, TrueLeaf,
    additionalRegisters = Map(ErgoBox.R4 -> BigIntArrayConstant(bigIntArr1)))
  val tx1Output1 = ErgoBox(minToRaise, projectPubKey)
  val tx1Output2 = ErgoBox(1, projectPubKey)
  val tx1 = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(tx1Output1, tx1Output2))
  val ergoCtx = ErgoLikeContext(
    currentHeight = timeout - 1,
    lastBlockUtxoRoot = AvlTreeData.dummy,
    boxesToSpend = IndexedSeq(),
    spendingTransaction = tx1,
    self = boxToSpend,
    extension = ContextExtension(Map(
      backerPubKeyId -> SigmaPropConstant(backerPubKey),
      projectPubKeyId -> SigmaPropConstant(projectPubKey),
      3.toByte -> BigIntArrayConstant(bigIntArr1)
    )))

  case class Result[+T](calc: Option[T], cost: Option[Int], size: Option[Long])
  object Result {
    def Ignore = Result(None, None, None)
    def apply[T](calc: T): Result[T] = Result[T](Some(calc), None, None)
    def apply[T](calc: T, cost: Int, size: Long): Result[T] = Result[T](Some(calc), Some(cost), Some(size))
  }
  def NoResult[T] = Result[T](None, None, None)
  case class EsTestCase[T](
      name: String,  // name of the test case, used in forming file names in test-out directory
      env: EsEnv,
      script: String,
      ergoCtx: Option[ErgoLikeContext] = None,
      testContract: Option[DContext => T] = None,
      expectedCalc: Option[Rep[Context] => Rep[T]] = None,
      expectedCost: Option[Rep[Context] => Rep[Int]] = None,
      expectedSize: Option[Rep[Context] => Rep[Long]] = None,
      expectedTree: Option[SValue] = None,
      expectedResult: Result[T] = NoResult,
      printGraphs: Boolean = true,
      measureTime: Boolean = false)
  {
    lazy val expectedCalcF = expectedCalc.map(fun(_))
    lazy val expectedCostF = expectedCost.map(fun(_))
    lazy val expectedSizeF = expectedSize.map(fun(_))

    def checkExpected[T](block: => T, expected: Option[T], messageFmt: String) = {
      if (expected.isDefined) {
        val x = block
        x shouldBe expected.get
      }
//          String.format(messageFmt, x.asInstanceOf[AnyRef], expected.get.asInstanceOf[AnyRef]))
    }

    def pairify(xs: Seq[Sym]): Sym = xs match {
      case Seq(x) => x
      case Seq(a, b) => Pair(a, b)
      case _ => Pair(xs.head, pairify(xs.tail))
    }

    def doCosting: Rep[(Context => T, (Context => Int, Context => Long))] = {
      val costed = cost(env, script)
      val res @ Tuple(calcF, costF, sizeF) = split(costed.asRep[Context => Costed[T]])
      if (printGraphs) {
        val str = struct(
          "calc" -> calcF,
          "cost" -> costF,
          "size" -> sizeF
        )
        val strExp = struct(
          expectedCalcF.map("calc" -> _).toSeq ++
          expectedCostF.map("cost" -> _).toSeq ++
          expectedSizeF.map("size" -> _).toSeq
        )
        val graphs = Seq(str, strExp)
        emit(name, graphs:_*)
      }
      checkExpected(calcF, expectedCalcF, "Calc function actual: %s, expected: %s")
      checkExpected(costF, expectedCostF, "Cost function actual: %s, expected: %s")
      checkExpected(sizeF, expectedSizeF, "Size function actual: %s, expected: %s")
      res
    }

    def doReduce(): Unit = {
      val Tuple(calcF, costF, sizeF) = doCosting
      verifyCostFunc(costF) shouldBe Success(())
      verifyIsValid(calcF) shouldBe Success(())

      checkExpected(IR.buildTree(calcF.asRep[Context => SType#WrappedType]), expectedTree,
        "Compiled Tree actual: %s, expected: %s")

      if (ergoCtx.isDefined) {
        val calcCtx = ergoCtx.get.toTestContext(isCost = false)
        val testContractRes = testContract.map(_(calcCtx))
        testContractRes.foreach { res =>
          checkExpected(res, expectedResult.calc, "Test Contract actual: %s, expected: %s")
        }

        // check cost
        val costCtx = ergoCtx.get.toTestContext(isCost = true)
        val costFun = IR.compile[SInt.type](getDataEnv, costF)
        val IntConstant(estimatedCost) = costFun(costCtx)
        checkExpected(estimatedCost, expectedResult.cost,
          "Cost evaluation: estimatedCost = %s, expectedResult.cost = %s")
        (estimatedCost < CostTable.ScriptLimit) shouldBe true

        // check size
        val sizeFun = IR.compile[SLong.type](getDataEnv, sizeF)
        val LongConstant(estimatedSize) = sizeFun(costCtx)
        checkExpected(estimatedSize, expectedResult.size,
          "Size evaluation: estimatedSize = %s, expectedResult.size: %s"
        )

        // check calc
        val valueFun = IR.compile[SType](getDataEnv, calcF.asRep[Context => SType#WrappedType])
        val res = valueFun(calcCtx) match {
          case Constant(res: T @unchecked, _) => res
          case v => v
        }
        checkExpected(res, expectedResult.calc,
          "Calc evaluation:\n value = %s,\n expectedResult.calc: %s\n")
      }
    }
  }

  def Case[T](env: EsEnv, name: String, script: String, ctx: ErgoLikeContext,
      calc: Rep[Context] => Rep[T],
      cost: Rep[Context] => Rep[Int],
      size: Rep[Context] => Rep[Long],
      tree: SValue,
      result: Result[T]) =
    EsTestCase[T](name, env, script, Option(ctx), None,
      Option(calc), Option(cost), Option(size),
      Option(tree), result)

  def checkAll[T](env: EsEnv, name: String, script: String, ergoCtx: ErgoLikeContext,
      calc: Rep[Context] => Rep[T],
      cost: Rep[Context] => Rep[Int],
      size: Rep[Context] => Rep[Long],
      tree: SValue,
      result: Result[T]): Unit =
  {
    val tcase = Case[T](env, name, script, ergoCtx, calc, cost, size, tree, result)
    tcase.doReduce()
  }

  def checkInEnv[T](env: EsEnv, name: String, script: String,
      expectedCalc: Rep[Context] => Rep[T],
      expectedCost: Rep[Context] => Rep[Int] = null,
      expectedSize: Rep[Context] => Rep[Long] = null
  ): Rep[(Context => T, (Context => Int, Context => Long))] =
  {
    val tc = EsTestCase[T](name, env, script, None, None,
      Option(expectedCalc),
      Option(expectedCost),
      Option(expectedSize), expectedTree = None, expectedResult = NoResult, printGraphs = true )
    val res = tc.doCosting
    res
  }

  def check[T](name: String, script: String,
      expectedCalc: Rep[Context] => Rep[T],
      expectedCost: Rep[Context] => Rep[Int] = null,
      expectedSize: Rep[Context] => Rep[Long] = null
      ): Rep[(Context => T, (Context => Int, Context => Long))] =
  {
    checkInEnv(Map(), name, script, expectedCalc, expectedCost, expectedSize)
  }

  def reduce(env: EsEnv, name: String, script: String, ergoCtx: ErgoLikeContext, expectedResult: Any): Unit = {
    val tcase = EsTestCase[SType#WrappedType](name, env, script, Some(ergoCtx), expectedResult = Result(expectedResult.asInstanceOf[SType#WrappedType]))
    tcase.doReduce()
  }

  def build(env: Map[String, Any], name: String, script: String, expected: SValue): Unit = {
    val costed = cost(env, script)
    val Tuple(valueF, costF, sizeF) = split(costed)
    emit(name, valueF, costF, sizeF)
    verifyCostFunc(costF) shouldBe(Success(()))
    verifyIsValid(valueF) shouldBe(Success(()))
    IR.buildTree(valueF) shouldBe expected
  }

  def measure[T](nIters: Int, okShow: Boolean = true)(action: Int => Unit): Unit = {
    for (i <- 0 until nIters) {
      val start = System.currentTimeMillis()
      val res = action(i)
      val end = System.currentTimeMillis()
      val iterTime = end - start
      if (okShow)
        println(s"Iter $i: $iterTime ms")
    }
  }

}
