package sigmastate.eval

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import sigma.ast.syntax._
import sigma.ast.{Constant, ConcreteCollection, ErgoTree, FixedCostItem, FuncValue, JitCost,
  MethodCall, SCollection, SCollectionHelpers, SCollectionMethods, SInt, SType, ValUse}
import sigma.data.RType._
import sigma.data.{CSigmaDslBuilder, DataValueComparer}
import sigma.eval.EvalSettings
import sigma.exceptions.CostLimitException
import sigma.{Coll, ContractsTestkit, IntType, VersionContext}
import sigmastate.interpreter.CErgoTreeEvaluator.DefaultProfiler
import sigmastate.interpreter.{CErgoTreeEvaluator, CostAccumulator}

class CollectionPrefixCostTests extends AnyFunSuite with ContractsTestkit with Matchers {

  private def newEvaluator(costLimitInBlock: Int): (CostAccumulator, CErgoTreeEvaluator) = {
    val evalSettings = EvalSettings(
      isMeasureOperationTime = false,
      isMeasureScriptTime = false,
      costTracingEnabled = true)
    val accumulator = new CostAccumulator(
      initialCost = JitCost(0),
      costLimit = Some(JitCost.fromBlockCost(costLimitInBlock)))
    val evaluator = new CErgoTreeEvaluator(
      context = null,
      constants = ErgoTree.EmptyConstants,
      coster = accumulator,
      DefaultProfiler,
      evalSettings)
    (accumulator, evaluator)
  }

  private def eval(exp: SValue, costLimitInBlock: Int = 1000000): (Any, Int) = {
    val (accumulator, evaluator) = newEvaluator(costLimitInBlock)
    VersionContext.withVersions(VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion) {
      val res = exp.evalTo[Any](Map.empty)(evaluator)
      (res, accumulator.totalCost.value)
    }
  }

  private def evalException(exp: SValue, costLimitInBlock: Int = 1000000): Throwable = {
    val (_, evaluator) = newEvaluator(costLimitInBlock)
    the[Exception] thrownBy {
      VersionContext.withVersions(VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion) {
        exp.evalTo[Any](Map.empty)(evaluator)
      }
    }
  }

  private def compare[A](xs: Coll[A], ys: Coll[A], offset: Int,
                         costLimitInBlock: Int = 1000000): (Boolean, CErgoTreeEvaluator) = {
    val (_, evaluator) = newEvaluator(costLimitInBlock)
    val res = VersionContext.withVersions(VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion) {
      SCollectionHelpers.equalElementsFrom(xs, ys, offset)(evaluator)
    }
    (res, evaluator)
  }

  private def compareException[A](xs: Coll[A], ys: Coll[A], offset: Int,
                                  costLimitInBlock: Int = 1000000): Throwable = {
    val (_, evaluator) = newEvaluator(costLimitInBlock)
    the[Exception] thrownBy {
      VersionContext.withVersions(VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion) {
        SCollectionHelpers.equalElementsFrom(xs, ys, offset)(evaluator)
      }
    }
  }

  private def startsWithSpec(tpe: SType) = VersionContext.withVersions(3, 3) {
    SCollectionMethods.StartsWithMethod.withConcreteTypes(Map(SCollection.tIV -> tpe))
  }

  private def endsWithSpec(tpe: SType) = VersionContext.withVersions(3, 3) {
    SCollectionMethods.EndsWithMethod.withConcreteTypes(Map(SCollection.tIV -> tpe))
  }

  private def startsWith(xs: SValue, ys: SValue, tpe: SType) =
    MethodCall(xs, startsWithSpec(tpe), IndexedSeq(ys), Map.empty)

  private def endsWith(xs: SValue, ys: SValue, tpe: SType) =
    MethodCall(xs, endsWithSpec(tpe), IndexedSeq(ys), Map.empty)

  private def intColl(values: Int*): Constant[SCollection[SInt.type]] =
    Constant[SCollection[SInt.type]](
      CSigmaDslBuilder.Colls.fromArray(values.toArray), SCollection(SInt))

  private def nestedIntColl(width: Int, start: Int = 0): Constant[SCollection[SCollection[SInt.type]]] = {
    val row = CSigmaDslBuilder.Colls.fromArray(Array.tabulate(width)(_ + start))
    val nested = CSigmaDslBuilder.Colls.fromArray(Array.fill(width)(row))
    Constant[SCollection[SCollection[SInt.type]]](nested, SCollection(SCollection(SInt)))
  }

  private def primComparisonCount(evaluator: CErgoTreeEvaluator): Int =
    evaluator.getCostTrace().count {
      case item: FixedCostItem if item.costKind == DataValueComparer.CostKind_EQ_Prim => true
      case _ => false
    }

  test("equalElementsFrom compares only the selected segment") {
    val xs = intColl(9, 1, 2, 8).value
    val segment = intColl(1, 2).value

    compare(xs, segment, 1)._1 shouldBe true
    compare(xs, intColl(1, 3).value, 1)._1 shouldBe false
    compare(xs, intColl().value, xs.length)._1 shouldBe true
    compare(intColl(1, 2).value, intColl(1, 2, 3).value, -1)._1 shouldBe false
  }

  test("equalElementsFrom stops at the first mismatch and charges only compared items") {
    val (firstMismatch, firstEvaluator) =
      compare(intColl(9, 9, 9).value, intColl(1, 2, 3).value, 0)
    val (lastMismatch, lastEvaluator) =
      compare(intColl(1, 2, 9).value, intColl(1, 2, 3).value, 0)

    firstMismatch shouldBe false
    lastMismatch shouldBe false
    primComparisonCount(firstEvaluator) shouldBe 1
    primComparisonCount(lastEvaluator) shouldBe 3
  }

  test("equalElementsFrom supports nested collections, tuples and options") {
    val nestedA = nestedIntColl(2)
    val nestedB = nestedIntColl(2)
    val nestedMismatch = nestedIntColl(2, 1)
    val wrapA = ConcreteCollection(Array(nestedA), nestedA.tpe)
    val wrapB = ConcreteCollection(Array(nestedB), nestedB.tpe)
    val wrapMismatch = ConcreteCollection(Array(nestedMismatch), nestedMismatch.tpe)

    compare(wrapA.value, wrapB.value, 0)._1 shouldBe true
    compare(wrapA.value, wrapMismatch.value, 0)._1 shouldBe false

    val tuplesA = Colls.fromArray(Array((1, 2), (3, 4)))
    val tuplesB = Colls.fromArray(Array((1, 2), (3, 4)))
    val tuplesMismatch = Colls.fromArray(Array((1, 2), (3, 5)))
    compare(tuplesA, tuplesB, 0)._1 shouldBe true
    compare(tuplesA, tuplesMismatch, 0)._1 shouldBe false

    val optionsA = Colls.fromArray(Array[Option[Int]](Some(1), None))
    val optionsB = Colls.fromArray(Array[Option[Int]](Some(1), None))
    val optionsMismatch = Colls.fromArray(Array[Option[Int]](Some(2), None))
    compare(optionsA, optionsB, 0)._1 shouldBe true
    compare(optionsA, optionsMismatch, 0)._1 shouldBe false
  }

  test("equalElementsFrom rejects invalid offsets") {
    val xs = intColl(1, 2).value
    val ys = intColl(1).value

    compareException(xs, ys, -1) shouldBe a[IndexOutOfBoundsException]
    compareException(xs, ys, xs.length) shouldBe a[IndexOutOfBoundsException]
  }

  test("equalElementsFrom rejects function elements") {
    val fn: Int => Int = x => x
    val fnColl = Colls.fromArray(Array(fn, fn))

    val error = compareException(fnColl, fnColl, 0)

    error shouldBe a[RuntimeException]
    error.getMessage should include("Cannot compare")
  }

  test("equalElementsFrom enforces the evaluator cost limit") {
    val left = nestedIntColl(4)
    val right = nestedIntColl(4)
    val wrapLeft = ConcreteCollection(Array(left), left.tpe)
    val wrapRight = ConcreteCollection(Array(right), right.tpe)

    val error = compareException(wrapLeft.value, wrapRight.value, 0, costLimitInBlock = 5)

    error shouldBe a[CostLimitException]
  }

  test("startsWith and endsWith charge for nested element comparisons") {
    val c1a = nestedIntColl(1)
    val c1b = nestedIntColl(1)
    val c8a = nestedIntColl(8)
    val c8b = nestedIntColl(8)
    val wrap1a = ConcreteCollection(Array(c1a), c1a.tpe)
    val wrap1b = ConcreteCollection(Array(c1b), c1b.tpe)
    val wrap8a = ConcreteCollection(Array(c8a), c8a.tpe)
    val wrap8b = ConcreteCollection(Array(c8b), c8b.tpe)

    val (start1, startCost1) = eval(startsWith(wrap1a, wrap1b, c1a.tpe))
    val (start8, startCost8) = eval(startsWith(wrap8a, wrap8b, c8a.tpe))
    val (end1, endCost1) = eval(endsWith(wrap1a, wrap1b, c1a.tpe))
    val (end8, endCost8) = eval(endsWith(wrap8a, wrap8b, c8a.tpe))

    start1 shouldBe true
    start8 shouldBe true
    end1 shouldBe true
    end8 shouldBe true
    startCost8 should be > startCost1
    endCost8 should be > endCost1
  }

  test("startsWith and endsWith reject function collections") {
    val fnNode = FuncValue(Vector((1, SInt)), ValUse(1, SInt))
    val fnColl = ConcreteCollection(Array(fnNode), fnNode.tpe)

    val startError = evalException(startsWith(fnColl, fnColl, fnNode.tpe))
    val endError = evalException(endsWith(fnColl, fnColl, fnNode.tpe))

    startError.getCause shouldBe a[RuntimeException]
    endError.getCause shouldBe a[RuntimeException]
    startError.getCause.getMessage should include("Cannot compare")
    endError.getCause.getMessage should include("Cannot compare")
  }

  test("nested comparisons stop at the cost limit") {
    val left = nestedIntColl(4)
    val right = nestedIntColl(4)
    val wrapLeft = ConcreteCollection(Array(left), left.tpe)
    val wrapRight = ConcreteCollection(Array(right), right.tpe)

    val error = evalException(startsWith(wrapLeft, wrapRight, left.tpe), costLimitInBlock = 5)

    error shouldBe a[CostLimitException]
  }

  test("startsWith and endsWith preserve boundary semantics") {
    eval(startsWith(intColl(1, 2, 3), intColl(1, 2), SInt))._1 shouldBe true
    eval(startsWith(intColl(1, 2, 3), intColl(1, 2, 4), SInt))._1 shouldBe false
    eval(startsWith(intColl(1, 2), intColl(1, 2, 3), SInt))._1 shouldBe false
    eval(startsWith(intColl(1, 2), intColl(), SInt))._1 shouldBe true

    eval(endsWith(intColl(1, 2, 3), intColl(2, 3), SInt))._1 shouldBe true
    eval(endsWith(intColl(1, 2, 3), intColl(1, 2), SInt))._1 shouldBe false
    eval(endsWith(intColl(1, 2), intColl(1, 2, 3), SInt))._1 shouldBe false
    eval(endsWith(intColl(1, 2), intColl(), SInt))._1 shouldBe true
  }
}
