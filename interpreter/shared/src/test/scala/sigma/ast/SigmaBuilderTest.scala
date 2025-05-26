package sigma.ast

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sigma.Extensions.ArrayOps
import sigma.data.{CAnyValue, CAvlTree, CBox, Nullable, RType}
import sigma.eval.SigmaDsl
import sigma.exceptions.ConstraintFailed
import sigma.serialization.OpCodes
import sigma.{Environment, SigmaTestingData, VersionContext}
import sigmastate._

import java.math.BigInteger

class SigmaBuilderTest extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers
    with SigmaTestingData
    with CrossVersionProps {

  property("StdSigmaBuilder construct nodes") {
    import StdSigmaBuilder._
    mkEQ(TrueLeaf, FalseLeaf) shouldEqual EQ(TrueLeaf, FalseLeaf)
    mkNEQ(TrueLeaf, FalseLeaf) shouldEqual NEQ(TrueLeaf, FalseLeaf)

    mkGT(IntConstant(1), IntConstant(1)) shouldEqual GT(IntConstant(1), IntConstant(1))
    mkGE(IntConstant(1), IntConstant(1)) shouldEqual GE(IntConstant(1), IntConstant(1))
    mkLT(IntConstant(1), IntConstant(1)) shouldEqual LT(IntConstant(1), IntConstant(1))
    mkLE(IntConstant(1), IntConstant(1)) shouldEqual LE(IntConstant(1), IntConstant(1))

    mkPlus(IntConstant(1), IntConstant(1)) shouldEqual
      ArithOp(IntConstant(1), IntConstant(1), OpCodes.PlusCode)
    mkMinus(IntConstant(1), IntConstant(1)) shouldEqual
      ArithOp(IntConstant(1), IntConstant(1), OpCodes.MinusCode)
    mkMultiply(IntConstant(1), IntConstant(1)) shouldEqual
      ArithOp(IntConstant(1), IntConstant(1), OpCodes.MultiplyCode)
    mkDivide(IntConstant(1), IntConstant(1)) shouldEqual
      ArithOp(IntConstant(1), IntConstant(1), OpCodes.DivisionCode)
    mkModulo(IntConstant(1), IntConstant(1)) shouldEqual
      ArithOp(IntConstant(1), IntConstant(1), OpCodes.ModuloCode)
  }

  property("TransformingSigmaBuilder apply upcast") {
    import TransformingSigmaBuilder._
    mkEQ(LongConstant(1), IntConstant(1)) shouldEqual EQ(LongConstant(1), Upcast(IntConstant(1), SLong))
    mkNEQ(LongConstant(1), IntConstant(1)) shouldEqual NEQ(LongConstant(1), Upcast(IntConstant(1), SLong))

    mkGT(LongConstant(1), IntConstant(1)) shouldEqual GT(LongConstant(1), Upcast(IntConstant(1), SLong))
    mkGE(LongConstant(1), IntConstant(1)) shouldEqual GE(LongConstant(1), Upcast(IntConstant(1), SLong))
    mkLT(LongConstant(1), IntConstant(1)) shouldEqual LT(LongConstant(1), Upcast(IntConstant(1), SLong))
    mkLE(LongConstant(1), IntConstant(1)) shouldEqual LE(LongConstant(1), Upcast(IntConstant(1), SLong))

    mkPlus(LongConstant(1), IntConstant(1)) shouldEqual
      ArithOp(LongConstant(1), Upcast(IntConstant(1), SLong), OpCodes.PlusCode)
    mkMinus(LongConstant(1), IntConstant(1)) shouldEqual
      ArithOp(LongConstant(1), Upcast(IntConstant(1), SLong), OpCodes.MinusCode)
    mkMultiply(LongConstant(1), IntConstant(1)) shouldEqual
      ArithOp(LongConstant(1), Upcast(IntConstant(1), SLong), OpCodes.MultiplyCode)
    mkDivide(LongConstant(1), IntConstant(1)) shouldEqual
      ArithOp(LongConstant(1), Upcast(IntConstant(1), SLong), OpCodes.DivisionCode)
    mkModulo(LongConstant(1), IntConstant(1)) shouldEqual
      ArithOp(LongConstant(1), Upcast(IntConstant(1), SLong), OpCodes.ModuloCode)
  }

  property("CheckingSigmaBuilder failing constraint") {
    import CheckingSigmaBuilder._
    an[ConstraintFailed] should be thrownBy mkEQ(LongConstant(1), IntConstant(1))
    an[ConstraintFailed] should be thrownBy mkNEQ(LongConstant(1), IntConstant(1))

    an[ConstraintFailed] should be thrownBy mkGT(LongConstant(1), IntConstant(1))
    an[ConstraintFailed] should be thrownBy mkGE(LongConstant(1), IntConstant(1))
    an[ConstraintFailed] should be thrownBy mkLT(LongConstant(1), IntConstant(1))
    an[ConstraintFailed] should be thrownBy mkLE(LongConstant(1), IntConstant(1))

    an[ConstraintFailed] should be thrownBy mkPlus(LongConstant(1), IntConstant(1))
    an[ConstraintFailed] should be thrownBy mkMinus(LongConstant(1), IntConstant(1))
    an[ConstraintFailed] should be thrownBy mkMultiply(LongConstant(1), IntConstant(1))
    an[ConstraintFailed] should be thrownBy mkDivide(LongConstant(1), IntConstant(1))
    an[ConstraintFailed] should be thrownBy mkModulo(LongConstant(1), IntConstant(1))
  }
  import sigma.Platform.liftToConstant

  def testSuccess(v: Any, c: Constant[SType]): Unit = {
    liftToConstant(v, TransformingSigmaBuilder) shouldBe Nullable(c)
  }

  def testFailure(v: Any): Unit = {
    liftToConstant(v, TransformingSigmaBuilder) shouldBe Nullable.None
  }

  def testNumeric[T <: SType]
      (v: T#WrappedType, c: Constant[T]) = {
    if (Environment.current.isJVM) {
      testSuccess(v, c)
    } else {
      // Byte, Short, Int are the same on JS and thus lifted to IntConstant
      testSuccess(v, IntConstant(1))
    }
  }

  /** Test behavior of platform-specific liftToConstant method on CAnyValue. */
  def testLiftingOfCAnyValue[T <: SType]
      (v: T#WrappedType, c: Constant[T])(implicit t: RType[T#WrappedType]) = {
    if (Environment.current.isJVM) {
      // behavior on JVM is consensus critical, make sure lifitng of CAnyValue is failing
      testFailure(CAnyValue(v))
    } else {
      // JS version however handles lifiting of CAnyValue
      testSuccess(CAnyValue(v), c)
    }
  }

  def test[T <: SType]
      (v: T#WrappedType, c: Constant[T])(implicit t: RType[T#WrappedType]) = {
    testSuccess(v, c)
    testLiftingOfCAnyValue[T](v, c)
  }

  def testArray[T <: SType]
      (v: T#WrappedType, c: Constant[T])(implicit t: RType[T#WrappedType]) = {
    // for any Byte and Short value `v`, lifting of array should succeed
    val arr = Array.fill[T#WrappedType](10)(v)(t.classTag)
    testSuccess(arr, TransformingSigmaBuilder.mkCollectionConstant(arr, c.tpe))
  }

  def testColl[T <: SType]
      (v: T#WrappedType, c: Constant[T])(implicit t: RType[T#WrappedType]) = {
    // for any Byte and Short value `v`, lifting of Coll should succeed
    val arr = Array.fill[T#WrappedType](10)(v)(t.classTag)
    val coll = arr.toColl
    testSuccess(coll, TransformingSigmaBuilder.mkCollectionConstant(coll, c.tpe))
  }

  property("liftToConstant Boolean") {
      val v = true
      val c = BooleanConstant(v)
      test[SBoolean.type](v, c)
      testArray[SBoolean.type](v, c)  // TODO v6.0: arrays should not be liftable directly (see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/905)
      testColl[SBoolean.type](v, c)
  }

  property("liftToConstant Byte") {
      val v = 1.toByte
      val c = ByteConstant(v)
      testNumeric[SByte.type](v, c)
      testLiftingOfCAnyValue[SByte.type](v, c)
      testArray[SByte.type](v, c)  // TODO v6.0: arrays should not be liftable directly (see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/905)
      testColl[SByte.type](v, c)
  }

  property("liftToConstant Short") {
    val v = 1.toShort
    val c = ShortConstant(v)
    testNumeric[SShort.type](v, c)
    testLiftingOfCAnyValue[SShort.type](v, c)
    testArray[SShort.type](v, c)   // TODO v6.0: arrays should not be liftable directly (see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/905)
    testColl[SShort.type](v, c)
  }

  property("liftToConstant Int") {
    val v = 1
    val c = IntConstant(v)
    test[SInt.type](v, c)
    testArray[SInt.type](v, c)     // TODO v6.0: arrays should not be liftable directly (see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/905)
    testColl[SInt.type](v, c)
  }

  property("liftToConstant Long") {
    val v = 1L
    val c = LongConstant(v)
    test[SLong.type](v, c)
    testArray[SLong.type](v, c)    // TODO v6.0: arrays should not be liftable directly (see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/905)
    testColl[SLong.type](v, c)
  }
  
  property("liftToConstant String") {
    val v = "abc"
    val c = StringConstant(v)
    test[SString.type](v, c)
    testArray[SString.type](v, c)    // TODO v6.0: String should be liftable at all (not supported in ErgoTree) (see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/905)
    testColl[SString.type](v, c)
  }

  property("liftToConstant BigInteger") {
    val v = BigInteger.valueOf(1L)
    val c = BigIntConstant(v)
    testSuccess(v, c)             // TODO v6.0: both BigInteger and arrays should not be liftable directly (see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/905)
    val arr = Array.fill(10)(v)
    testSuccess(arr, TransformingSigmaBuilder.mkCollectionConstant[SBigInt.type](arr.map(SigmaDsl.BigInt), c.tpe))
  }

  property("liftToConstant BigInt") {
    val v = SigmaDsl.BigInt(BigInteger.valueOf(1L))
    val c = BigIntConstant(v)
    test[SBigInt.type](v, c)
    testFailure(Array.fill(10)(v), c)
    testColl[SBigInt.type](v, c)
  }

  property("liftToConstant GroupElement") {
    val v = TestData.ge1
    val c = GroupElementConstant(v)
    test[SGroupElement.type](v, c)
    testFailure(Array.fill(10)(v))
    testColl[SGroupElement.type](v, c)
  }

  property("liftToConstant ErgoBox") {
    val v = TestData.b2.asInstanceOf[CBox].wrappedValue
    val c = BoxConstant(TestData.b2)
    testSuccess(v, c) // TODO v6.0: ErgoBox should not be liftable directly (see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/905)
    testFailure(Array.fill(10)(v))
  }

  property("liftToConstant Box") {
    val v = TestData.b2
    val c = BoxConstant(v)
    if (VersionContext.current.isJitActivated) {
      test[SBox.type](v, c)
    } else {
      testFailure(v)
    }
    testFailure(Array.fill(10)(v))
    testColl[SBox.type](v, c)
  }

  property("liftToConstant AvlTree") {
    val v = TestData.t1
    val c = AvlTreeConstant(v)
    test[SAvlTree.type](v, c)
    testFailure(Array.fill(10)(v))
    testColl[SAvlTree.type](v, c)
  }

  property("liftToConstant AvlTreeData") {
    val v = TestData.t1.asInstanceOf[CAvlTree].wrappedValue
    val c = AvlTreeConstant(SigmaDsl.avlTree(v))
    testSuccess(v, c) // TODO v6.0: AvlTreeData should not be liftable directly (see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/905)
    testFailure(Array.fill(10)(v))
  }

  property("liftToConstant SigmaBoolean") {
    val v = TestData.create_dlog()
    val c = SigmaPropConstant(v)
    testSuccess(v, c)
    testFailure(Array.fill(10)(v))
  }

  property("liftToConstant SigmaProp") {
    val v = SigmaDsl.SigmaProp(TestData.create_dlog())
    val c = SigmaPropConstant(v)
    test[SSigmaProp.type](v, c)
    testFailure(Array.fill(10)(v))
    testColl[SSigmaProp.type](v, c)
  }
}
