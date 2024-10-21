package sigma.serialization.generators

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbString
import sigma.ast._

trait TypeGenerators {
  implicit val booleanTypeGen: Gen[SBoolean.type] = Gen.const(SBoolean)
  implicit val byteTypeGen: Gen[SByte.type] = Gen.const(SByte)
  implicit val shortTypeGen: Gen[SShort.type] = Gen.const(SShort)
  implicit val intTypeGen: Gen[SInt.type] = Gen.const(SInt)
  implicit val longTypeGen: Gen[SLong.type] = Gen.const(SLong)
  implicit val bigIntTypeGen: Gen[SBigInt.type] = Gen.const(SBigInt)
  implicit val groupElementTypeGen: Gen[SGroupElement.type] = Gen.const(SGroupElement)
  implicit val sigmaPropTypeGen: Gen[SSigmaProp.type] = Gen.const(SSigmaProp)
  implicit val boxTypeGen: Gen[SBox.type] = Gen.const(SBox)
  implicit val avlTreeTypeGen: Gen[SAvlTree.type] = Gen.const(SAvlTree)
  implicit val optionSigmaPropTypeGen: Gen[SOption[SSigmaProp.type]] = Gen.const(SOption(SSigmaProp))
  implicit val headerTypeGen: Gen[SHeader.type] = Gen.const(SHeader)

  implicit val primTypeGen: Gen[SPrimType] =
    Gen.oneOf[SPrimType](SBoolean, SByte, SShort, SInt, SLong, SBigInt, SGroupElement, SSigmaProp, SUnit)
  implicit val arbPrimType: Arbitrary[SPrimType] = Arbitrary(primTypeGen)
  implicit val predefTypeGen: Gen[SPredefType] =
    Gen.oneOf[SPredefType](SBoolean, SByte, SShort, SInt, SLong, SBigInt, SGroupElement, SSigmaProp, SUnit, SBox, SAvlTree, SHeader)
  implicit val arbPredefType: Arbitrary[SPredefType] = Arbitrary(predefTypeGen)

  implicit def genToArbitrary[T: Gen]: Arbitrary[T] = Arbitrary(implicitly[Gen[T]])

  def sTupleGen(min: Int, max: Int): Gen[STuple] = for {
    length <- Gen.chooseNum(min, max)
    values <- Gen.listOfN[SType](length, Gen.oneOf(
      byteTypeGen,
      shortTypeGen,
      intTypeGen,
      longTypeGen,
      bigIntTypeGen
    ))
  } yield STuple(values.toIndexedSeq)

  val sTypeIdentGen: Gen[STypeVar] = for {
    name <- arbString.arbitrary.suchThat(_.length <= 50)
  } yield STypeVar(name)
}
