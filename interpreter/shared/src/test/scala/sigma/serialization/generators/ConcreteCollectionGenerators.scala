package sigma.serialization.generators

import org.scalacheck.{Arbitrary, Gen}
import sigma.ast.syntax.IntConstant
import sigma.ast.{ConcreteCollection, SBoolean, SInt, SSigmaProp, SType, Value}

trait ConcreteCollectionGenerators { self: ObjectGenerators =>
  val minCollLength = 1
  val maxCollLength = 10

  def concreteCollectionGen[T <: SType](implicit constGen: Gen[Value[T]]): Gen[ConcreteCollection[T]] =
    for {
      size <- Gen.chooseNum(minCollLength, maxCollLength)
      c <- constGen
      listOfConsts <- Gen.listOfN(size, constGen)
    } yield ConcreteCollection(listOfConsts.toArray[Value[T]], c.tpe)

  lazy val intConstCollectionGen: Gen[ConcreteCollection[SInt.type]] = for {
    size <- Gen.chooseNum(minCollLength, maxCollLength)
    listOfConstInts <- Gen.listOfN(size, intConstGen)
  } yield ConcreteCollection.fromSeq(listOfConstInts.toArray[IntConstant])

  implicit val arbCCOfIntConstant: Arbitrary[ConcreteCollection[SInt.type]] = Arbitrary(intConstCollectionGen)
  implicit val arbCCOfBoolConstant: Arbitrary[ConcreteCollection[SBoolean.type]] = Arbitrary(concreteCollectionGen[SBoolean.type](booleanConstGen))
  implicit val arbCCOfSigmaPropConstant: Arbitrary[ConcreteCollection[SSigmaProp.type]] = Arbitrary(concreteCollectionGen[SSigmaProp.type](sigmaPropValueGen))
}
