package sigma.serialization

import sigma.Evaluation
import sigma.ast.syntax.{IntConstant, OptionValueOps}
import sigma.ast._
import sigma.ast.syntax._
import scala.util.Random

class ConcreteCollectionSerializerSpecification extends TableSerializationSpecification {

  private def testCollectionWithConstant[T <: SType](tpe: T) = {
    implicit val wWrapped = wrappedTypeGen(tpe)
    implicit val tT = Evaluation.stypeToRType(tpe)
    implicit val tag = tT.classTag
    forAll { x: Array[T#WrappedType] =>
      roundTripTest(ConcreteCollection[T](x.map(v => Constant(v, tpe)),
        tpe))
    }
  }

  property("ConcreteCollection (Constant[SBoolean.type]): Serializer round trip ") {
    testCollectionWithConstant(SBoolean)
  }

  property("ConcreteCollection (Constant): Serializer round trip ") {
    testCollectionWithConstant(SByte)
    testCollectionWithConstant(SShort)
    testCollectionWithConstant(SInt)
    testCollectionWithConstant(SLong)
    testCollectionWithConstant(SBigInt)
    testCollectionWithConstant(SGroupElement)
    testCollectionWithConstant(SSigmaProp)
    testCollectionWithConstant(SUnit)
    testCollectionWithConstant(SBox)
    testCollectionWithConstant(SAvlTree)
  }

  property("ConcreteCollection: Serializer round trip with different types seq") {
    forAll(intConstGen, getVarIntGen) { (i: IntConstant, ti: GetVar[SInt.type]) =>
      val seq = Random.shuffle(Seq(i.asIntValue, ti.get)).toArray
      roundTripTest(ConcreteCollection.fromSeq(seq))
    }
  }

  override def objects = Table(
    ("object", "bytes"),
    (ConcreteCollection.fromItems(TrueLeaf, FalseLeaf, TrueLeaf),
      Array[Byte](OpCodes.ConcreteCollectionBooleanConstantCode, 3, 5)) // bits: 00000101
  )

  tableRoundTripTest("Specific objects serializer round trip")
  tablePredefinedBytesTest("Specific objects deserialize from predefined bytes")

  property("ConcreteCollection: deserialize collection of a crazy size") {
    val bytes = Array[Byte](OpCodes.ConcreteCollectionCode) ++
      SigmaSerializer.startWriter().putUInt(Int.MaxValue).toBytes
    an[IllegalArgumentException] should be thrownBy ValueSerializer.deserialize(bytes)
  }
}
