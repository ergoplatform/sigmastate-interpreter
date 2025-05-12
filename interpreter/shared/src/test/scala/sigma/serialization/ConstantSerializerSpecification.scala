package sigma.serialization

import java.math.BigInteger
import org.ergoplatform._
import org.scalacheck.Arbitrary._
import sigma.data.{RType, SigmaBoolean, TupleColl}
import sigma.ast.SCollection.SByteArray
import sigma.ast.{BigIntConstant, ByteArrayConstant, Constant, DeserializationSigmaBuilder, FalseLeaf, GroupGenerator, LongConstant, TrueLeaf}
import sigmastate.eval._
import sigma.Extensions.ArrayOps
import sigma.ast._
import sigma.{AvlTree, Colls, Evaluation, Header, VersionContext}
import sigma.ast.SType.AnyOps
import scorex.util.encode.Base16
import sigma.ast.BoolArrayConstant.BoolArrayTypeCode
import sigma.ast.ByteArrayConstant.ByteArrayTypeCode
import sigma.ast.syntax.{BoolValue, SValue}
import sigma.crypto.EcPointType
import sigma.util.Extensions.{BigIntegerOps, EcpOps, SigmaBooleanOps}

import scala.annotation.nowarn

class ConstantSerializerSpecification extends TableSerializationSpecification {

  private def testCollection[T <: SType](tpe: T) = {
    implicit val wWrapped = wrappedTypeGen(tpe)
    implicit val tT = Evaluation.stypeToRType(tpe)
    implicit val tag = tT.classTag

    val withVersion = if (tpe == SHeader) {
      Some(VersionContext.V6SoftForkVersion)
    } else {
      None
    }

    forAll { xs: Array[T#WrappedType] =>
      implicit val tAny = sigma.AnyType
      roundTripTest(Constant[SCollection[T]](xs.toColl, SCollection(tpe)), withVersion)
      roundTripTest(Constant[SType](xs.toColl.map(x => (x, x)).asWrappedType, SCollection(STuple(tpe, tpe))), withVersion) // pairs are special case
      val triples = xs.toColl.map(x => TupleColl(x, x, x)).asWrappedType
      roundTripTest(Constant[SType](triples, SCollection(STuple(tpe, tpe, tpe))), withVersion)
      val quartets = xs.toColl.map(x => TupleColl(x, x, x, x)).asWrappedType
      roundTripTest(Constant[SType](quartets, SCollection(STuple(tpe, tpe, tpe, tpe))), withVersion)
      roundTripTest(Constant[SCollection[SCollection[T]]](xs.toColl.map(x => Colls.fromItems(x, x)), SCollection(SCollection(tpe))), withVersion)
      roundTripTest(Constant[SType](
        xs.toColl.map { x =>
          val arr = Colls.fromItems(x, x)
          (arr, arr)
        }.asWrappedType,
        SCollection(STuple(SCollection(tpe), SCollection(tpe)))
      ), withVersion)
    }
  }

  def testTuples[T <: SType](tpe: T) = {
    implicit val wWrapped = wrappedTypeGen(tpe)
    implicit val tT = Evaluation.stypeToRType(tpe)
    @nowarn implicit val tag = tT.classTag
    implicit val tAny: RType[Any] = sigma.AnyType
    val withVersion = if (tpe == SHeader) {
      Some(VersionContext.V6SoftForkVersion)
    } else {
      None
    }
    forAll { in: (T#WrappedType, T#WrappedType) =>
      val (x,y) = (in._1, in._2)
      roundTripTest(Constant[SType]((x, y).asWrappedType, STuple(tpe, tpe)), withVersion)
      roundTripTest(Constant[SType](TupleColl(x, y, x).asWrappedType, STuple(tpe, tpe, tpe)), withVersion)
      roundTripTest(Constant[SType](TupleColl(x, y, x, y).asWrappedType, STuple(tpe, tpe, tpe, tpe)), withVersion)
      roundTripTest(Constant[STuple](Colls.fromItems[Any](x, y, (x, y)), STuple(tpe, tpe, STuple(tpe, tpe))), withVersion)
      roundTripTest(Constant[STuple](Colls.fromItems[Any](x, y, TupleColl(x, y, x)), STuple(tpe, tpe, STuple(tpe, tpe, tpe))), withVersion)
      roundTripTest(Constant[STuple](Colls.fromItems[Any](x, y, TupleColl(x, y, (x, y))), STuple(tpe, tpe, STuple(tpe, tpe, STuple(tpe, tpe)))), withVersion)
    }
  }

  property("Constant serialization round trip") {
    forAll { _: Unit => roundTripTest(UnitConstant()) }
    forAll { x: Byte => roundTripTest(Constant[SByte.type](x, SByte)) }
    forAll { x: Boolean => roundTripTest(BooleanConstant.fromBoolean(x)) }
    forAll { x: Long => roundTripTest(Constant[SLong.type](x, SLong)) }
    forAll { x: String => roundTripTest(Constant[SString.type](x, SString)) }
    forAll { x: BigInteger => roundTripTest(Constant[SBigInt.type](x.toBigInt, SBigInt)) }
    forAll { x: EcPointType => roundTripTest(Constant[SGroupElement.type](x.toGroupElement, SGroupElement)) }
    forAll { x: SigmaBoolean => roundTripTest(Constant[SSigmaProp.type](x.toSigmaProp, SSigmaProp)) }
    forAll { x: ErgoBox => roundTripTest(Constant[SBox.type](x, SBox)) }
    forAll { x: AvlTree => roundTripTest(Constant[SAvlTree.type](x, SAvlTree)) }
    forAll { x: Header => roundTripTest(Constant[SHeader.type](x, SHeader), Some(VersionContext.V6SoftForkVersion)) }
    forAll { x: Array[Byte] => roundTripTest(Constant[SByteArray](x.toColl, SByteArray)) }
    forAll { t: SPredefType => testCollection(t) }
    forAll { t: SPredefType => testTuples(t) }
  }

  property("CollectionConstant serialization round trip") {
    testCollection(SBoolean)
    testCollection(SByte)
    testCollection(SShort)
    testCollection(SInt)
    testCollection(SLong)
    testCollection(SBigInt)
    testCollection(SGroupElement)
    testCollection(SSigmaProp)
    testCollection(SUnit)
    testCollection(SBox)
    testCollection(SAvlTree)
    testCollection(SHeader)
  }

  private def caseObjectValue(v: SValue) = (v, Array[Byte](v.opCode))

  override def objects = Table(
    ("object", "bytes"),
    (FalseLeaf, Array[Byte](1, 0)),
    (TrueLeaf, Array[Byte](1, 1)),
    caseObjectValue(Context),
    caseObjectValue(Height),
    caseObjectValue(Inputs),
    caseObjectValue(Outputs),
    caseObjectValue(LastBlockUtxoRootHash),
    caseObjectValue(Self),
    caseObjectValue(GroupGenerator),
    (LongConstant(1), Array[Byte](SLong.typeCode, 2)),
    (BigIntConstant(BigInteger.valueOf(0)), Array[Byte](SBigInt.typeCode, 1, 0)),
    (BigIntConstant(new BigInteger(Array[Byte](3,4,5))), Array[Byte](SBigInt.typeCode, 3, 3, 4, 5)),
    (ByteArrayConstant(Array[Byte](1, 3, 5, 9, 10, 100)), Array[Byte](ByteArrayTypeCode, 6, 1, 3, 5, 9, 10, 100)),
    (ByteArrayConstant(Array[Byte]()), Array[Byte](ByteArrayTypeCode, 0)),
    (ByteArrayConstant(Array[Byte](1)), Array[Byte](ByteArrayTypeCode, 1, 1)),
    (ByteArrayConstant(Array[Byte](1, 2, 3, 4, 5)), Array[Byte](ByteArrayTypeCode, 5, 1, 2, 3, 4, 5)),
    (BoolArrayConstant(Array[Boolean](true, false, true)), Array[Byte](BoolArrayTypeCode, 3, 5)) // bits: 00000101
  )

  tableRoundTripTest("Specific objects serializer round trip")
  tablePredefinedBytesTest("Specific objects deserialize from predefined bytes")

  property("Element type checked in collection of Boolean") {
    val builder = DeserializationSigmaBuilder
    val ser = ConcreteCollectionBooleanConstantSerializer(builder.mkConcreteCollection)

    // successfull case
    ser.toBytes(ConcreteCollection(Array(TrueLeaf), SBoolean)) shouldBe Base16.decode("0101").get

    assertExceptionThrown( {
      val coll = ConcreteCollection(
        Array(TrueLeaf, ByteConstant(0).asInstanceOf[BoolValue]),
        SBoolean)
      ser.toBytes(coll)
    },
    {
      case e: SerializerException =>
        e.getMessage.contains("Expected collection of BooleanConstant values")
      case _ => false
    })
  }
}
