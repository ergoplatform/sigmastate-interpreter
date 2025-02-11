package sigma.serialization

import java.math.BigInteger
import org.ergoplatform.ErgoBox
import org.scalacheck.Arbitrary._
import sigma.data.{DataValueComparer, RType, SigmaBoolean, TupleColl}
import sigma.ast.SCollection.SByteArray
import sigmastate.eval._
import sigma.{AvlTree, Colls, Evaluation}
import sigma.ast.SType.AnyOps
import sigma.ast._
import org.scalacheck.Gen
import sigma.Extensions.ArrayOps
import sigma.crypto.EcPointType
import sigma.eval.SigmaDsl
import sigma.util.Extensions.{BigIntegerOps, EcpOps, SigmaBooleanOps}
import sigmastate.interpreter.{CostAccumulator, CErgoTreeEvaluator}
import sigmastate.interpreter.CErgoTreeEvaluator.DefaultProfiler
import sigmastate.utils.Helpers

import scala.annotation.nowarn
import scala.reflect.ClassTag

class DataSerializerSpecification extends SerializationSpecification {

  def roundtrip[T <: SType](obj: T#WrappedType, tpe: T) = {
    val w = SigmaSerializer.startWriter()
    DataSerializer.serialize(obj, tpe, w)
    val bytes = w.toBytes
    val r = SigmaSerializer.startReader(bytes)
    val res = DataSerializer.deserialize(tpe, r)
    res shouldBe obj

    val es = CErgoTreeEvaluator.DefaultEvalSettings
    val accumulator = new CostAccumulator(
      initialCost = JitCost(0),
      costLimit = Some(JitCost.fromBlockCost(es.scriptCostLimitInEvaluator)))
    val evaluator = new CErgoTreeEvaluator(
      context = null,
      constants = ErgoTree.EmptyConstants,
      coster = accumulator, DefaultProfiler, es)
    val ok = DataValueComparer.equalDataValues(res, obj)(evaluator)
    ok shouldBe true

    val randomPrefix = arrayGen[Byte].sample.get
    val r2 = SigmaSerializer.startReader(randomPrefix ++ bytes, randomPrefix.length)
    val res2 = DataSerializer.deserialize(tpe, r2)
    res2 shouldBe obj
  }

  def testCollection[T <: SType](tpe: T) = {
    implicit val wWrapped = wrappedTypeGen(tpe)
    implicit val tT = Evaluation.stypeToRType(tpe)
    implicit val tagT = tT.classTag
    implicit val tAny = sigma.AnyType
    forAll { xs: Array[T#WrappedType] =>
      roundtrip[SCollection[T]](xs.toColl, SCollection(tpe))
      roundtrip[SType](xs.toColl.map(x => (x, x)).asWrappedType, SCollection(STuple(tpe, tpe)))

      val triples = xs.toColl.map(x => TupleColl(x, x, x)).asWrappedType
      roundtrip(triples, SCollection(STuple(tpe, tpe, tpe)))

      val quartets = xs.toColl.map(x => TupleColl(x, x, x, x)).asWrappedType
      roundtrip(quartets, SCollection(STuple(tpe, tpe, tpe, tpe)))

      val nested = xs.toColl.map(x => Colls.fromItems[T#WrappedType](x, x))
      roundtrip[SCollection[SCollection[T]]](nested, SCollection(SCollection(tpe)))

      roundtrip[SType](
        xs.toColl.map { x =>
          val arr = Colls.fromItems[T#WrappedType](x, x)
          (arr, arr)
        }.asWrappedType,
        SCollection(STuple(SCollection(tpe), SCollection(tpe)))
      )
    }
  }

  def testTuples[T <: SType](tpe: T) = {
    implicit val wWrapped: Gen[T#WrappedType] = wrappedTypeGen(tpe)
    val tT = Evaluation.stypeToRType(tpe)
    @nowarn implicit val tag: ClassTag[T#WrappedType] = tT.classTag
    implicit val tAny       : RType[Any]              = sigma.AnyType
    forAll { in: (T#WrappedType, T#WrappedType) =>
      val (x,y) = (in._1, in._2)
      roundtrip[SType]((x, y).asWrappedType, STuple(tpe, tpe))
      roundtrip[SType](TupleColl(x, y, x).asWrappedType, STuple(tpe, tpe, tpe))
      roundtrip[SType](TupleColl(x, y, x, y).asWrappedType, STuple(tpe, tpe, tpe, tpe))
      roundtrip[STuple](Colls.fromItems[Any](x, y, (x, y)), STuple(tpe, tpe, STuple(tpe, tpe)))
      roundtrip[STuple](Colls.fromItems[Any](x, y, TupleColl(x, y, x)), STuple(tpe, tpe, STuple(tpe, tpe, tpe)))
      roundtrip[STuple](Colls.fromItems[Any](x, y, TupleColl(x, y, (x, y))), STuple(tpe, tpe, STuple(tpe, tpe, STuple(tpe, tpe))))
    }
  }

  property("Data serialization round trip") {
    forAll { x: Byte => roundtrip[SByte.type](x, SByte) }
    forAll { x: Boolean => roundtrip[SBoolean.type](x, SBoolean) }
    forAll { x: Long => roundtrip[SLong.type](x, SLong) }
    forAll { x: String => roundtrip[SString.type](x, SString) }
    forAll { x: BigInteger => roundtrip[SBigInt.type](x.toBigInt, SBigInt) }
    forAll { x: EcPointType => roundtrip[SGroupElement.type](x.toGroupElement, SGroupElement) }
    forAll { x: SigmaBoolean => roundtrip[SSigmaProp.type](x.toSigmaProp, SSigmaProp) }
    forAll { x: ErgoBox => roundtrip[SBox.type](x, SBox) }
    forAll { x: AvlTree => roundtrip[SAvlTree.type](x, SAvlTree) }
    forAll { x: Array[Byte] => roundtrip[SByteArray](x.toColl, SByteArray) }
    forAll { t: SPredefType => testCollection(t) }
    forAll { t: SPredefType => testTuples(t) }
  }

  property("Should check limits and fail") {
    val len = 0xFFFF + 1
    val tup = SigmaDsl.Colls.replicate(len, 1.asInstanceOf[Any])(sigma.AnyType)
    assertExceptionThrown({
        val w = SigmaSerializer.startWriter()
        DataSerializer.serialize(tup.asWrappedType, STuple(Array.fill(len)(SInt)), w)
      },
      { t =>
        t.isInstanceOf[RuntimeException] &&
        t.getMessage.contains("The tuple length (65536) is too long (maximum limit is 65535).")
      })

    val tooBigBytes  = Helpers.decodeBytes(
      "80e0ff7f02807fff72807f0a00ff7fb7c57f75c11ba2802970fd250052807fc37f6480ffff007fff18eeba44").toArray
    val tooBig = SigmaDsl.BigInt(new BigInteger(tooBigBytes))

    assertExceptionThrown({
        val w = SigmaSerializer.startWriter()
        DataSerializer.serialize(tooBig.asWrappedType, SBigInt, w)
      },
      exceptionLike[IllegalArgumentException]("doesn't fit into 256 bits")
    )

    assertExceptionThrown({
      val bytes = SigmaSerializer.startWriter().putUShort(tooBigBytes.length).toBytes ++ tooBigBytes
      val r = SigmaSerializer.startReader(bytes)
      DataSerializer.deserialize(SBigInt, r)
    },
    { t =>
      t.isInstanceOf[SerializerException] &&
          t.getMessage.contains(s"BigInt value doesn't not fit into ${SBigInt.MaxSizeInBytes} bytes")
    })

  }
}
