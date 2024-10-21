package sigma.serialization

import java.math.BigInteger
import org.ergoplatform.ErgoBox
import org.scalacheck.Arbitrary._
import sigma.data.{CBigInt, CHeader, DataValueComparer, OptionType, RType, SigmaBoolean, TupleColl}
import sigma.ast.SCollection.SByteArray
import sigmastate.eval._
import sigma.{AvlTree, Colls, Evaluation, Header, VersionContext}
import sigma.ast.SType.AnyOps
import sigma.ast._
import org.scalacheck.Gen
import sigma.Extensions.ArrayOps
import sigma.crypto.EcPointType
import sigma.eval.SigmaDsl
import sigma.util.Extensions.{BigIntegerOps, EcpOps, SigmaBooleanOps}
import sigmastate.interpreter.{CErgoTreeEvaluator, CostAccumulator}
import sigmastate.interpreter.CErgoTreeEvaluator.DefaultProfiler
import sigmastate.utils.Helpers

import scala.annotation.nowarn
import scala.reflect.ClassTag

class DataSerializerSpecification extends SerializationSpecification {

  def roundtrip[T <: SType](obj: T#WrappedType, tpe: T, withVersion: Option[Byte] = None) = {

    def test() = {
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

    withVersion match {
      case Some(ver) =>
        VersionContext.withVersions(ver, 1) {
          test()
        }
      case None =>
        test()
    }
  }

  def testCollection[T <: SType](tpe: T) = {
    implicit val wWrapped = wrappedTypeGen(tpe)
    implicit val tT = Evaluation.stypeToRType(tpe)
    implicit val tagT = tT.classTag
    implicit val tAny = sigma.AnyType

    val withVersion = if (tpe == SHeader) {
      Some(VersionContext.V6SoftForkVersion)
    } else {
      None
    }
    forAll { xs: Array[T#WrappedType] =>
      roundtrip[SCollection[T]](xs.toColl, SCollection(tpe), withVersion)
      roundtrip[SType](xs.toColl.map(x => (x, x)).asWrappedType, SCollection(STuple(tpe, tpe)), withVersion)

      val triples = xs.toColl.map(x => TupleColl(x, x, x)).asWrappedType
      roundtrip(triples, SCollection(STuple(tpe, tpe, tpe)), withVersion)

      val quartets = xs.toColl.map(x => TupleColl(x, x, x, x)).asWrappedType
      roundtrip(quartets, SCollection(STuple(tpe, tpe, tpe, tpe)), withVersion)

      val nested = xs.toColl.map(x => Colls.fromItems[T#WrappedType](x, x))
      roundtrip[SCollection[SCollection[T]]](nested, SCollection(SCollection(tpe)), withVersion)

      roundtrip[SType](
        xs.toColl.map { x =>
          val arr = Colls.fromItems[T#WrappedType](x, x)
          (arr, arr)
        }.asWrappedType,
        SCollection(STuple(SCollection(tpe), SCollection(tpe))),
        withVersion
      )
    }
  }

  def testTuples[T <: SType](tpe: T) = {
    implicit val wWrapped: Gen[T#WrappedType] = wrappedTypeGen(tpe)
    val tT = Evaluation.stypeToRType(tpe)
    @nowarn implicit val tag: ClassTag[T#WrappedType] = tT.classTag
    implicit val tAny       : RType[Any]              = sigma.AnyType
    val withVersion = if (tpe == SHeader) {
      Some(VersionContext.V6SoftForkVersion)
    } else {
      None
    }
    forAll { in: (T#WrappedType, T#WrappedType) =>
      val (x,y) = (in._1, in._2)
      roundtrip[SType]((x, y).asWrappedType, STuple(tpe, tpe), withVersion)
      roundtrip[SType](TupleColl(x, y, x).asWrappedType, STuple(tpe, tpe, tpe), withVersion)
      roundtrip[SType](TupleColl(x, y, x, y).asWrappedType, STuple(tpe, tpe, tpe, tpe), withVersion)
      roundtrip[STuple](Colls.fromItems[Any](x, y, (x, y)), STuple(tpe, tpe, STuple(tpe, tpe)), withVersion)
      roundtrip[STuple](Colls.fromItems[Any](x, y, TupleColl(x, y, x)), STuple(tpe, tpe, STuple(tpe, tpe, tpe)), withVersion)
      roundtrip[STuple](Colls.fromItems[Any](x, y, TupleColl(x, y, (x, y))), STuple(tpe, tpe, STuple(tpe, tpe, STuple(tpe, tpe))), withVersion)
    }
  }

  def testOption[T <: SType](tpe: T) = {
    implicit val wWrapped: Gen[T#WrappedType] = wrappedTypeGen(tpe)
    val tT = Evaluation.stypeToRType(tpe)

    an[Exception] should be thrownBy (
      VersionContext.withVersions((VersionContext.V6SoftForkVersion - 1).toByte, 1) {
        forAll { in: T#WrappedType =>
          roundtrip[SType](Some(in).asWrappedType, SOption(tpe))
          roundtrip[SOption[SCollection[T]]](Some(Colls.fromItems(in)(tT)), SOption(SCollectionType(tpe)))
        }
      })

    VersionContext.withVersions(VersionContext.V6SoftForkVersion, 1) {
      forAll { in: T#WrappedType =>
        roundtrip[SType](Some(in).asWrappedType, SOption(tpe))
        roundtrip[SOption[T]](None, SOption(tpe))
        roundtrip[SOption[T]](Some(in), SOption(tpe))
        roundtrip[SOption[SCollection[T]]](Some(Colls.fromItems(in)(tT)), SOption(SCollectionType(tpe)))
        roundtrip[SCollection[SOption[T]]](Colls.fromItems(Option(in), None.asInstanceOf[Option[T#WrappedType]])(OptionType(tT)), SCollectionType(SOption(tpe)))
        roundtrip[SOption[SOption[T]]](None, SOption(SOption(tpe)))
        roundtrip[SOption[SOption[T]]](Some(Some(in)), SOption(SOption(tpe)))
        roundtrip[SOption[SOption[T]]](Some(None), SOption(SOption(tpe)))
      }
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
    forAll { x: Header => roundtrip[SHeader.type](x, SHeader, Some(VersionContext.V6SoftForkVersion)) }
    forAll { t: SPredefType => testCollection(t) }
    forAll { t: SPredefType => testTuples(t) }
    forAll { t: SPredefType => testOption(t) }
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
        t.getMessage.contains(s"Length of tuple $len exceeds ${0xFFFF} limit.")
      })

    val tooBig = SigmaDsl.BigInt(new BigInteger(Helpers.decodeBytes(
      "80e0ff7f02807fff72807f0a00ff7fb7c57f75c11ba2802970fd250052807fc37f6480ffff007fff18eeba44").toArray))

    assertExceptionThrown({
      val w = SigmaSerializer.startWriter()
      DataSerializer.serialize(tooBig.asWrappedType, SBigInt, w)
      val r = SigmaSerializer.startReader(w.toBytes)
      DataSerializer.deserialize(SBigInt, r)
    },
    { t =>
      t.isInstanceOf[SerializerException] &&
          t.getMessage.contains(s"BigInt value doesn't not fit into ${SBigInt.MaxSizeInBytes} bytes")
    })
  }

  property("nuanced versioned test for header roundtrip") {
    VersionContext.withVersions(VersionContext.V6SoftForkVersion, 1) {
      forAll { x: Header => roundtrip[SHeader.type](x, SHeader) }
    }

    an[SerializerException] should be thrownBy (
      VersionContext.withVersions((VersionContext.V6SoftForkVersion - 1).toByte, 1) {
        val h = headerGen.sample.get
        roundtrip[SHeader.type](h, SHeader)
      })
  }

  property("header vector") {
    val header = CHeader(
      0.toByte,
      Helpers.decodeBytes("7a7fe5347f09017818010062000001807f86808000ff7f66ffb07f7ad27f3362"),
      Helpers.decodeBytes("c1d70ad9b1ffc1fb9a715fff19807f2401017fcd8b73db017f1cff77727fff08"),
      Helpers.decodeBytes("54d23dd080006bdb56800100356080935a80ffb77e90b800057f00661601807f17"),
      Helpers.decodeBytes("5e7f1164ccd0990080c501fc0e0181cb387fc17f00ff00c7d5ff767f91ff5e68"),
      -7421721754642387858L,
      -4826493284887861030L,
      10,
      Helpers.decodeBytes("e580c88001ff6fc89c5501017f80e001ff0101fe48c153ff7f00666b80d780ab"),
      Helpers.decodeGroupElement("03e7f2875298fddd933c2e0a38968fe85bdeeb70dd8b389559a1d36e2ff1b58fc5"),
      Helpers.decodeGroupElement("034e2d3b5f9e409e3ae8a2e768340760362ca33764eda5855f7a43487f14883300"),
      Helpers.decodeBytes("974651c9efff7f00"),
      CBigInt(new BigInteger("478e827dfa1e4b57", 16)),
      Helpers.decodeBytes("01ff13"),
      Colls.emptyColl
    )

    VersionContext.withVersions(VersionContext.V6SoftForkVersion, 1) {
      roundtrip[SHeader.type](header, SHeader)
    }
  }

}
