package org.ergoplatform.sdk


import java.math.BigInteger
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import sigma.data.{CAnyValue, RType, SigmaBoolean, TupleColl}
import sigma.ast._
import sigma.ast.SCollection.SByteArray
import sigma.ast.SType.AnyOps
import sigma.crypto.EcPointType
import sigma.serialization.SerializerException
import sigma.util.Extensions.{BigIntegerOps, EcpOps, SigmaBooleanOps}
import sigma.Extensions.ArrayOps
import sigma.eval.SigmaDsl
import sigma.{AvlTree, Box, Colls, Evaluation}
import sigma.serialization.SerializationSpecification

import scala.annotation.nowarn
import scala.reflect.ClassTag

class DataJsonEncoderSpecification extends SerializationSpecification {
  object JsonCodecs extends JsonCodecs

  def roundtrip[T <: SType](obj: T#WrappedType, tpe: T) = {
    val json = DataJsonEncoder.encode(obj, tpe)
    val res = DataJsonEncoder.decode(json)
    res shouldBe obj
  }

  def testCollection[T <: SType](tpe: T) = {
    implicit val wWrapped = wrappedTypeGen(tpe)
    implicit val tT = Evaluation.stypeToRType(tpe)
    implicit val tagT = tT.classTag
    forAll { xs: Array[T#WrappedType] =>
      roundtrip[SCollection[T]](xs.toColl, SCollection(tpe))
      roundtrip[SType](xs.toColl.map(x => (x, x)).asWrappedType, SCollection(STuple(tpe, tpe)))

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
    implicit val wWrapped: Gen[T#WrappedType]      = wrappedTypeGen(tpe)
    val tT = Evaluation.stypeToRType(tpe)
    @nowarn implicit val tag     : ClassTag[T#WrappedType] = tT.classTag
    @nowarn implicit val tAny    : RType[Any]              = sigma.AnyType
    forAll { in: (T#WrappedType, T#WrappedType) =>
      val (x,y) = (in._1, in._2)
      roundtrip[SType]((x, y).asWrappedType, STuple(tpe, tpe))
      roundtrip[SType](((x, y), (x, y)).asWrappedType, STuple(STuple(tpe, tpe), STuple(tpe, tpe)))
      roundtrip[SType](((x, y), ((x, y), (x, y))).asWrappedType, STuple(STuple(tpe, tpe), STuple(STuple(tpe, tpe), STuple(tpe, tpe))))
    }
  }

  @nowarn def testAnyValue[T <: SType](tpe: T) = {
    implicit val wWrapped = wrappedTypeGen(tpe)
    implicit val tT = Evaluation.stypeToRType(tpe)
    implicit val tag = tT.classTag
    implicit val tAny = sigma.AnyType
    forAll { in: T#WrappedType =>
      val x = CAnyValue(in)
      val json = JsonCodecs.anyValueEncoder(x)
      val y = JsonCodecs.anyValueDecoder.decodeJson(json).right.get
      x shouldBe y
      
      implicit val tTup = Evaluation.stypeToRType(STuple(tpe, tpe)).asInstanceOf[RType[(T#WrappedType, T#WrappedType)]]
      val xTup = CAnyValue((in, in))
      val jsonTup = JsonCodecs.anyValueEncoder(xTup)
      val yTup = JsonCodecs.anyValueDecoder.decodeJson(jsonTup).right.get
      xTup shouldBe yTup

      implicit val tColl = Evaluation.stypeToRType(SCollection(tpe))
      val xColl = CAnyValue(SigmaDsl.Colls.fromItems(in, in))
      val jsonColl = JsonCodecs.anyValueEncoder(xColl)
      val yColl = JsonCodecs.anyValueDecoder.decodeJson(jsonColl).right.get
      xColl shouldBe yColl
    }
  }

  property("Data Json serialization round trip") {
    forAll { x: Byte => roundtrip[SByte.type](x, SByte) }
    forAll { x: Boolean => roundtrip[SBoolean.type](x, SBoolean) }
    forAll { x: Long => roundtrip[SLong.type](x, SLong) }
    forAll { x: String => roundtrip[SString.type](x, SString) }
    forAll { x: BigInteger => roundtrip[SBigInt.type](x.toBigInt, SBigInt) }
    forAll { x: EcPointType => roundtrip[SGroupElement.type](x.toGroupElement, SGroupElement) }
    forAll { x: SigmaBoolean => roundtrip[SSigmaProp.type](x.toSigmaProp, SSigmaProp) }
    forAll { x: AvlTree => roundtrip[SAvlTree.type](x, SAvlTree) }
    forAll { x: Array[Byte] => roundtrip[SByteArray](x.toColl, SByteArray) }
    forAll { x: Box => roundtrip[SBox.type](x, SBox) }
    forAll { x: Option[Byte] => roundtrip[SOption[SByte.type]](x, SOption[SByte.type]) }
    testCollection(SOption[SLong.type])
    testTuples(SOption[SLong.type])
    forAll { t: SPredefType => testCollection(t) }
    forAll { t: SPredefType => testTuples(t) }
  }

  property("Example test") {
    def toUnifiedString(from: String): String = from.replaceAll("[\n ]", "")

    toUnifiedString(DataJsonEncoder.encode(().asWrappedType, SUnit).toString()) shouldBe
      toUnifiedString(
        """
          |{ "type": "Unit",
          |  "value": {}
          |}""".stripMargin)
    toUnifiedString(DataJsonEncoder.encode(Some(10).asWrappedType, SOption(SInt)).toString()) shouldBe
      toUnifiedString(
        """
          |{ "type": "Option[Int]",
          |  "value": [10]
          |}""".stripMargin)
    toUnifiedString(DataJsonEncoder.encode(None.asWrappedType, SOption(SInt)).toString()) shouldBe
      toUnifiedString(
        """
          |{ "type": "Option[Int]",
          |  "value": null
          |}""".stripMargin)
    toUnifiedString(DataJsonEncoder.encode(Some(None).asWrappedType, SOption(SOption(SInt))).toString()) shouldBe
      toUnifiedString(
        """
          |{ "type": "Option[Option[Int]]",
          |  "value": [null]
          |}""".stripMargin)
    toUnifiedString(DataJsonEncoder.encode((10, 20).asWrappedType, STuple(SInt, SInt)).toString()) shouldBe
      toUnifiedString(
        """
          |{ "type": "(Int, Int)",
          |  "value": {
          |    "_1": 10,
          |    "_2": 20
          |  }
          |}""".stripMargin)
    toUnifiedString(DataJsonEncoder.encode(SigmaDsl.Colls.fromItems(1.toByte, 2.toByte, 3.toByte).asWrappedType, SCollectionType(SByte)).toString()) shouldBe
      toUnifiedString(
        """
          |{ "type": "Coll[Byte]",
          |  "value": [1, 2, 3]
          |}""".stripMargin)
    toUnifiedString(DataJsonEncoder.encode(SigmaDsl.Colls.fromItems((1, 10), (2, 20), (3, 30)).asWrappedType, SCollectionType(STuple(SInt, SInt))).toString()) shouldBe
      toUnifiedString(
        """
          |{ "type": "Coll[(Int, Int)]",
          |  "value": {
          |    "_1": [1, 2, 3],
          |    "_2": [10, 20, 30]
          |  }
          |}""".stripMargin)
    toUnifiedString(
      DataJsonEncoder.encode(SigmaDsl.Colls.pairColl(
        SigmaDsl.Colls.fromItems(1, 2, 3),
        SigmaDsl.Colls.fromItems(10, 20, 30)
      ).asWrappedType, SCollectionType(STuple(SInt, SInt))).toString()) shouldBe
      toUnifiedString(
        """
          |{ "type": "Coll[(Int, Int)]",
          |  "value": {
          |    "_1": [1, 2, 3],
          |    "_2": [10, 20, 30]
          |  }
          |}""".stripMargin)
    toUnifiedString(DataJsonEncoder.encode((SigmaDsl.Colls.fromItems((1, SigmaDsl.Colls.replicate(3, 1.toByte)), (2, SigmaDsl.Colls.replicate(2, 2.toByte)), (3, SigmaDsl.Colls.replicate(1, 3.toByte)), (4, SigmaDsl.Colls.replicate(0, 4.toByte))), 100.toLong).asWrappedType,
      STuple(SCollectionType(STuple(SInt, SCollectionType(SByte))), SLong)).toString()) shouldBe
      toUnifiedString(
        """
          |{ "type": "(Coll[(Int, Coll[Byte])], Long)",
          |  "value": {
          |    "_1": { "_1": [1, 2, 3, 4], "_2": [ [1, 1, 1], [2, 2], [3], [] ] },
          |    "_2": 100
          |  }
          |}
          |""".stripMargin)
  }

  def testEncodeError[T <: SType](tpe: T) = {
    implicit val wWrapped = wrappedTypeGen(tpe)
    val tT = Evaluation.stypeToRType(tpe)
    @nowarn implicit val tag = tT.classTag
    @nowarn implicit val tAny = sigma.AnyType
    forAll { x: T#WrappedType =>
      an[SerializerException] should be thrownBy {
        DataJsonEncoder.encode(TupleColl(x, x, x).asWrappedType, STuple(tpe, tpe, tpe))
      }

      // supported case
      DataJsonEncoder.encode(SigmaDsl.Colls.fromItems(TupleColl(x, x)).asWrappedType, SCollection(STuple(tpe, tpe)))

      // not supported case
      an[SerializerException] should be thrownBy {
        DataJsonEncoder.encode(SigmaDsl.Colls.fromItems(TupleColl(x, x, x)).asWrappedType, SCollection(STuple(tpe, tpe, tpe)))
      }
    }
  }

  property("AnyValue") {
    forAll { t: SPredefType =>
      testAnyValue(t)
      testAnyValue(SOption(t))
    }
  }

  property("Tuples with > 2 items are not supported") {
    forAll { t: SPredefType =>
      testEncodeError(t)
    }
  }

  property("Reject decoding Tuple with more than 2 items") {
    val pair = io.circe.parser.parse(
      """
       |{ "type": "(Int, Int)",
       |  "value": {
       |    "_1": 10,
       |    "_2": 20
       |  }
       |}""".stripMargin).right.get

    DataJsonEncoder.decode(pair) shouldBe (10, 20)

    val triple = io.circe.parser.parse(
      """
       |{ "type": "(Int, Int, Int)",
       |  "value": {
       |    "_1": 10,
       |    "_2": 20,
       |    "_3": 30
       |  }
       |}""".stripMargin).right.get

     assertExceptionThrown(
       DataJsonEncoder.decode(triple),
       { t =>
         t.isInstanceOf[SerializerException] &&
         t.getMessage.contains("Tuples with length not equal to 2 are not supported")
       }
     )

    val tripleColl = io.circe.parser.parse(
      """
       |{ "type": "Coll[(Int, Int, Int)]",
       |  "value": {
       |    "_1": [1, 2, 3],
       |    "_2": [10, 20, 30]
       |  }
       |}""".stripMargin).right.get

    assertExceptionThrown(
      DataJsonEncoder.decode(tripleColl),
      { t =>
        t.isInstanceOf[SerializerException] &&
            t.getMessage.contains("Tuples with length not equal to 2 are not supported")
      }
    )
  }
}
