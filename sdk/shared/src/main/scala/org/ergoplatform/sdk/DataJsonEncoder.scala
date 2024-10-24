package org.ergoplatform.sdk

import java.math.BigInteger
import io.circe._
import io.circe.syntax._
import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.{NonMandatoryRegisterId, Token}
import sigma.data.{CAnyValue, RType}
import scorex.util._
import sigmastate.lang.SigmaParser
import sigmastate.eval._
import sigma._
import debox.cfor
import scorex.util.encode.Base16

import scala.collection.compat.immutable.ArraySeq
import scala.collection.mutable
import sigma.ast._
import sigma.eval.SigmaDsl
import sigma.serialization.SerializerException
import sigma.serialization.{DataSerializer, SigmaSerializer}
import sigma.serialization.ErgoTreeSerializer

object DataJsonEncoder {
  def encode[T <: SType](v: T#WrappedType, tpe: T): Json = {
    val encodedType = tpe.toTermString
    val encodedData = encodeData(v, tpe)
    Json.obj(
      "type" -> Json.fromString(encodedType),
      "value" -> encodedData
    )
  }

  private def encodeBytes: Encoder[Array[Byte]] = Encoder.instance((bytes: Array[Byte]) => {
    Base16.encode(bytes).asJson
  })

  def encodeAnyValue(v: AnyValue): Json = {
    val encodedType = Evaluation.rtypeToSType(v.tVal)
    val encodedData = encodeData[SType](v.value.asInstanceOf[SType#WrappedType], encodedType)
    Json.obj(
      "type" -> Json.fromString(encodedType.toTermString),
      "value" -> encodedData
    )
  }

  def encodeData[T <: SType](v: T#WrappedType, tpe: T): Json = tpe match {
    case SUnit => Json.fromFields(ArraySeq.empty)
    case SBoolean => v.asInstanceOf[Boolean].asJson
    case SByte => v.asInstanceOf[Byte].asJson
    case SShort => v.asInstanceOf[Short].asJson
    case SInt => v.asInstanceOf[Int].asJson
    case SLong => v.asInstanceOf[Long].asJson
    case SBigInt =>
      encodeBytes(v.asInstanceOf[BigInt].toBytes.toArray)
    case SString =>
      encodeBytes(v.asInstanceOf[String].getBytes)
    case tColl: SCollectionType[a] =>
      val coll = v.asInstanceOf[tColl.WrappedType]
      tColl.elemType match {
        case tup: STuple =>
          val tArr = tup.items.toArray
          if (tArr.length != 2) {
            throw new SerializerException("Tuples with length not equal to 2 are not supported")
          }
          val rtypeArr = tArr.map(x => Evaluation.stypeToRType(x))

          val leftSource = { // this code works both for Scala 2.12 and 2.13
            implicit val ct = rtypeArr(0).classTag
            mutable.ArrayBuilder.make[SType#WrappedType] // make's signature is changed in 2.13
          }
          val rightSource = {
            implicit val ct = rtypeArr(1).classTag
            mutable.ArrayBuilder.make[SType#WrappedType]
          }
          cfor(0)(_ < coll.length, _ + 1) { i =>
            val arr = Evaluation.fromDslTuple(coll(i), tup).asInstanceOf[tup.WrappedType]
            leftSource += arr(0)
            rightSource += arr(1)
          }
          val left = Colls.fromArray(leftSource.result())(rtypeArr(0)).asInstanceOf[SType#WrappedType]
          val leftType: SType = SCollectionType(tArr(0))
          val right = Colls.fromArray(rightSource.result())(rtypeArr(1)).asInstanceOf[SType#WrappedType]
          val rightType: SType = SCollectionType(tArr(1))

          Json.fromFields(List(
            "_1" -> encodeData[SType](left, leftType),
            "_2" -> encodeData[SType](right, rightType)
          ))
        case _ =>
          val jsons = mutable.ArrayBuffer.empty[Json]
          cfor(0)(_ < coll.length, _ + 1) { i =>
            val x = coll(i)
            jsons += encodeData(x, tColl.elemType)
          }
          Json.fromValues(jsons.toSeq)
      }

    case tOpt: SOption[a] =>
      val opt = v.asInstanceOf[tOpt.WrappedType]
      if (opt.isDefined) {
        // save the single value as an array with one item
        val valueJson = encodeData(opt.get, tOpt.elemType)
        Json.fromValues(Array(valueJson))
      } else {
        Json.Null
      }
    case t: STuple =>
      val arr = Evaluation.fromDslTuple(v, t).asInstanceOf[t.WrappedType]
      val tArr = t.items.toArray
      if (tArr.length != 2) {
        throw new SerializerException("Tuples with length not equal to 2 are not supported")
      }
      val len = arr.length
      assert(len == tArr.length, s"Type $t doesn't correspond to value $arr")
      val obj = mutable.ArrayBuffer.empty[(String, Json)]
      cfor(0)(_ < len, _ + 1) { i =>
        obj += (s"_${i + 1}" -> encodeData[SType](arr(i), tArr(i)))
      }
      Json.fromFields(obj)
    case SGroupElement =>
      val w = SigmaSerializer.startWriter()
      DataSerializer.serialize(v, tpe, w)
      encodeBytes(w.toBytes)
    case SHeader =>
      val w = SigmaSerializer.startWriter()
      DataSerializer.serialize(v, tpe, w)
      encodeBytes(w.toBytes)
    case SAvlTree =>
      val w = SigmaSerializer.startWriter()
      DataSerializer.serialize(v, tpe, w)
      encodeBytes(w.toBytes)
    case SSigmaProp =>
      val w = SigmaSerializer.startWriter()
      DataSerializer.serialize(v, tpe, w)
      encodeBytes(w.toBytes)
    case SBox =>
      val ergoBox = v.asInstanceOf[Box]
      val obj = mutable.ArrayBuffer.empty[(String, Json)]
      obj += ("value" -> encodeData(ergoBox.value.asInstanceOf[SType#WrappedType], SLong))
      obj += ("ergoTree" -> encodeBytes(ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(ergoBox.ergoTree)))
      obj += "tokens" -> encodeData(
        ergoBox.additionalTokens.asInstanceOf[SType#WrappedType],
        SCollectionType(STuple(SCollectionType(SByte), SLong))
      )
      ergoBox.additionalRegisters.foreach { case (id, value) =>
        obj += (s"r${id.number}" -> encode[SType](value.value, value.tpe))
      }
      obj += ("txId" -> encodeBytes(ergoBox.transactionId.toBytes))
      obj += ("index" -> encodeData(ergoBox.index.asInstanceOf[SType#WrappedType], SShort))
      obj += ("creationHeight" -> encodeData(ergoBox.creationHeight.asInstanceOf[SType#WrappedType], SInt))
      Json.fromFields(obj)
    case t => throw new SerializerException(s"Not defined DataSerializer for type $t")
  }

  private def decodeBytes(json: Json): Array[Byte] = {
    val jsonStr = json.as[String]
    jsonStr match {
      case Right(jsonStr) => Base16.decode(jsonStr).get
      case Left(error) => throw new SerializerException(error.getMessage)
    }
  }

  def decodeData[T <: SType](json: Json, tpe: T): (T#WrappedType) = {
    val res = (tpe match {
      case SUnit => ()
      case SBoolean => json.asBoolean.get
      case SByte => json.asNumber.get.toByte.get
      case SShort => json.asNumber.get.toShort.get
      case SInt => json.asNumber.get.toInt.get
      case SLong => json.asNumber.get.toLong.get
      case SBigInt =>
        SigmaDsl.BigInt(new BigInteger(decodeBytes(json)))
      case SString =>
        new String(decodeBytes(json))
      case tColl: SCollectionType[a] =>
        val tpeElem = tColl.elemType
        decodeColl(json, tpeElem)
      case tOpt: SOption[a] =>
        if (json == Json.Null) {
          None
        } else {
          // read the array with single value
          val items = decodeColl(json, tOpt.elemType)
          Some(items(0))
        }
      case t: STuple =>
        val tArr = t.items.toArray
        if (tArr.length != 2) {
          throw new SerializerException("Tuples with length not equal to 2 are not supported")
        }
        val collSource = mutable.ArrayBuilder.make[Any]
        cfor(1)(_ <= tArr.length, _ + 1) { i =>
          collSource += decodeData(json.hcursor.downField(s"_${i}").focus.get, tArr(i - 1))
        }
        val coll = Colls.fromArray(collSource.result())(sigma.AnyType)
        Evaluation.toDslTuple(coll, t)
      case SGroupElement =>
        val str = decodeBytes(json)
        val r = SigmaSerializer.startReader(str)
        DataSerializer.deserialize(SGroupElement, r)
      case SAvlTree =>
        val str = decodeBytes(json)
        val r = SigmaSerializer.startReader(str)
        DataSerializer.deserialize(SAvlTree, r)
      case SSigmaProp =>
        val str = decodeBytes(json)
        val r = SigmaSerializer.startReader(str)
        DataSerializer.deserialize(SSigmaProp, r)
      case SHeader => // for Sigma < 6.0 , exception will be thrown by DataSerializer
        val str = decodeBytes(json)
        val r = SigmaSerializer.startReader(str)
        DataSerializer.deserialize(SHeader, r)
      case SBox =>
        val value = decodeData(json.hcursor.downField(s"value").focus.get, SLong)
        val tree = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(decodeBytes(json.hcursor.downField(s"ergoTree").focus.get))
        val tokens = decodeData(
          json.hcursor.downField(s"tokens").focus.get,
          SCollectionType(STuple(SCollectionType(SByte), SLong))).asInstanceOf[Coll[Token]]
        val txId = decodeBytes(json.hcursor.downField(s"txId").focus.get).toModifierId
        val index = decodeData(json.hcursor.downField(s"index").focus.get, SShort)
        val creationHeight = decodeData(json.hcursor.downField(s"creationHeight").focus.get, SInt)
        val additionalRegisters = mutable.ArrayBuffer.empty[(NonMandatoryRegisterId, _ <: EvaluatedValue[_ <: SType])]
        for (register <- ErgoBox.nonMandatoryRegisters) {
          val opt = json.hcursor.downField(s"r${register.number}").focus
          if (opt.isDefined && !opt.get.isNull) {
            val (decoded, tpe) = decodeWithTpe(opt.get)
            additionalRegisters += (register -> Constant(decoded, tpe))
          }
        }
        SigmaDsl.Box(new ErgoBox(value, tree, tokens, additionalRegisters.toMap, txId, index, creationHeight))
      case t =>
        throw new SerializerException(s"Not defined DataSerializer for type $t")
    }).asInstanceOf[T#WrappedType]
    res
  }

  private def decodeColl[T <: SType](json: Json, tpe: T): Coll[T#WrappedType] = {
    implicit val tItem = (tpe match {
      case tTup: STuple if tTup.items.length == 2 =>
        Evaluation.stypeToRType(tpe)
      case _: STuple =>
        throw new SerializerException("Tuples with length not equal to 2 are not supported")
      case _ =>
        Evaluation.stypeToRType(tpe)
    }).asInstanceOf[RType[T#WrappedType]]

    tpe match {
      case tup: STuple =>
        val tArr = tup.items.toArray
        val leftColl = decodeColl(json.hcursor.downField(s"_1").focus.get, tArr(0))
        val rightColl = decodeColl(json.hcursor.downField(s"_2").focus.get, tArr(1))
        assert(leftColl.length == rightColl.length)
        SigmaDsl.Colls.pairColl(leftColl, rightColl).asInstanceOf[Coll[T#WrappedType]]
      case _ =>
        val jsonList = json.as[List[Json]]
        jsonList match {
          case Right(jsonList) =>
            val collSource = { // this code works both for Scala 2.12 and 2.13
              implicit val ct = tItem.classTag
              mutable.ArrayBuilder.make[T#WrappedType]
            }
            for (i <- jsonList) {
              collSource += decodeData(i, tpe).asInstanceOf[T#WrappedType]
            }
            Colls.fromArray(collSource.result())
          case Left(error) => throw new SerializerException(error.getMessage)
        }
    }
  }

  private def decodeWithTpe(json: Json): (SType#WrappedType, SType) = {
    val tpe = SigmaParser.parseType(json.hcursor.downField("type").focus.get.asString.get)
    val value = json.hcursor.downField("value").focus.get
    val data = decodeData(value, tpe)
    (data, tpe)
  }

  def decode(json: Json): SType#WrappedType = {
    val (data, _) = decodeWithTpe(json)
    data
  }

  def decodeAnyValue(json: Json): AnyValue = {
    val tpe = SigmaParser.parseType(json.hcursor.downField("type").focus.get.asString.get)
    val value = json.hcursor.downField("value").focus.get
    val data = decodeData(value, tpe)
    CAnyValue(data, Evaluation.stypeToRType(tpe).asInstanceOf[RType[Any]])
  }
}
