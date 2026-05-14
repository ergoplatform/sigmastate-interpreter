package org.ergoplatform.sdk

import java.math.BigInteger
import io.circe._
import io.circe.syntax._
import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.{NonMandatoryRegisterId, Token}
import sigma.data.{CAnyValue, CollType, OptionType, PairType, RType, TupleType}
import scorex.util._
import sigmastate.lang.SigmaParser
import sigmastate.eval._
import sigma._
import debox.cfor
import scorex.util.encode.Base16

import scala.collection.compat.immutable.ArraySeq
import scala.collection.mutable
import sigma.ast._
import sigma.crypto.BigIntegers
import sigma.eval.SigmaDsl
import sigma.serialization.SerializerException
import sigma.serialization.{DataSerializer, SigmaSerializer}
import sigma.serialization.ErgoTreeSerializer

object DataJsonEncoder {
  // TODO: consider switching the JSON envelope to be RType-based in the next major
  // SDK version. This would require a String -> RType parser (today the "type" 
  // field is parsed by SigmaParser into SType), and would enable encoding of
  // RType-only types (FuncType, ThunkType, ArrayType, GeneralType/AnyType).
  // The current refactor keeps the on-wire format byte-identical and only moves
  // the internal dispatch to RType.
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

  // TODO: see note on `encode` above. Once the envelope is RType-based, this can
  // avoid the rtypeToSType conversion entirely.
  def encodeAnyValue(v: AnyValue): Json = {
    val rtype = v.tVal.asInstanceOf[RType[Any]]
    val encodedType = Evaluation.rtypeToSType(rtype)
    val encodedData = encodeRData(v.value, rtype)
    Json.obj(
      "type" -> Json.fromString(encodedType.toTermString),
      "value" -> encodedData
    )
  }

  /** SType-typed entry point. Kept for backward compatibility; delegates to
    * the RType-based implementation. */
  def encodeData[T <: SType](v: T#WrappedType, tpe: T): Json = {
    val rtype = Evaluation.stypeToRType(tpe).asInstanceOf[RType[Any]]
    encodeRData(v.asInstanceOf[Any], rtype)
  }

  /** RType-driven encoding of a data value into Json.
    *
    * Note: this intentionally does not yet handle RType-only descriptors
    * (FuncType, ThunkType, ArrayType, GeneralType/AnyType) — see the
    * TODO comment on `encode` above.
    */
  private def encodeRData[A](v: A, t: RType[A]): Json = t match {
    case UnitType => Json.fromFields(ArraySeq.empty)
    case BooleanType => v.asInstanceOf[Boolean].asJson
    case ByteType => v.asInstanceOf[Byte].asJson
    case ShortType => v.asInstanceOf[Short].asJson
    case IntType => v.asInstanceOf[Int].asJson
    case LongType => v.asInstanceOf[Long].asJson
    case BigIntRType =>
      encodeBytes(v.asInstanceOf[BigInt].toBytes.toArray)
    case UnsignedBigIntRType =>
      encodeBytes(v.asInstanceOf[UnsignedBigInt].toBytes.toArray)
    case StringType =>
      encodeBytes(v.asInstanceOf[String].getBytes)
    case ct: CollType[a] =>
      val coll = v.asInstanceOf[Coll[a]]
      ct.tItem match {
        case _: TupleType =>
          throw new SerializerException("Tuples with length not equal to 2 are not supported")

        case pt: PairType[x, y] =>
          val len = coll.length
          val leftSource = {
            implicit val ctg = pt.tFst.classTag
            mutable.ArrayBuilder.make[x]
          }
          val rightSource = {
            implicit val ctg = pt.tSnd.classTag
            mutable.ArrayBuilder.make[y]
          }
          cfor(0)(_ < len, _ + 1) { i =>
            (coll(i): Any) match {
              case t: Tuple2[_, _] =>
                leftSource += t._1.asInstanceOf[x]
                rightSource += t._2.asInstanceOf[y]
              case c: Coll[Any] @unchecked =>
                leftSource += c(0).asInstanceOf[x]
                rightSource += c(1).asInstanceOf[y]
              case other =>
                sys.error(s"Cannot encode collection element $other as $pt")
            }
          }
          val left = Colls.fromArray(leftSource.result())(pt.tFst)
          val right = Colls.fromArray(rightSource.result())(pt.tSnd)
          Json.fromFields(List(
            "_1" -> encodeRData[Coll[x]](left, CollType(pt.tFst)),
            "_2" -> encodeRData[Coll[y]](right, CollType(pt.tSnd))
          ))
        case elem =>
          val itemRType = elem.asInstanceOf[RType[Any]]
          val jsons = mutable.ArrayBuffer.empty[Json]
          cfor(0)(_ < coll.length, _ + 1) { i =>
            jsons += encodeRData(coll(i).asInstanceOf[Any], itemRType)
          }
          Json.fromValues(jsons.toSeq)
      }

    case ot: OptionType[a] =>
      val opt = v.asInstanceOf[Option[a]]
      if (opt.isDefined) {
        // save the single value as an array with one item
        Json.fromValues(Array(encodeRData(opt.get, ot.tA)))
      } else {
        Json.Null
      }
    case _: TupleType =>
      // STuple(2) is converted to PairType (not TupleType) by stypeToRType,
      // so this branch is effectively unreachable in practice. Kept defensive.
      throw new SerializerException("Tuples with length not equal to 2 are not supported")
    
    case pt: PairType[a, b] =>
      val (fst, snd): (Any, Any) = (v: Any) match {
        case t: Tuple2[_, _] => (t._1, t._2)
        case c: Coll[Any] @unchecked => (c(0), c(1))
        case other => sys.error(s"Cannot encode pair value $other as $pt")
      }
      Json.fromFields(List(
        "_1" -> encodeRData(fst, pt.tFst.asInstanceOf[RType[Any]]),
        "_2" -> encodeRData(snd, pt.tSnd.asInstanceOf[RType[Any]])
      ))
    case GroupElementRType =>
      val stype = Evaluation.rtypeToSType(t)
      val w = SigmaSerializer.startWriter()
      DataSerializer.serialize(v.asInstanceOf[SType#WrappedType], stype, w)
      encodeBytes(w.toBytes)
    case HeaderRType =>
      val stype = Evaluation.rtypeToSType(t)
      val w = SigmaSerializer.startWriter()
      DataSerializer.serialize(v.asInstanceOf[SType#WrappedType], stype, w)
      encodeBytes(w.toBytes)
    case AvlTreeRType =>
      val stype = Evaluation.rtypeToSType(t)
      val w = SigmaSerializer.startWriter()
      DataSerializer.serialize(v.asInstanceOf[SType#WrappedType], stype, w)
      encodeBytes(w.toBytes)
    case SigmaPropRType =>
      val stype = Evaluation.rtypeToSType(t)
      val w = SigmaSerializer.startWriter()
      DataSerializer.serialize(v.asInstanceOf[SType#WrappedType], stype, w)
      encodeBytes(w.toBytes)
    case BoxRType =>
      val ergoBox = v.asInstanceOf[Box]
      val obj = mutable.ArrayBuffer.empty[(String, Json)]
      obj += ("value" -> encodeRData(ergoBox.value, LongType))
      obj += ("ergoTree" -> encodeBytes(ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(ergoBox.ergoTree)))
      val tokensRType: RType[Coll[(Coll[Byte], Long)]] =
        CollType(PairType(CollType(ByteType), LongType))
      obj += "tokens" -> encodeRData(
        ergoBox.additionalTokens.asInstanceOf[Coll[(Coll[Byte], Long)]],
        tokensRType
      )
      ergoBox.additionalRegisters.foreach { case (id, value) =>
        obj += (s"r${id.number}" -> encode[SType](value.value, value.tpe))
      }
      obj += ("txId" -> encodeBytes(ergoBox.transactionId.toBytes))
      obj += ("index" -> encodeRData(ergoBox.index, ShortType))
      obj += ("creationHeight" -> encodeRData(ergoBox.creationHeight, IntType))
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

  /** SType-typed entry point. Kept for backward compatibility; delegates to
    * the RType-based implementation. */
  def decodeData[T <: SType](json: Json, tpe: T): (T#WrappedType) = {
    val rtype = Evaluation.stypeToRType(tpe).asInstanceOf[RType[Any]]
    decodeRData(json, rtype).asInstanceOf[T#WrappedType]
  }

  /** RType-driven decoding of a data value from Json. See [[encodeRData]] for
    * the set of supported types. */
  private def decodeRData[A](json: Json, t: RType[A]): A = {
    val res: Any = t match {
      case UnitType => ()
      case BooleanType => json.asBoolean.get
      case ByteType => json.asNumber.get.toByte.get
      case ShortType => json.asNumber.get.toShort.get
      case IntType => json.asNumber.get.toInt.get
      case LongType => json.asNumber.get.toLong.get
      case BigIntRType =>
        SigmaDsl.BigInt(new BigInteger(decodeBytes(json)))
      case UnsignedBigIntRType =>
        SigmaDsl.UnsignedBigInt(BigIntegers.fromUnsignedByteArray(decodeBytes(json)))
      case StringType =>
        new String(decodeBytes(json))
      case ct: CollType[a] =>
        decodeRColl(json, ct.tItem)
      case ot: OptionType[a] =>
        if (json == Json.Null) {
          None
        } else {
          val items = decodeRColl(json, ot.tA)
          Some(items(0))
        }
      case pt: PairType[a, b] =>
        val l = decodeRData(json.hcursor.downField("_1").focus.get, pt.tFst)
        val r = decodeRData(json.hcursor.downField("_2").focus.get, pt.tSnd)
        (l, r)
      case _: TupleType =>
        throw new SerializerException("Tuples with length not equal to 2 are not supported")
      case GroupElementRType =>
        val stype = Evaluation.rtypeToSType(t)
        val str = decodeBytes(json)
        val r = SigmaSerializer.startReader(str)
        DataSerializer.deserialize(stype, r)
      case AvlTreeRType =>
        val stype = Evaluation.rtypeToSType(t)
        val str = decodeBytes(json)
        val r = SigmaSerializer.startReader(str)
        DataSerializer.deserialize(stype, r)
      case SigmaPropRType =>
        val stype = Evaluation.rtypeToSType(t)
        val str = decodeBytes(json)
        val r = SigmaSerializer.startReader(str)
        DataSerializer.deserialize(stype, r)
      case HeaderRType =>
        val stype = Evaluation.rtypeToSType(t)
        val str = decodeBytes(json)
        val r = SigmaSerializer.startReader(str)
        DataSerializer.deserialize(stype, r)
      case BoxRType =>
        val value = decodeRData(json.hcursor.downField("value").focus.get, LongType)
        val tree = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(decodeBytes(json.hcursor.downField("ergoTree").focus.get))
        val tokensRType: RType[Coll[(Coll[Byte], Long)]] =
          CollType(PairType(CollType(ByteType), LongType))
        val tokens = decodeRData(json.hcursor.downField("tokens").focus.get, tokensRType)
          .asInstanceOf[Coll[Token]]
        val txId = decodeBytes(json.hcursor.downField("txId").focus.get).toModifierId
        val index = decodeRData(json.hcursor.downField("index").focus.get, ShortType)
        val creationHeight = decodeRData(json.hcursor.downField("creationHeight").focus.get, IntType)
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
    }
    res.asInstanceOf[A]
  }

  private def decodeRColl[A](json: Json, tItem: RType[A]): Coll[A] = {
    tItem match {
      case pt: PairType[x, y] =>
        val left = decodeRColl(json.hcursor.downField("_1").focus.get, pt.tFst)
        val right = decodeRColl(json.hcursor.downField("_2").focus.get, pt.tSnd)
        assert(left.length == right.length)
        SigmaDsl.Colls.pairColl(left, right).asInstanceOf[Coll[A]]
      case _: TupleType =>
        throw new SerializerException("Tuples with length not equal to 2 are not supported")
      case _ =>
        val jsonList = json.as[List[Json]]
        jsonList match {
          case Right(list) =>
            val collSource = {
              implicit val ct = tItem.classTag
              mutable.ArrayBuilder.make[A]
            }
            for (i <- list) {
              collSource += decodeRData(i, tItem)
            }
            Colls.fromArray(collSource.result())(tItem)
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
    val rtype = Evaluation.stypeToRType(tpe).asInstanceOf[RType[Any]]
    val data = decodeRData(value, rtype)
    CAnyValue(data, rtype)
  }
}
