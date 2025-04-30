package org.ergoplatform.sdk

import cats.syntax.either._
import io.circe._
import io.circe.syntax._
import org.ergoplatform.ErgoBox.{NonMandatoryRegisterId, Token, TokenId}
import org.ergoplatform._
import org.ergoplatform.settings.ErgoAlgos
import org.ergoplatform.validation.SigmaValidationSettingsSerializer
import scorex.crypto.authds.{ADDigest, ADKey}
import scorex.crypto.hash.Digest32
import scorex.util.ModifierId
import sigma.Extensions.ArrayOps
import sigma.ast.{ErgoTree, EvaluatedValue, SType}
import sigma.data.{AvlTreeData, AvlTreeFlags, CBigInt, Digest32Coll, WrapperOf}
import sigma.eval.Extensions.EvalIterableOps
import sigma.eval.SigmaDsl
import sigma.interpreter.{ContextExtension, ProverResult, SigmaMap}
import sigma.serialization.{ErgoTreeSerializer, ValueSerializer}
import sigma.validation.SigmaValidationSettings
import sigma.{AnyValue, Coll, Colls, Header, PreHeader, SigmaException}
import sigmastate.eval.{CPreHeader, _}
import sigmastate.utils.Helpers._

import java.math.BigInteger
import scala.collection.mutable
import scala.util.Try

trait JsonCodecs {

  def fromTry[T](tryResult: Try[T])(implicit cursor: ACursor): Either[DecodingFailure, T] = {
    tryResult.fold(e => Left(DecodingFailure(e.toString, cursor.history)), Right.apply)
  }

  def fromOption[T](maybeResult: Option[T])(implicit cursor: ACursor): Either[DecodingFailure, T] = {
    maybeResult.fold[Either[DecodingFailure, T]](Left(DecodingFailure("No value found", cursor.history)))(Right.apply)
  }

  def fromThrows[T](throwsBlock: => T)(implicit cursor: ACursor): Either[DecodingFailure, T] = {
    Either.catchNonFatal(throwsBlock).leftMap(e => DecodingFailure(e.toString, cursor.history))
  }

  private def bytesDecoder[T](transform: Array[Byte] => T): Decoder[T] = Decoder.instance({ implicit cursor =>
    for {
      str <- cursor.as[String]
      bytes <- fromTry(ErgoAlgos.decode(str))
    } yield transform(bytes)
  })

  implicit val anyValueEncoder: Encoder[AnyValue] = Encoder.instance({ anyval => DataJsonEncoder.encodeAnyValue(anyval) })

  implicit val anyValueDecoder: Decoder[AnyValue] = Decoder.instance({ implicit cursor =>
    fromTry(Try.apply(DataJsonEncoder.decodeAnyValue(cursor.value)))
  })

  implicit val sigmaBigIntEncoder: Encoder[sigma.BigInt] = Encoder.instance({ bigInt =>
    JsonNumber.fromDecimalStringUnsafe(bigInt.asInstanceOf[WrapperOf[BigInteger]].wrappedValue.toString).asJson
  })

  implicit val sigmaBigIntDecoder: Decoder[sigma.BigInt] = Decoder.instance({ implicit cursor =>
    for {
      jsonNumber <- cursor.as[JsonNumber]
      bigInt <- fromOption(jsonNumber.toBigInt)
    } yield CBigInt(bigInt.bigInteger)
  })

  implicit val arrayBytesEncoder: Encoder[Array[Byte]] = Encoder.instance(ErgoAlgos.encode(_).asJson)
  implicit val arrayBytesDecoder: Decoder[Array[Byte]] = bytesDecoder(x => x)

  implicit val collBytesEncoder: Encoder[Coll[Byte]] = Encoder.instance(ErgoAlgos.encode(_).asJson)
  implicit val collBytesDecoder: Decoder[Coll[Byte]] = bytesDecoder(Colls.fromArray(_))

  implicit val adKeyEncoder: Encoder[ADKey] = Encoder.instance(_.array.asJson)
  implicit val adKeyDecoder: Decoder[ADKey] = bytesDecoder(ADKey @@ _)

  implicit val adDigestEncoder: Encoder[ADDigest] = Encoder.instance(_.array.asJson)
  implicit val adDigestDecoder: Decoder[ADDigest] = bytesDecoder(ADDigest @@ _)

  implicit val digest32Encoder: Encoder[Digest32] = Encoder.instance(_.array.asJson)
  implicit val digest32Decoder: Decoder[Digest32] = bytesDecoder(Digest32 @@ _)

  implicit val digest32CollEncoder: Encoder[Digest32Coll] = Encoder.instance(d => ErgoAlgos.encode(d).asJson)
  implicit val digest32CollDecoder: Decoder[Digest32Coll] = bytesDecoder(bytes => Digest32Coll @@ bytes.toColl)

  implicit val assetEncoder: Encoder[Token] = Encoder.instance({ asset =>
    Json.obj(
      "tokenId" -> asset._1.asJson,
      "amount" -> asset._2.asJson
    )
  })

  implicit val assetDecoder: Decoder[Token] = Decoder.instance({ cursor =>
    for {
      tokenId <- cursor.downField("tokenId").as[TokenId]
      amount <- cursor.downField("amount").as[Long]
    } yield (tokenId, amount)
  })

  implicit val modifierIdEncoder: Encoder[ModifierId] = Encoder.instance(_.asInstanceOf[String].asJson)
  implicit val modifierIdDecoder: Decoder[ModifierId] = Decoder.instance(ModifierId @@ _.as[String])

  implicit val registerIdEncoder: KeyEncoder[NonMandatoryRegisterId] = KeyEncoder.instance({ regId =>
    s"R${regId.number}"
  })

  implicit val registerIdDecoder: KeyDecoder[NonMandatoryRegisterId] = KeyDecoder.instance({ key =>
    ErgoBox.registerByName.get(key).collect {
      case nonMandatoryId: NonMandatoryRegisterId => nonMandatoryId
    }
  })

  implicit val headerEncoder: Encoder[Header] = Encoder.instance({ h: Header =>
    Map(
      "id" -> h.id.asJson,
      "version" -> h.version.asJson,
      "parentId" -> h.parentId.asJson,
      "adProofsRoot" -> h.ADProofsRoot.asJson,
      "stateRoot" -> SigmaDsl.toAvlTreeData(h.stateRoot).asJson,
      "transactionsRoot" -> h.transactionsRoot.asJson,
      "timestamp" -> h.timestamp.asJson,
      "nBits" -> h.nBits.asJson,
      "height" -> h.height.asJson,
      "extensionRoot" -> h.extensionRoot.asJson,
      "minerPk" -> h.minerPk.getEncoded.asJson,
      "powOnetimePk" -> h.powOnetimePk.getEncoded.asJson,
      "powNonce" -> h.powNonce.asJson,
      "powDistance" -> h.powDistance.asJson,
      "votes" -> h.votes.asJson
    ).asJson
  })

  implicit val headerDecoder: Decoder[Header] = Decoder.instance({ cursor =>
    for {
      id <- cursor.downField("id").as[Coll[Byte]]
      version <- cursor.downField("version").as[Byte]
      parentId <- cursor.downField("parentId").as[Coll[Byte]]
      adProofsRoot <- cursor.downField("adProofsRoot").as[Coll[Byte]]
      stateRoot <- cursor.downField("stateRoot").as[AvlTreeData]
      transactionsRoot <- cursor.downField("transactionsRoot").as[Coll[Byte]]
      timestamp <- cursor.downField("timestamp").as[Long]
      nBits <- cursor.downField("nBits").as[Long]
      height <- cursor.downField("height").as[Int]
      extensionRoot <- cursor.downField("extensionRoot").as[Coll[Byte]]
      minerPk <- cursor.downField("minerPk").as[Coll[Byte]]
      powOnetimePk <- cursor.downField("powOnetimePk").as[Coll[Byte]]
      powNonce <- cursor.downField("powNonce").as[Coll[Byte]]
      powDistance <- cursor.downField("powDistance").as[sigma.BigInt]
      votes <- cursor.downField("votes").as[Coll[Byte]]
    } yield new CHeader(id, version, parentId, adProofsRoot, stateRoot, transactionsRoot, timestamp, nBits,
      height, extensionRoot, SigmaDsl.decodePoint(minerPk), SigmaDsl.decodePoint(powOnetimePk), powNonce, powDistance, votes)
  })

  implicit val preHeaderEncoder: Encoder[PreHeader] = Encoder.instance({ v: PreHeader =>
    Map(
      "version" -> v.version.asJson,
      "parentId" -> v.parentId.asJson,
      "timestamp" -> v.timestamp.asJson,
      "nBits" -> v.nBits.asJson,
      "height" -> v.height.asJson,
      "minerPk" -> v.minerPk.getEncoded.asJson,
      "votes" -> v.votes.asJson
    ).asJson
  })

  implicit val preHeaderDecoder: Decoder[PreHeader] = Decoder.instance({ cursor =>
    for {
      version <- cursor.downField("version").as[Byte]
      parentId <- cursor.downField("parentId").as[Coll[Byte]]
      timestamp <- cursor.downField("timestamp").as[Long]
      nBits <- cursor.downField("nBits").as[Long]
      height <- cursor.downField("height").as[Int]
      minerPk <- cursor.downField("minerPk").as[Coll[Byte]]
      votes <- cursor.downField("votes").as[Coll[Byte]]
    } yield CPreHeader(version, parentId, timestamp, nBits, height, SigmaDsl.decodePoint(minerPk), votes)
  })

  implicit val evaluatedValueEncoder: Encoder[EvaluatedValue[_ <: SType]] = Encoder.instance({ value =>
    ValueSerializer.serialize(value).asJson
  })

  implicit val evaluatedValueDecoder: Decoder[EvaluatedValue[_ <: SType]] = {
    decodeEvaluatedValue(_.asInstanceOf[EvaluatedValue[SType]])
  }

  def decodeEvaluatedValue[T](transform: EvaluatedValue[SType] => T): Decoder[T] = Decoder.instance({ implicit cursor: ACursor =>
    cursor.as[Array[Byte]] flatMap { bytes =>
      fromThrows(transform(ValueSerializer.deserialize(bytes).asInstanceOf[EvaluatedValue[SType]]))
    }
  })

  implicit val dataInputEncoder: Encoder[DataInput] = Encoder.instance({ input =>
    Json.obj(
      "boxId" -> input.boxId.asJson
    )
  })

  implicit val dataInputDecoder: Decoder[DataInput] = Decoder.instance({ cursor =>
    for {
      boxId <- cursor.downField("boxId").as[ADKey]
    } yield DataInput(boxId)
  })

  implicit val inputEncoder: Encoder[Input] = Encoder.instance({ input =>
    Json.obj(
      "boxId" -> input.boxId.asJson,
      "spendingProof" -> input.spendingProof.asJson
    )
  })


  implicit val inputDecoder: Decoder[Input] = Decoder.instance({ cursor =>
    for {
      boxId <- cursor.downField("boxId").as[ADKey]
      proof <- cursor.downField("spendingProof").as[ProverResult]
    } yield Input(boxId, proof)
  })

  implicit val unsignedInputEncoder: Encoder[UnsignedInput] = Encoder.instance({ input =>
    Json.obj(
      "boxId" -> input.boxId.asJson,
      "extension" -> input.extension.asJson
    )
  })

  implicit val unsignedInputDecoder: Decoder[UnsignedInput] = Decoder.instance({ cursor =>
    for {
      boxId <- cursor.downField("boxId").as[ADKey]
      extension <- cursor.downField("extension").as[ContextExtension]
    } yield new UnsignedInput(boxId, extension)
  })

  implicit val contextExtensionEncoder: Encoder[ContextExtension] = Encoder.instance({ extension =>
    Json.obj(extension.values.iterator.toSeq.map { case (key, value) =>
      key.toString -> evaluatedValueEncoder(value)
    }: _*)
  })

  implicit val contextExtensionDecoder: Decoder[ContextExtension] = Decoder.instance({ cursor =>
    for {
      values <- cursor.as[Map[Byte, EvaluatedValue[SType]]]
    } yield ContextExtension(SigmaMap(values))
  })

  implicit val proverResultEncoder: Encoder[ProverResult] = Encoder.instance({ v =>
    Json.obj(
      "proofBytes" -> v.proof.asJson,
      "extension" -> v.extension.asJson
    )
  })

  implicit val proverResultDecoder: Decoder[ProverResult] = Decoder.instance({ cursor =>
    for {
      proofBytes <- cursor.downField("proofBytes").as[Array[Byte]]
      ext <- cursor.downField("extension").as[ContextExtension]
    } yield ProverResult(proofBytes, ext)
  })


  implicit val avlTreeDataEncoder: Encoder[AvlTreeData] = Encoder.instance({ v =>
    Json.obj(
      "digest" -> v.digest.asJson,
      "treeFlags" -> v.treeFlags.serializeToByte.asJson,
      "keyLength" -> v.keyLength.asJson,
      "valueLength" -> v.valueLengthOpt.asJson
    )
  })

  implicit val avlTreeDataDecoder: Decoder[AvlTreeData] = Decoder.instance({ cursor =>
    for {
      digest <- cursor.downField("digest").as[ADDigest]
      treeFlagsByte <- cursor.downField("treeFlags").as[Byte]
      keyLength <- cursor.downField("keyLength").as[Int]
      valueLength <- cursor.downField("valueLength").as[Option[Int]]
    } yield new AvlTreeData(Colls.fromArray(digest), AvlTreeFlags(treeFlagsByte), keyLength, valueLength)
  })

  implicit val ergoTreeEncoder: Encoder[ErgoTree] = Encoder.instance({ value =>
    ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(value).asJson
  })

  def decodeErgoTree[T](transform: ErgoTree => T): Decoder[T] = Decoder.instance({ implicit cursor: ACursor =>
    cursor.as[Array[Byte]] flatMap { bytes =>
      fromThrows(transform(ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(bytes)))
    }
  })

  implicit val ergoTreeDecoder: Decoder[ErgoTree] = {
    decodeErgoTree(_.asInstanceOf[ErgoTree])
  }

  implicit def registersEncoder[T <: EvaluatedValue[_ <: SType]]: Encoder[scala.collection.Map[NonMandatoryRegisterId, T]] = Encoder.instance({ m =>
    Json.obj(
      m.toSeq
        .sortBy(_._1.number)
        .map { case (k, v) => registerIdEncoder(k) -> evaluatedValueEncoder(v) }: _*)
  })

  implicit def registersDecoder[T <: EvaluatedValue[_ <: SType]]: Decoder[scala.collection.Map[NonMandatoryRegisterId, T]] = Decoder.instance({ implicit m =>
    m.as[mutable.LinkedHashMap[NonMandatoryRegisterId, EvaluatedValue[SType]]].asInstanceOf[Decoder.Result[scala.collection.Map[NonMandatoryRegisterId, T]]]
  })

  implicit val ergoBoxEncoder: Encoder[ErgoBox] = Encoder.instance({ box =>
    Json.obj(
      "boxId" -> box.id.asJson,
      "value" -> box.value.asJson,
      "ergoTree" -> ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(box.ergoTree).asJson,
      "assets" -> box.additionalTokens.toArray.toSeq.asJson,
      "creationHeight" -> box.creationHeight.asJson,
      "additionalRegisters" -> box.additionalRegisters.asJson,
      "transactionId" -> box.transactionId.asJson,
      "index" -> box.index.asJson
    )
  })

  implicit val ergoBoxDecoder: Decoder[ErgoBox] = Decoder.instance({ cursor =>
    for {
      value <- cursor.downField("value").as[Long]
      ergoTreeBytes <- cursor.downField("ergoTree").as[Array[Byte]]
      additionalTokens <- cursor.downField("assets").as(Decoder.decodeSeq(assetDecoder))
      creationHeight <- cursor.downField("creationHeight").as[Int]
      additionalRegisters <- cursor.downField("additionalRegisters").as(registersDecoder)
      transactionId <- cursor.downField("transactionId").as[ModifierId]
      index <- cursor.downField("index").as[Short]
    } yield new ErgoBox(
      value = value,
      ergoTree = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(ergoTreeBytes),
      additionalTokens = additionalTokens.toColl,
      additionalRegisters = additionalRegisters,
      transactionId = transactionId,
      index = index,
      creationHeight = creationHeight
    )
  })

  implicit val ergoBoxCandidateEncoder: Encoder[ErgoBoxCandidate] = Encoder.instance({ box =>
    Json.obj(
      "value" -> box.value.asJson,
      "ergoTree" -> ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(box.ergoTree).asJson,
      "assets" -> box.additionalTokens.toArray.toSeq.asJson,
      "creationHeight" -> box.creationHeight.asJson,
      "additionalRegisters" -> box.additionalRegisters.asJson
    )
  })

  implicit val ergoBoxCandidateDecoder: Decoder[ErgoBoxCandidate] = Decoder.instance({ cursor =>
    for {
      value <- cursor.downField("value").as[Long]
      ergoTreeBytes <- cursor.downField("ergoTree").as[Array[Byte]]
      additionalTokens <- cursor.downField("assets").as(Decoder.decodeSeq(assetDecoder))
      creationHeight <- cursor.downField("creationHeight").as[Int]
      additionalRegisters <- cursor.downField("additionalRegisters").as(registersDecoder)
    } yield new ErgoBoxCandidate(
      value = value,
      ergoTree = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(ergoTreeBytes),
      creationHeight = creationHeight,
      additionalTokens = additionalTokens.toColl,
      additionalRegisters = additionalRegisters
    )
  })

  implicit val ergoLikeTransactionEncoder: Encoder[ErgoLikeTransaction] = Encoder.instance({ tx =>
    Json.obj(
      "id" -> tx.id.asJson,
      "inputs" -> tx.inputs.asJson,
      "dataInputs" -> tx.dataInputs.asJson,
      "outputs" -> tx.outputs.asJson
    )
  })

  implicit val ergoLikeTransactionDecoder: Decoder[ErgoLikeTransaction] = Decoder.instance({ implicit cursor =>
    for {
      inputs <- cursor.downField("inputs").as[IndexedSeq[Input]]
      dataInputs <- cursor.downField("dataInputs").as[IndexedSeq[DataInput]]
      outputs <- cursor.downField("outputs").as[IndexedSeq[ErgoBoxCandidate]]
    } yield new ErgoLikeTransaction(inputs, dataInputs, outputs)
  })

  implicit val unsignedErgoLikeTransactionEncoder: Encoder[UnsignedErgoLikeTransaction] = Encoder.instance({ tx =>
    Json.obj(
      "id" -> tx.id.asJson,
      "inputs" -> tx.inputs.asJson,
      "dataInputs" -> tx.dataInputs.asJson,
      "outputs" -> tx.outputs.asJson
    )
  })

  implicit val unsignedErgoLikeTransactionDecoder: Decoder[UnsignedErgoLikeTransaction] = Decoder.instance({ implicit cursor =>
    for {
      inputs <- cursor.downField("inputs").as[IndexedSeq[UnsignedInput]]
      dataInputs <- cursor.downField("dataInputs").as[IndexedSeq[DataInput]]
      outputs <- cursor.downField("outputs").as[IndexedSeq[ErgoBoxCandidate]]
    } yield new UnsignedErgoLikeTransaction(inputs, dataInputs, outputs)
  })

  implicit def ergoLikeTransactionTemplateEncoder[T <: UnsignedInput]: Encoder[ErgoLikeTransactionTemplate[T]] = Encoder.instance({
    case transaction: ErgoLikeTransaction => ergoLikeTransactionEncoder(transaction)
    case transaction: UnsignedErgoLikeTransaction => unsignedErgoLikeTransactionEncoder(transaction)
    case t => throw new SigmaException(s"Don't know how to encode transaction $t")
  })

  implicit val ergoLikeTransactionTemplateDecoder: Decoder[ErgoLikeTransactionTemplate[_ <: UnsignedInput]] = {
    ergoLikeTransactionDecoder.asInstanceOf[Decoder[ErgoLikeTransactionTemplate[_ <: UnsignedInput]]] or
        unsignedErgoLikeTransactionDecoder.asInstanceOf[Decoder[ErgoLikeTransactionTemplate[_ <: UnsignedInput]]]
  }

  implicit val sigmaValidationSettingsEncoder: Encoder[SigmaValidationSettings] = Encoder.instance({ v =>
    SigmaValidationSettingsSerializer.toBytes(v).asJson
  })

  implicit val sigmaValidationSettingsDecoder: Decoder[SigmaValidationSettings] = Decoder.instance({ implicit cursor: ACursor =>
    cursor.as[Array[Byte]] flatMap { bytes =>
      fromThrows(SigmaValidationSettingsSerializer.fromBytes(bytes))
    }
  })

  implicit val ergoLikeContextEncoder: Encoder[ErgoLikeContext] = Encoder.instance({ ctx =>
    Json.obj(
      "lastBlockUtxoRoot" -> ctx.lastBlockUtxoRoot.asJson,
      "headers" -> ctx.headers.toArray.toSeq.asJson,
      "preHeader" -> ctx.preHeader.asJson,
      "dataBoxes" -> ctx.dataBoxes.asJson,
      "boxesToSpend" -> ctx.boxesToSpend.asJson,
      "spendingTransaction" -> ctx.spendingTransaction.asJson,
      "selfIndex" -> ctx.selfIndex.asJson,
      "extension" -> ctx.extension.asJson,
      "validationSettings" -> ctx.validationSettings.asJson,
      "costLimit" -> ctx.costLimit.asJson,
      "initCost" -> ctx.initCost.asJson,
      "scriptVersion" -> ctx.activatedScriptVersion.asJson
    )
  })

  implicit val ergoLikeContextDecoder: Decoder[ErgoLikeContext] = Decoder.instance({ cursor =>
    for {
      lastBlockUtxoRoot <- cursor.downField("lastBlockUtxoRoot").as[AvlTreeData]
      headers <- cursor.downField("headers").as[Seq[Header]]
      preHeader <- cursor.downField("preHeader").as[PreHeader]
      dataBoxes <- cursor.downField("dataBoxes").as[IndexedSeq[ErgoBox]]
      boxesToSpend <- cursor.downField("boxesToSpend").as[IndexedSeq[ErgoBox]]
      spendingTransaction <- cursor.downField("spendingTransaction").as(ergoLikeTransactionTemplateDecoder)
      selfIndex <- cursor.downField("selfIndex").as[Int]
      extension <- cursor.downField("extension").as[ContextExtension]
      validationSettings <- cursor.downField("validationSettings").as[SigmaValidationSettings]
      costLimit <- cursor.downField("costLimit").as[Long]
      initCost <- cursor.downField("initCost").as[Long]
      version <- cursor.downField("scriptVersion").as[Byte]
    } yield new ErgoLikeContext(
      lastBlockUtxoRoot, Colls.fromArray(headers.toArray), preHeader,
      dataBoxes, boxesToSpend, spendingTransaction, selfIndex, extension,
      validationSettings, costLimit, initCost, version
    )
  })
}
