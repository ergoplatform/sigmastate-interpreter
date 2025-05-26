package org.ergoplatform.sdk


import io.circe._
import io.circe.syntax._
import org.ergoplatform.ErgoBox._
import org.ergoplatform.validation.ValidationRules
import org.ergoplatform._
import org.scalacheck.Arbitrary.arbitrary
import scorex.crypto.authds.{ADDigest, ADKey}
import scorex.util.ModifierId
import scorex.util.encode.Base16
import sigma.data.{AvlTreeData, Digest32Coll, ProveDlog}
import sigma.{Coll, Header, PreHeader}
import sigma.ast._
import sigma.crypto.CryptoConstants
import sigma.interpreter.{ContextExtension, ProverResult}
import sigma.serialization.SerializationSpecification
import sigmastate.utils.Helpers.DecoderResultOps  // required for Scala 2.11

class JsonSerializationSpec extends SerializationSpecification with JsonCodecs {

  def jsonRoundTrip[T](v: T)(implicit encoder: Encoder[T], decoder: Decoder[T]): Unit = {
    val json = v.asJson
    withClue(s"\n for JSON: ${json.spaces2}") { json.as[T].toTry.get shouldEqual v }
  }

  // TODO enable after https://github.com/ScorexFoundation/sigmastate-interpreter/issues/681
  ignore("ErgoLikeContext should be encoded into JSON and decoded back correctly") {
    forAll(ergoLikeContextGen, MinSuccessful(50)) { v: ErgoLikeContext => jsonRoundTrip(v) }
  }

  property("sigma.BigInt should be encoded into JSON and decoded back correctly") {
    forAll { v: sigma.BigInt => jsonRoundTrip(v) }
  }

  property("byte array should be encoded into JSON and decoded back correctly") {
    forAll(arrayOfRange(0, 1000, arbitrary[Byte])) { v: Array[Byte] => jsonRoundTrip(v) }
  }

  property("byte coll should be encoded into JSON and decoded back correctly") {
    forAll(collOfRange(0, 1000, arbitrary[Byte])) { v: Coll[Byte] => jsonRoundTrip(v) }
  }

  property("ADKey should be encoded into JSON and decoded back correctly") {
    forAll(boxIdGen) { v: ADKey => jsonRoundTrip(v) }
  }

  property("ADDigest should be encoded into JSON and decoded back correctly") {
    forAll(aDDigestGen) { v: ADDigest => jsonRoundTrip(v) }
  }

  property("Digest32 should be encoded into JSON and decoded back correctly") {
    forAll(digest32Gen) { v: Digest32Coll => jsonRoundTrip(v) }
  }

  property("ModifierId should be encoded into JSON and decoded back correctly") {
    forAll(modifierIdGen) { v: ModifierId => jsonRoundTrip(v) }
  }

  property("Header should be encoded into JSON and decoded back correctly") {
    forAll(headerGen) { v: Header => jsonRoundTrip(v) }
  }

  property("PreHeader should be encoded into JSON and decoded back correctly") {
    forAll(preHeaderGen) { v: PreHeader => jsonRoundTrip(v) }
  }

  property("EvaluatedValue[SType] should be encoded into JSON and decoded back correctly") {
    forAll(evaluatedValueGen) { v: EvaluatedValue[SType] => jsonRoundTrip(v) }
  }

  property("DataInput should be encoded into JSON and decoded back correctly") {
    forAll(dataInputGen) { v: DataInput => jsonRoundTrip(v) }
  }

  property("Input should be encoded into JSON and decoded back correctly") {
    forAll(inputGen, MinSuccessful(50)) { v: Input => jsonRoundTrip(v) }
  }

  property("UnsignedInput should be encoded into JSON and decoded back correctly") {
    forAll(unsignedInputGen, MinSuccessful(50)) { v: UnsignedInput => jsonRoundTrip(v) }
  }

  property("ContextExtension should be encoded into JSON and decoded back correctly") {
    forAll(contextExtensionGen, MinSuccessful(500)) { v: ContextExtension => jsonRoundTrip(v) }
  }

  property("AdditionalRegisters should be encoded into JSON and decoded back correctly") {
    forAll(additionalRegistersGen, MinSuccessful(500)) { regs =>
      jsonRoundTrip(regs)(registersEncoder, registersDecoder)
    }
  }

  property("ProverResult should be encoded into JSON and decoded back correctly") {
    forAll(serializedProverResultGen, MinSuccessful(500)) { v: ProverResult => jsonRoundTrip(v) }
  }

  property("AvlTreeData should be encoded into JSON and decoded back correctly") {
    forAll(avlTreeDataGen, MinSuccessful(500)) { v: AvlTreeData => jsonRoundTrip(v) }
  }

  property("ErgoTree should be encoded into JSON and decoded back correctly") {
    forAll(ergoTreeGen, MinSuccessful(500)) { v: ErgoTree => jsonRoundTrip(v) }
  }

  property("ErgoBox should be encoded into JSON and decoded back correctly") {
    forAll(ergoBoxGen, MinSuccessful(500)) { v: ErgoBox => jsonRoundTrip(v) }
  }

  // TODO enable after https://github.com/ScorexFoundation/sigmastate-interpreter/issues/681
  ignore("ErgoLikeTransaction should be encoded into JSON and decoded back correctly") {
    forAll(ergoLikeTransactionGen, MinSuccessful(50)) { v: ErgoLikeTransaction => jsonRoundTrip(v) }
  }

  // TODO enable after https://github.com/ScorexFoundation/sigmastate-interpreter/issues/681
  ignore("UnsignedErgoLikeTransaction should be encoded into JSON and decoded back correctly") {
    forAll(unsignedErgoLikeTransactionGen, MinSuccessful(50)) { v: UnsignedErgoLikeTransaction => jsonRoundTrip(v) }
  }

  // TODO enable after https://github.com/ScorexFoundation/sigmastate-interpreter/issues/681
  ignore("ErgoLikeTransactionTemplate should be encoded into JSON and decoded back correctly") {
    forAll(ergoLikeTransactionTemplateGen, MinSuccessful(50)) { v: ErgoLikeTransactionTemplate[_ <: UnsignedInput] =>
      v.asJson.as(ergoLikeTransactionTemplateDecoder).toTry.get shouldEqual v
    }
  }

  property("SigmaValidationSettingsEncoder should be encoded into JSON and decoded back correctly") {
    jsonRoundTrip(ValidationRules.currentSettings)
  }

  property("ErgoBox.additionalRegisters should be encoded into JSON in certain order") {
    val minerPkHex = "0326df75ea615c18acc6bb4b517ac82795872f388d5d180aac90eaa84de750b942"
    val minerPk = Base16.decode(minerPkHex).map { point =>
      ProveDlog(
        CryptoConstants.dlogGroup.ctx.decodePoint(point)
      )
    }.get
    val regs = scala.collection.Map(
      R7 -> LongArrayConstant(Array(1L, 2L, 1234123L)),
      R4 -> ByteConstant(1),
      R6 -> IntConstant(10),
      R5 -> SigmaPropConstant(minerPk),
      R8 -> ByteArrayConstant(Base16.decode("123456123456123456123456123456123456123456123456123456123456123456").get)
    )
    // registers should be ordered by their number in Json
    regs.asJson.spaces2 shouldEqual
      """{
        |  "R4" : "0201",
        |  "R5" : "08cd0326df75ea615c18acc6bb4b517ac82795872f388d5d180aac90eaa84de750b942",
        |  "R6" : "0414",
        |  "R7" : "1103020496d39601",
        |  "R8" : "0e21123456123456123456123456123456123456123456123456123456123456123456"
        |}""".stripMargin
  }
}
