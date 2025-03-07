package org.ergoplatform.sdk

import cats.syntax.either._ // required for Scala 2.11
import debox.cfor
import io.circe._
import io.circe.syntax.EncoderOps
import org.ergoplatform.sdk
import org.ergoplatform.sdk.utils.SerializationUtils.{parseString, serializeString}
import org.ergoplatform.sdk.utils.Zero
import sigma.Evaluation
import sigma.ast.{DeserializationSigmaBuilder, SType, StdSigmaBuilder}
import sigma.serialization.{SerializerException, SigmaByteReader, SigmaByteWriter}
import sigma.util.safeNewArray
import sigma.ast.ErgoTree.{ZeroHeader, headerWithVersion, setConstantSegregation}
import sigma.ast._
import sigma.ast.syntax.SigmaPropValue
import sigma.serialization._

import java.util.Objects
import scala.collection.mutable

/**
 * Represents a ContractTemplate parameter.
 * @param name user readable parameter name (string bytes in UTF-8 encoding)
 * @param description user readable parameter description (string bytes in UTF-8 encoding)
 * @param constantIndex index in the ErgoTree.constants array
 */
case class Parameter(
  name: String,
  description: String,
  constantIndex: Int
)

object Parameter {

  /** Immutable empty IndexedSeq, can be used to save allocations in many places. */
  val EmptySeq: IndexedSeq[Parameter] = Array.empty[Parameter]

  /** HOTSPOT: don't beautify this code. */
  object serializer extends SigmaSerializer[Parameter, Parameter] {
    override def serialize(data: Parameter, w: SigmaByteWriter): Unit = {
      serializeString(data.name, w)
      serializeString(data.description, w)
      w.putUInt(data.constantIndex)
    }

    override def parse(r: SigmaByteReader): Parameter = {
      val name = parseString(r)
      val description = parseString(r)
      val constantIndex = r.getUInt().toInt
      Parameter(name, description, constantIndex)
    }
  }

  /** Json encoder for Parameter. */
  implicit val encoder: Encoder[Parameter] = Encoder.instance({ p =>
    Json.obj(
      "name" -> Json.fromString(p.name),
      "description" -> Json.fromString(p.description),
      "constantIndex" -> Json.fromInt(p.constantIndex)
    )
  })

  /** Json decoder for Parameter. */
  implicit val decoder: Decoder[Parameter] = Decoder.instance({ cursor =>
    for {
      name <- cursor.downField("name").as[String]
      description <- cursor.downField("description").as[String]
      constantIndex <- cursor.downField("constantIndex").as[Int]
    } yield new Parameter(name, description, constantIndex)
  })
}

/**
 * Represents a reusable ContractTemplate with support to generate ErgoTree based on provided parameters.
 *
 * @param treeVersion    the optional version of ErgoTree which should be used. If this value is not provided here then
 *                       it must be provided while generating the `ErgoTree` by calling `applyTemplate`.
 * @param name           user readable name (non-empty string bytes in UTF-8 encoding)
 * @param description    user readable contract description (string bytes in UTF-8 encoding)
 * @param constTypes     list denoting the type of ConstantPlaceholders in the expressionTree
 * @param constValues    optional list of optional default values for the ConstantPlaceholders in the expressionTree.
 *                       If an entry in the sequence is None, it must have a corresponding entry in parameters and its
 *                       value must be provided while generating the `ErgoTree` by calling `applyTemplate`. If all the
 *                       entries are None, the whole `constValues` field can be set to None.
 * @param parameters     typed template parameters of the contract template. It must have an entry for each
 *                       `ConstantPlaceholder` which has a `None` in the `constValues` field. Other fields which do have
 *                       a value defined in `constValues` can also be allowed to be optionally overridden by accepting
 *                       it in `parameters`.
 * @param expressionTree root of the contract which is a valid expression of `SigmaProp` type. Must have constants
 *                       segregated into `constTypes` and optionally `constValues`
 */
case class ContractTemplate(
  treeVersion: Option[Byte],
  name: String,
  description: String,
  constTypes: IndexedSeq[SType],
  constValues: Option[IndexedSeq[Option[SType#WrappedType]]],
  parameters: IndexedSeq[Parameter],
  expressionTree: SigmaPropValue
) {

  validate()

  private def validate(): Unit = {
    require(constValues.isEmpty || constValues.get.size == constTypes.size,
      s"constValues must be empty or of same length as constTypes. Got ${constValues.get.size}, expected ${constTypes.size}")
    require(parameters.size <= constTypes.size, "number of parameters must be <= number of constants")

    // Validate that no parameter is duplicated i.e. points to the same position & also to a valid constant.
    // Also validate that no two parameters exist with the same name.
    val paramNames = mutable.Set[String]()
    val paramIndices = this.parameters.map(p => {
      require(constTypes.isDefinedAt(p.constantIndex),
        s"parameter constantIndex must be in range [0, ${constTypes.size})")
      require(!paramNames.contains(p.name),
        s"parameter names must be unique. Found duplicate parameters with name ${p.name}")
      paramNames += p.name
      p.constantIndex
    }).toSet
    require(paramIndices.size == parameters.size, "multiple parameters point to the same constantIndex")

    // Validate that any constValues[i] = None has a parameter.
    if (constValues.isEmpty) {
      require(parameters.size == constTypes.size,
        "all the constants must be provided via parameter since constValues == None")
    } else {
      cfor(0)(_ < constTypes.size, _ + 1) { i =>
        require(constValues.get(i).isDefined || paramIndices.contains(i),
          s"constantIndex $i does not have a default value and absent from parameter as well")
      }
    }
  }

  /**
   * Generate the ErgoTree from the template by providing the values for parameters.
   *
   * @param version the version of the `ErgoTree` to use. Must be provided if the `treeVersion` was not provided in the
   *                template.
   * @param paramValues the name-value map for the parameters accepted by the `ContractTemplate`. Must contain an entry
   *                    for each parameter for which no default value was provided in the template. Optionally, can also
   *                    provide values to override for parameters which do have a default value defined in the template.
   *                    The type of the provided value must match with the corresponding entry in the `constTypes`
   *                    provided in the template.
   * @return `ErgoTree` generated by replacing the template parameters with the value provided in `paramValues`.
   */
  def applyTemplate(version: Option[Byte], paramValues: Map[String, Constant[SType]]): ErgoTree = {
    require(treeVersion.isDefined || version.isDefined, "ErgoTreeVersion must be provided to generate the ErgoTree.")
    val nConsts = constTypes.size
    val requiredParameterNames =
      if (constValues.isEmpty)
        this.parameters.map(p => p.name)
      else this.parameters
        .filter(p => constValues.get(p.constantIndex).isEmpty)
        .map(p => p.name)
    requiredParameterNames.foreach(name => require(
      paramValues.contains(name),
      s"value for parameter `$name` was not provided while it does not have a default value."))

    val parameterizedConstantIndices = this.parameters.map(p => p.constantIndex).toSet
    val constIndexToParamIndex = this.parameters.zipWithIndex.map(pi => pi._1.constantIndex -> pi._2).toMap
    val constants = safeNewArray[Constant[SType]](nConsts)
    cfor(0)(_ < nConsts, _ + 1) { i =>
      // constants which are parameterized have to taken from the parameters when available.
      if (parameterizedConstantIndices.contains(i) && paramValues.contains(parameters(constIndexToParamIndex(i)).name)) {
        val paramValue = paramValues(parameters(constIndexToParamIndex(i)).name)
        require(paramValue.tpe == constTypes(i),
          s"parameter type mismatch, expected ${constTypes(i)}, got ${paramValue.tpe}")
        constants(i) = StdSigmaBuilder.mkConstant(paramValue.value, constTypes(i))
      } else {
        constants(i) = StdSigmaBuilder.mkConstant(constValues.get(i).get, constTypes(i))
      }
    }

    val usedErgoTreeHeader = headerWithVersion(ZeroHeader, if (version.isDefined) version.get else treeVersion.get)
    ErgoTree(
      setConstantSegregation(usedErgoTreeHeader),
      constants,
      this.expressionTree
    )
  }

  /** @return Json representation of this contract template */
  def toJson: Json = ContractTemplate.jsonEncoder.encoder(this)

  /** @return JSON representation of this contract template pretty-printed to a string
    *         indentation of two spaces.
    */
  def toJsonString: String = toJson.spaces2

  override def hashCode(): Int = Objects.hash(treeVersion, name, description, constTypes, constValues, parameters, expressionTree)

  override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) ||
    ((obj.asInstanceOf[AnyRef] != null) && (obj match {
      case other: ContractTemplate =>
        other.treeVersion == treeVersion && other.name == name && other.description == description && other.constTypes == constTypes && other.constValues == constValues && other.parameters == parameters && other.expressionTree == expressionTree
      case _ => false
    }))
}

object ContractTemplate {
  def apply(name: String,
            description: String,
            constTypes: IndexedSeq[SType],
            constValues: Option[IndexedSeq[Option[SType#WrappedType]]],
            parameters: IndexedSeq[Parameter],
            expressionTree: SigmaPropValue): ContractTemplate = {
    new ContractTemplate(None, name, description, constTypes, constValues, parameters, expressionTree)
  }

  /** Create a new contract template from a JSON string.
    *
    * @param json JSON string representing a contract template.
    * @return a new contract template.
    */
  def fromJsonString(json: String): ContractTemplate = {
    io.circe.parser.parse(json) match {
      case Left(err) => throw err
      case Right(json) =>
        val ct = sdk.ContractTemplate.jsonEncoder.decoder(json.hcursor).toOption.get
        ct
    }
  }


  object serializer extends SigmaSerializer[ContractTemplate, ContractTemplate] {

    override def serialize(data: ContractTemplate, w: SigmaByteWriter): Unit = {
      w.putOption(data.treeVersion)(_.putUByte(_))
      serializeString(data.name, w)
      serializeString(data.description, w)

      val nConstants = data.constTypes.length
      w.putUInt(nConstants)
      cfor(0)(_ < nConstants, _ + 1) { i =>
        TypeSerializer.serialize(data.constTypes(i), w)
      }
      w.putOption(data.constValues)((_, values) => {
        cfor(0)(_ < nConstants, _ + 1) { i =>
          w.putOption(values(i))((_, const) =>
            DataSerializer.serialize(const, data.constTypes(i), w))
        }
      })

      val nParameters = data.parameters.length
      w.putUInt(nParameters)
      cfor(0)(_ < nParameters, _ + 1) { i =>
        Parameter.serializer.serialize(data.parameters(i), w)
      }

      val expressionTreeWriter = SigmaSerializer.startWriter()
      ValueSerializer.serialize(data.expressionTree, expressionTreeWriter)
      val expressionBytes = expressionTreeWriter.toBytes
      w.putUInt(expressionBytes.length)
      w.putBytes(expressionBytes)
    }

    override def parse(r: SigmaByteReader): ContractTemplate = {
      val treeVersion = r.getOption(r.getUByte().toByte)
      val name = parseString(r)
      val description = parseString(r)

      val nConstants = r.getUInt().toInt
      val constTypes: IndexedSeq[SType] = {
        if (nConstants > 0) {
          val res = safeNewArray[SType](nConstants)
          cfor(0)(_ < nConstants, _ + 1) { i =>
            res(i) = TypeSerializer.deserialize(r)
          }
          res
        } else {
          SType.EmptySeq
        }
      }
      val constValues: Option[IndexedSeq[Option[SType#WrappedType]]] = r.getOption {
        if (nConstants > 0) {
          val res = safeNewArray[Option[SType#WrappedType]](nConstants)
          cfor(0)(_ < nConstants, _ + 1) { i =>
            res(i) = r.getOption((() => DataSerializer.deserialize(constTypes(i), r))())
          }
          res
        } else {
          Array.empty[Option[SType#WrappedType]]
        }
      }

      val nParameters = r.getUInt().toInt
      val parameters: IndexedSeq[Parameter] = {
        if (nParameters > 0) {
          val res = safeNewArray[Parameter](nParameters)
          cfor(0)(_ < nParameters, _ + 1) { i =>
            res(i) = Parameter.serializer.parse(r)
          }
          res
        } else {
          Parameter.EmptySeq
        }
      }

      // Populate constants in constantStore so that the expressionTree can be deserialized.
      val constants = constTypes.indices.map(i => {
        val t = constTypes(i)
        DeserializationSigmaBuilder.mkConstant(Zero.zeroOf(Zero.typeToZero(Evaluation.stypeToRType(t))), t)
      })
      constants.foreach(c => r.constantStore.put(c)(DeserializationSigmaBuilder))

      val _ = r.getUInt().toInt
      val expressionTree = ValueSerializer.deserialize(r)
      if (!expressionTree.tpe.isSigmaProp) {
        throw SerializerException(
          s"Failed deserialization, expected deserialized script to have type SigmaProp; got ${expressionTree.tpe}")
      }

      ContractTemplate(
        treeVersion, name, description,
        constTypes, constValues, parameters,
        expressionTree.toSigmaProp)
    }
  }

  object jsonEncoder extends JsonCodecs {

    /** Json encoder for SType */
    implicit val sTypeEncoder: Encoder[SType] = Encoder.instance({ tpe =>
      val w = SigmaSerializer.startWriter()
      TypeSerializer.serialize(tpe, w)
      w.toBytes.asJson
    })

    /** Json decoder for SType */
    implicit val sTypeDecoder: Decoder[SType] = Decoder.instance({ implicit cursor =>
      cursor.as[Array[Byte]] flatMap { bytes =>
        val r = SigmaSerializer.startReader(bytes)
        fromThrows(TypeSerializer.deserialize(r))
      }
    })

    /** Json encoder for ContractTemplate */
    implicit val encoder: Encoder[ContractTemplate] = Encoder.instance({ ct =>
      val expressionTreeWriter = SigmaSerializer.startWriter()
      ValueSerializer.serialize(ct.expressionTree, expressionTreeWriter)

      Json.obj(
        "treeVersion" -> ct.treeVersion.asJson,
        "name" -> Json.fromString(ct.name),
        "description" -> Json.fromString(ct.description),
        "constTypes" -> ct.constTypes.asJson,
        "constValues" -> (
          if (ct.constValues.isEmpty) Json.Null
          else ct.constValues.get.indices.map(i => ct.constValues.get(i) match {
            case Some(const) => DataJsonEncoder.encodeData(const, ct.constTypes(i))
            case None => Json.Null
          }).asJson),
        "parameters" -> ct.parameters.asJson,
        "expressionTree" -> expressionTreeWriter.toBytes.asJson
      )
    })

    /** Json decoder for ContractTemplate */
    implicit val decoder: Decoder[ContractTemplate] = Decoder.instance({ implicit cursor =>
      val constTypesResult = cursor.downField("constTypes").as[IndexedSeq[SType]]
      val expressionTreeBytesResult = cursor.downField("expressionTree").as[Array[Byte]]
      (constTypesResult, expressionTreeBytesResult) match {
        case (Right(constTypes), Right(expressionTreeBytes)) =>
          val constValuesOpt = {
            val constValuesJson = cursor.downField("constValues").focus.get
            if (constValuesJson != Json.Null) {
              val jsonValues = constValuesJson.asArray.get
              Some(jsonValues.indices.map(
                i => if (jsonValues(i) == Json.Null) None
                else Some(DataJsonEncoder.decodeData(jsonValues(i), constTypes(i)))))
            } else {
              None
            }
          }

          // Populate synthetic constants in the constant store for deserialization of expression tree.
          val r = SigmaSerializer.startReader(expressionTreeBytes)
          val constants = constTypes.indices.map(i => {
            val t = constTypes(i)
            DeserializationSigmaBuilder.mkConstant(Zero.zeroOf(Zero.typeToZero(Evaluation.stypeToRType(t))), t)
          })
          constants.foreach(c => r.constantStore.put(c)(DeserializationSigmaBuilder))

          for {
            treeVersion <- cursor.downField("treeVersion").as[Option[Byte]]
            name <- cursor.downField("name").as[String]
            description <- cursor.downField("description").as[String]
            parameters <- cursor.downField("parameters").as[IndexedSeq[Parameter]]
          } yield new ContractTemplate(
            treeVersion,
            name,
            description,
            constTypes,
            constValuesOpt,
            parameters,
            ValueSerializer.deserialize(r).toSigmaProp)
        case _ => Left(DecodingFailure("Failed to decode contract template", cursor.history))
      }
    })
  }
}
