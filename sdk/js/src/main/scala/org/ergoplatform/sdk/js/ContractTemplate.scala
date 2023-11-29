package org.ergoplatform.sdk.js

import org.ergoplatform.sdk
import org.ergoplatform.sdk.js.ContractTemplate.isoToSdk
import org.scalablytyped.runtime.StringDictionary
import sigma.ast.Constant
import sigma.ast.js.{ErgoTree, Expr}
import sigma.ast.syntax.SigmaPropValue
import sigma.data.Iso
import sigma.js.Type
import sigma.js.Isos._

import scala.scalajs.js
import scala.scalajs.js.UndefOr
import scala.scalajs.js.annotation.JSExportTopLevel

/**
  * Represents a ContractTemplate parameter.
  */
@JSExportTopLevel("Parameter")
class Parameter(
    /** User readable parameter name (string bytes in UTF-8 encoding) */
    val name: String,
    /** User readable parameter description (string bytes in UTF-8 encoding) */
    val description: String,
    /** Index in the ErgoTree.constants array */
    val constantIndex: Int
) extends js.Object

@JSExportTopLevel("ParameterObj")
object Parameter extends js.Object {
  implicit val isoToSdk: sigma.data.Iso[Parameter, sdk.Parameter] =
    new sigma.data.Iso[Parameter, sdk.Parameter] {
      override def to(p: Parameter): sdk.Parameter =
        sdk.Parameter(p.name, p.description, p.constantIndex)

      override def from(p: sdk.Parameter): Parameter =
        new Parameter(p.name, p.description, p.constantIndex)
    }
}


/** Represents a reusable ContractTemplate with support to generate ErgoTree based on provided parameters.
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
@JSExportTopLevel("ContractTemplate")
class ContractTemplate(
    val treeVersion: UndefOr[Byte],
    val name: String,
    val description: String,
    val constTypes: js.Array[sigma.js.Type],
    val constValues: UndefOr[js.Array[UndefOr[sigma.js.Value]]],
    val parameters: js.Array[Parameter],
    val expressionTree: Expr
) extends js.Object {
  /**
    * Generate the ErgoTree from the template by providing the values for parameters.
    *
    * @param version the version of the `ErgoTree` to use. Must be provided if the `treeVersion` was not provided in the
    * template.
    * @param paramValues the name-value map for the parameters accepted by the `ContractTemplate`. Must contain an entry
    * for each parameter for which no default value was provided in the template. Optionally, can also
    * provide values to override for parameters which do have a default value defined in the template.
    * The type of the provided value must match with the corresponding entry in the `constTypes`
    * provided in the template.
    * @return `ErgoTree` generated by replacing the template parameters with the value provided in `paramValues`.
    */
  def applyTemplate(
      version: UndefOr[Byte],
      paramValues: StringDictionary[sigma.js.Value]): sigma.ast.js.ErgoTree = {
    val params = StringDictionary
        .wrapStringDictionary(paramValues)
        .view.mapValues(v => sigma.ast.js.isoValueToConstant.to(v))
        .toMap
    val tree = isoToSdk.to(this).applyTemplate(
      version = isoUndefOr(Iso.identityIso[Byte]).to(version),
      paramValues = params
    )
    new ErgoTree(tree)
  }

  /** @return JSON representation of this contract template pretty-printed to a string
    *         indentation of two spaces.
    */
  def toJsonString(): String = {
    val template = ContractTemplate.isoToSdk.to(this)
    template.toJsonString
  }
}

@JSExportTopLevel("ContractTemplate$")
object ContractTemplate extends js.Object {

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
        ContractTemplate.isoToSdk.from(ct)
    }
  }

  private val constsIso = isoUndefOr(isoArrayToIndexed(isoUndefOr(sigma.ast.js.isoValueToConstant)))

  implicit val isoToSdk: sigma.data.Iso[ContractTemplate, sdk.ContractTemplate] =
    new sigma.data.Iso[ContractTemplate, sdk.ContractTemplate] {
      override def to(ct: ContractTemplate): sdk.ContractTemplate = {
        new sdk.ContractTemplate(
          isoUndefOr(Iso.identityIso[Byte]).to(ct.treeVersion),
          ct.name,
          ct.description,
          isoArrayToIndexed(Type.isoToSType).to(ct.constTypes),
          constsIso.to(ct.constValues).map(_.map(costOpt => costOpt.map(_.value))),
          isoArrayToIndexed(Parameter.isoToSdk).to(ct.parameters),
          ct.expressionTree.wrappedValue.asInstanceOf[SigmaPropValue]
        )
      }

      override def from(ct: sdk.ContractTemplate): ContractTemplate = {
        // optionally, match each value with its type
        val constants = ct.constValues.map(values => values.zip(ct.constTypes).map {
          case (value, tpe) => value.map(v => Constant(v, tpe))
        })
        new ContractTemplate(
          isoUndefOr(Iso.identityIso[Byte]).from(ct.treeVersion),
          ct.name,
          ct.description,
          isoArrayToIndexed(Type.isoToSType).from(ct.constTypes),
          constsIso.from(constants),
          isoArrayToIndexed(Parameter.isoToSdk).from(ct.parameters),
          new Expr(ct.expressionTree)
        )
      }
    }
}