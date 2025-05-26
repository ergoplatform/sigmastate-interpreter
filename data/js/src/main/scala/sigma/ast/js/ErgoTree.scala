package sigma.ast.js

import sigma.ast
import sigma.js.Value

import scala.scalajs.js
import scala.scalajs.js.JSConverters.JSRichIterableOnce
import scala.scalajs.js.annotation.JSExportTopLevel

/** An exported JavaScript class wrapping the Scala `ErgoTree` type. */
@JSExportTopLevel("ErgoTree")
class ErgoTree(val tree: ast.ErgoTree) extends js.Object {
  /** Root of the contract which is a valid expression of `SigmaProp` type. */
  val root: Expr = new Expr(tree.root.toOption.get)

  /** The first byte of serialized byte array which determines interpretation of the rest of the array. */
  def header(): Byte = tree.header

  /** Version of this tree (== BlockVersion - 1). */
  def version(): Byte = tree.version

  /** @return true, if constant segregation bit is set in the header. */
  def isConstantSegregation(): Boolean = tree.isConstantSegregation

  /** @return true, if size bit is set in the header. */
  def hasSize(): Boolean = tree.hasSize

  /** Serializes the ErgoTree instance to an array of bytes. */
  def bytes(): Array[Byte] = tree.bytes

  /** Serializes the ErgoTree instance to a hexadecimal string. */
  def toHex(): String = tree.bytesHex

  /** Serialized proposition expression of SigmaProp type with
    * ConstantPlaceholder nodes not replaced by Constant nodes.
    */
  def template(): Array[Byte] = tree.template

  /** Base16 encoding of `template` bytes. */
  def templateHex(): String = tree.templateHex

  /** Returns segregated constants of this tree as [[js.Array]]. */
  def constants(): js.Array[Value] = {
    val constants = tree.constants
    val values = constants.map(isoValueToConstant.from)
    values.toJSArray
  }
}

/** An exported JavaScript object providing utility methods for working with ErgoTree instances. */
@JSExportTopLevel("ErgoTree$")
object ErgoTree extends js.Object {

  /** Deserializes an ErgoTree instance from a hexadecimal string.
    *
    * @param hex a hexadecimal string representing the serialized ErgoTree
    */
  def fromHex(hex: String): ErgoTree =
    new ErgoTree(ast.ErgoTree.fromHex(hex))

  /** Deserializes an ErgoTree instance from an array of bytes.
    *
    * @param bytes an array of bytes representing the serialized ErgoTree
    */
  def fromBytes(bytes: Array[Byte]): ErgoTree = {
    new ErgoTree(ast.ErgoTree.fromBytes(bytes))
  }
}
