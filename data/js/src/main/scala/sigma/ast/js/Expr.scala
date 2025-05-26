package sigma.ast.js

import sigma.ast.syntax.SValue
import sigma.js.JsWrapper
import sigma.serialization.ValueSerializer

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.typedarray.Int8Array

/** Represents a node of ErgoTree.
  * An exported JavaScript class wrapping the Scala [[sigma.ast.Value]].
  */
@JSExportTopLevel("Expr")
class Expr(override val wrappedValue: SValue) extends JsWrapper[SValue]{
  /** Serialize this expression using sigma serializer [[ValueSerializer]]. */
  def toBytes: Int8Array = {
    val bytes = ValueSerializer.serialize(wrappedValue)
    Int8Array.of(bytes:_*)
  }
}

@JSExportTopLevel("Expr$")
object Expr extends js.Object {
  /** Deserialize an expression from bytes using sigma serializer [[ValueSerializer]]. */
  def fromBytes(bytes: Int8Array): Expr = {
    val value = ValueSerializer.deserialize(bytes.toArray)
    new Expr(value)
  }
}
