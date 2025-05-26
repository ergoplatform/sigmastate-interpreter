package sigma.js

import scorex.util.encode.Base16

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.typedarray.Int8Array

@JSExportTopLevel("Utils")
object Utils extends js.Object {
  /** Convert an Int8Array to a hex string. */
  def int8ArrayToHex(arr: Int8Array): String = {
    Base16.encode(arr.toArray)
  }
}
