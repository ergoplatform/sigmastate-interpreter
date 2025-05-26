package sigma.js

import scorex.util.encode.Base16
import sigma.crypto.{CryptoFacade, CryptoFacadeJs, Ecp, Platform}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Equivalent of [[sigma.GroupElement]] available from JS. */
@JSExportTopLevel("GroupElement")
class GroupElement(val point: Ecp) extends js.Object {
  /** Returns the point encoded as hex string (ASN.1 encoding).
    * @see CryptoFacade.getASN1Encoding
    */
  def toPointHex(): String = {
    Base16.encode(CryptoFacade.getASN1Encoding(point, true))
  }
}

@JSExportTopLevel("GroupElement$")
object GroupElement extends js.Object {
  /** Creates a new [[GroupElement]] from the given hex string (ASN.1 encoding)
    * representation of the underlying [[sigmastate.crypto.Platform.Point]].
    */
  def fromPointHex(pointHex: String): GroupElement = {
    val point = CryptoFacadeJs.createCryptoContext().decodePoint(pointHex)
    new GroupElement(new Platform.Ecp(point))
  }
}