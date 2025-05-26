package sigma.js

import sigma.data.{ProveDHTuple, ProveDlog, SigmaBoolean}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Equivalent of [[sigma.SigmaProp]] available from JS. */
@JSExportTopLevel("SigmaProp")
class SigmaProp(val sigmaBoolean: SigmaBoolean) extends js.Object {
}

@JSExportTopLevel("SigmaProp$")
object SigmaProp extends js.Object {
  /** Creates a new [[SigmaProp]] from the given hex string of public key.
    * @param pointHex  hex representation of elliptic curve point (ASN.1 encoded)
    * @see CryptoFacade.getASN1Encoding, GroupElement.fromPointHex, Point
    */
  def fromPointHex(pointHex: String): SigmaProp = {
    val ge = GroupElement.fromPointHex(pointHex)
    dlog(ge)
  }

  /** @param publicKey a [[GroupElement]] representing public key of discrete logarithm signature protocol
    * @return a new [[SigmaProp]] value representing public key of discrete logarithm signature protocol.
    */
  def dlog(publicKey: GroupElement): SigmaProp = {
    new SigmaProp(ProveDlog(publicKey.point))
  }

  /** Construct a new [[SigmaProp]] value representing public key of Diffie Hellman signature protocol. */
  def dht(g: GroupElement, h: GroupElement, u: GroupElement, v: GroupElement): SigmaProp = {
    new SigmaProp(ProveDHTuple(g.point, h.point, u.point, v.point))
  }
}
