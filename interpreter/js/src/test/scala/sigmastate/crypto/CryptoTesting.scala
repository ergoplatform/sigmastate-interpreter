package sigmastate.crypto

import scala.scalajs.js

trait CryptoTesting {
  val modulus = js.BigInt("115792089237316195423570985008687907853269984665640564039457584007908834671663")
  val order = js.BigInt("115792089237316195423570985008687907852837564279074904382605163141518161494337")
}