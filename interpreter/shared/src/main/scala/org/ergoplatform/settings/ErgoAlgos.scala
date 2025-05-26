package org.ergoplatform.settings

import scorex.crypto.hash.Blake2b256
import scorex.util._
import sigma.Coll

import scala.util.Try

trait ErgoAlgos extends ScorexEncoding {

  type HF = Blake2b256.type

  val hash: HF = Blake2b256

  @inline def encode(bytes: Array[Byte]): String = encoder.encode(bytes)

  @inline def encode(bytes: Coll[Byte]): String = encoder.encode(bytes.toArray)

  @inline def decode(str: String): Try[Array[Byte]] = encoder.decode(str)

  @inline def decodeUnsafe(str: String): Array[Byte] = decode(str).get
}

object ErgoAlgos extends ErgoAlgos
