package sigmastate.crypto

import scorex.crypto.hash.Blake2b256
import sigma.crypto.CryptoConstants

object CryptoFunctions {
  lazy val soundnessBytes: Int = CryptoConstants.soundnessBits / 8

  /** Hashes the given `input` into 32 bytes hash, then returns the first `soundnessBytes`
    * bytes of the hash in a new array.
    * The method trims `32 - soundnessBytes` bytes at the end of the hash array.
    *
    * @hotspot don't beautify this code. Used in Interpreter.verify. */
  def hashFn(input: Array[Byte]): Array[Byte] = {
    val h = Blake2b256.hash(input)
    val res = new Array[Byte](soundnessBytes)
    Array.copy(h, 0, res, 0, soundnessBytes)
    res
  }
}
