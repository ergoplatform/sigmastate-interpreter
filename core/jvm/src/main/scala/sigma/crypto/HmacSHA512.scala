package sigma.crypto

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

/** A utility object to compute HMAC-SHA512 hashes. */
object HmacSHA512 {
  private val HashAlgo = "HmacSHA512"

  /** Computes HMAC-SHA512 hash of the given data using the specified key.
    *
    * @param key  the secret key used for hashing
    * @param data the input data to be hashed
    */
  def hash(key: Array[Byte], data: Array[Byte]): Array[Byte] = initialize(key).doFinal(data)

  private def initialize(byteKey: Array[Byte]) = {
    val hmacSha512 = Mac.getInstance(HashAlgo)
    val keySpec = new SecretKeySpec(byteKey, HashAlgo)
    hmacSha512.init(keySpec)
    hmacSha512
  }
}
