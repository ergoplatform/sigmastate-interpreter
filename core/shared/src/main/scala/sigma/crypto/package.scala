package sigma

package object crypto {
  /** Length of hash function used in the signature scheme. Blake2b hash function is used. */
  val hashLengthBits = 256

  /** Length of hash in bytes. */
  val hashLength: Int = hashLengthBits / 8

  /** Number of bytes to represent any group element as byte array */
  val groupSize: Int = 256 / 8 //32 bytes

  /** Instance of Elliptic Curve descriptor. */
  type Curve = Platform.Curve

  /** Instance of Elliptic Curve point. */
  type Ecp = Platform.Ecp

  /** Instance of Elliptic Curve field element. */
  type ECFieldElem = Platform.ECFieldElem

  /** A cryptographically strong random number generator. */
  type SecureRandom = Platform.SecureRandom

  /** Type of group elements used in the signature scheme. */
  type EcPointType = Ecp
}
