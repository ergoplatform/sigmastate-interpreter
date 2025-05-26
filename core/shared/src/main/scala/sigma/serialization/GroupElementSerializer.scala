package sigma.serialization

import sigma.crypto.{CryptoContext, CryptoFacade, EcPointType}
import sigma.util.safeNewArray

/**
  * A serializer which encodes group elements, so elliptic curve points in our case, to bytes, and decodes points
  * from bytes.
  * Every point is encoded in compressed form (so only X coordinate and sign of Y are stored).
  * Thus for secp256k1 point, 33 bytes are needed. The first bytes is whether equals 2 or 3 depending on the sign of
  * Y coordinate(==2 is Y is positive, ==3, if Y is negative). Other 32 bytes are containing the X coordinate.
  * Special case is infinity point, which is encoded by 33 zeroes.
  * Thus elliptic curve point is always encoded with 33 bytes.
  */
object GroupElementSerializer extends CoreSerializer[EcPointType, EcPointType] {

  private val encodingSize = 1 + sigma.crypto.groupSize
  private lazy val identityPointEncoding = Array.fill(encodingSize)(0: Byte)

  override def serialize(point: EcPointType, w: CoreByteWriter): Unit = {
    val bytes = if (CryptoFacade.isInfinityPoint(point)) {
      identityPointEncoding
    } else {
      val normed = CryptoFacade.normalizePoint(point)
      val ySign = CryptoFacade.signOf(CryptoFacade.getAffineYCoord(normed))
      val X = CryptoFacade.encodeFieldElem(CryptoFacade.getXCoord(normed))
      val PO = safeNewArray[Byte](X.length + 1)
      PO(0) = (if (ySign) 0x03 else 0x02).toByte
      System.arraycopy(X, 0, PO, 1, X.length)
      PO
    }
    w.putBytes(bytes)
  }

  override def parse(r: CoreByteReader): EcPointType = {
    val encoded = r.getBytes(encodingSize)
    if (encoded(0) != 0) {
      CryptoContext.default.decodePoint(encoded)
    } else {
      CryptoContext.default.infinity // identity point of multiplicative group
    }
  }

}
