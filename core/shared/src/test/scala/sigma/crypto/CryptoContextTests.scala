package sigma.crypto

import sigma.BaseTests

class CryptoContextTests extends BaseTests {

  test("decodePoint accepts 1-byte infinity and rejects malformed 0x00 or empty inputs") {
    val ctx = CryptoFacade.createCryptoContext()
    val inf = ctx.decodePoint(Array[Byte](0))
    CryptoFacade.isInfinityPoint(inf) shouldBe true

    intercept[IllegalArgumentException] {
      ctx.decodePoint(Array.fill[Byte](33)(0))
    }
    intercept[IllegalArgumentException] {
      ctx.decodePoint(Array[Byte](0, 1, 2, 3))
    }
    intercept[Exception] {
      ctx.decodePoint(Array.emptyByteArray)
    }
    intercept[Exception] {
      ctx.decodePoint(null)
    }
  }
}
