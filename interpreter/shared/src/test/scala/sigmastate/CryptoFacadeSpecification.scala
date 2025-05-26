package sigmastate

import org.ergoplatform.settings.ErgoAlgos
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.util.encode.Base16
import sigma.crypto.CryptoFacade

import java.math.BigInteger

class CryptoFacadeSpecification extends AnyPropSpec with Matchers with ScalaCheckPropertyChecks {

  val G_hex = "0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"

  property("CryptoFacade.HashHmacSHA512") {
    val cases = Table(
      ("string", "hash"),
      ("abc", "2c15e87cde0f876fd8f060993748330cbe5f37c8bb3355e8ef44cea57890ec1d9b3274ef2b67bbe046cf8a012fba69796ec7803b1cc227521b9f5191e80a7da2"),
      ("", "300b155f751964276c0536230bd9b16fe7a86533c3cbaa7575e8d0431dbedf23f9945bb8b052bd0b0802c10c7c852e7765b69b61ce7233d9fe5a35ab108ca3b6"),
      ("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq", "888ead7cb2ff330420333cac103f1062a6a443170108f6f74e2cdf39468015ae792c4a822664ce5d865424d2569d67bec03abd2df2a924977d635d06a0b550a3"),
      ("abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu", "57edbc19570001de233edcb104237ea81439e59c5b0000d4db7bd991d228453827428d3cf30ecb4cdb17f00de3444579fa771f8933d2a7d9430b56dd989e55d9"),
      ("a" * 1000, "2b53959bef11414f0592f8d6e834857734c7b9fcfbbfcafd84db989a85f7dfa76990a832fd24d8a3f804801898e59ac11a4dcd3e23a9121adc40a5498dcf4bc5")
    )
    forAll(cases) { (in, expectedRes) =>
      val inBytes = in.getBytes(CryptoFacade.Encoding)
      val res = CryptoFacade.hashHmacSHA512(CryptoFacade.BitcoinSeed, inBytes)
      Base16.encode(res) shouldBe expectedRes
    }
  }

  property("CryptoFacade.normalizeChars") {
    val cases = Table(
      ("chars", "keyHex"),
      ("slow silly start wash bundle suffer bulb ancient height spin express remind today effort helmet".toCharArray,
          "slow silly start wash bundle suffer bulb ancient height spin express remind today effort helmet"),
      ("pwd".toCharArray, "pwd")
    )

    forAll(cases) { (chars, expected) =>
      val res = CryptoFacade.normalizeChars(chars)
      res shouldBe expected
    }
  }

  property("CryptoFacade.generatePbkdf2Key") {
    val cases = Table(
      ("mnemonic", "password", "keyHex"),
      ("slow silly start wash bundle suffer bulb ancient height spin express remind today effort helmet", "", "e97efb594affe44261494fb366f6f3e8f506265b2865d3a5d173c5127d67c5d3fcb5bf7fe1b05cdad344df43ab87796810d545dbcba24f596275d8fceb846c98"),
      ("slow silly start wash bundle suffer bulb ancient height spin express remind today effort helmet", "pwd", "0a8ea2ea0c4c12a9df88b005bda00c4de51ff36834b5fcd6a83667c371ad1da94bca1798690d87f2603b8f51d5ae025209e31f6cf81e12b84e4c543d236e58d0")
    )

    forAll(cases) { (mnemonic, password, keyHex) =>
      val res = CryptoFacade.generatePbkdf2Key(mnemonic, password)
      Base16.encode(res) shouldBe keyHex
    }
  }

  property("CryptoFacade.getASN1Encoding") {
    val ctx = CryptoFacade.createCryptoContext()
    val G = ctx.generator
    val Q = ctx.order
    val vectors = Table(
      ("point", "expectedHex"),
      (ctx.infinity(), "00"),
      (CryptoFacade.exponentiatePoint(G, Q), "00"),
      (G, G_hex),
      (CryptoFacade.exponentiatePoint(G, BigInteger.ONE), G_hex),
      (CryptoFacade.exponentiatePoint(G, Q.subtract(BigInteger.ONE)), "0379be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798")
    )
    forAll (vectors) { (point, expectedHex) =>
      val res = ErgoAlgos.encode(CryptoFacade.getASN1Encoding(point, true))
      res shouldBe expectedHex
    }
  }

  property("CryptoContext.decodePoint") {
    val ctx = CryptoFacade.createCryptoContext()
    
    val inf = ctx.decodePoint(Array[Byte](0))
    CryptoFacade.isInfinityPoint(inf) shouldBe true

    val G = ctx.generator
    ctx.decodePoint(ErgoAlgos.decode(G_hex).get) shouldBe G

    val Q = ctx.order
    val Q_minus_1 = Q.subtract(BigInteger.ONE)
    val maxExp = CryptoFacade.exponentiatePoint(G, Q_minus_1)
    val maxExpBytes = ErgoAlgos.decode("0379be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798").get
    ctx.decodePoint(maxExpBytes) shouldBe maxExp
  }
}