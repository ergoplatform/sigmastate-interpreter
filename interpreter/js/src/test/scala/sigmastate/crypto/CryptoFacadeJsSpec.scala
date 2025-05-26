package sigmastate.crypto

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import sigma.crypto.Platform.Ecp
import sigma.crypto.utils.bytesToHex
import sigma.crypto.{CryptoFacade, CryptoFacadeJs, Point}

import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array

class CryptoFacadeJsSpec extends AnyPropSpec with Matchers with CryptoTesting {
  val p1 = Point.fromHex("0381c5275b1d50c39a0c36c4561c3a37bff1d87e37a9ad69eab029e426c0b1a8ac")
  val p2 = Point.fromHex("02198064ec24024bb8b300e20dd18e33cc1fccb0fea73940bd9a1d3d9d6c3ddd8f")
  val infinity = Point.ZERO
  val ctx = CryptoFacadeJs.createCryptoContext()

  property("CryptoFacade.createCryptoContext") {
    CryptoFacadeJs.createCryptoContext().getModulus() shouldBe modulus
  }

  property("CryptoFacade.normalizePoint") {
    CryptoFacade.normalizePoint(new Ecp(p1)) shouldBe new Ecp(p1)
  }

  property("CryptoFacade.negatePoint") {
    CryptoFacadeJs.negatePoint(p1).toHex(true) shouldBe
        "0281c5275b1d50c39a0c36c4561c3a37bff1d87e37a9ad69eab029e426c0b1a8ac"
  }

  property("CryptoFacade.isInfinityPoint") {
    CryptoFacadeJs.isInfinityPoint(infinity) shouldBe true
    CryptoFacadeJs.isInfinityPoint(p1) shouldBe false
  }

  property("CryptoFacade.multiplyPoint") {
    CryptoFacadeJs.multiplyPoint(p1, ctx.getOrder() - js.BigInt(1)).toHex(true) shouldBe
      "0281c5275b1d50c39a0c36c4561c3a37bff1d87e37a9ad69eab029e426c0b1a8ac"
  }

  property("CryptoFacade.addPoint") {
    CryptoFacadeJs.addPoint(p1, p2).toHex(true) shouldBe
      "03de5e9c2806c05cd45a57d18c469743f42a0d2c84370b6662eb39d8a2990abed8"
  }

  property("CryptoFacade.showPoint") {
    CryptoFacadeJs.showPoint(p1) shouldBe "ECPoint(81c527,db5d99,...)"
    CryptoFacadeJs.showPoint(p2) shouldBe "ECPoint(198064,811477,...)"
  }

  property("CryptoFacade.testBitZeroOfFieldElem") {
    case class Item(value: js.BigInt, expected: Boolean)
    val testVectors = Array(
      Item(
        value = js.BigInt("58696697963552658225209022604545906839720992139040109583461478932841590007980"),
        expected = false
      ),
      Item(js.BigInt("99221890655421281687240657051313379827030356718751695543666346070432252019955"), true),
      Item(js.BigInt("11534674179847572339525410967292119666111600146005056703881161120062101118351"), true),
      Item(js.BigInt("58384518810610603488166176247797257725226278383152129497027289229952751610898"), false),
      Item(js.BigInt("53248006962404494469764696243319434838764093672738035895911945007004750906420"), false),
      Item(js.BigInt("101865731467179904889950494818132658695802833332454014925098273981492319479977"), true),
    )
    for ( tv <- testVectors ) {
      CryptoFacadeJs.testBitZeroOfFieldElem(tv.value) shouldBe tv.expected
    }
  }

  def bytesToJsShorts(bytes: Array[Byte]): js.Array[Short] = {
    js.Array(bytes.map(x => (x & 0xFF).toShort): _*)
  }

  property("CryptoFacade.getEncodedOfFieldElem") {
    bytesToHex(CryptoFacadeJs.getEncodedOfFieldElem(p1.x)) shouldEqual
        "81c5275b1d50c39a0c36c4561c3a37bff1d87e37a9ad69eab029e426c0b1a8ac"
    bytesToHex(CryptoFacadeJs.getEncodedOfFieldElem(p1.y)) shouldBe
        "db5d999704ec84b62962f3e35889901d04a619cd6d81d251c69d0f625c2dc4f3"
  }
  
  property("CryptoFacade should get coordinates") {
    CryptoFacadeJs.getXCoord(p1) shouldBe p1.x
    CryptoFacadeJs.getYCoord(p1) shouldBe p1.y
  }

  property("CryptoFacade should get affine coordinates") {
    CryptoFacadeJs.getAffineXCoord(p1) shouldBe p1.x
    CryptoFacadeJs.getAffineYCoord(p1) shouldBe p1.y
  }

  property("CryptoFacade.hashHmacSHA512") {
    val key = Uint8Array.from(bytesToJsShorts(CryptoFacade.BitcoinSeed))
    val data = Uint8Array.from(bytesToJsShorts("abc".getBytes(CryptoFacade.Encoding)))
    val res = bytesToHex(CryptoFacadeJs.hashHmacSHA512(key, data))
    res shouldBe "2c15e87cde0f876fd8f060993748330cbe5f37c8bb3355e8ef44cea57890ec1d9b3274ef2b67bbe046cf8a012fba69796ec7803b1cc227521b9f5191e80a7da2"
  }

  property("CryptoFacade.generatePbkdf2Key") {
    val mnemonic = "slow silly start wash bundle suffer bulb ancient height spin express remind today effort helmet"
    val password = "pwd"
    val res = bytesToHex(CryptoFacadeJs.generatePbkdf2Key(mnemonic, password))
    res shouldBe "0a8ea2ea0c4c12a9df88b005bda00c4de51ff36834b5fcd6a83667c371ad1da94bca1798690d87f2603b8f51d5ae025209e31f6cf81e12b84e4c543d236e58d0"
  }
}
