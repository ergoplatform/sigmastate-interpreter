package sigmastate.crypto

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import sigma.crypto.CryptoContextJs

import scala.scalajs.js

class CryptoContextJsSpec extends AnyPropSpec with Matchers with CryptoTesting {
  val p1_x = js.BigInt("58696697963552658225209022604545906839720992139040109583461478932841590007980")
  val p1_y = js.BigInt("99221890655421281687240657051313379827030356718751695543666346070432252019955")
  val pointHex = "0381c5275b1d50c39a0c36c4561c3a37bff1d87e37a9ad69eab029e426c0b1a8ac"
  val generatorPointHex = "0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"
  val ctx = new CryptoContextJs

  property("CryptoContext.getModulus") {
    ctx.getModulus() shouldBe modulus
  }

  property("CryptoContext.getOrder") {
    ctx.getOrder() shouldBe order
  }

  property("CryptoContext.validatePoint") {
    val p1 = ctx.validatePoint(p1_x, p1_y)
    p1.x shouldBe p1_x
    p1.y shouldBe p1_y
  }

  property("CryptoContext.getInfinity") {
    ctx.getInfinity().toHex(true) should not be(empty)
  }

  property("CryptoContext.decodePoint") {
    val p1_decoded = ctx.decodePoint(pointHex)
    p1_decoded.x shouldBe p1_x
    p1_decoded.y shouldBe p1_y
  }

  property("CryptoContext.getGenerator") {
    ctx.getGenerator().toHex(true) shouldBe generatorPointHex
  }
}
