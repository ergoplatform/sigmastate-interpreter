package org.ergoplatform.sdk.js

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.util.encode.Base16
import sigma.ast.{DeserializationSigmaBuilder, SType}
import sigma.ast._
import sigma.crypto.CryptoConstants.dlogGroup
import sigma.crypto.CryptoFacade
import sigma.serialization.ConstantSerializer
import sigmastate.utils.Helpers
import sigma.SigmaTestingData
import sigma.data.{CSigmaProp, ProveDlog}
import sigma.ast.js.isoValueToConstant
import sigma.js.Value

import java.math.BigInteger

class ValueSpec extends AnyPropSpec with Matchers with SigmaTestingData with ScalaCheckPropertyChecks {

  def test[T <: SType](c: Constant[T], expectedHex: String) = {
    val v = isoValueToConstant.from(c)
    val S = ConstantSerializer(DeserializationSigmaBuilder)
    Base16.encode(S.toBytes(c)) shouldBe expectedHex
    v.toHex() shouldBe expectedHex
    isoValueToConstant.to(Value.fromHex(expectedHex)) shouldBe c
  }

  property("Boolean toHex()/fromHex()") {
    test(BooleanConstant(true), "0101")
  }

  property("Numeric toHex()/fromHex()") {
    test(ByteConstant(127), "027f")
    test(ShortConstant(Short.MaxValue), "03feff03")
    test(IntConstant(Int.MaxValue), "04feffffffffffffffff01")
    test(LongConstant(Long.MaxValue), "05feffffffffffffffff01")
  }

  property("BigInt toHex()/fromHex()") {
    val bigValue = BigInteger.valueOf(Long.MaxValue).multiply(BigInteger.valueOf(2))
    test(BigIntConstant(bigValue), "060900fffffffffffffffe")
  }

  property("GroupElement toHex()/fromHex()") {
    val ge = CryptoFacade.exponentiatePoint(dlogGroup.generator, BigInteger.valueOf(2))
    test(GroupElementConstant(ge), "0702c6047f9441ed7d6d3045406e95c07cd85c778e4b8cef3ca7abac09b95c709ee5")
  }

  property("SigmaProp toHex()/fromHex()") {
    val c = SigmaPropConstant(
      CSigmaProp(
        ProveDlog(
          Helpers.decodeECPoint("0297c44a12f4eb99a85d298fa3ba829b5b42b9f63798c980ece801cc663cc5fc9e")
        )))
    test(c, "08cd0297c44a12f4eb99a85d298fa3ba829b5b42b9f63798c980ece801cc663cc5fc9e")
  }

  property("AvlTree toHex()/fromHex()") {
    test(AvlTreeConstant(TestData.t3), "643100d2e101ff01fc047c7f6f00ff80129df69a5090012f01ffca99f5bfff0c803601800100")
  }

  // TODO turn on when Value.fromHex is implemented for Box
  ignore("Box toHex()/fromHex()") {
    test(BoxConstant(TestData.b2), "63b96000d1968302010100ff83020193040204020100c0843d000401010e32297000800b80f1d56c809a8c6affbed864b87f007f6f007f00ac00018c01c4fdff011088807f0100657f00f9ab0101ff6d6505a4a7b5a2e7a4a4dd3a05feffffffffffffffff01003bd5c630803cfff6c1ff7f7fb980ff136afc011f8080b8b04ad4dbda2d7f4e01")
  }

  property("Unit toHex()/fromHex()") {
    test(UnitConstant.instance, "62")
  }

}
