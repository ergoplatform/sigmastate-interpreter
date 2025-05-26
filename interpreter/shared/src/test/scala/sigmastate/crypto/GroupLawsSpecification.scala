package sigmastate.crypto

import java.math.BigInteger
import org.scalacheck.Gen
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sigma.crypto.{CryptoConstants, CryptoFacade, EcPointType, Ecp}
import sigmastate.TestsBase
import sigmastate.utils.Helpers

import scala.util.Random

class GroupLawsSpecification extends AnyPropSpec with ScalaCheckPropertyChecks with TestsBase {
  private val group = CryptoConstants.dlogGroup

  val groupElementGen: Gen[EcPointType] = Gen.const(group.createRandomElement())
  val groupGeneratorGen: Gen[EcPointType] = Gen.const(group.createRandomGenerator())
  val bigIntGen: Gen[BigInteger] = Gen.const{
    val bytes = Array.fill(32)(Random.nextInt(Byte.MaxValue).toByte)
    new BigInteger(1, bytes).mod(group.order)
  }

  property("multiplication law is complete") {
    forAll(groupElementGen) { ge =>
      val identity = group.identity
      group.multiplyGroupElements(ge, ge) shouldBe group.exponentiate(ge, new BigInteger("2"))
      group.multiplyGroupElements(ge, identity) shouldBe group.exponentiate(ge, BigInteger.ONE)
      group.multiplyGroupElements(ge, identity) shouldBe ge
      group.multiplyGroupElements(identity, identity) shouldBe identity

      val inverse = group.inverseOf(ge)
      group.multiplyGroupElements(ge, inverse) shouldBe identity
    }
  }

  property("exponentiation") {
    val identity = group.identity
    forAll(groupElementGen) { ge =>
      group.exponentiate(ge, BigInteger.ZERO) shouldBe identity
      group.exponentiate(ge, BigInteger.ONE) shouldBe ge
      group.exponentiate(ge, group.order) shouldBe identity
      group.exponentiate(ge, group.order.add(BigInteger.ONE)) shouldBe ge
    }
  }

  property("double inverse") {
    forAll(groupElementGen) { ge =>
      group.inverseOf(group.inverseOf(ge)) shouldBe ge
    }
  }

  property("precomputed"){
    forAll(groupGeneratorGen, bigIntGen) { case (base, exp) =>
      group.exponentiateWithPreComputedValues(base, exp) shouldBe group.exponentiate(base, exp)
    }
  }

// uncommment to generate new test vectors
//def printPoints(points: Seq[(String, Any)]) = {
//  points.foreach { case (name, p) =>
//    println(s"val $name = ${SigmaPPrint.apply(p).plainText}")
//  }
//}
//
//  property("generate initial points") {
//    printPoints(Seq(
//      "identity" -> group.identity,
//      "order" -> group.order,
//      "p1" -> group.createRandomElement(),
//      "p2" -> group.createRandomElement(),
//      "p3" -> group.createRandomElement()
//    ))
//  }

  val p1: Ecp = Helpers.decodeECPoint("0381c5275b1d50c39a0c36c4561c3a37bff1d87e37a9ad69eab029e426c0b1a8ac")
  val p2 = Helpers.decodeECPoint("02198064ec24024bb8b300e20dd18e33cc1fccb0fea73940bd9a1d3d9d6c3ddd8f")
  val p3 = Helpers.decodeECPoint("02e135f5f905fb843698d48959c6c792b2c6f9605b90be2280d53b4b69ef23e8a9")

// uncommment to generate new test vectors
//  property("generate op results") {
//    printPoints(Seq(
//      "p1" -> p1,
//      "p2" -> p2,
//      "p3" -> p3,
//      "p1.add(p2)" -> CryptoFacade.multiplyPoints(p1, p2),
//      "p1.multiply(order)" -> CryptoFacade.exponentiatePoint(p1, group.order),
//      "p1.multiply(order + 1)" -> CryptoFacade.exponentiatePoint(p1, group.order.add(BigInteger.ONE)),
//      "p1.inverse" -> CryptoFacade.negatePoint(p1)
//    ))
//  }

  property("check test vectors") {
    CryptoFacade.multiplyPoints(p1, p2) shouldBe
      Helpers.decodeECPoint("03de5e9c2806c05cd45a57d18c469743f42a0d2c84370b6662eb39d8a2990abed8")

    CryptoFacade.exponentiatePoint(p1, group.order) shouldBe
      Helpers.decodeECPoint("000000000000000000000000000000000000000000000000000000000000000000")

    CryptoFacade.exponentiatePoint(p1, group.order.add(BigInteger.ONE)) shouldBe
      Helpers.decodeECPoint("0381c5275b1d50c39a0c36c4561c3a37bff1d87e37a9ad69eab029e426c0b1a8ac")

    CryptoFacade.negatePoint(p1) shouldBe
      Helpers.decodeECPoint("0281c5275b1d50c39a0c36c4561c3a37bff1d87e37a9ad69eab029e426c0b1a8ac")
  }
}
