package sigmastate.utils

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sigma.serialization.generators.ObjectGenerators
import sigma.utils.SparseArrayContainer

class SparseArrayContainerSpecification extends AnyPropSpec
  with ObjectGenerators
  with ScalaCheckPropertyChecks
  with Matchers {

  private val distinctCodeValuePairsGen: Gen[Seq[(Byte, Long)]] = for {
    bytes <- distinctListOfGen(Arbitrary.arbByte.arbitrary)
    longs <- Gen.listOfN(bytes.length, Arbitrary.arbLong.arbitrary)
  } yield bytes.zip(longs)

  property("distinct codes") {
    val codeValuePairs = Seq[(Byte, Long)]((1, 1), (1, 2))
    an[RuntimeException] should be thrownBy
      new SparseArrayContainer(codeValuePairs)
  }

  property("get") {
    forAll(distinctCodeValuePairsGen) { codeValuePairs =>
      val cont = new SparseArrayContainer(codeValuePairs)
      codeValuePairs.foreach { case (code, v) =>
          cont(code) shouldBe v
      }
      val mappedValues = codeValuePairs.toMap
      (Byte.MinValue to Byte.MaxValue).foreach { i =>
        if (mappedValues.get(i.toByte).isEmpty)
          cont(i.toByte) shouldBe 0L
      }
    }
  }
}
