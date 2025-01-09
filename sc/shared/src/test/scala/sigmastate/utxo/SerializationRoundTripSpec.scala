package sigmastate.utxo

import org.ergoplatform._
import sigma.util.BenchmarkUtil
import sigmastate.helpers.CompilerTestingCommons
import sigma.interpreter.{ContextExtension, ProverResult}
import sigma.serialization.generators.ObjectGenerators
import debox.{Buffer => DBuffer}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import sigma.util.{MaxArrayLength, safeNewArray}

class SerializationRoundTripSpec extends AnyPropSpec
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with ObjectGenerators
  with CompilerTestingCommons {

  case class Run(size: Int, time: Long)

  property("ValueSerializer.newArray") {
    safeNewArray[Int](0).length shouldBe 0
    safeNewArray[Int](MaxArrayLength).length shouldBe MaxArrayLength

    // test vector to catch constant changes
    MaxArrayLength shouldBe 100000

    assertExceptionThrown(
      safeNewArray[Int](MaxArrayLength + 1),
      exceptionLike[RuntimeException]("Cannot allocate array of Int"))
  }

  property("ErgoBoxCandidate: Serializer round trip benchmark") {
    val runs = DBuffer.empty[Run]
    forAll(MinSuccessful(20)) { t: ErgoBoxCandidate =>
      val (_, time) = BenchmarkUtil.measureTime {
        var i = 0
        while (i < 100) {
          roundTripTest(t)(ErgoBoxCandidate.serializer)
          i += 1
        }
      }
      runs += Run(t.bytesWithNoRef.length, time)
    }
    val ordered = runs.toArray().sortBy(_.size)
    for (r <- ordered) {
      printDebug(s"Size: ${r.size}, Time: ${r.time}")
    }
  }

  property("ErgoBoxCandidate: Serializer round trip") {
    forAll { t: ErgoBoxCandidate => roundTripTest(t)(ErgoBoxCandidate.serializer) }
    forAll { t: ErgoBoxCandidate => roundTripTestWithPos(t)(ErgoBoxCandidate.serializer) }
  }

  property("ErgoBox: Serializer round trip") {
    forAll { t: ErgoBox =>
      val parsed = roundTripTest(t)(ErgoBox.sigmaSerializer)
      parsed.bytes shouldBe t.bytes
    }
    forAll { t: ErgoBox =>
      val parsed = roundTripTestWithPos(t)(ErgoBox.sigmaSerializer)
      parsed.bytes shouldBe t.bytes
    }
  }

  property("ContextExtension: Serializer round trip") {
    forAll { t: ContextExtension => roundTripTest(t)(ContextExtension.serializer) }
    forAll { t: ContextExtension => roundTripTestWithPos(t)(ContextExtension.serializer) }
  }

  property("SerializedProverResult: Serializer round trip") {
    forAll { t: ProverResult => roundTripTest(t)(ProverResult.serializer) }
    forAll { t: ProverResult => roundTripTestWithPos(t)(ProverResult.serializer) }
  }

  property("Input: Serializer round trip") {
    forAll { t: Input => roundTripTest(t)(Input.serializer) }
    forAll { t: Input => roundTripTestWithPos(t)(Input.serializer) }
  }
}
