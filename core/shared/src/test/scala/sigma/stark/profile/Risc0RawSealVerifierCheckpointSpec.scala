package sigma.stark.profile

import sigma.BaseTests
import sigma.stark.VerifierOperationObserver

class Risc0RawSealVerifierCheckpointSpec extends BaseTests {
  import Risc0RawSealVerifier._

  private final class ObserverSentinel extends RuntimeException

  private final class CountingOperationObserver extends VerifierOperationObserver {
    var calls: Int = 0

    override def onOperation(operationId: Int): Unit = calls += 1
  }

  test("checkpoint-disabled probes do not evaluate diagnostic payloads") {
    val sentinel = new ObserverSentinel
    var evaluations = 0

    def rejectedPayload(): Array[Int] = {
      evaluations += 1
      throw sentinel
    }

    checkpoint(NoProbe, "no-probe", rejectedPayload())
    evaluations shouldBe 0

    val operationObserver = new CountingOperationObserver
    val operationOnly = new OperationOnlyProbe(operationObserver)
    checkpoint(operationOnly, "operation-only", rejectedPayload())
    evaluations shouldBe 0
    operationOnly.operationSinkOrNull should be theSameInstanceAs operationObserver
    operationObserver.calls shouldBe 0
  }

  test("capturing probes evaluate once and receive an owned snapshot") {
    val source = Array(1, 2, 3)
    var evaluations = 0
    var observedLabel: String = null
    var observedValues: Array[Int] = null
    val probe = new Probe {
      override def onCheckpoint(label: String, values: Array[Int]): Unit = {
        observedLabel = label
        observedValues = values
      }
    }

    checkpoint(probe, "captured", {
      evaluations += 1
      source
    })

    evaluations shouldBe 1
    observedLabel shouldBe "captured"
    observedValues shouldBe source
    (observedValues eq source) shouldBe false

    source(0) = 9
    observedValues(0) shouldBe 1
    observedValues(1) = 8
    source(1) shouldBe 2
  }

  test("capturing probes preserve payload and callback exception identity") {
    val payloadSentinel = new ObserverSentinel
    val payloadProbe = new Probe {}
    val payloadFailure = intercept[ObserverSentinel] {
      checkpoint(payloadProbe, "payload-failure", throw payloadSentinel)
    }
    payloadFailure should be theSameInstanceAs payloadSentinel

    val callbackSentinel = new ObserverSentinel
    var evaluations = 0
    val callbackProbe = new Probe {
      override def onCheckpoint(label: String, values: Array[Int]): Unit =
        throw callbackSentinel
    }
    val callbackFailure = intercept[ObserverSentinel] {
      checkpoint(callbackProbe, "callback-failure", {
        evaluations += 1
        Array(4, 5)
      })
    }
    callbackFailure should be theSameInstanceAs callbackSentinel
    evaluations shouldBe 1
  }
}
