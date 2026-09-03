package sigma.eval

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Eip0045DispatchLookupCensusSpec extends AnyFunSuite with Matchers {
  import StarkVerificationCapability._

  private final case class Census(
      result: Option[ProfileEntry],
      entryComparisons: Int,
      byteComparisons: Int)

  private final class CountingObserver extends DispatchLookupObserver {
    var entryComparisons: Int = 0
    var byteComparisons: Int = 0

    override def onEntryComparison(): Unit = entryComparisons += 1
    override def onByteComparison(): Unit = byteComparisons += 1
  }

  private final class LookupSentinel extends RuntimeException

  private final class ThrowingObserver(
      throwOnEntry: Boolean,
      sentinel: LookupSentinel) extends DispatchLookupObserver {
    override def onEntryComparison(): Unit =
      if (throwOnEntry) throw sentinel

    override def onByteComparison(): Unit =
      if (!throwOnEntry) throw sentinel
  }

  private def numericId(value: Int): Array[Byte] = {
    val result = new Array[Byte](ProfileIdBytes)
    result(28) = (value >>> 24).toByte
    result(29) = (value >>> 16).toByte
    result(30) = (value >>> 8).toByte
    result(31) = value.toByte
    result
  }

  private def firstByteId(value: Int): Array[Byte] = {
    val result = new Array[Byte](ProfileIdBytes)
    result(0) = value.toByte
    result
  }

  private def right[A](value: Either[ConstructionFailure, A]): A = value match {
    case Right(result) => result
    case Left(failure) => fail("unexpected capability rejection: " + failure)
  }

  private def quarantinedEntry(profileId: Array[Byte]): ProfileEntry =
    right(quarantined(profileId))

  private def capability(entries: IndexedSeq[ProfileEntry]): Snapshot =
    right(snapshot(
      numericId(7),
      protocolGeneration = 11,
      HistoricalBlockValidation,
      dispatchJit = 1709,
      entries))

  private def observe(snapshot: Snapshot, profileId: Array[Byte]): Census = {
    val observer = new CountingObserver
    val result = snapshot.lookupObserved(profileId, observer)
    Census(result, observer.entryComparisons, observer.byteComparisons)
  }

  private def resultId(result: Option[ProfileEntry]): Option[Seq[Byte]] =
    result.map(_.profileId.toSeq)

  private lazy val maximumSnapshot: Snapshot = {
    val entries = Vector.tabulate(MaxTransitionEntries) { index =>
      quarantinedEntry(numericId((index + 1) * 2))
    }
    capability(entries)
  }

  test("maximum snapshot absent-high lookup performs exactly sixteen entry comparisons") {
    val census = observe(
      maximumSnapshot,
      numericId(MaxTransitionEntries * 2 + 2))

    census.result shouldBe None
    census.entryComparisons shouldBe 16
    census.byteComparisons should be <= (16 * ProfileIdBytes)
  }

  test("empty and singleton snapshots preserve zero and one comparison bounds") {
    val empty = observe(capability(Vector.empty), numericId(1))
    empty.result shouldBe None
    empty.entryComparisons shouldBe 0
    empty.byteComparisons shouldBe 0

    val singleton = capability(Vector(quarantinedEntry(numericId(2))))
    val hit = observe(singleton, numericId(2))
    hit.result.map(_.profileId.toSeq) shouldBe Some(numericId(2).toSeq)
    hit.entryComparisons shouldBe 1
    hit.byteComparisons shouldBe ProfileIdBytes

    val miss = observe(singleton, firstByteId(0xff))
    miss.result shouldBe None
    miss.entryComparisons shouldBe 1
    miss.byteComparisons shouldBe 1
  }

  test("maximum snapshot bounds middle, edge, internal-miss and external-miss lookups") {
    val middleIndex = MaxTransitionEntries / 2
    val cases = Seq(
      ("first", numericId(2), true),
      ("middle", numericId((middleIndex + 1) * 2), true),
      ("last", numericId(MaxTransitionEntries * 2), true),
      ("absent-low", numericId(0), false),
      ("absent-internal", numericId((middleIndex + 1) * 2 - 1), false),
      ("absent-high", numericId(MaxTransitionEntries * 2 + 2), false))

    cases.foreach { case (label, profileId, expectedPresent) =>
      withClue(label + ": ") {
        val census = observe(maximumSnapshot, profileId)
        census.result.isDefined shouldBe expectedPresent
        census.entryComparisons should be <= 16
        census.byteComparisons should be <=
          (census.entryComparisons * ProfileIdBytes)
      }
    }
  }

  test("observed, null-observer and production lookup routes return the same entry") {
    val queries = Seq(
      numericId(2),
      numericId((MaxTransitionEntries / 2 + 1) * 2),
      numericId(MaxTransitionEntries * 2),
      numericId(0),
      numericId(MaxTransitionEntries * 2 + 2))

    queries.foreach { profileId =>
      val observed = observe(maximumSnapshot, profileId).result
      val nullObserved = maximumSnapshot.lookupObserved(profileId, null)
      val production = maximumSnapshot.lookup(profileId)
      resultId(observed) shouldBe resultId(production)
      resultId(nullObserved) shouldBe resultId(production)
    }
  }

  test("lookup ordering remains unsigned across the 0x7f and 0x80 boundary") {
    val snapshot = capability(Vector(
      quarantinedEntry(firstByteId(0x7f)),
      quarantinedEntry(firstByteId(0x80))))

    resultId(observe(snapshot, firstByteId(0x7f)).result) shouldBe
      Some(firstByteId(0x7f).toSeq)
    resultId(observe(snapshot, firstByteId(0x80)).result) shouldBe
      Some(firstByteId(0x80).toSeq)
    observe(snapshot, firstByteId(0xff)).result shouldBe None
  }

  test("observer exceptions propagate without becoming lookup results") {
    val snapshot = capability(Vector(quarantinedEntry(numericId(2))))

    val byteSentinel = new LookupSentinel
    val observedByte = intercept[LookupSentinel] {
      snapshot.lookupObserved(
        numericId(2),
        new ThrowingObserver(throwOnEntry = false, byteSentinel))
    }
    observedByte should be theSameInstanceAs byteSentinel

    val entrySentinel = new LookupSentinel
    val observedEntry = intercept[LookupSentinel] {
      snapshot.lookupObserved(
        numericId(2),
        new ThrowingObserver(throwOnEntry = true, entrySentinel))
    }
    observedEntry should be theSameInstanceAs entrySentinel
  }

  test("observer API has no payload and production objects retain no observer state") {
    val observerClass = classOf[DispatchLookupObserver]
    val threadLocalClass = Class.forName("java.lang.ThreadLocal")
    observerClass.getDeclaredFields.toSeq shouldBe empty
    observerClass.getDeclaredMethods.map(_.getName).toSet shouldBe
      Set("onEntryComparison", "onByteComparison")
    observerClass.getDeclaredMethods.foreach { method =>
      method.getParameterTypes.toSeq shouldBe empty
      method.getReturnType shouldBe java.lang.Void.TYPE
    }

    Seq(classOf[Snapshot], StarkVerificationCapability.getClass).foreach { clazz =>
      val retained = clazz.getDeclaredFields.filter { field =>
        val lowerName = field.getName.toLowerCase(java.util.Locale.ROOT)
        observerClass.isAssignableFrom(field.getType) ||
          threadLocalClass.isAssignableFrom(field.getType) ||
          lowerName.contains("observer") || lowerName.contains("probe")
      }
      withClue(clazz.getName + ": ") {
        retained.toSeq shouldBe empty
      }
    }

    classOf[Snapshot].getDeclaredMethod(
      "lookup",
      classOf[Array[Byte]]).getParameterTypes.toSeq shouldBe
      Seq(classOf[Array[Byte]])
    classOf[Snapshot].getDeclaredMethod(
      "lookupObserved",
      classOf[Array[Byte]],
      observerClass).getParameterTypes.toSeq shouldBe
      Seq(classOf[Array[Byte]], observerClass)
    classOf[Snapshot].getDeclaredMethods.exists(
      _.getName.startsWith("lookup$default$")) shouldBe false
  }
}
