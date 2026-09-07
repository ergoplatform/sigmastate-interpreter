package sigmastate

import org.scalactic.source.Position
import org.scalatest.{Args, Outcome, Reporter, Suite, Tag}
import org.scalatest.events.{Event, TestCanceled, TestFailed, TestPending, TestSucceeded}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.propspec.{AnyPropSpec, AnyPropSpecLike}
import sigma.VersionContext
import sigmastate.eval.CProfiler

import scala.collection.mutable.ArrayBuffer

class CrossVersionPropsSpecification extends AnyFunSuite {
  private val probeTag = new Tag("CrossVersionPropsSpecification.probe")

  private case class Visit(
      activated: Byte,
      tree: Byte,
      context: VersionContext,
      profiler: Option[CProfiler])

  private class RecordingReporter extends Reporter {
    val events = ArrayBuffer.empty[Event]
    def apply(event: Event): Unit = events += event
  }

  private def runSuite(suite: Suite): Vector[Event] = {
    val reporter = new RecordingReporter
    val status = suite.run(None, Args(reporter))
    assert(status.isCompleted)
    reporter.events.toVector
  }

  private val supportedPairs = (0 to VersionContext.MaxSupportedScriptVersion).flatMap { activated =>
    (0 to activated).map(tree => (activated.toByte, tree.toByte))
  }.toVector

  private class RepeatedSuite(warmups: Int, stop: String = "none", stopAt: Int = -1)
      extends AnyPropSpec with CrossVersionProps {
    override def perTestWarmUpIters: Int = warmups

    val visits = ArrayBuffer.empty[Visit]
    val diagnosticNames = ArrayBuffer.empty[String]
    val expectedFailure = new IllegalStateException("expected adapter test failure")
    var fixtureCalls = 0

    override def withFixture(test: NoArgTest): Outcome = {
      fixtureCalls += 1
      super.withFixture(test)
    }

    override protected def testFun_Run(testName: String, testFun: => Any): Unit = {
      diagnosticNames += testName
      super.testFun_Run(testName, testFun)
    }

    val propertyLine = Position.here.lineNumber + 1
    property("repeated", probeTag) {
      visits += Visit(activatedVersionInTests, ergoTreeVersionInTests, VersionContext.current, warmupProfiler)
      if (visits.size - 1 == stopAt) {
        stop match {
          case "failed" => throw expectedFailure
          case "canceled" => cancel("expected adapter test cancellation")
          case "pending" => pending
          case _ => ()
        }
      }
    }
  }

  private trait LowerFixture extends AnyPropSpecLike {
    val fixtureTrace = ArrayBuffer.empty[String]

    override def withFixture(test: NoArgTest): Outcome = {
      fixtureTrace += "enter"
      try super.withFixture(test)
      finally fixtureTrace += "exit"
    }
  }

  private class LowerFixtureSuite(failAtThirdVersion: Boolean)
      extends AnyPropSpec with LowerFixture with CrossVersionProps {
    override def perTestWarmUpIters: Int = 2
    val expectedFailure = new IllegalStateException("expected lower-fixture test failure")
    private var versionedCalls = 0

    property("lower fixture") {
      if (warmupProfiler.nonEmpty) fixtureTrace += "warmup"
      else {
        fixtureTrace += s"body:$activatedVersionInTests:$ergoTreeVersionInTests"
        versionedCalls += 1
        if (failAtThirdVersion && versionedCalls == 3) throw expectedFailure
      }
    }
  }

  test("lower fixtures enclose all warmups and versioned bodies on success and failure") {
    for (failAtThirdVersion <- Seq(false, true)) {
      withClue(s"failAtThirdVersion=$failAtThirdVersion: ") {
        val suite = new LowerFixtureSuite(failAtThirdVersion)
        val events = runSuite(suite)
        val pairs = if (failAtThirdVersion) supportedPairs.take(3) else supportedPairs
        val expectedBodies = pairs.map { case (activated, tree) => s"body:$activated:$tree" }

        assert(suite.fixtureTrace.toVector ==
          Vector("enter", "warmup", "warmup") ++ expectedBodies ++ Vector("exit"))
        val failures = events.collect { case event: TestFailed => event }
        if (failAtThirdVersion) {
          assert(failures.map(_.throwable) == Vector(Some(suite.expectedFailure)))
          assert(!events.exists(_.isInstanceOf[TestSucceeded]))
        } else {
          assert(failures.isEmpty)
          assert(events.count(_.isInstanceOf[TestSucceeded]) == 1)
        }
      }
    }
  }

  test("each supported version pair executes once with its context and one outer fixture") {
    val suite = new RepeatedSuite(0)
    val before = VersionContext.current
    val events = runSuite(suite)

    assert(suite.visits.map(v => (v.activated, v.tree)).toVector == supportedPairs)
    assert(suite.visits.forall(v => v.context == VersionContext(v.activated, v.tree)))
    assert(suite.visits.forall(_.profiler.isEmpty))
    assert(suite.diagnosticNames.toVector == Vector.fill(supportedPairs.size)("repeated"))
    assert(suite.fixtureCalls == 1)
    assert(events.count(_.isInstanceOf[TestSucceeded]) == 1)
    assert(VersionContext.current == before)
  }

  test("warmups share one profiler and precede every versioned execution") {
    val suite = new RepeatedSuite(2)
    val before = VersionContext.current
    val initialPair = (suite.activatedVersionInTests, suite.ergoTreeVersionInTests)
    val events = runSuite(suite)
    val warmups = suite.visits.take(2)
    val versioned = suite.visits.drop(2)

    assert(suite.visits.size == 2 + supportedPairs.size)
    assert(warmups.forall(v => (v.activated, v.tree) == initialPair && v.context == before))
    assert(warmups.forall(_.profiler.nonEmpty))
    assert(warmups(0).profiler.get eq warmups(1).profiler.get)
    assert(versioned.map(v => (v.activated, v.tree)).toVector == supportedPairs)
    assert(versioned.forall(_.profiler.isEmpty))
    assert(suite.diagnosticNames.size == suite.visits.size)
    assert(suite.warmupProfiler.isEmpty)
    assert(suite.fixtureCalls == 1)
    assert(events.count(_.isInstanceOf[TestSucceeded]) == 1)
    assert(VersionContext.current == before)
    assert((suite.activatedVersionInTests, suite.ergoTreeVersionInTests) == initialPair)
  }

  test("failure cancellation and pending stop warmup or version iteration without losing the outcome") {
    for (stop <- Seq("failed", "canceled", "pending"); duringWarmup <- Seq(false, true)) {
      withClue(s"$stop, duringWarmup=$duringWarmup: ") {
        val expectedCalls = if (duringWarmup) 1 else 3
        val suite = new RepeatedSuite(if (duringWarmup) 2 else 0, stop, expectedCalls - 1)
        val before = VersionContext.current
        val initialPair = (suite.activatedVersionInTests, suite.ergoTreeVersionInTests)
        val events = runSuite(suite)

        assert(suite.visits.size == expectedCalls)
        assert(suite.diagnosticNames.size == expectedCalls)
        assert(suite.visits.forall(_.profiler.nonEmpty == duringWarmup))
        if (!duringWarmup)
          assert(suite.visits.map(v => (v.activated, v.tree)).toVector == supportedPairs.take(expectedCalls))
        assert(suite.fixtureCalls == 1)
        assert(suite.warmupProfiler.isEmpty)
        assert(VersionContext.current == before)
        assert((suite.activatedVersionInTests, suite.ergoTreeVersionInTests) == initialPair)
        assert(!events.exists(_.isInstanceOf[TestSucceeded]))
        stop match {
          case "failed" =>
            val failures = events.collect { case event: TestFailed => event }
            assert(failures.size == 1)
            assert(failures.head.throwable.contains(suite.expectedFailure))
          case "canceled" => assert(events.count(_.isInstanceOf[TestCanceled]) == 1)
          case "pending" => assert(events.count(_.isInstanceOf[TestPending]) == 1)
        }
      }
    }
  }

  test("ordinary properties retain their caller position and tags") {
    val suite = new RepeatedSuite(0)
    val data = suite.testDataFor("repeated")
    assert(data.pos.exists(p => p.fileName == "CrossVersionPropsSpecification.scala" && p.lineNumber == suite.propertyLine))
    assert(data.tags.contains(probeTag.name))
  }

  test("property2 bypasses warmup version iteration and diagnostics and retains its caller position") {
    class BypassSuite extends AnyPropSpec with CrossVersionProps {
      override def perTestWarmUpIters: Int = 2
      val visits = ArrayBuffer.empty[Visit]
      var diagnosticCalls = 0

      override protected def testFun_Run(testName: String, testFun: => Any): Unit = {
        diagnosticCalls += 1
        super.testFun_Run(testName, testFun)
      }

      val propertyLine = Position.here.lineNumber + 1
      property2("unversioned", probeTag) {
        visits += Visit(activatedVersionInTests, ergoTreeVersionInTests, VersionContext.current, warmupProfiler)
      }
    }

    val suite = new BypassSuite
    val before = VersionContext.current
    val initialPair = (suite.activatedVersionInTests, suite.ergoTreeVersionInTests)
    val data = suite.testDataFor("unversioned")
    val events = runSuite(suite)

    assert(suite.visits.toVector == Vector(Visit(initialPair._1, initialPair._2, before, None)))
    assert(suite.diagnosticCalls == 0)
    assert(events.count(_.isInstanceOf[TestSucceeded]) == 1)
    assert(data.pos.exists(p => p.fileName == "CrossVersionPropsSpecification.scala" && p.lineNumber == suite.propertyLine))
    assert(data.tags.contains(probeTag.name))
  }
}
