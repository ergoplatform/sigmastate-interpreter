package sigmastate

import org.scalactic.source.Position
import org.scalatest.{Args, Reporter, Tag}
import org.scalatest.events.{Event, TestFailed, TestSucceeded}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.propspec.AnyPropSpec
import sigma.VersionContext

import scala.collection.mutable.ArrayBuffer

class CompilerPropertyRegistrationSpecification extends AnyFunSuite {
  private val probeTag = new Tag("CompilerPropertyRegistrationSpecification.probe")

  private class RegistrationSuite(additionalMode: Boolean)
      extends AnyPropSpec with CompilerCrossVersionProps {
    override val okRunTestsWithoutMCLowering: Boolean = additionalMode
    val visits = ArrayBuffer.empty[(Byte, Byte)]
    val registrationLine = Position.here.lineNumber + 1
    compilerProperty("registered", probeTag) {
      visits += ((activatedVersionInTests, ergoTreeVersionInTests))
    }
  }

  test("compiler registration preserves names tags positions and version iteration") {
    val pairs = (0 to VersionContext.MaxSupportedScriptVersion).flatMap { activated =>
      (0 to activated).map(tree => (activated.toByte, tree.toByte))
    }.toVector

    for (additionalMode <- Seq(false, true)) {
      val suite = new RegistrationSuite(additionalMode)
      val names = if (additionalMode) Vector("registered", "registered_MCLowering")
                  else Vector("registered")
      assert(suite.testNames.toVector == names)
      names.foreach { name =>
        val data = suite.testDataFor(name)
        assert(data.tags.contains(probeTag.name))
        assert(data.pos.exists(_.lineNumber == suite.registrationLine))
      }

      val events = ArrayBuffer.empty[Event]
      val reporter = new Reporter {
        def apply(event: Event): Unit = events += event
      }
      val before = VersionContext.current
      val status = suite.run(None, Args(reporter))
      assert(status.isCompleted())
      assert(!events.exists(_.isInstanceOf[TestFailed]))
      assert(events.count(_.isInstanceOf[TestSucceeded]) == names.size)
      assert(suite.visits.toVector == Vector.fill(names.size)(pairs).flatten)
      assert(VersionContext.current == before)
    }
  }
}
