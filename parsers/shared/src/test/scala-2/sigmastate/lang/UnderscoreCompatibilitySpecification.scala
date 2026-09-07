package sigmastate.lang

import fastparse._
import fastparse.NoWhitespace._
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import sigmastate.lang.parsers.{Core, CoreUnderscore, Key}

class UnderscoreCompatibilitySpecification extends AnyPropSpec with Matchers {
  property("Core retains its Scala 2 underscore-token entry point") {
    val parser: Core = SigmaParser
    def legacy[Ctx:P]: P[Unit] = P(parser.`_`[Ctx] ~ End)
    parse("_", legacy(_)) shouldBe Parsed.Success((), 1)
    parse("_suffix", legacy(_)).isSuccess shouldBe false
  }

  property("the shared token entry point respects Scala 2 underscore overrides") {
    val parser = new CoreUnderscore {
      override def `_`[Ctx:P]: P[Unit] = Key.W("legacy")
    }
    def shared[Ctx:P]: P[Unit] = P(parser.Underscore[Ctx] ~ End)
    parse("legacy", shared(_)) shouldBe Parsed.Success((), 6)
    parse("_", shared(_)).isSuccess shouldBe false
  }
}
