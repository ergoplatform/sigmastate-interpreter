package sigmastate.utils

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class GenPredefFuncsDescriptionSpec extends AnyPropSpec with Matchers {

  property("description abbreviation is line-ending invariant at the length boundary") {
    val atLimitLf = ("a" * 75) + "\n" + ("b" * 74)
    val overLimitLf = atLimitLf + "c"

    for (description <- Seq(atLimitLf, atLimitLf.replace("\n", "\r\n"))) {
      GenPredefFuncsApp.shouldAbbreviateDescription(description) shouldBe false
    }
    for (description <- Seq(overLimitLf, overLimitLf.replace("\n", "\r\n"))) {
      GenPredefFuncsApp.shouldAbbreviateDescription(description) shouldBe true
    }
  }

  property("the DHTuple description is rendered in full") {
    val (rows, _) = GenPredefFuncsApp.generatePredefFuncTables()

    rows should include ("ErgoTree operation to create a new SigmaProp value representing public key")
    rows should include ("of Diffie Hellman signature protocol.")
    rows should include ("Common input: (g,h,u,v)")
  }
}
