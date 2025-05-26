package sigmastate.utils

import sigma.serialization.generators.ObjectGenerators
import Helpers._
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sigma.Extensions.ArrayOps

class HelpersTests extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers with ObjectGenerators {
  property("xorU") {
    forAll(arrayGen[Byte]) { arr =>

      val x = xor(arr, arr)
      val xColl = xor(arr.toColl, arr.toColl)
      x shouldBe xColl.toArray

      val cloned = arr.clone()
      xorU(cloned, arr)
      cloned shouldBe x

      val arr1 = x
      val arr2 = cloned
      val arr3 = xor(arr1, arr2)
      val arr3Coll = xor(arr1.toColl, arr2.toColl)
      arr3 shouldBe arr3Coll.toArray

      val res1 = xor(cloned, arr1, arr2, arr3)
      val res1Coll = xor(cloned.toColl, arr1.toColl, arr2.toColl, arr3.toColl)
      res1 shouldBe res1Coll.toArray

      val res2 = cloned
      xorU(res2, Seq(arr1, arr2, arr3))

      res1 shouldBe res2
      printDebug(arr.length)
    }
  }
}
