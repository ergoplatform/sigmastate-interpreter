package sigmastate

import org.scalacheck.Gen
import sigma.ast.{EvaluatedValue, IntConstant, SType}
import sigma.interpreter.SigmaMap
import sigmastate.helpers.TestingCommons

import scala.util.Random

class SigmaMapSpecification extends TestingCommons {

  property("SigmaMap.empty") {
    val empty = SigmaMap.empty
    empty.size shouldBe 0
    empty.iterator.toSeq.isEmpty shouldBe true
  }

  property("SigmaMap.get") {
    val id = 1.toByte
    val value = IntConstant(1)
    val map = Map(id -> value)
    val empty = SigmaMap(map)
    empty.size shouldBe map.size
    empty.maxKey shouldBe 1
    empty.iterator.toSeq.toMap shouldBe map
  }

  property("traversal - vector - 0 to 4") {
      val sm = SigmaMap(Map(
        0.toByte -> IntConstant(0),
        1.toByte -> IntConstant(1),
        2.toByte -> IntConstant(2),
        3.toByte -> IntConstant(3),
        4.toByte -> IntConstant(4)
      ))

    sm.maxKey shouldBe 4
    sm.size shouldBe 5
    sm.iterator.toList.map(_._1) shouldBe Array[Byte](0, 1, 2, 3, 4)
  }

  property("traversal - vector - [73,35,31]") {
    val m = Map(
      31.toByte -> IntConstant(2),
      73.toByte -> IntConstant(0),
      35.toByte -> IntConstant(1)
    )

    val sm = SigmaMap(m)
    val ml = m.iterator.toList
    val sml = sm.iterator.toList

    println("ml: " + ml)
    println("sml: " + sml)

    ml shouldBe sml
  }

  property("traversal - single") {
    val id = 106.toByte

    val sm = SigmaMap(Map(
      id -> IntConstant(0)
    ))

    sm.maxKey shouldBe id
    sm.size shouldBe 1
    sm.iterator.toList.map(_._1) shouldBe Array[Byte](id)
  }

  property("traversal equivalence") {
    val fullArr = (0 to Byte.MaxValue).map { b => b.toByte -> IntConstant(b) }
    forAll(Gen.chooseNum(0, Byte.MaxValue)) { n =>
      val rnd = Random.shuffle(fullArr).take(n)
      val oldRepr: scala.collection.Map[Byte, EvaluatedValue[_ <: SType]] = rnd.toMap
      val sm = SigmaMap(oldRepr)

      oldRepr.iterator.toList shouldBe sm.iterator.toList
    }
  }

}
