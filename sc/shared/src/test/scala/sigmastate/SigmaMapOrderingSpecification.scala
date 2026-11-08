package sigmastate

import sigma.ast.{EvaluatedValue, IntConstant, SType}
import sigma.interpreter.SigmaMap
import sigmastate.helpers.TestingCommons

import scala.util.Random

/** Platform-independent properties of the [[SigmaMap]] traversal ordering contract
  * (runs on JVM and JS legs):
  *  - subset-restriction property of the hash-trie order for maps with more than 4 entries;
  *  - order-independence of equality and hashCode;
  *  - a deterministic smoke sample recorded from the Scala 2.12 SDK.
  * The full dataset-driven conformance tests (16384 golden vectors) run on the JVM leg in
  * [[sigmastate.SigmaMapGoldenVectorsSpecification]] and
  * [[sigmastate.SigmaMapLiveDifferentialSpecification]].
  */
class SigmaMapOrderingSpecification extends TestingCommons {

  private def valuesFor(keys: Array[Byte]): Array[EvaluatedValue[_ <: SType]] = {
    val vs = new Array[EvaluatedValue[_ <: SType]](keys.length)
    var i = 0
    while (i < keys.length) {
      vs(i) = IntConstant(keys(i).toInt)
      i += 1
    }
    vs
  }

  private def keysOf(m: SigmaMap): Seq[Byte] = m.iterator.map(_._1).toList

  property("subset restriction property of hash-trie order (maps with more than 4 entries)") {
    val rng = new Random(42)
    val allKeys = (0 to Byte.MaxValue).map(_.toByte).toArray
    for (_ <- 0 until 3000) {
      val n = 5 + rng.nextInt(Byte.MaxValue - 4) // 5..127
      val subset = rng.shuffle(allKeys.toVector).take(n).toArray
      val shuffledInsertion = rng.shuffle(subset.toVector).toArray
      val sm = SigmaMap(shuffledInsertion, valuesFor(shuffledInsertion))
      val expected = SigmaMap.indices.filter(subset.contains)
      keysOf(sm) shouldBe expected.toList
    }
  }

  property("equals and hashCode are independent of construction order") {
    val rng = new Random(13)
    val allKeys = (0 to Byte.MaxValue).map(_.toByte).toArray
    for (_ <- 0 until 500) {
      val n = 1 + rng.nextInt(Byte.MaxValue)
      val keys = rng.shuffle(allKeys.toVector).take(n).toArray
      val m1 = SigmaMap(keys, valuesFor(keys))
      val rev = keys.reverse
      val m2 = SigmaMap(rev, valuesFor(rev))
      m1 shouldBe m2
      m1.hashCode() shouldBe m2.hashCode()
    }
  }

  property("smoke vectors recorded from Scala 2.12.20") {
    // insertion-order regime (size <= 4): traversal follows insertion sequence
    keysOf(SigmaMap(Array[Byte](73, 35, 31), valuesFor(Array[Byte](73, 35, 31)))) shouldBe
      Seq[Byte](73, 35, 31)
    keysOf(SigmaMap(Array[Byte](31, 73, 35), valuesFor(Array[Byte](31, 73, 35)))) shouldBe
      Seq[Byte](31, 73, 35)

    // hash-trie regime (size > 4): traversal is a function of the key set alone,
    // regardless of insertion order; sample recorded under Scala 2.12.20 and consistent
    // with filter(fullOrder128) from the committed golden dataset
    val ks6 = Array[Byte](10, 20, 30, 40, 50, 60)
    keysOf(SigmaMap(ks6, valuesFor(ks6))) shouldBe Seq[Byte](10, 20, 60, 50, 40, 30)
    val ks14 = Array[Byte](69, 61, 117, 9, 27, 3, 43, 23, 75, 30, 90, 63, 18, 95)
    keysOf(SigmaMap(ks14, valuesFor(ks14))) shouldBe
      Seq[Byte](69, 61, 117, 9, 27, 3, 63, 18, 95, 43, 23, 75, 30, 90)
  }
}
