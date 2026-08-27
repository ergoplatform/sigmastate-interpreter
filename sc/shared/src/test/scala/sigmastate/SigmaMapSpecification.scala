package sigmastate

import sigma.ast.{EvaluatedValue, IntConstant, SType}
import sigma.interpreter.SigmaMap
import sigmastate.helpers.TestingCommons

class SigmaMapSpecification extends TestingCommons {

  private def v(b: Int): EvaluatedValue[_ <: SType] = IntConstant(b)

  private def mk(keys: Byte*): SigmaMap =
    SigmaMap(keys.toArray, keys.toArray.map(k => v(k.toInt)))

  property("SigmaMap.empty") {
    val empty = SigmaMap.empty
    empty.size shouldBe 0
    empty.iterator.toSeq.isEmpty shouldBe true
    empty.isEmpty shouldBe true
    empty.maxKey shouldBe -1
    empty.contains(0.toByte) shouldBe false
    empty.get(0.toByte) shouldBe None
    empty.getNullable(0.toByte) shouldBe null
  }

  property("SigmaMap.get") {
    val id = 1.toByte
    val value = IntConstant(1)
    val map = Map(id -> value)
    val sm = SigmaMap(map)
    sm.size shouldBe map.size
    sm.maxKey shouldBe 1
    sm.iterator.toSeq.toMap shouldBe map
    sm.get(id) shouldBe Some(value)
  }

  property("get returns None (not null) for absent keys") {
    val m1 = mk(5)
    m1.get(6) shouldBe None
    m1.get(-1) shouldBe None

    val m4 = mk(1, 2, 3, 4)
    m4.get(5) shouldBe None
    m4.get(-1) shouldBe None
    m4.get(Byte.MaxValue) shouldBe None

    val multi = mk((0 to 10).map(_.toByte): _*)
    multi.get(11) shouldBe None
    multi.get(-1) shouldBe None
    multi.get(Byte.MaxValue) shouldBe None
  }

  property("contains and getNullable for absent keys") {
    val multi = mk((0 to 10).map(_.toByte): _*)
    multi.contains(11) shouldBe false
    multi.contains(-1) shouldBe false
    multi.getNullable(11) shouldBe null
    multi.getNullable(-1) shouldBe null

    val m2 = mk(3, 8)
    m2.contains(4) shouldBe false
    m2.getNullable(4) shouldBe null
  }

  property("maxKey and size") {
    mk(7).maxKey shouldBe 7
    mk(3, 9).maxKey shouldBe 9
    mk(9, 3).maxKey shouldBe 9
    mk(1, 2, 3).maxKey shouldBe 3
    mk(4, 3, 2, 1).maxKey shouldBe 4
    mk(10, 0, 100, 50, 20, 30).maxKey shouldBe 100
  }

  property("duplicate keys are reduced to first position with last value") {
    val sm = SigmaMap(Array[Byte](5, 7, 5), Array[EvaluatedValue[_ <: SType]](v(50), v(70), v(55)))
    sm.size shouldBe 2
    sm.iterator.toList shouldBe List((5.toByte, v(55)), (7.toByte, v(70)))
    sm.get(5) shouldBe Some(v(55))
  }

  property("negative keys are rejected") {
    an[IllegalArgumentException] should be thrownBy
      SigmaMap(Array[Byte](-1), Array[EvaluatedValue[_ <: SType]](v(1)))
  }

  property("arrays of different lengths are rejected") {
    a[IllegalArgumentException] should be thrownBy
      SigmaMap(Array[Byte](1, 2), Array[EvaluatedValue[_ <: SType]](v(1)))
  }

  property("iterator exhaustion throws NoSuchElementException") {
    val multi = mk(10, 20, 30, 40, 50)
    val it = multi.iterator
    it.hasNext shouldBe true
    while (it.hasNext) it.next()
    it.hasNext shouldBe false
    a[NoSuchElementException] should be thrownBy it.next()

    val m1 = mk(1)
    val it1 = m1.iterator
    it1.next()
    it1.hasNext shouldBe false
    a[NoSuchElementException] should be thrownBy it1.next()
  }

  property("traversal - single") {
    val id = 106.toByte
    val sm = mk(id)
    sm.maxKey shouldBe id
    sm.size shouldBe 1
    sm.iterator.toList.map(_._1) shouldBe Seq(id)
  }

  property("empty constructions return the singleton EmptySigmaMap") {
    SigmaMap(Map.empty[Byte, EvaluatedValue[_ <: SType]]) shouldBe theSameInstanceAs(SigmaMap.empty)
    SigmaMap(Array.empty[Byte], Array.empty[EvaluatedValue[_ <: SType]]) shouldBe theSameInstanceAs(SigmaMap.empty)
  }

  property("exactly 4 entries are traversed in insertion order") {
    val insertion = Array[Byte](73, 35, 31, 0)
    val sm = SigmaMap(insertion, valuesFor(insertion))
    sm.size shouldBe 4
    sm.iterator.toList.map(_._1) shouldBe insertion.toList
  }

  property("exactly 5 entries are traversed in hash-trie order, not insertion order") {
    val insertion = Array[Byte](10, 20, 30, 40, 50)
    val sm = SigmaMap(insertion, valuesFor(insertion))
    sm.size shouldBe 5
    // recorded under Scala 2.12.20; see SigmaMap.indices
    sm.iterator.toList.map(_._1) shouldBe Seq[Byte](10, 20, 50, 40, 30)
  }

  property("apply(scala.collection.Map) with duplicate keys keeps first position and last value") {
    val map = scala.collection.immutable.Map[Byte, EvaluatedValue[_ <: SType]](
      5.toByte -> v(50), 7.toByte -> v(70), 5.toByte -> v(55)
    )
    val sm = SigmaMap(map)
    sm.size shouldBe 2
    sm.iterator.toList shouldBe List((5.toByte, v(55)), (7.toByte, v(70)))
    sm.get(5) shouldBe Some(v(55))
  }

  property("apply(scala.collection.Map) with negative key is rejected") {
    val map = scala.collection.immutable.Map[Byte, EvaluatedValue[_ <: SType]](
      (-1).toByte -> v(1)
    )
    an[IllegalArgumentException] should be thrownBy SigmaMap(map)
  }

  property("apply(keys, values) rejects all-negative keys") {
    an[IllegalArgumentException] should be thrownBy
      SigmaMap(Array[Byte](-1, -2, -3), Array[EvaluatedValue[_ <: SType]](v(1), v(2), v(3)))
  }

  property("anyIterator yields the same order with AnyValue payloads") {
    val keys = Array[Byte](10, 20, 30, 40, 50)
    val sm = SigmaMap(keys, valuesFor(keys))
    val any = sm.anyIterator.toList
    val normal = sm.iterator.toList
    any.map(_._1) shouldBe normal.map(_._1)
    any.map(_._2.asInstanceOf[sigma.data.CAnyValue[_]].value) shouldBe
      normal.map(_._2.value)
  }

  property("sparse max key in SigmaMapMulti is handled correctly") {
    val sm = mk(127)
    sm.maxKey shouldBe 127
    sm.size shouldBe 1
    sm.contains(127.toByte) shouldBe true
    sm.contains(126.toByte) shouldBe false
    sm.get(127.toByte) shouldBe Some(v(127))
    sm.getNullable(127.toByte) should not be null
    sm.getNullable(126.toByte) shouldBe null
  }

  private def valuesFor(keys: Array[Byte]): Array[EvaluatedValue[_ <: SType]] =
    keys.map(k => v(k.toInt))
}
