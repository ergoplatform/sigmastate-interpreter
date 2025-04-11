package org.ergoplatform.sdk

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sigma.{Coll, CollGens}
import org.ergoplatform.sdk.Extensions.{CollBuilderOps, CollOps, PairCollOps}
import sigma.Extensions.ArrayOps
import sigma.data.{CSigmaDslBuilder, RType}

class ExtensionsSpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers with CollGens {
  def Coll[T](items: T*)(implicit cT: RType[T]) =
    CSigmaDslBuilder.Colls.fromItems(items: _*)

  val items: Iterable[(Int, String)] = Array((1, "a"), (2, "b"), (1, "c"))


  property("Coll.partition") {
    forAll(collGen) { col: Coll[Int] =>
      val (lsC, rsC) = col.partition(lt0)
      val (ls, rs) = col.toArray.partition(lt0)
      lsC.toArray shouldBe ls
      rsC.toArray shouldBe rs
    }
  }

  property("Coll.mapReduce") {
    def m(x: Int): (Int, Int) = (math.abs(x) % 10, x)

    forAll(collGen) { col =>
      val res = col.mapReduce(m, plusF)
      val (ks, vs) = builder.unzip(res)
      vs.toArray.sum shouldBe col.toArray.sum
      ks.length <= 10 shouldBe true
      res.toArray shouldBe col.toArray.toColl.mapReduce[Int, Int](m, plusF).toArray
    }
  }

  property("Coll.groupBy") {
    def key(x: Int) = math.abs(x) % 10

    forAll(collGen) { col =>
      val res = col.groupBy(key)
      val (ks, vs) = builder.unzip(res)
      vs.flatMap(identity).toArray.sum shouldBe col.toArray.sum
      ks.length <= 10 shouldBe true
      val pairs = col.map(x => (key(x), x))
      val res2 = pairs.groupByKey
      val (ks2, vs2) = builder.unzip(res)
      ks shouldBe ks2
      vs shouldBe vs2
    }
  }

  property("PairColl.mapFirst") {
    val minSuccess = minSuccessful(30)
    forAll(collGen, minSuccess) { col =>
      val pairs = col.zip(col)
      pairs.mapFirst(inc).toArray shouldBe pairs.toArray.map { case (x, y) => (inc(x), y) }
      pairs.mapSecond(inc).toArray shouldBe pairs.toArray.map { case (x, y) => (x, inc(y)) }
    }
  }

  property("PairColl.sumByKey") {
    val table = Table(("in", "out"),
      (Coll[(String, Int)](), Coll[(String, Int)]()),
      (Coll("a" -> 1), Coll("a" -> 1)),
      (Coll("a" -> 1, "a" -> 1), Coll("a" -> 2)),
      (Coll("a" -> 1, "b" -> 1, "a" -> 1), Coll("a" -> 2, "b" -> 1)),
      (Coll("b" -> 1, "a" -> 1, "b" -> 1, "a" -> 1), Coll("b" -> 2, "a" -> 2))
    )
    forAll(table) { (in, out) =>
      in.sumByKey shouldBe out
    }
  }

  property("CollBuilder.outerJoin") {
    def test(col: Coll[Int]) = {
      val inner = col.indices
      val rightOnly = inner.map(i => i + col.length)
      val leftOnly = rightOnly.map(i => -i)
      val leftKeys = inner.append(leftOnly)
      val leftValues = col.append(col.map(x => x + 2))
      val rightKeys = inner.append(rightOnly)
      val rightValues = col.append(col.map(x => x + 3))
      val left = builder.pairColl(leftKeys, leftValues)
      val right = builder.pairColl(rightKeys, rightValues)
      val res = builder.outerJoin(left, right)(l => l._2 - 2, r => r._2 - 3, i => i._2._1 + 5)
      val (_, vs) = builder.unzip(res)
      vs.sum shouldBe (col.sum * 2 + col.map(_ + 5).sum)
    }
    //    test(builder.fromItems(0))
    //    val gen = containerOfN[Array, Int](100, choose(20, 100))
    //        .map(xs => builder.fromArray(xs.distinct))
    forAll(collGen) { col =>
      test(col)
    }
  }

}
