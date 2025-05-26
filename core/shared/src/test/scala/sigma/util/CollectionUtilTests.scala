package sigma.util

import sigma.BaseTests

import scala.collection.{Seq, mutable}
import scala.reflect.ClassTag

class CollectionUtilTests extends BaseTests {
  import sigma.util.CollectionUtil._

  test("updateMany") {
    val xs: Seq[Byte] = Array[Byte](1,2,3)
    xs.updateMany(Seq.empty) shouldBe xs
    xs.updateMany(Seq(0 -> 2)) shouldBe Seq(2, 2, 3)
    xs.updateMany(Seq(0 -> 2, 2 -> 2)) shouldBe Seq(2, 2, 2)
    an[IndexOutOfBoundsException] should be thrownBy {
      xs.updateMany(Seq(3 -> 2))
    }
  }

  test("concatArrays") {
    val xs = Array[Byte](1,2,3)
    val ys = Array[Byte](4,5,6)
    val zs = concatArrays_v4(xs, ys)
    assertResult(Array[Byte](1, 2, 3, 4, 5, 6))(zs)

    val pairs = xs.zip(ys)
    // this reproduces the problem which takes place in v3.x, v4.x (ErgoTree v0, v1)
    an[Throwable] should be thrownBy(concatArrays_v4(pairs, pairs))

    // and this is the fix in v5.0
    concatArrays(pairs, pairs) shouldBe Array((1, 4), (2, 5), (3, 6), (1, 4), (2, 5), (3, 6))

    val xOpts = xs.map(Option(_))
    concatArrays(xOpts, xOpts) shouldBe Array(Some(1), Some(2), Some(3), Some(1), Some(2), Some(3))
  }

  def joinSeqs(l: Seq[Int], r: Seq[Int]) =
    outerJoinSeqs(l, r)(l => l, r => r)((_,l) => l, (_,r) => r, (_,l,r) => l + r).map(_._2)
  def joinPairs(l: Seq[(String,Int)], r: Seq[(String,Int)]) =
    outerJoinSeqs(l, r)(l => l._1, r => r._1)((_,l) => l._2, (_,r) => r._2, (_,l,r) => l._2 + r._2)

  test("joinSeqs") {
    def key(p : (Int, String)): Int = p._1

    {
      val res = CollectionUtil.joinSeqs(
        outer = Seq(1 -> "o1", 1 -> "o1"),
        inner = Seq(1 -> "i1", 2 -> "i2"))(key, key)
      res shouldBe Seq(
        (1 -> "o1") -> (1 -> "i1"),
        (1 -> "o1") -> (1 -> "i1")
      )
    }

    { // same as above, but swapping inner and outer
      val res = CollectionUtil.joinSeqs(
        outer = Seq(1 -> "o1", 2 -> "o2"),
        inner = Seq(1 -> "i1", 1 -> "i1"))(key, key)
      res shouldBe Seq(
        (1 -> "o1") -> (1 -> "i1"),
        (1 -> "o1") -> (1 -> "i1")
      )
    }
  }

  test("outerJoinSeqs") {
    val left = Seq(1, 2, 3)
    val right = Seq(2, 3, 4)

    assertResult(Seq(1, 4, 6, 4))(joinSeqs(left, right))
    assertResult(Seq(1, 2, 3))(joinSeqs(left,Seq()))
    assertResult(Seq(2, 3, 4))(joinSeqs(Seq(), right))
    assertResult(Seq(4, 6, 8))(joinSeqs(right, right))

    val inner = Seq("a" -> 1, "b" -> 2, "c" -> 3)
    val outer = Seq("b" -> 2, "c" -> 3, "d" -> 4)

    assertResult(Seq("a" -> 1, "b" -> 4, "c" -> 6, "d" -> 4))(joinPairs(inner, outer))
    assertResult(Seq("a" -> 1, "b" -> 2, "c" -> 3))(joinPairs(inner,Seq()))
    assertResult(Seq("b" -> 2, "c" -> 3, "d" -> 4))(joinPairs(Seq(), outer))
    assertResult(Seq("b" -> 4, "c" -> 6, "d" -> 8))(joinPairs(outer, outer))
  }

  val items: Iterable[(Int, String)] = Array((1, "a"), (2, "b"), (1, "c"))

  test("distinctBy") {
    val res = items.distinctBy(_._1)
    assertResult(Array((1, "a"), (2, "b")))(res)
  }

  def treeStep(tree: Array[List[Int]]): Int => List[Int] = i => tree(i)

  test("traverseDepthFirst") {
    {
      val tree = Array(
        List(1, 2), // 0
        List(),     // 1
        List(3),    // 2
        List())     // 3
      assertResult(List(0, 1, 2, 3))(0.traverseDepthFirst(treeStep(tree)))
    }
    {
      /*
       0
         1
           3
             5
             6
         2
           4
      */
      val tree = Array(
        List(1, 2),  // 0
        List(3),     // 1
        List(4),     // 2
        List(5,6),   // 3
        List(),      // 4
        List(),      // 5
        List()       // 6
      )
      assertResult(List(0, 1, 3, 5, 6, 2, 4))(0.traverseDepthFirst(treeStep(tree)))
    }
  }

  test("sameElements2") {
    Seq(1, 2).sameElementsNested(List(1, 2)) shouldBe true
    new mutable.WrappedArray.ofInt(Array(1, 2)).sameElementsNested(Vector(1, 2)) shouldBe true
    Seq(new mutable.WrappedArray.ofInt(Array(1, 2)), 3).sameElementsNested(Array(Vector(1, 2), 3)) shouldBe true
    Seq(Array(1, 2), 3).sameElementsNested(Array(Vector(1, 2), 3)) shouldBe true
    Seq(Array(1, 2), Option(3)).sameElementsNested(Array(Vector(1, 2), List(3))) shouldBe false

    Seq(1, 2).sameElementsNested(List(1, 2, 3)) shouldBe false
    new mutable.WrappedArray.ofInt(Array(1, 2, 3)).sameElementsNested(Vector(1, 2)) shouldBe false
    Seq(new mutable.WrappedArray.ofInt(Array(1, 2, 3)), 3).sameElementsNested(Array(Vector(1, 2), 3)) shouldBe false

  }

  def unboxedArray[T:ClassTag](in: Seq[T]): Array[T] = {
    in.toArray[T]
  }

  test("unboxedArray") {
    // empty list
    unboxedArray(Seq[Any]()).isInstanceOf[Array[Any]] shouldBe true

    // primitive types
    unboxedArray(Seq[Byte](java.lang.Byte.valueOf(1.toByte))).isInstanceOf[Array[Byte]] shouldBe true
    Seq[Any](java.lang.Byte.valueOf(1.toByte)).toArray.isInstanceOf[Array[Byte]] shouldBe false
    Seq[Byte](1.toByte).toArray[Byte].isInstanceOf[Array[Byte]] shouldBe true

    unboxedArray(Seq[Short](java.lang.Short.valueOf(1.toShort))).isInstanceOf[Array[Short]] shouldBe true
    unboxedArray(Seq[Int](java.lang.Integer.valueOf(1))).isInstanceOf[Array[Int]] shouldBe true
    unboxedArray(Seq[Long](java.lang.Long.valueOf(1))).isInstanceOf[Array[Long]] shouldBe true
    unboxedArray(Seq[Double](java.lang.Double.valueOf(1.0))).isInstanceOf[Array[Double]] shouldBe true
    unboxedArray(Seq[Float](java.lang.Float.valueOf(1.0f))).isInstanceOf[Array[Float]] shouldBe true
    unboxedArray(Seq[Boolean](java.lang.Boolean.valueOf(true))).isInstanceOf[Array[Boolean]] shouldBe true
    unboxedArray(Seq[Char](java.lang.Character.valueOf('a'))).isInstanceOf[Array[Char]] shouldBe true
    unboxedArray(Seq[String]("str")).isInstanceOf[Array[String]] shouldBe true

    // non-primitive type
    unboxedArray(Seq[Any](Option.empty[Boolean])).isInstanceOf[Array[Any]] shouldBe true
    unboxedArray(Seq[Seq[Any]](Seq())).isInstanceOf[Array[Seq[Any]]] shouldBe true
  }

}
