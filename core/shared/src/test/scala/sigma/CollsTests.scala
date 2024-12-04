package sigma

import org.scalacheck.Gen
import org.scalatest.exceptions.TestFailedException
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sigma.data._

import scala.language.existentials

class CollsTests extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers with CollGens with VersionTestingProperty { testSuite =>
  import Gen._
  import sigma.Extensions._

  def squared[A](f: A => A): ((A, A)) => (A, A) = (p: (A, A)) => (f(p._1), f(p._2))

  property("Coll.indices") {
    val minSuccess = MinSuccessful(30)
    forAll(collGen, collGen, minSuccess) { (col1: Coll[Int], _: Coll[Int]) =>
      col1.indices.toArray shouldBe col1.toArray.indices.toArray
    }
    forAll(superGen, minSuccess) { cl =>
      cl.indices.toArray shouldBe cl.toArray.indices.toArray
    }
  }

  property("Coll.length") {
    def equalLength[A: RType](xs: Coll[A]) = {
      val arr = xs.toArray
      xs.length shouldBe arr.length
      xs.zip(xs).length shouldBe arr.zip(arr).length
      xs.zip(xs.append(xs)).length shouldBe arr.zip(arr ++ arr).length
      xs.append(xs).zip(xs).length shouldBe (arr ++ arr).zip(arr).length
    }

    def equalLengthMapped[A: RType](xs: Coll[A], f: A => A) = {
      val arr = xs.toArray
      val ys = xs.map(f)
      ys.length shouldBe xs.length
      ys.length shouldBe arr.map(f).length

      equalLength(ys)
      equalLength(xs.append(ys))
      equalLength(ys.append(xs))
    }

    // make sure forall: T, col: Coll[T] => col.length shouldBe col.toArray.length
    // The above equality should hold for all possible collection instances

    forAll(MinSuccessful(300)) { xs: Coll[Int] =>
      equalLength(xs)
      equalLengthMapped(xs, inc)

      equalLength(xs.append(xs))
      equalLengthMapped(xs.append(xs), inc)

      val pairs = xs.zip(xs)
      equalLength(pairs)

      if (xs.nonEmpty && !VersionContext.current.isJitActivated) {
        an[Throwable] should be thrownBy {
          equalLengthMapped(pairs, squared(inc))  // due to problem with append
        }
      }
      VersionContext.withVersions(VersionContext.JitActivationVersion, VersionContext.JitActivationVersion) {
// TODO v5.0: make it work
//        equalLengthMapped(pairs, squared(inc))  // problem fixed in v5.0
      }

      equalLength(pairs.append(pairs))

      if (xs.nonEmpty && !VersionContext.current.isJitActivated) {
        an[Throwable] should be thrownBy {
          equalLengthMapped(pairs.append(pairs), squared(inc)) // due to problem with append
        }
      }
      VersionContext.withVersions(VersionContext.JitActivationVersion, VersionContext.JitActivationVersion) {
// TODO v5.0: make it work
//        equalLengthMapped(pairs.append(pairs), squared(inc)) // problem fixed in v5.0
      }
    }
  }

  property("Coll.flatMap") {
    implicit val t: Coll[Int] => Traversable[Int] = traversableColl
    forAll(containerOfN[Coll, Int](3, valGen), collGen) { (zs, col) =>
      val matrix = zs.map(_ => col)
      val res = zs.zip(matrix).flatMap(_._2)
      res.toArray shouldBe zs.toArray.flatMap(_ => col.toArray)
    }
  }

  property("Coll.segmentLength") {
    forAll(collGen, indexGen) { (col, from) =>
      col.segmentLength(lt0, from) shouldBe col.toArray.segmentLength(lt0, from)
    }
  }

  property("Coll.indexWhere") {
    forAll(collGen, indexGen) { (col, from) =>
      col.indexWhere(eq0, from) shouldBe col.toArray.indexWhere(eq0, from)
      def p2(ab: (Int, Int)) = eq0(ab._1) && eq0(ab._2)
      col.zip(col).indexWhere(p2, from) shouldBe col.toArray.zip(col.toArray).indexWhere(p2, from)
    }
  }

  property("Coll.indexOf") {
    forAll(collGen, indexGen, valGen) { (col, from, elem) =>
      col.indexOf(elem, from) shouldBe col.toArray.indexOf(elem, from)
      col.zip(col).indexOf((elem, elem), from) shouldBe col.toArray.zip(col.toArray).indexOf((elem, elem), from)
    }
  }

  property("Coll.lastIndexWhere") {
    forAll(collGen, indexGen) { (col, end) =>
      col.lastIndexWhere(eq0, end) shouldBe col.lastIndexWhere(eq0, end)
      def p2(ab: (Int, Int)) = eq0(ab._1) && eq0(ab._2)
      col.zip(col).lastIndexWhere(p2, end) shouldBe col.toArray.zip(col.toArray).lastIndexWhere(p2, end)
    }
  }

  property("Coll.patch") {
    forAll(collGen, choose(-100, 100), collGen, replacedGen) { (col, from, patch, replaced) =>
      whenever(col.isValidIndex(from)) {
        val patchedC = col.patch(from, patch, replaced)
        val patched = col.toArray.patch(from, patch.toArray, replaced)
        patchedC.toArray shouldBe patched
      }
    }
  }

  property("Coll.updated") {
    forAll(collGen, indexGen, valGen, MinSuccessful(200)) { (col, index, elem) =>
      whenever(col.isValidIndex(index)) {
        val patchedC = col.updated(index, elem)
        val patched = col.toArray.updated(index, elem)
        patchedC.toArray shouldBe patched
      }
      whenever(col.isInstanceOf[CollOverArray[_]]) {
        an[IndexOutOfBoundsException] should be thrownBy {
          col.updated(col.length, elem)
        }
        an[IndexOutOfBoundsException] should be thrownBy {
          col.updated(-1, elem)
        }
      }
    }
  }

  property("Coll.updateMany") {
    forAll(collGen, indexesGen, MinSuccessful(200)) { (col, indexes) =>
      whenever(indexes.forall(col.isValidIndex(_))) {
        val updatedC = col.updateMany(indexes, indexes)
        val updated = col.toArray.clone()
        for (i <- indexes)
          updated.update(i, i)
        updatedC.toArray shouldBe updated
      }
      whenever(col.isInstanceOf[CollOverArray[_]]) {
        an[IndexOutOfBoundsException] should be thrownBy {
          col.updateMany(builder.fromItems(col.length), builder.fromItems(0))
        }
        an[IndexOutOfBoundsException] should be thrownBy {
          col.updateMany(builder.fromItems(-1), builder.fromItems(0))
        }
      }
    }
  }

  property("Coll methods") {
    forAll(collGen, indexGen) { (col, index) =>
      {
        val res = col.map(inc)
        res.toArray shouldBe col.toArray.map(inc)
        val pairs = col.zip(col)
        pairs.map(plusF).toArray shouldBe pairs.toArray.map(plusF)
      }
      {
        val res = col.filter(lt0)
        res.toArray shouldBe col.toArray.filter(lt0)
      }
      {
        val res = col.forall(lt0)
        val emptyRepl = builder.replicate(0, 10)
        val repl = builder.replicate(col.length, 10)
        res shouldBe col.toArray.forall(lt0)
        emptyRepl.forall(lt0) shouldBe Array[Int]().forall(lt0)
        col.zip(repl).forall(predF) shouldBe col.zip(repl).toArray.forall(predF)
      }
      {
        val res = col.exists(lt0)
        res shouldBe col.toArray.exists(lt0)
        builder.replicate(0, -10).exists(lt0) shouldBe Array[Int]().exists(lt0)
      }
      {
        val res = col.foldLeft[Int](0, plusF)
        res shouldBe col.toArray.foldLeft(0)(plus)
        val pairs = col.zip(col)
        val op = (in: (Int,(Int,Int))) => in._1 + in._2._1 + in._2._2
        pairs.foldLeft(0, op) shouldBe pairs.toArray.foldLeft(0)((b,a) => op((b,a)))
      }
      whenever(col.isValidIndex(index)) {
        val res = col(index)
        res shouldBe col.toArray(index)

        val res2 = col.getOrElse(index, index)
        res2 shouldBe col.toArray(index)
      }
      
      col.getOrElse(col.length, index) shouldBe index
      col.getOrElse(-1, index) shouldBe index
    }
    forAll(superGen, indexGen) { (col, index) =>
      whenever(col.isValidIndex(index)) {
        val res = col(index)
        res shouldBe col.toArray(index)
      }
    }
  }

  property("Coll.slice") {
    forAll(collGen, indexGen, indexGen) { (col, from, until) =>
      whenever(col.isValidIndex(until)) {
        val res = col.slice(from, until)
        res.toArray shouldBe col.toArray.slice(from, until)
      }
    }

    forAll(superGen, indexGen, indexGen) { (col, from, until) =>
      whenever(col.isValidIndex(until)) {
        val res = col.slice(from, until)
        res.toArray shouldBe col.toArray.slice(from, until)
      }
    }
  }

  property("Coll.append") {

    { // this shows how the problem with CollectionUtil.concatArrays manifests itself in Coll.append (v4.x)
      val xs = builder.fromItems(1, 2)
      val pairs = xs.zip(xs)
      val ys = pairs.map(squared(inc)) // this map transforms PairOfCols to CollOverArray

      if (VersionContext.current.isJitActivated) {
        // problem fixed in v5.0
        ys.append(ys).toArray shouldBe ys.toArray ++ ys.toArray
      } else {
        // due to the problem with CollectionUtil.concatArrays
        an[Throwable] should be thrownBy (ys.append(ys))
      }
    }

    {
      val col1 = builder.fromItems(1, 2, 3)
      val col2 = builder.fromItems(10, 20, 30, 40)
      val pairs = col1.zip(col2)
      val pairsSwap = col2.zip(col1)
      assert(pairs.isInstanceOf[PairOfCols[_,_]])

      val pairsArr = pairs.toArray
      pairsArr shouldBe Array((1, 10), (2, 20), (3, 30))

      val pairsArrSwap = pairsSwap.toArray
      pairsArr shouldBe Array((1, 10), (2, 20), (3, 30))
      pairsArrSwap shouldBe Array((10, 1), (20, 2), (30, 3))

      val appended = pairs.append(pairs)
      val appendedSwap = pairsSwap.append(pairsSwap)

      // here is the problem with append which is fixed in v5.0
      if (VersionContext.current.isJitActivated) {
        // the issue is fixed starting from v5.0
        appended.toArray shouldBe (pairsArr ++ pairsArr)
        appended.toArray shouldBe Array((1, 10), (2, 20), (3, 30), (1, 10), (2, 20), (3, 30))
        appendedSwap.toArray shouldBe (pairsArrSwap ++ pairsArrSwap)
        appendedSwap.toArray shouldBe Array((10, 1), (20, 2), (30, 3), (10, 1), (20, 2), (30, 3))
      } else {
        // Append should fail for ErgoTree v0, v1 regardless of the activated version
        an[TestFailedException] should be thrownBy(
          appended.toArray shouldBe (pairsArr ++ pairsArr)
        )
        an[TestFailedException] should be thrownBy(
          appendedSwap.toArray shouldBe (pairsArrSwap ++ pairsArrSwap)
        )
        // Note, the last element of col2 (40) is zipped with 1
        // this is because PairOfCols keeps ls and rs separated and zip doesn't do truncation
        // the they are of different length
        appended.toArray shouldBe Array((1, 10), (2, 20), (3, 30), (1, 40), (2, 10), (3, 20))
        appendedSwap.toArray shouldBe Array((10, 1), (20, 2), (30, 3), (40, 1), (10, 2), (20, 3))
      }
    }

    forAll(collGen, collGen, valGen, MinSuccessful(50)) { (col1, col2, v) =>

      {
        val res = col1.append(col2)
        res.toArray shouldBe (col1.toArray ++ col2.toArray)
        val pairs1 = col1.zip(col1)
        val pairs2 = col2.zip(col2)
        val apairs = pairs1.append(pairs2)
        apairs.toArray shouldBe (pairs1.toArray ++ pairs2.toArray)
      }

      {
        val repl1 = builder.replicate(col1.length, v)
        val repl2 = builder.replicate(col2.length, v)
        assert(repl1.isInstanceOf[CollOverArray[Int]])

        val arepl = repl1.append(repl2)
        assert(arepl.isInstanceOf[CollOverArray[Int]])
        arepl.toArray shouldBe (repl1.toArray ++ repl2.toArray)
        
        val pairs1 = repl1.zip(repl1)
        assert(pairs1.isInstanceOf[PairOfCols[Int, Int]])

        val pairs2 = repl2.zip(repl2)
        val apairs = pairs1.append(pairs2)
        assert(apairs.isInstanceOf[PairOfCols[Int, Int]])
        apairs.toArray shouldBe (pairs1.toArray ++ pairs2.toArray)

        apairs match {
          case ps: PairOfCols[_,_] =>
            assert(ps.ls.isInstanceOf[CollOverArray[Int]])
            assert(ps.rs.isInstanceOf[CollOverArray[Int]])
          case _ =>
            assert(false, "Invalid type")
        }
      }
    }
  }

  property("Coll.zip") {
    val col1 = builder.fromItems(1, 2, 3)
    val col2 = builder.fromItems(10, 20, 30, 40)
    val pairs = col1.zip(col2)
    pairs.length shouldBe col1.length
    pairs.length shouldNot be(col2.length)
    pairs.length shouldBe col2.zip(col1).length

    val pairOfColls = pairs.asInstanceOf[PairOfCols[Int, Int]]

    // here is problem with zip
    if (VersionContext.current.isJitActivated) {
      // which is fixed in v5.0
      pairOfColls.ls.length shouldBe pairOfColls.rs.length
    } else {
      // not fixed in v4.x
      pairOfColls.ls.length should not be(pairOfColls.rs.length)
    }
  }

  property("Coll.reverse") {
    val minSuccess = minSuccessful(50)
    forAll(allGen, minSuccess) { col =>
      val res = col.reverse
      res.toArray shouldBe col.toArray.reverse
      val pairs = col.zip(col)
      pairs.reverse.toArray shouldBe pairs.toArray.reverse
    }
  }

  property("Coll.take") {
    val minSuccess = minSuccessful(50)
    forAll(allGen, minSuccess) { col =>
      val n = col.length / 2
      val res = col.take(n)
      res.toArray shouldBe col.toArray.take(n)
      val pairs = col.zip(col)
      pairs.take(n).toArray shouldBe pairs.toArray.take(n)
    }
  }

  property("Coll.distinct") {
    forAll(collGen) { col =>
      val res = col.distinct
      res.toArray shouldBe col.toArray.distinct
      val pairs = col.zip(col)
      pairs.distinct.toArray shouldBe pairs.toArray.distinct
    }
    forAll(superGen) { col =>
        val res = col.distinct
        res.toArray shouldBe col.toArray.distinct
        val pairs = col.zip(col)
        pairs.distinct.toArray shouldBe pairs.toArray.distinct
    }
  }

  property("Coll.startsWith") {
    val minSuccess = minSuccessful(50)
    forAll(collGen, minSuccess) { col =>
      val n = col.length / 2
      val prefix = col.take(n)
      val pairs = col.zip(col)
      pairs.startsWith(prefix.zip(prefix)) shouldBe true
      col.startsWith(prefix) shouldBe true
      val pairOfCols = new PairOfCols[Int, Int](col, col)
      pairOfCols.startsWith(pairOfCols.take(n)) shouldBe true
    }
  }

  property("Coll.endsWith") {
    val minSuccess = minSuccessful(50)
    forAll(collGen, minSuccess) { col =>
      val n = col.length / 2
      val suffix = col.slice(n, col.length)
      col.endsWith(suffix) shouldBe true
      val pairs = col.zip(col)
      pairs.endsWith(suffix.zip(suffix)) shouldBe true
      val pairOfCols = new PairOfCols[Int, Int](col, col)
      pairOfCols.endsWith(pairOfCols.slice(n, col.length)) shouldBe true
    }
  }

  property("Coll.equals") {
    def checkColls(repl: Coll[_], coll: Coll[_]) = {
      assert(coll == repl)
      assert(repl == coll)
      repl.hashCode() shouldBe coll.hashCode()

      val zip1 = repl.zip(repl)
      val zip2 = repl.zip(coll)
      val zip3 = coll.zip(coll)
      val zip4 = coll.zip(repl)

      assert(zip1 == zip2)
      assert(zip2 == zip3)
      assert(zip3 == zip4)
      assert(zip4 == zip1)
      zip1.hashCode() shouldBe zip2.hashCode()
      zip2.hashCode() shouldBe zip3.hashCode()
      zip3.hashCode() shouldBe zip4.hashCode()
      zip4.hashCode() shouldBe zip1.hashCode()
    }
    val minSuccess = minSuccessful(50)
    forAll(byteGen, indexGen, minSuccess) { (x, n) =>
      val repl = builder.replicate(n, x)
      val coll = builder.fromArray(Array.fill(n)(x))

      checkColls(repl, coll)
    }
    forAll(shortGen, indexGen, minSuccess) { (x, n) =>
      val repl = builder.replicate(n, x)
      val coll = builder.fromArray(Array.fill(n)(x))

      checkColls(repl, coll)
    }
    forAll(intGen, indexGen, minSuccess) { (x, n) =>
      val repl = builder.replicate(n, x)
      val coll = builder.fromArray(Array.fill(n)(x))

      checkColls(repl, coll)
    }
    forAll(longGen, indexGen, minSuccess) { (x, n) =>
      val repl = builder.replicate(n, x)
      val coll = builder.fromArray(Array.fill(n)(x))

      checkColls(repl, coll)
    }
    forAll (indexGen, minSuccess) { (n) =>
      val replTrue = builder.replicate(n, true)
      val collTrue = builder.fromArray(Array.fill(n)(true))
      val replFalse = builder.replicate(n, false)
      val collFalse = builder.fromArray(Array.fill(n)(false))

      checkColls(replTrue, collTrue)
      checkColls(replFalse, collFalse)
    }
    forAll(indexGen, minSuccess) { n =>
      val repl = builder.replicate(n, builder.fromItems(Array(1, 2, 3)))
      val coll = builder.fromArray(Array.fill(n)(builder.fromItems(Array(1, 2, 3))))

      checkColls(repl, coll)
    }
    forAll(indexGen, indexGen, minSuccess) { (n, m) =>
      val repl = builder.replicate(n, builder.replicate(m, 1))
      val coll = builder.fromArray(Array.fill(n)(builder.fromArray(Array.fill(m)(1))))

      checkColls(repl, coll)
    }
    // This tuple tests fail with previous implementation
    forAll (byteGen, doubleGen, intGen, indexGen, minSuccess) { (b, _, i, n) =>
      val repl = builder.replicate(n, (b, i))
      val coll = builder.fromArray(Array.fill[(Byte, Int)](n)((b, i)))

      checkColls(repl, coll)
    }
    forAll (byteGen, longGen, intGen, indexGen, minSuccess) { (b, d, i, n) =>
      val repl = builder.replicate(n, (b, (i, (d, b))))
      val coll = builder.fromArray(Array.fill[(Byte, (Int, (Long, Byte)))](n)((b, (i, (d, b)))))

      checkColls(repl, coll)
    }
    forAll (byteGen, longGen, intGen, indexGen, indexGen, minSuccess) { (b, d, i, n, m) =>
      val repl = builder.replicate(n, (b, ((i, (("string", builder.replicate(m, n)), Array(1, 2, 3, 4))), (d, b))))
      val coll = builder.fromArray(Array.fill(n)((b, ((i, (("string", builder.fromArray(Array.fill(m)(n))), Array(1, 2, 3, 4))), (d, b)))))

      checkColls(repl, coll)
    }
  }

  property("Coll.unionSet") {
    forAll(collGen, collGen) { (col1, col2) =>
      val res = col1.unionSet(col2)
      res.toArray shouldBe (col1.toArray.union(col2.toArray).distinct)
    }
    builder.replicate(2, 10).unionSet(builder.replicate(3, 10)).toArray shouldBe Array(10)
    forAll(superGen) {
      case cl1: Coll[(_, _)] => {
        val res = cl1.unionSet(cl1)
        res.toArray shouldBe (cl1.toArray.union(cl1.toArray).distinct)
      }
      case _ => assert(false, "Generator returned invalid PairColl")
    }
    /* TODO: simplify the above code
     * match-case removal gives the following compilation error:
        type mismatch;
        found   : sigma.PairColl[_$1(in value res),_$2(in value res)] where type _$2(in value res), type _$1(in value res)
        required: sigma.Coll[(_$1(in method getSuperGen), _$2(in method getSuperGen))]
          val res = col1.unionSet(col1)
     */
  }

  property("Coll.diff") {
    forAll(collGen, collGen) { (col1, col2) =>
      val res = col1.diff(col2)
      res.toArray shouldBe (col1.toArray.diff(col2.toArray))
    }
    forAll(superGen) {
      case col: Coll[(_, _)] =>
        val res = col.diff(col)
        res.toArray shouldBe (col.toArray.diff(col.toArray))
      case _ => assert(false, "Generator returned invalid PairColl") // TODO make similar gens
    }
    /* TODO: simplify the above code
     * match-case removal gives the following compilation error:
        type mismatch;
        found   : sigma.PairColl[_$1(in value res),_$2(in value res)] where type _$2(in value res), type _$1(in value res)
        required: sigma.Coll[(_$1(in method getSuperGen), _$2(in method getSuperGen))]
          val res = col.diff(col)
     */
    builder.replicate(2, 10).diff(builder.replicate(1, 10)).toArray shouldBe Array(10)
  }

  property("Coll.intersect") {
    forAll(collGen, collGen) { (col1, col2) =>
      val res = col1.intersect(col2)
      res.toArray shouldBe (col1.toArray.intersect(col2.toArray))
    }
    builder.replicate(2, 10).intersect(builder.replicate(3, 10)).toArray shouldBe Array(10, 10)
  }

  property("CollBuilder.xor") {
    forAll(bytesGen, bytesGen) { (col1, col2) =>
      val n = col1.length min col2.length
      val c1 = col1.take(n)
      val c2 = col2.take(n)
      builder.xor(c1, c2).toArray shouldBe c1.toArray.zip(c2.toArray).map { case (l,r) => (l ^ r).toByte }
    }
  }

  property("Coll equality") {
    val arr1 = Array[Int](1, 2, 3)
    val arr2 = Array[Int](1, 2, 3)
    val repl1 = Array[Int](1,1,1)
    val repl2 = Array[Int](1,1,1)
    val pairs1 = arr1.zip(repl1)
    val replPairs = repl1.zip(repl2)
    def ids = Array.tabulate(3) { i => Array.fill(32)(i.toByte) }
    def replIds = Array.fill(3) { Array.fill(32)(1) }
    val tokensArr = ids.zip(arr1)
    case class NoShrink[T](x: T)


    val collGen = Gen.oneOf(builder.fromArray(arr1), builder.fromArray(arr2), builder.fromItems(1, 2, 3)).map(NoShrink(_))
    val replGen = Gen.oneOf(builder.fromArray(repl1), builder.replicate(3, 1)).map(NoShrink(_))
    val idsGen = Gen.oneOf(builder.fromArray(ids), builder.fromArray(ids)).map(NoShrink(_))
    val replIdsGen = Gen.oneOf(builder.fromArray(replIds), builder.replicate(3, Array.fill(32)(1))).map(NoShrink(_))

    val pairsGen = Gen.oneOf(
      for { c1 <- collGen; c2 <- replGen } yield builder.pairColl(c1.x, c2.x): Coll[(Int, Int)],
      Gen.const(builder.fromArray(pairs1)),
      Gen.const(builder.fromItems((1, 1), (2, 1), (3, 1)))
    ).map(NoShrink(_))
    val replPairsGen = Gen.oneOf(
      for { c1 <- replGen; c2 <- replGen } yield builder.pairColl(c1.x, c2.x): Coll[(Int, Int)],
      Gen.const(builder.replicate(3, (1,1))),
      Gen.const(builder.fromArray(replPairs))
    ).map(NoShrink(_))

    val tokensGen = Gen.oneOf(
      for { c1 <- idsGen; c2 <- collGen } yield builder.pairColl(c1.x, c2.x): Coll[(Array[Byte], Int)],
      Gen.const(builder.fromArray(tokensArr)),
      Gen.const(builder.fromItems(tokensArr(0), tokensArr(1), tokensArr(2)))
    ).map(NoShrink(_))

    val minSuccess = MinSuccessful(30)
    
    forAll(collGen, collGen, minSuccess) { (c1, c2) =>
      assert(c1.x == c2.x)
      assert(c2.x == c1.x)
    }
    forAll(replGen, replGen, minSuccess) { (c1, c2) =>
      assert(c1.x == c2.x)
      assert(c2.x == c1.x)
    }
    forAll(collGen, replGen, minSuccess) { (c1, c2) =>
      assert(c1.x != c2.x)
      assert(c2.x != c1.x)
    }
    forAll(pairsGen, pairsGen, minSuccess) { (c1, c2) =>
      assert(c1.x == c2.x)
      assert(c2.x == c1.x)
    }
    forAll(replPairsGen, replPairsGen, minSuccess) { (c1, c2) =>
      assert(c1.x == c2.x)
      assert(c2.x == c1.x)
//      println(s"c1=$c1; c2=$c2")
    }

    forAll(idsGen, idsGen, minSuccess) { (c1, c2) =>
      assert(c1.x == c2.x)
      assert(c2.x == c1.x)
//      println(s"c1=$c1; c2=$c2")
    }

    forAll(replIdsGen, replIdsGen, minSuccess) { (c1, c2) =>
      assert(c1.x == c2.x)
      assert(c2.x == c1.x)
//      println(s"c1=$c1; c2=$c2")
    }

    forAll(tokensGen, tokensGen, minSuccess) { (c1, c2) =>
      assert(c1.x == c2.x)
      assert(c2.x == c1.x)
//      println(s"c1=$c1; c2=$c2")
    }
  }

}
