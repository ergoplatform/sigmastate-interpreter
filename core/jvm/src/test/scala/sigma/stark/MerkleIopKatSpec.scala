package sigma.stark

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.io.Source

/** Known-Answer-Test parity for the EIP-0045 verifyStark transcript reader
  * ([[ReadIop]]) and Merkle branch verifier ([[MerkleVerifier]]).
  *
  * Expected values come from an EXTERNAL oracle — risc0-zkp 1.2.6's own
  * prover (`MerkleTreeProver` + `WriteIOP`) and verifier (`ReadIOP` +
  * `MerkleTreeVerifier`), captured by `stark-kat/src/bin/merkle_iop_kat.rs`
  * into `resources/stark-kats/` — never from the Scala code under test.
  * Negative vectors (corrupt word / truncation / wrong row index) were
  * confirmed as rejects by replaying them through risc0-zkp's own verifier
  * in the generator before being emitted.
  *
  * The vectors are generated with risc0-zkp 1.2.6 and are byte-valid for
  * 3.0.4 (the version the node's succinct verify path links): the two
  * versions are value-identical on this surface, differing only in error
  * plumbing — see the generator's version note.
  */
class MerkleIopKatSpec extends AnyFunSuite with Matchers {

  private def lines(resource: String): Seq[String] = {
    val is = getClass.getResourceAsStream(resource)
    require(is != null, s"missing KAT resource $resource — run stark-kat merkle_iop_kat generator")
    try Source.fromInputStream(is, "UTF-8").getLines().filterNot(_.startsWith("#")).toList
    finally is.close()
  }

  /** Wire words are u32; parse via Long so values >= 2^31 round-trip into
    * bit-identical Ints.
    */
  private def words(s: String): Array[Int] =
    if (s.isEmpty) Array.empty[Int] else s.split(',').map(w => java.lang.Long.parseLong(w).toInt)

  /** Smallest r with 2^r >= v — mirror of risc0-zkp `log2_ceil` (the query
    * index draw uses `random_bits(log2_ceil(rows))`).
    */
  private def log2Ceil(v: Int): Int = {
    var r = 0
    while ((1 << r) < v) r += 1
    r
  }

  // --------------------------------------------------------------------
  // ReadIOP transcript script
  // --------------------------------------------------------------------

  test("ReadIop transcript replay matches risc0-zkp WriteIOP/ReadIOP script") {
    val script = lines("/stark-kats/readiop_script.tsv")
    script should not be empty
    script.head should startWith("proof:")
    val iop = new ReadIop(words(script.head.stripPrefix("proof:")))
    script.tail.foreach { line =>
      withClue(s"op '$line': ") {
        if (line.startsWith("u32s:")) {
          val Array(n, exp) = line.stripPrefix("u32s:").split(" -> ")
          iop.readU32s(n.trim.toInt).get shouldBe words(exp)
        } else if (line.startsWith("elems:")) {
          val Array(n, exp) = line.stripPrefix("elems:").split(" -> ")
          iop.readFieldElemSlice(n.trim.toInt).get shouldBe words(exp)
        } else if (line.startsWith("pod:")) {
          val Array(n, exp) = line.stripPrefix("pod:").split(" -> ")
          iop.readPodSlice(n.trim.toInt).get.flatten shouldBe words(exp)
        } else if (line.startsWith("commit:")) {
          iop.commit(words(line.stripPrefix("commit:")))
        } else if (line.startsWith("elem -> ")) {
          iop.randomElem() shouldBe line.stripPrefix("elem -> ").toInt
        } else if (line.startsWith("bits:")) {
          val Array(spec, exp) = line.stripPrefix("bits:").split(" -> ")
          iop.randomBits(spec.trim.toInt) shouldBe exp.trim.toInt
        } else if (line.startsWith("ext -> ")) {
          val e = words(line.stripPrefix("ext -> "))
          iop.randomExtElem() shouldBe Ext4(e(0), e(1), e(2), e(3))
        } else if (line == "complete") {
          iop.verifyComplete shouldBe true
        } else fail(s"unknown op line: $line")
      }
    }
  }

  test("ReadIop bounds-checks reads instead of throwing (upstream panics = reject)") {
    val iop = new ReadIop(Array(1, 2, 3))
    iop.readU32s(4) shouldBe None
    iop.readU32s(-1) shouldBe None
    iop.readPodSlice(1) shouldBe None
    iop.readU32s(3).get shouldBe Array(1, 2, 3)
    iop.readU32s(1) shouldBe None
    iop.verifyComplete shouldBe true
    // A wire word >= P fails the checked field-element cast.
    new ReadIop(Array(BabyBear.P)).readFieldElemSlice(1) shouldBe None
    new ReadIop(Array(0x80000000)).readFieldElemSlice(1) shouldBe None
  }

  // --------------------------------------------------------------------
  // Merkle tree vectors
  // --------------------------------------------------------------------

  private case class TreeVec(
      rows: Int,
      cols: Int,
      queries: Int,
      proof: Array[Int],
      root: Array[Int],
      queryRows: Seq[(Int, Array[Int])],
      badWords: Seq[(Int, Int)],
      truncates: Seq[Int],
      badQueries: Seq[Int])

  private def parseTrees(ls: Seq[String]): Seq[TreeVec] = {
    var trees = Vector.empty[TreeVec]
    var cur: TreeVec = null
    ls.foreach { line =>
      if (line.startsWith("tree:")) {
        val p = line.stripPrefix("tree:").split(',')
        cur = TreeVec(p(0).toInt, p(1).toInt, p(2).toInt, null, null, Vector.empty,
          Vector.empty, Vector.empty, Vector.empty)
      } else if (line.startsWith("proof:")) {
        cur = cur.copy(proof = words(line.stripPrefix("proof:")))
      } else if (line.startsWith("root:")) {
        cur = cur.copy(root = words(line.stripPrefix("root:")))
      } else if (line.startsWith("query:")) {
        val Array(idx, row) = line.stripPrefix("query:").split(" -> ")
        cur = cur.copy(queryRows = cur.queryRows :+ ((idx.trim.toInt, words(row))))
      } else if (line.startsWith("badword:")) {
        val p = line.stripPrefix("badword:").split(',')
        cur = cur.copy(badWords = cur.badWords :+ ((p(0).toInt, java.lang.Long.parseLong(p(1)).toInt)))
      } else if (line.startsWith("truncate:")) {
        cur = cur.copy(truncates = cur.truncates :+ line.stripPrefix("truncate:").toInt)
      } else if (line.startsWith("badquery:")) {
        cur = cur.copy(badQueries = cur.badQueries :+ line.stripPrefix("badquery:").toInt)
      } else if (line == "endtree") {
        trees = trees :+ cur
        cur = null
      } else if (line != "complete") fail(s"unknown merkle KAT line: $line")
    }
    trees
  }

  private lazy val trees: Seq[TreeVec] = parseTrees(lines("/stark-kats/merkle_kat.tsv"))

  /** Full replay of a tree vector against its recorded expectations —
    * mirror of the generator's `oracle_replay_ok` (same accept criterion,
    * so a Scala reject here corresponds 1:1 to an oracle-confirmed reject).
    * `wrongQuery`: verify that query with `(idx + 1) % rows`, passing only
    * if the wrong row is (impossibly) accepted.
    */
  private def replayOk(v: TreeVec, proof: Array[Int], wrongQuery: Option[Int]): Boolean = {
    val iop = new ReadIop(proof)
    MerkleVerifier.create(iop, v.rows, v.cols, v.queries) match {
      case Left(_) => false
      case Right(ver) =>
        if (!java.util.Arrays.equals(ver.rootRaw, v.root)) false
        else {
          var ok = true
          var acceptedWrongRow = false
          val it = v.queryRows.iterator.zipWithIndex
          while (ok && !acceptedWrongRow && it.hasNext) {
            val ((expIdx, expRow), q) = it.next()
            var idx = iop.randomBits(log2Ceil(v.rows))
            if (idx != expIdx) ok = false
            else {
              if (wrongQuery.contains(q)) idx = (idx + 1) % v.rows
              ver.verify(iop, idx) match {
                case Right(row) =>
                  if (wrongQuery.contains(q)) acceptedWrongRow = true
                  else if (!java.util.Arrays.equals(row, expRow)) ok = false
                case Left(_) => ok = false
              }
            }
          }
          ok && (acceptedWrongRow || iop.verifyComplete)
        }
    }
  }

  test("MerkleVerifier replays prover-built trees: root, query indices, rows, completion") {
    trees should not be empty
    trees.foreach { v =>
      withClue(s"tree ${v.rows}x${v.cols} q${v.queries}: ") {
        val iop = new ReadIop(v.proof)
        val ver = MerkleVerifier.create(iop, v.rows, v.cols, v.queries).right.get
        ver.rootRaw shouldBe v.root
        val exposedRoot = ver.rootRaw
        exposedRoot(0) ^= 1
        ver.rootRaw shouldBe v.root
        v.queryRows.foreach { case (expIdx, expRow) =>
          val idx = iop.randomBits(log2Ceil(v.rows))
          idx shouldBe expIdx
          ver.verify(iop, idx).right.get shouldBe expRow
        }
        iop.verifyComplete shouldBe true
      }
    }
  }

  test("MerkleVerifier rejects corrupted proof words (oracle-confirmed rejects)") {
    trees.flatMap(v => v.badWords.map(v -> _)) should not be empty
    trees.foreach { v =>
      v.badWords.foreach { case (pos, xor) =>
        val corrupted = v.proof.clone()
        corrupted(pos) ^= xor
        withClue(s"tree ${v.rows}x${v.cols} q${v.queries} badword $pos^$xor: ") {
          replayOk(v, corrupted, None) shouldBe false
        }
      }
    }
  }

  test("MerkleVerifier rejects truncated proofs (oracle-confirmed rejects)") {
    trees.foreach { v =>
      v.truncates.foreach { keep =>
        withClue(s"tree ${v.rows}x${v.cols} q${v.queries} truncate $keep: ") {
          replayOk(v, v.proof.take(keep), None) shouldBe false
        }
      }
    }
  }

  test("MerkleVerifier rejects a query for the wrong row index (oracle-confirmed rejects)") {
    trees.flatMap(_.badQueries) should not be empty
    trees.foreach { v =>
      v.badQueries.foreach { q =>
        withClue(s"tree ${v.rows}x${v.cols} q${v.queries} badquery $q: ") {
          replayOk(v, v.proof, Some(q)) shouldBe false
        }
      }
    }
  }

  test("MerkleVerifier rejects an out-of-range query index without reading the stream") {
    val v = trees.head
    val iop = new ReadIop(v.proof)
    val ver = MerkleVerifier.create(iop, v.rows, v.cols, v.queries).right.get
    val before = iop.remaining
    ver.verify(iop, v.rows).isLeft shouldBe true
    ver.verify(iop, -1).isLeft shouldBe true
    iop.remaining shouldBe before
  }
}
