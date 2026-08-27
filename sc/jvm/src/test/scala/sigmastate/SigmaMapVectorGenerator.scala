package sigmastate

import java.io.PrintWriter
import java.time.Instant

import scala.collection.immutable
import scala.util.{Properties, Random}

/** Generates golden traversal-order vectors for [[sigma.interpreter.SigmaMap]] by observing
  * the REAL default scala.collection.immutable.Map of the RUNNING Scala SDK, which must be
  * Scala 2.12 (the reference version the SigmaMap ordering contract is pinned to).
  *
  * Outputs (both are committed to the repository):
  *  - a line-based dataset under the test resources, consumed by the Scala conformance
  *    tests (see sigma.interpreter dataset loader in sc/jvm test scope);
  *  - a JSON document for implementations of the ordering contract in other languages
  *    (see docs/SigmaMapSpec.md).
  *
  * Run it as (one-off):
  * {{{
  *   sbt '++2.12.20 scJVM/Test/runMain sigmastate.SigmaMapVectorGenerator \
  *     <vectors-txt-output> <vectors-json-output>'
  * }}}
  *
  * Every generated vector is validated against multiple construction paths of the runtime
  * library before being recorded:
  *  - maps with 1..4 entries must traverse in first-insertion order (Map1..Map4 classes);
  *  - maps with 5+ entries must traverse in hash-trie order which is a function of the key set
  *    only: verified via builder (`Map(...)`), foldLeft(`+`), `toMap`, growth from a 4-entry map,
  *    shrinking a larger map by removals, and in-place value replacement.
  */
object SigmaMapVectorGenerator {

  private val Seed = 20250821L

  /** number of random key subsets per size for sizes > 4 */
  private val SubsetsPerSize = 126

  /** number of DISTINCT random key sets per size for sizes 1..4; each set contributes its
    * DISTINCT permutations (1 for a single key, 2 for two keys, 3 otherwise), so no
    * duplicate vectors are ever emitted */
  private val SetsPerSmallSize = 97

  private case class Vector(insertionKeys: IndexedSeq[Byte], expectedOrder: IndexedSeq[Byte]) {
    def encode: String =
      s"${insertionKeys.length}|${insertionKeys.mkString(",")}|${expectedOrder.mkString(",")}"
  }

  def main(args: Array[String]): Unit = {
    require(args.length == 2, "usage: SigmaMapVectorGenerator <vectors-txt-output> <vectors-json-output>")
    val txtPath = args(0)
    val jsonPath = args(1)
    val scalaVer = Properties.versionNumberString
    require(scalaVer.startsWith("2.12"),
      s"This generator must run under Scala 2.12 to observe the reference ordering; got $scalaVer")

    implicit val rng: Random = new Random(Seed)

    val vectors = {
      val small = genSmallVectors
      val large = genLargeVectors
      small ++ large
    }

    require(vectors.map(_.encode).distinct.size == vectors.size,
      "duplicate vectors generated")

    // sanity: all vectors well-formed
    vectors.foreach { v =>
      require(v.insertionKeys.nonEmpty, "empty vector")
      require(v.insertionKeys.distinct == v.insertionKeys.toSeq, "duplicate keys in vector")
      require(v.expectedOrder.distinct == v.expectedOrder.toSeq, "duplicates in expected order")
      require(v.insertionKeys.toSet == v.expectedOrder.toSet, "expected order is not a permutation")
    }

    val fullOrder = refMap((0 to Byte.MaxValue).map(b => b.toByte -> b.toInt)).keys.toIndexedSeq

    writeVectorsTxt(txtPath, scalaVer, vectors, fullOrder)
    writeJson(jsonPath, scalaVer, vectors, fullOrder)

    val smallCount = vectors.count(_.insertionKeys.length <= 4)
    println(s"Generated ${vectors.size} vectors " +
      s"($smallCount small (1..4 entries), ${vectors.size - smallCount} large (5..128 entries))")
    println(s"Full 128-key order recorded: ${fullOrder.take(8).mkString(",")}...")
    println(s"Written to $txtPath")
    println(s"Written to $jsonPath")
  }

  /** Builds a fresh default immutable.Map via the vararg factory and returns its iteration order. */
  private def refMap(pairs: Seq[(Byte, Int)]): immutable.Map[Byte, Int] = {
    val m: immutable.Map[Byte, Int] = Map(pairs: _*)
    m
  }

  private def orderOf(m: immutable.Map[Byte, Int]): IndexedSeq[Byte] =
    m.iterator.map(_._1).toIndexedSeq

  /** Validates the given bindings against the runtime library and returns the canonical
    * traversal order. */
  private def validatedOrder(keys: IndexedSeq[Byte])(implicit rng: Random): IndexedSeq[Byte] = {
    val pairs = keys.map(k => k -> k.toInt)
    val n = keys.length

    val base = refMap(pairs)
    val baseOrder = orderOf(base)

    if (n <= 4) {
      // small-map regime: traversal must follow first-insertion order
      require(baseOrder == keys,
        s"runtime Map violates insertion-order contract for $n entries: $baseOrder vs $keys")
      // foldLeft path agrees
      val folded = pairs.foldLeft(immutable.Map.empty[Byte, Int])(_ + _)
      require(orderOf(folded) == baseOrder, s"foldLeft path disagrees for $keys")
      // replacing a value keeps position
      val replaced = base.updated(keys(n / 2), -1)
      require(orderOf(replaced) == baseOrder, s"replacement changed order for $keys")
    } else {
      // hash-trie regime: traversal is a function of the key set only
      val folded = pairs.foldLeft(immutable.Map.empty[Byte, Int])(_ + _)
      require(orderOf(folded) == baseOrder, s"foldLeft path disagrees for n=$n")
      val toMapped = pairs.toMap
      require(orderOf(toMapped) == baseOrder, s"toMap path disagrees for n=$n")
      // growth from a 4-entry map (the Map4 -> HashMap transition)
      val grownFrom4 = {
        var m = refMap(pairs.take(4))
        pairs.drop(4).foreach { kv => m = m + kv }
        m
      }
      require(orderOf(grownFrom4) == baseOrder, s"growth-from-4 path disagrees for n=$n")
      // shrinking a larger map by removals yields the restriction of its order
      if (n >= 6) {
        val extraCount = math.min(8, 128 - n)
        val extras = rng.shuffle((0 to Byte.MaxValue).map(_.toByte).filterNot(keys.contains)).take(extraCount)
        var big = base
        extras.foreach(k => big = big + (k -> k.toInt))
        val removed = extras.foldLeft(big)(_ - _)
        require(orderOf(removed) == baseOrder, s"shrink path disagrees for n=$n")
      }
      // in-place value replacement keeps order
      val replaced = base.updated(keys(n / 2), -1)
      require(orderOf(replaced) == baseOrder, s"replacement changed order for n=$n")
    }

    baseOrder
  }

  private def randomDistinctKeys(n: Int)(implicit rng: Random): IndexedSeq[Byte] =
    rng.shuffle((0 to Byte.MaxValue).map(_.toByte)).take(n).toIndexedSeq

  /** deterministic small-map edge cases, shared between emission and uniqueness bookkeeping */
  private val deterministicSmallSets: Seq[IndexedSeq[Byte]] = Seq(
    IndexedSeq[Byte](0),
    IndexedSeq[Byte](127),
    IndexedSeq[Byte](1, 2),
    IndexedSeq[Byte](2, 1),
    IndexedSeq[Byte](0, 1, 2, 3),
    IndexedSeq[Byte](3, 2, 1, 0),
    IndexedSeq[Byte](127, 0, 64, 33),
    IndexedSeq[Byte](73, 35, 31)
  )

  private def genSmallVectors(implicit rng: Random): Seq[Vector] = {
    val buf = Seq.newBuilder[Vector]
    // deterministic edge cases
    deterministicSmallSets.foreach { keys =>
      buf += Vector(keys, validatedOrder(keys))
    }

    // randomized sets (unique per size, also unique w.r.t. the deterministic sets below)
    // and their distinct permutations
    for (size <- 1 to 4) {
      val seenSets = scala.collection.mutable.HashSet[Set[Byte]]()
      deterministicSmallSets.filter(_.length == size).foreach(s => seenSets += s.toSet)
      var added = 0
      while (added < SetsPerSmallSize) {
        val keys = randomDistinctKeys(size)
        if (seenSets.add(keys.toSet)) {
          added += 1
          val maxPerms = math.min(3, {
            var f = 1
            var i = 2
            while (i <= size) { f *= i; i += 1 }
            f
          })
          val seen = scala.collection.mutable.HashSet[IndexedSeq[Byte]]()
          var attempts = 0
          while (seen.size < maxPerms && attempts < 50) {
            val perm = rng.shuffle(keys).toIndexedSeq
            if (seen.add(perm)) {
              buf += Vector(perm, validatedOrder(perm))
            }
            attempts += 1
          }
          require(seen.size == maxPerms, s"could not sample $maxPerms distinct permutations for $keys")
        }
      }
    }
    buf.result()
  }

  private def genLargeVectors(implicit rng: Random): Seq[Vector] = {
    val buf = Seq.newBuilder[Vector]
    for (size <- 5 to Byte.MaxValue;
         _ <- 0 until SubsetsPerSize) {
      val keys = randomDistinctKeys(size)
      buf += Vector(keys, validatedOrder(keys))
    }
    // deterministic boundaries
    for (keys <- Seq[IndexedSeq[Byte]](
      randomDistinctKeysDetermined(5),
      (0 to 4).map(_.toByte),
      (123 to 127).map(_.toByte),
      (0 to Byte.MaxValue).map(_.toByte).filter(_ % 2 == 0),
      (0 to Byte.MaxValue).map(_.toByte).filter(_ % 2 == 1)
    )) {
      buf += Vector(keys, validatedOrder(keys))
    }
    buf.result()
  }

  private def randomDistinctKeysDetermined(n: Int): IndexedSeq[Byte] = {
    val all = (0 to Byte.MaxValue).map(_.toByte)
    // stride-sampled deterministic subset
    val step = 128 / n
    (0 until n).map(i => all(i * step))
  }

  /** Emits the dataset as a line-based text file to be placed under src/test/resources and
    * loaded from the test classpath by the Scala conformance tests.
    *
    * Format:
    *  - lines starting with '#' are comments (provenance metadata);
    *  - one line "fullOrder128=<comma-separated keys>" with the traversal order of a map
    *    containing ALL keys 0..127;
    *  - one line per vector: "size|insertionKeys|expectedTraversalKeys" where keys are
    *    comma-separated byte values in 0..127 range. For size <= 4 the traversal follows
    *    the insertion order; for size > 4 it is the hash-trie order, which equals
    *    fullOrder128 filtered by the key set.
    */
  private def writeVectorsTxt(outPath: String, scalaVer: String, vectors: Seq[Vector],
                              fullOrder: IndexedSeq[Byte]): Unit = {
    val w = new PrintWriter(outPath)
    try {
      w.println("# Auto-generated golden vectors recording the traversal order of the default")
      w.println("# scala.collection.immutable.Map of the Scala 2.12 SDK.")
      w.println(s"# Generated by sigmastate.SigmaMapVectorGenerator under Scala $scalaVer")
      w.println(s"# on ${Instant.now()} with seed $Seed. DO NOT EDIT MANUALLY.")
      w.println("# See docs/SigmaMapSpec.md for the contract this data pins down.")
      val fullOrderStr = fullOrder.mkString(",")
      w.println("fullOrder128=" + fullOrderStr)
      vectors.foreach { v => w.println(v.encode) }
    } finally {
      w.close()
    }
  }

  /** Emits the same vectors as a JSON document for implementations of the SigmaMap
    * ordering contract in other languages (see docs/SigmaMapSpec.md). */
  private def writeJson(outPath: String, scalaVer: String, vectors: Seq[Vector],
                        fullOrder: IndexedSeq[Byte]): Unit = {
    val w = new PrintWriter(outPath)
    try {
      val sb = new StringBuilder()
      def ints(a: IndexedSeq[Byte]): String = a.mkString("[", ",", "]")
      sb.append("{\n")
      sb.append("  \"meta\": {\n")
      sb.append("    \"name\": \"SigmaMap traversal golden vectors\",\n")
      sb.append("    \"description\": \"Traversal order of the default scala.collection.immutable.Map of the Scala 2.12 SDK; see docs/SigmaMapSpec.md for the contract this data pins down.\",\n")
      sb.append("    \"generator\": \"sigmastate.SigmaMapVectorGenerator\",\n")
      sb.append("    \"scalaVersion\": \"").append(scalaVer).append("\",\n")
      sb.append("    \"seed\": ").append(Seed).append(",\n")
      sb.append("    \"createdAt\": \"").append(Instant.now()).append("\",\n")
      sb.append("    \"keyDomainMin\": 0,\n")
      sb.append("    \"keyDomainMax\": 127,\n")
      sb.append("    \"smallMapMaxSize\": 4,\n")
      sb.append("    \"traversalSmallMaps\": \"insertion order (a duplicate key keeps the position of its first occurrence and the value of its last occurrence)\",\n")
      sb.append("    \"traversalLargeMaps\": \"filter of fullOrder128 by the key set\"\n")
      sb.append("  },\n")
      sb.append("  \"fullOrder128\": ").append(ints(fullOrder)).append(",\n")
      sb.append("  \"vectors\": [")
      val n = vectors.length
      vectors.zipWithIndex.foreach { case (v, i) =>
        val sep = if (i < n - 1) "," else ""
        sb.append("\n    {\"size\": ").append(v.insertionKeys.length)
          .append(", \"insertionKeys\": ").append(ints(v.insertionKeys))
          .append(", \"expectedOrder\": ").append(ints(v.expectedOrder))
          .append("}").append(sep)
      }
      sb.append("\n  ]\n")
      sb.append("}\n")
      w.print(sb.result())
    } finally {
      w.close()
    }
  }
}
