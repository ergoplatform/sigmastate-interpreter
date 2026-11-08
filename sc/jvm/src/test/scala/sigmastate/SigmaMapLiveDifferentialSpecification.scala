package sigmastate

import io.circe.Json
import io.circe.parser.parse
import sigma.ast.{EvaluatedValue, IntConstant, SType}
import sigma.interpreter.SigmaMap
import sigmastate.helpers.TestingCommons
import sigmastate.SigmaMapVectorsFile.Dataset

import scala.io.Source
import scala.util.Random

/** JVM-only tests for [[SigmaMap]]:
  *  - differential comparison directly against the scala.collection library of the RUNNING
  *    SDK (full strength under Scala 2.12, which SigmaMap ordering is pinned to;
  *    excluded via the SigmaMap212Only tag on other versions, see build.sbt);
  *  - three-way lockstep check between the committed dataset resource (.txt) and the JSON
    * export consumed by implementations in other languages.
  *
  * This spec lives in the JVM test scope because it relies on `java.io` and circe-based
  * JSON parsing (and on `scala.util.Properties`-style version awareness, now expressed
  * through build-time tag exclusion).
  */
class SigmaMapLiveDifferentialSpecification extends TestingCommons {

  private val dataset: Dataset = SigmaMapVectorsFile.load

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

  property("differential test against runtime scala.Map under Scala 2.12",
      SigmaMapTestTags.Scala212Only) {
    val rng = new Random(7)
    val allKeys = (0 to Byte.MaxValue).map(_.toByte).toArray
    for (_ <- 0 until 2000) {
      val n = 1 + rng.nextInt(Byte.MaxValue) // 1..128
      val insertion = rng.shuffle(allKeys.toVector).take(n).toArray
      val refValues = valuesFor(insertion)
      val refMap: scala.collection.immutable.Map[Byte, EvaluatedValue[_ <: SType]] =
        insertion.zip(refValues).toMap
      val sm = SigmaMap(insertion, refValues)
      keysOf(sm) shouldBe refMap.keys.toList
      if (n <= 4) {
        // insertion-order regime: traversal must follow the given insertion sequence,
        // so a differently-ordered construction traverses differently (but is equal as a map)
        val revIns = insertion.reverse
        val smRev = SigmaMap(revIns, valuesFor(revIns))
        keysOf(smRev) shouldBe revIns.toList
        smRev shouldBe sm
      }
    }
  }

  property("hashCode equals hashCode of equivalent runtime scala.Map under Scala 2.12",
      SigmaMapTestTags.Scala212Only) {
    val rng = new Random(11)
    val allKeys = (0 to Byte.MaxValue).map(_.toByte).toArray
    for (_ <- 0 until 500) {
      val n = 1 + rng.nextInt(Byte.MaxValue)
      val insertion = rng.shuffle(allKeys.toVector).take(n).toArray
      val vals = valuesFor(insertion)
      val refMap: scala.collection.immutable.Map[Byte, EvaluatedValue[_ <: SType]] =
        insertion.zip(vals).toMap
      val sm = SigmaMap(insertion, vals)
      sm.hashCode() shouldBe refMap.hashCode()
    }
  }

  property("JSON export is in lockstep with the .txt dataset resource") {
    val stream = getClass.getResourceAsStream("/sigmastate/SigmaMapVectors.json")
    assume(stream != null, "SigmaMapVectors.json resource not found on test classpath")
    val jsonText = Source.fromInputStream(stream, "UTF-8").mkString
    stream.close()
    val json = parse(jsonText).fold(e => fail(s"invalid JSON: $e"), identity)

    def field(c: io.circe.HCursor, name: String): Json =
      c.downField(name).focus.getOrElse(fail(s"missing field $name"))

    // meta sanity
    val meta = json.hcursor.downField("meta").focus.getOrElse(fail("missing meta"))
    field(meta.hcursor, "scalaVersion").asString.getOrElse(fail("meta.scalaVersion")) should
      startWith("2.12")

    val fullOrderJson = field(json.hcursor, "fullOrder128").asArray
      .getOrElse(fail("fullOrder128 must be an array"))
      .map(j => j.asNumber.flatMap(_.toInt).getOrElse(fail("non-integer key")).toByte)
    fullOrderJson.toList shouldBe dataset.fullOrder128.toList

    val vectorsJson = field(json.hcursor, "vectors").asArray
      .getOrElse(fail("vectors must be an array"))
    vectorsJson.length shouldBe dataset.vectors.length

    dataset.vectors.zip(vectorsJson).foreach { case (v, vj) =>
      val c = vj.hcursor
      c.get[Int]("size").fold(e => fail(e.getMessage), identity) shouldBe v.size
      c.get[List[Int]]("insertionKeys").fold(e => fail(e.getMessage), identity)
        .map(_.toByte) shouldBe v.insertionKeys.toList
      c.get[List[Int]]("expectedOrder").fold(e => fail(e.getMessage), identity)
        .map(_.toByte) shouldBe v.expectedOrder.toList
    }
  }
}
