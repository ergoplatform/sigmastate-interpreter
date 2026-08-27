package sigmastate

import sigma.ast.{EvaluatedValue, IntConstant, SType}
import sigma.interpreter.{ContextExtension, SigmaMap}
import sigmastate.helpers.TestingCommons
import sigmastate.SigmaMapVectorsFile.Dataset


/** Dataset-driven conformance tests for [[SigmaMap]] traversal ordering against thousands
  * of golden vectors recorded from a real Scala 2.12.20 run
  * (resource /sigmastate/SigmaMapVectors.txt; see docs/SigmaMapSpec.md).
  *
  * This spec is JVM-only because the dataset is loaded from a test-classpath resource,
  * which cannot be linked by ScalaJS.
  */
class SigmaMapGoldenVectorsSpecification extends TestingCommons {

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

  property("golden vectors count is in many thousands") {
    dataset.vectors.length should be > 16000
  }

  property("indices table equals the full 128-key order recorded under Scala 2.12") {
    SigmaMap.indices.toList shouldBe dataset.fullOrder128.toList
  }

  property("golden vectors - SigmaMap(keys, values) traverses in Scala 2.12 Map order") {
    dataset.vectors.foreach { v =>
      val sm = SigmaMap(v.insertionKeys, valuesFor(v.insertionKeys))
      sm.size shouldBe v.expectedOrder.length
      sm.maxKey shouldBe v.expectedOrder.max
      keysOf(sm) shouldBe v.expectedOrder.toList
    }
  }

  property("golden vectors - ContextExtension serialization round-trip preserves order") {
    dataset.vectors.foreach { v =>
      val ext = ContextExtension(SigmaMap(v.insertionKeys, valuesFor(v.insertionKeys)))
      val bytes = ContextExtension.serializer.toBytes(ext)
      val parsed = ContextExtension.serializer.fromBytes(bytes)
      parsed shouldBe ext
      parsed.values.hashCode() shouldBe ext.values.hashCode()
      keysOf(parsed.values) shouldBe v.expectedOrder.toList
    }
  }

  property("golden vectors - apply(scala.collection.Map), maps with more than 4 entries") {
    // for such sizes the hash-trie order does not depend on the insertion order,
    // so this holds regardless of the runtime SDK version
    dataset.vectors.foreach { v =>
      if (v.insertionKeys.length > 4) {
        val sm = SigmaMap(v.insertionKeys.zip(valuesFor(v.insertionKeys)).toMap)
        keysOf(sm) shouldBe v.expectedOrder.toList
      }
    }
  }

  property("golden vectors - apply(scala.collection.Map), maps with at most 4 entries",
      SigmaMapTestTags.NotScala211) {
    dataset.vectors.foreach { v =>
      if (v.insertionKeys.length <= 4) {
        val sm = SigmaMap(v.insertionKeys.zip(valuesFor(v.insertionKeys)).toMap)
        keysOf(sm) shouldBe v.expectedOrder.toList
      }
    }
  }
}
