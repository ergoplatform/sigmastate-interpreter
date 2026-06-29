package sigmastate.serialization

import org.scalameter.api._
import org.scalameter.picklers.Implicits._
import sigma.ast._
import sigma.serialization.{SigmaSerializer, TypeSerializer}

/** Benchmarks for `TypeSerializer`.
  *
  * Run with:
  * {{{
  * sbt --client -batch -no-colors -mem 8192 \
  *   "scJVM/Test/runMain sigmastate.serialization.TypeSerializationBenchmarks"
  * }}}
  *
  * Covers the primary encoding shapes of `TypeSerializer`:
  *   - primitive embeddable types (1-byte path)
  *   - generics over primitives (1-byte path via type-code embedding)
  *   - nested generics (recursive path)
  *   - tuples (pair, triple, quadruple, n-ary)
  *
  * Scalameter requires a `Pickler` for the axis type, so the benchmark uses
  * an Int index into a fixed lookup table rather than carrying tuples through
  * the `Gen` machinery.
  *
  * Baseline (Apple M1 Pro, OpenJDK 11):
  *
  * `serialize`:                                `deserialize`:
  *   SInt                      0.0014 ms       SInt                      0.0044 ms
  *   SLong                     0.0012 ms       SLong                     0.0045 ms
  *   SBoolean                  0.0013 ms       SBoolean                  0.0042 ms
  *   SBigInt                   0.0012 ms       SBigInt                   0.0042 ms
  *   SGroupElement             0.0012 ms       SGroupElement             0.0042 ms
  *   SSigmaProp                0.0012 ms       SSigmaProp                0.0043 ms
  *   Coll[Int]                 0.0018 ms       Coll[Int]                 0.0039 ms
  *   Coll[Coll[Int]]           0.0019 ms       Coll[Coll[Int]]           0.0039 ms
  *   Coll[Coll[Box]]           0.0016 ms       Coll[Coll[Box]]           0.0038 ms
  *   Option[Int]               0.0015 ms       Option[Int]               0.0025 ms
  *   Option[Coll[Byte]]        0.0015 ms       Option[Coll[Byte]]        0.0024 ms
  *   (Int, Int)                0.0019 ms       (Int, Int)                0.0048 ms
  *   (Int, Long)               0.0022 ms       (Int, Long)               0.0053 ms
  *   (Int, Long, Box)          0.0027 ms       (Int, Long, Box)          0.0070 ms
  *   (Int, Long, Box, AvlTree) 0.0020 ms       (Int, Long, Box, AvlTree) 0.0075 ms
  *   (Int x 8)                 0.0024 ms       (Int x 8)                 0.0087 ms
  */
object TypeSerializationBenchmarks extends Bench.LocalTime { suite: Bench[Double] =>

  // ---- representative types covering each branch of TypeSerializer ----
  private val types: Array[(String, SType)] = Array(
    "SInt" -> SInt,
    "SLong" -> SLong,
    "SBoolean" -> SBoolean,
    "SBigInt" -> SBigInt,
    "SGroupElement" -> SGroupElement,
    "SSigmaProp" -> SSigmaProp,
    "Coll[Int]" -> SCollection(SInt),
    "Coll[Coll[Int]]" -> SCollection(SCollection(SInt)),
    "Coll[Coll[Box]]" -> SCollection(SCollection(SBox)),
    "Option[Int]" -> SOption(SInt),
    "Option[Coll[Byte]]" -> SOption(SCollection(SByte)),
    "(Int, Int)" -> STuple(SInt, SInt),
    "(Int, Long)" -> STuple(SInt, SLong),
    "(Int, Long, Box)" -> STuple(SInt, SLong, SBox),
    "(Int, Long, Box, AvlTree)" -> STuple(Vector[SType](SInt, SLong, SBox, SAvlTree)),
    "(Int x 8)" -> STuple(Vector.fill[SType](8)(SInt))
  )

  private val typeBytes: Array[Array[Byte]] = types.map { case (_, tpe) =>
    val w = SigmaSerializer.startWriter()
    TypeSerializer.serialize(tpe, w)
    w.toBytes
  }

  private val typeIdx: Gen[Int] = Gen.range("typeIdx")(0, types.length - 1, 1)

  performance of "TypeSerializer.serialize" in {
    measure method "by type" in {
      using(typeIdx) in { idx =>
        val w = SigmaSerializer.startWriter()
        TypeSerializer.serialize(types(idx)._2, w)
        w.toBytes
      }
    }
  }

  performance of "TypeSerializer.deserialize" in {
    measure method "by type" in {
      using(typeIdx) in { idx =>
        TypeSerializer.deserialize(SigmaSerializer.startReader(typeBytes(idx)))
      }
    }
  }
}
