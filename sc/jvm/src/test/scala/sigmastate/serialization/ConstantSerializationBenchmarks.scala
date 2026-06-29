package sigmastate.serialization

import debox.cfor
import org.scalameter.api._
import org.scalameter.picklers.Implicits._
import sigma.Colls
import sigma.ast.{BigIntConstant, Constant, ConstantPlaceholder, DeserializationSigmaBuilder, GroupElementConstant, IntArrayConstant, IntConstant, LongConstant, SInt, SType, StdSigmaBuilder}
import sigma.crypto.CryptoConstants
import sigma.serialization.{ConstantPlaceholderSerializer, ConstantSerializer, ConstantStore, SigmaSerializer}

/** Benchmarks for `ConstantSerializer` and `ConstantPlaceholderSerializer`.
  *
  * Run with:
  * {{{
  * sbt --client -batch -no-colors -mem 8192 \
  *   "scJVM/Test/runMain sigmastate.serialization.ConstantSerializationBenchmarks"
  * }}}
  *
  * `ConstantSerializer` writes a type tag followed by `DataSerializer` output;
  * benchmarking it independently exposes regressions in the type-tag path
  * specifically. `ConstantPlaceholderSerializer` is the segregated-constants
  * counterpart — its deserialize path hits the reader's `ConstantStore`.
  *
  * Baseline (Apple M1 Pro, OpenJDK 11):
  *
  *   ConstantSerializer.serialize     deserialize
  *     SInt          0.0013 ms          0.0030 ms
  *     SLong         0.0018 ms          0.0037 ms
  *     SBigInt       0.0032 ms          0.0049 ms
  *     SGroupElement 0.0054 ms          0.0260 ms
  *     Coll[Int]   10   0.0029 ms       0.0059 ms
  *     Coll[Int]  100   0.0036 ms       0.0077 ms
  *     Coll[Int] 1000   0.0152 ms       0.0233 ms
  *     Coll[Int]10000   0.1388 ms       0.1983 ms
  *
  *   ConstantPlaceholderSerializer.serialize: ~0.00083 ms (constant across ids)
  *   ConstantPlaceholderSerializer.parse:     ~0.0025 ms  (constant across ids)
  */
object ConstantSerializationBenchmarks extends Bench.LocalTime { suite: Bench[Double] =>

  private val constSer = ConstantSerializer(StdSigmaBuilder)
  private val placeholderSer = ConstantPlaceholderSerializer(DeserializationSigmaBuilder.mkConstantPlaceholder)

  // ---- single-value Constants of various SType ----
  private val intConst: Constant[SType]      = IntConstant(42).asInstanceOf[Constant[SType]]
  private val longConst: Constant[SType]     = LongConstant(1234567890123456789L).asInstanceOf[Constant[SType]]
  private val bigIntConst: Constant[SType]   = BigIntConstant(new java.math.BigInteger("1234567890123456789")).asInstanceOf[Constant[SType]]
  private val groupElemConst: Constant[SType] = GroupElementConstant(CryptoConstants.dlogGroup.generator).asInstanceOf[Constant[SType]]

  // ---- collection Constants of growing size ----
  private val intCollSizes: Gen[Int] = Gen.exponential("size")(10, 10000, 10)
  private val intCollConstants: Gen[Constant[SType]] = intCollSizes.map { size =>
    val arr = new Array[Int](size)
    cfor(0)(_ < size, _ + 1) { i => arr(i) = i }
    IntArrayConstant(Colls.fromArray(arr)).asInstanceOf[Constant[SType]]
  }

  private def writeConstant(c: Constant[SType]): Array[Byte] = {
    val w = SigmaSerializer.startWriter()
    constSer.serialize(c, w)
    w.toBytes
  }

  private val intConstBytes: Array[Byte] = writeConstant(intConst)
  private val longConstBytes: Array[Byte] = writeConstant(longConst)
  private val bigIntConstBytes: Array[Byte] = writeConstant(bigIntConst)
  private val groupElemConstBytes: Array[Byte] = writeConstant(groupElemConst)
  private val intCollConstBytesGen: Gen[Array[Byte]] = intCollConstants.map(writeConstant)

  // ---- ConstantPlaceholder ids: a spread of UInt-VLQ encoding widths ----
  private val placeholderIds: Gen[Int] = Gen.enumeration("id")(0, 127, 16384, 268435456)
  // Populated constant store for deserialize-side measure
  private val populatedStore = new ConstantStore(
    IndexedSeq.tabulate(128)(i => IntConstant(i).asInstanceOf[Constant[SType]])
  )
  private val placeholderDeserIds: Gen[Int] = Gen.enumeration("id")(0, 1, 64, 127)

  private val once: Gen[Unit] = Gen.unit("once")

  performance of "ConstantSerializer.serialize" in {
    measure method "SInt" in {
      using(once) in { _ => writeConstant(intConst) }
    }
    measure method "SLong" in {
      using(once) in { _ => writeConstant(longConst) }
    }
    measure method "SBigInt" in {
      using(once) in { _ => writeConstant(bigIntConst) }
    }
    measure method "SGroupElement" in {
      using(once) in { _ => writeConstant(groupElemConst) }
    }
    measure method "Coll[Int]" in {
      using(intCollConstants) in { c => writeConstant(c) }
    }
  }

  performance of "ConstantSerializer.deserialize" in {
    measure method "SInt" in {
      using(once) in { _ => constSer.deserialize(SigmaSerializer.startReader(intConstBytes)) }
    }
    measure method "SLong" in {
      using(once) in { _ => constSer.deserialize(SigmaSerializer.startReader(longConstBytes)) }
    }
    measure method "SBigInt" in {
      using(once) in { _ => constSer.deserialize(SigmaSerializer.startReader(bigIntConstBytes)) }
    }
    measure method "SGroupElement" in {
      using(once) in { _ => constSer.deserialize(SigmaSerializer.startReader(groupElemConstBytes)) }
    }
    measure method "Coll[Int]" in {
      using(intCollConstBytesGen) in { bytes => constSer.deserialize(SigmaSerializer.startReader(bytes)) }
    }
  }

  performance of "ConstantPlaceholderSerializer.serialize" in {
    measure method "by id" in {
      using(placeholderIds) in { id =>
        val w = SigmaSerializer.startWriter()
        placeholderSer.serialize(ConstantPlaceholder(id, SInt), w)
        w.toBytes
      }
    }
  }

  performance of "ConstantPlaceholderSerializer.parse" in {
    measure method "by id" in {
      using(placeholderDeserIds) in { id =>
        val w = SigmaSerializer.startWriter()
        placeholderSer.serialize(ConstantPlaceholder(id, SInt), w)
        val bytes = w.toBytes
        val r = SigmaSerializer.startReader(bytes, populatedStore, resolvePlaceholdersToConstants = false)
        placeholderSer.parse(r)
      }
    }
  }
}
