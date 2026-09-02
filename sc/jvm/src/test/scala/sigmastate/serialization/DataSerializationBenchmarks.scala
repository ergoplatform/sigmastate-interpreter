package sigmastate.serialization

import debox.cfor
import org.scalameter.api._
import sigma.ast._
import sigma.crypto.CryptoConstants
import sigma.serialization.{DataSerializer, SigmaSerializer}

/** Benchmarks for `DataSerializer.serialize` / `deserialize` per `SType`.
  *
  * Run with:
  * {{{
  * sbt --client -batch -no-colors -mem 8192 \
  *   "scJVM/Test/runMain sigmastate.serialization.DataSerializationBenchmarks"
  * }}}
  *
  * Each measurement serializes (or deserializes) a typed value once per
  * iteration. `Coll[Byte]` is parameterized by length to expose any
  * non-linearity in byte-array (de)serialization. Values are constructed
  * through typed `Constant` factories and threaded through `DataSerializer`
  * via `c.value` / `c.tpe` to keep the existential `S#WrappedType`
  * relationship intact for type inference.
  *
  * Baseline (Apple M1 Pro, OpenJDK 11):
  *
  *  `serialize`:                          `deserialize`:
  *    SInt              0.0025 ms          SInt              0.0035 ms
  *    SLong             0.0026 ms          SLong             0.0037 ms
  *    SBigInt           0.0040 ms          SBigInt           0.0041 ms
  *    SGroupElement     0.0056 ms          SGroupElement     0.0230 ms
  *    Coll[Byte] 16     0.0035 ms          Coll[Byte] 16     0.0032 ms
  *    Coll[Byte] 256    0.0029 ms          Coll[Byte] 256    0.0033 ms
  *    Coll[Byte] 4096   0.0037 ms          Coll[Byte] 4096   0.0035 ms
  *    Coll[Byte] 16384  0.0043 ms          Coll[Byte] 16384  0.0055 ms
  */
object DataSerializationBenchmarks extends Bench.LocalTime { suite: Bench[Double] =>

  // ---- fixed-shape Constants per SType ----
  private val intConst: Constant[SType]    = IntConstant(42).asInstanceOf[Constant[SType]]
  private val longConst: Constant[SType]   = LongConstant(1234567890123456789L).asInstanceOf[Constant[SType]]
  private val bigIntConst: Constant[SType] = BigIntConstant(new java.math.BigInteger("1234567890123456789")).asInstanceOf[Constant[SType]]
  private val groupElemConst: Constant[SType] = GroupElementConstant(CryptoConstants.dlogGroup.generator).asInstanceOf[Constant[SType]]

  // ---- collection sizes ----
  private val byteCollSizes: Gen[Int] = Gen.exponential("size")(16, 16384, 4)
  private val byteCollConstants: Gen[Constant[SType]] = byteCollSizes.map { size =>
    val arr = new Array[Byte](size)
    cfor(0)(_ < size, _ + 1) { i => arr(i) = (i & 0xFF).toByte }
    ByteArrayConstant(arr).asInstanceOf[Constant[SType]]
  }

  // ---- helpers ----
  private def writeConst(c: Constant[SType]): Array[Byte] = {
    val w = SigmaSerializer.startWriter()
    DataSerializer.serialize(c.value, c.tpe, w)
    w.toBytes
  }

  private def readType(tpe: SType, bytes: Array[Byte]): Any =
    DataSerializer.deserialize(tpe, SigmaSerializer.startReader(bytes))

  // ---- pre-serialized bytes for deserialize side ----
  private val intBytes: Array[Byte]        = writeConst(intConst)
  private val longBytes: Array[Byte]       = writeConst(longConst)
  private val bigIntBytes: Array[Byte]     = writeConst(bigIntConst)
  private val groupElemBytes: Array[Byte]  = writeConst(groupElemConst)
  private val byteCollBytesGen: Gen[Array[Byte]] = byteCollConstants.map(writeConst)

  private val once: Gen[Unit] = Gen.unit("once")

  performance of "DataSerializer.serialize" in {
    measure method "SInt" in {
      using(once) in { _ => writeConst(intConst) }
    }
    measure method "SLong" in {
      using(once) in { _ => writeConst(longConst) }
    }
    measure method "SBigInt" in {
      using(once) in { _ => writeConst(bigIntConst) }
    }
    measure method "SGroupElement" in {
      using(once) in { _ => writeConst(groupElemConst) }
    }
    measure method "Coll[Byte]" in {
      using(byteCollConstants) in { c => writeConst(c) }
    }
  }

  performance of "DataSerializer.deserialize" in {
    measure method "SInt" in {
      using(once) in { _ => readType(SInt, intBytes) }
    }
    measure method "SLong" in {
      using(once) in { _ => readType(SLong, longBytes) }
    }
    measure method "SBigInt" in {
      using(once) in { _ => readType(SBigInt, bigIntBytes) }
    }
    measure method "SGroupElement" in {
      using(once) in { _ => readType(SGroupElement, groupElemBytes) }
    }
    measure method "Coll[Byte]" in {
      using(byteCollBytesGen) in { bytes => readType(SCollection(SByte), bytes) }
    }
  }
}
