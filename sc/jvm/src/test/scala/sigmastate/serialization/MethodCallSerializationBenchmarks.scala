package sigmastate.serialization

import org.scalameter.api._
import sigma.VersionContext
import sigma.ast.SCollection.SByteArray
import sigma.ast._
import sigma.serialization.{MethodCallSerializer, PropertyCallSerializer, SigmaSerializer}

/** Benchmarks for `MethodCallSerializer` (non-empty args) and
  * `PropertyCallSerializer` (zero args).
  *
  * Run with:
  * {{{
  * sbt --client -batch -no-colors -mem 8192 \
  *   "scJVM/Test/runMain sigmastate.serialization.MethodCallSerializationBenchmarks"
  * }}}
  *
  * MethodCall serialization requires V3+ ErgoTree version to enforce the
  * non-empty args invariant on deserialize, so the benchmark runs under
  * `VersionContext.V6SoftForkVersion`.
  *
  * Baseline (Apple M1 Pro, OpenJDK 11):
  *
  *   PropertyCallSerializer.serialize Outputs.size           0.0040 ms
  *   PropertyCallSerializer.parse     Outputs.size           0.0403 ms
  *   MethodCallSerializer.serialize   Outputs.flatMap        0.0160 ms
  *   MethodCallSerializer.serialize   Global.serialize       0.0114 ms
  *   MethodCallSerializer.parse       Outputs.flatMap        0.1072 ms
  *   MethodCallSerializer.parse       Global.serialize       0.0485 ms
  *
  * `parse` is significantly slower than `serialize` because
  * `SMethod.fromIds` + `specializeFor` chase the method registry on every call.
  */
object MethodCallSerializationBenchmarks extends Bench.LocalTime { suite: Bench[Double] =>

  private val mcSer = MethodCallSerializer(DeserializationSigmaBuilder.mkMethodCall)
  private val pcSer = PropertyCallSerializer(DeserializationSigmaBuilder.mkMethodCall)

  // ---- fixture method calls ----

  // PropertyCall (no args): Outputs.size
  private val sizeCall: MethodCall = MethodCall(
    Outputs,
    SCollectionMethods.SizeMethod.withConcreteTypes(Map(SCollection.tIV -> SBox)),
    Vector.empty,
    Map.empty
  )

  // MethodCall (1 function arg): Outputs.flatMap(b => b.scriptBytes)
  private val flatMapCall: MethodCall = MethodCall(
    Outputs,
    SCollectionMethods.FlatMapMethod.withConcreteTypes(
      Map(SCollection.tIV -> SBox, SCollection.tOV -> SByte)
    ),
    Vector(FuncValue(1, SBox, ExtractScriptBytes(ValUse(1, SBox)))),
    Map.empty
  )

  // MethodCall (1 explicit-type arg): Global.serialize[Coll[Byte]](bytes)
  private val serializeCall: MethodCall = MethodCall(
    Global,
    SGlobalMethods.serializeMethod.withConcreteTypes(Map(SType.tT -> SByteArray)),
    Vector(ByteArrayConstant(Array[Byte](1, 2, 3))),
    Map.empty
  )

  // Pre-serialized byte buffers (under V6 context, since some methods are V6-only)
  private val sizeCallBytes: Array[Byte] = VersionContext.withVersions(
    VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion
  ) {
    val w = SigmaSerializer.startWriter()
    pcSer.serialize(sizeCall, w)
    w.toBytes
  }

  private val flatMapCallBytes: Array[Byte] = VersionContext.withVersions(
    VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion
  ) {
    val w = SigmaSerializer.startWriter()
    mcSer.serialize(flatMapCall, w)
    w.toBytes
  }

  private val serializeCallBytes: Array[Byte] = VersionContext.withVersions(
    VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion
  ) {
    val w = SigmaSerializer.startWriter()
    mcSer.serialize(serializeCall, w)
    w.toBytes
  }

  private val once: Gen[Unit] = Gen.unit("once")

  private def underV6[A](block: => A): A =
    VersionContext.withVersions(VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion)(block)

  performance of "PropertyCallSerializer.serialize" in {
    measure method "Outputs.size" in {
      using(once) in { _ =>
        underV6 {
          val w = SigmaSerializer.startWriter()
          pcSer.serialize(sizeCall, w)
          w.toBytes
        }
      }
    }
  }

  performance of "PropertyCallSerializer.parse" in {
    measure method "Outputs.size" in {
      using(once) in { _ =>
        underV6 { pcSer.parse(SigmaSerializer.startReader(sizeCallBytes)) }
      }
    }
  }

  performance of "MethodCallSerializer.serialize" in {
    measure method "Outputs.flatMap (1 func arg)" in {
      using(once) in { _ =>
        underV6 {
          val w = SigmaSerializer.startWriter()
          mcSer.serialize(flatMapCall, w)
          w.toBytes
        }
      }
    }
    measure method "Global.serialize (1 explicit type arg)" in {
      using(once) in { _ =>
        underV6 {
          val w = SigmaSerializer.startWriter()
          mcSer.serialize(serializeCall, w)
          w.toBytes
        }
      }
    }
  }

  performance of "MethodCallSerializer.parse" in {
    measure method "Outputs.flatMap" in {
      using(once) in { _ =>
        underV6 { mcSer.parse(SigmaSerializer.startReader(flatMapCallBytes)) }
      }
    }
    measure method "Global.serialize" in {
      using(once) in { _ =>
        underV6 { mcSer.parse(SigmaSerializer.startReader(serializeCallBytes)) }
      }
    }
  }
}
