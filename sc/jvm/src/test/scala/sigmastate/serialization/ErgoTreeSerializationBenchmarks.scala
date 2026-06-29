package sigmastate.serialization

import org.scalameter.api._
import sigma.serialization.ErgoTreeSerializer

/** Benchmarks for top-level `ErgoTreeSerializer` round-trips.
  *
  * Run with:
  * {{{
  * sbt --client -batch -no-colors -mem 8192 \
  *   "scJVM/Test/runMain sigmastate.serialization.ErgoTreeSerializationBenchmarks"
  * }}}
  *
  * Targets:
  *   - `ErgoTreeSerializer.DefaultSerializer.serializeErgoTree`
  *   - `ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree`
  *
  * Sample shapes:
  *   - Deep nested `ArithOp(+)` chain wrapped in `EQ` + `BoolToSigmaProp`
  *   - Deep right-associated `AND` chain
  *   - `Coll[Int]` constant of growing size (capped at 500 to stay below
  *     `SigmaConstants.MaxPropositionBytes`)
  *   - Real v3 ErgoTree hex sample (with upcast)
  *
  * Baseline (Apple M1 Pro, OpenJDK 11):
  *
  *   serializeErgoTree            5    15    45
  *     ArithOp(+) chain        0.003 0.003 0.006 ms
  *     AND chain               0.006 0.012 0.027 ms
  *
  *   serializeErgoTree           10   100   500
  *     Coll[Int] constant      0.003 0.008 0.029 ms
  *
  *   deserializeErgoTree          5    15    45
  *     ArithOp(+) chain        0.031 0.016 0.038 ms
  *     AND chain               0.013 0.021 0.046 ms
  *
  *   deserializeErgoTree         10   100   500
  *     Coll[Int] constant      0.010 0.037 0.144 ms
  *
  *   deserializeErgoTree v3 hex sample:               0.076 ms
  *
  *   roundTrip                    5    15    45
  *     ArithOp(+) chain        0.006 0.011 0.031 ms
  *
  *   roundTrip                   10   100   500
  *     Coll[Int] constant      0.007 0.023 0.099 ms
  */
object ErgoTreeSerializationBenchmarks extends Bench.LocalTime with SerializationBenchmarkGens { suite: Bench[Double] =>

  private val ser = ErgoTreeSerializer.DefaultSerializer

  performance of "ErgoTreeSerializer.serializeErgoTree" in {
    measure method "ArithOp(+) chain" in {
      using(arithOpTrees) in { tree => ser.serializeErgoTree(tree) }
    }
    measure method "AND chain" in {
      using(logicalAndTrees) in { tree => ser.serializeErgoTree(tree) }
    }
    measure method "Coll[Int] constant" in {
      using(collIntTrees) in { tree => ser.serializeErgoTree(tree) }
    }
  }

  performance of "ErgoTreeSerializer.deserializeErgoTree" in {
    measure method "ArithOp(+) chain" in {
      using(arithOpTreeBytes) in { bytes => ser.deserializeErgoTree(bytes) }
    }
    measure method "AND chain" in {
      using(logicalAndTreeBytes) in { bytes => ser.deserializeErgoTree(bytes) }
    }
    measure method "Coll[Int] constant" in {
      using(collIntTreeBytes) in { bytes => ser.deserializeErgoTree(bytes) }
    }
    measure method "v3 hex sample" in {
      using(Gen.unit("v3-sample")) in { _ => ser.deserializeErgoTree(v3SampleBytes) }
    }
  }

  performance of "ErgoTreeSerializer.roundTrip" in {
    measure method "ArithOp(+) chain" in {
      using(arithOpTrees) in { tree =>
        ser.deserializeErgoTree(ser.serializeErgoTree(tree))
      }
    }
    measure method "Coll[Int] constant" in {
      using(collIntTrees) in { tree =>
        ser.deserializeErgoTree(ser.serializeErgoTree(tree))
      }
    }
  }
}
