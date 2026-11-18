package sigmastate

import scala.io.Source

/** Loads the SigmaMap golden-vector dataset from the test classpath resource
  * `/sigmastate/SigmaMapVectors.txt` (see docs/SigmaMapSpec.md and
  * sigmastate.SigmaMapVectorGenerator).
  *
  * The dataset lives in a resource file rather than generated source code so that it is
  * not compiled into bytecode; consequently only JVM tests consume it directly
  * (ScalaJS cannot link java.io-based resource loading).
  */
object SigmaMapVectorsFile {

  case class RawVector(size: Int, insertionKeys: Array[Byte], expectedOrder: Array[Byte])

  case class Dataset(fullOrder128: Array[Byte], vectors: Vector[RawVector])

  /** Parsed dataset, loaded once. */
  lazy val load: Dataset = {
    val stream = getClass.getResourceAsStream("/sigmastate/SigmaMapVectors.txt")
    require(stream != null, "resource /sigmastate/SigmaMapVectors.txt not found on test classpath")
    val lines = try {
      val src = Source.fromInputStream(stream, "UTF-8")
      try src.getLines().toList finally stream.close()
    } catch {
      case e: Exception => throw new RuntimeException("failed to read SigmaMapVectors.txt", e)
    }

    var fullOrder: Array[Byte] = null
    val vectors = Vector.newBuilder[RawVector]
    lines.foreach { raw =>
      val line = raw.trim
      if (line.nonEmpty && !line.startsWith("#")) {
        if (line.startsWith("fullOrder128=")) {
          fullOrder = line.drop("fullOrder128=".length).split(",").map(_.trim.toByte)
        } else {
          val parts = line.split("\\|")
          require(parts.length == 3, s"malformed vector line: $line")
          val size = parts(0).toInt
          val insKeys = parts(1).split(",").map(_.trim.toByte)
          val expected = parts(2).split(",").map(_.trim.toByte)
          require(size == insKeys.length && insKeys.length == expected.length,
            s"malformed vector line: $line")
          vectors += RawVector(size, insKeys, expected)
        }
      }
    }
    require(fullOrder != null, "fullOrder128 line missing")
    Dataset(fullOrder, vectors.result())
  }
}
