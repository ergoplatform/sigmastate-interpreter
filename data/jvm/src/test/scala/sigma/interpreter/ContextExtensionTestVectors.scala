package sigma.interpreter

import scorex.util.encode.Base16
import sigma.ast.IntConstant

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

/**
 * Dumps current ContextExtension serialization behaviour to a JSON file, labelled by the
 * running Scala version. Intended as a cross-implementation conformance artefact for sigma-rust.
 *
 * Run with:
 *   sbt "++2.12.21 dataJVM/Test/runMain sigma.interpreter.ContextExtensionTestVectors"
 *
 * Output: test-vectors/context-extension/{scalaMajor}.json
 *
 * Fixture values follow the convention IntConstant(key.toInt), so sigma-rust can reproduce
 * the same logical input without ambiguity about Scala value encoding.
 */
object ContextExtensionTestVectors {

  final case class Fixture(name: String, keys: Seq[Byte])

  val fixtures: Seq[Fixture] = Seq(
    Fixture("size-0",             Seq.empty),
    Fixture("size-1",             Seq[Byte](0)),
    Fixture("size-2",             Seq[Byte](0, 1)),
    Fixture("size-3",             Seq[Byte](0, 1, 2)),
    Fixture("size-4",             Seq[Byte](0, 1, 2, 3)),
    Fixture("size-5-contiguous",  Seq[Byte](0, 1, 2, 3, 4)),
    Fixture("size-6-contiguous",  Seq[Byte](0, 1, 2, 3, 4, 5)),
    Fixture("size-7-contiguous",  Seq[Byte](0, 1, 2, 3, 4, 5, 6)),
    Fixture("size-8-contiguous",  Seq[Byte](0, 1, 2, 3, 4, 5, 6, 7)),
    Fixture("size-16-contiguous", (0 until 16).map(_.toByte)),
    Fixture("size-8-sparse",      Seq[Byte](0, 7, 13, 31, 64, 100, 120, 127))
  )

  def renderFixture(f: Fixture): String = {
    val pairs = f.keys.map(k => k -> IntConstant(k.toInt))
    val ce = ContextExtension(pairs.toMap)
    val mapClass = ce.values.getClass.getName
    val observed = ce.values.keys.toList.map(_.toInt)
    val hex = Base16.encode(ContextExtension.serializer.toBytes(ce))
    val inputKeys = f.keys.map(_.toInt).mkString("[", ", ", "]")
    s"""    {
       |      "name": "${f.name}",
       |      "inputKeys": $inputKeys,
       |      "mapClass": "$mapClass",
       |      "observedIterationOrder": ${observed.mkString("[", ", ", "]")},
       |      "serializedHex": "$hex"
       |    }""".stripMargin
  }

  def main(args: Array[String]): Unit = {
    val scalaFull = scala.util.Properties.versionNumberString
    val scalaMajor = scalaFull.split('.').take(2).mkString(".")
    val body = fixtures.map(renderFixture).mkString(",\n")
    val json =
      s"""{
         |  "scalaVersion": "$scalaFull",
         |  "valueConvention": "IntConstant(key.toInt)",
         |  "fixtures": [
         |$body
         |  ]
         |}
         |""".stripMargin

    val out = Paths.get(s"test-vectors/context-extension/$scalaMajor.json")
    Option(out.getParent).foreach(Files.createDirectories(_))
    Files.write(out, json.getBytes(StandardCharsets.UTF_8))
    println(s"Wrote $out (${json.length} chars)")
  }
}
