package sigma.stark.profile

import java.io.File
import java.nio.ByteBuffer
import java.nio.charset.{CodingErrorAction, StandardCharsets}
import java.nio.file.Files
import java.security.MessageDigest

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Keeps the published RISC0 provenance map synchronized with every production
  * Scala source that it claims to cover.
  */
class Risc0SourceMapSpec extends AnyFunSuite with Matchers {

  private val RepositoryRoot = new File(".").getCanonicalFile
  private val StarkSourceRoot =
    new File(RepositoryRoot, "core/shared/src/main/scala/sigma/stark")
  private val SourceMap =
    new File(RepositoryRoot, "docs/eip-0045-risc0-source-map.json")

  private val Entry =
    """(?s)\{\s*"path"\s*:\s*"([^"]+)"\s*,\s*"sha256"\s*:\s*"([0-9a-f]{64})""".r

  private def canonicalUtf8(file: File): (Array[Byte], String) = {
    val bytes = Files.readAllBytes(file.toPath)
    withClue(file.getPath + ": ") {
      (bytes.length < 3 ||
        bytes(0) != 0xef.toByte ||
        bytes(1) != 0xbb.toByte ||
        bytes(2) != 0xbf.toByte) shouldBe true
      bytes.contains(13.toByte) shouldBe false
    }
    val decoder = StandardCharsets.UTF_8.newDecoder()
      .onMalformedInput(CodingErrorAction.REPORT)
      .onUnmappableCharacter(CodingErrorAction.REPORT)
    val text = decoder.decode(ByteBuffer.wrap(bytes)).toString
    java.util.Arrays.equals(text.getBytes(StandardCharsets.UTF_8), bytes) shouldBe true
    (bytes, text)
  }

  private def sha256Hex(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(bytes)
      .map(b => f"${b & 0xff}%02x").mkString

  private def productionScalaFiles(dir: File): Seq[File] = {
    val children = Option(dir.listFiles()).getOrElse(Array.empty[File])
    children.toSeq.flatMap { child =>
      if (child.isDirectory) productionScalaFiles(child)
      else if (child.isFile && child.getName.endsWith(".scala")) Seq(child)
      else Seq.empty
    }
  }

  private def repositoryPath(file: File): String =
    RepositoryRoot.toPath.relativize(file.getCanonicalFile.toPath)
      .toString.replace('\\', '/')

  test("source map covers and authenticates every production STARK Scala file") {
    val (_, sourceMapText) = canonicalUtf8(SourceMap)
    val filesStart = sourceMapText.indexOf("\"files\"")
    val evidenceStart = sourceMapText.indexOf("\"evidence\"", filesStart)
    filesStart should be >= 0
    evidenceStart should be > filesStart

    val entries = Entry.findAllMatchIn(
      sourceMapText.substring(filesStart, evidenceStart)).map { m =>
      m.group(1) -> m.group(2)
    }.toSeq
    entries should have size 18
    entries.map(_._1).distinct should have size entries.size

    val actualPaths = productionScalaFiles(StarkSourceRoot)
      .map(repositoryPath).sorted
    entries.map(_._1).sorted shouldBe actualPaths

    entries.foreach { case (path, expectedDigest) =>
      val file = new File(RepositoryRoot, path)
      file.isFile shouldBe true
      val (bytes, _) = canonicalUtf8(file)
      withClue(path + ": ") {
        sha256Hex(bytes) shouldBe expectedDigest
      }
    }
  }

  test("source repositories declare the licenses governing adapted code") {
    val (_, sourceMapText) = canonicalUtf8(SourceMap)
    sourceMapText should include("\"risc0\"")
    sourceMapText should include("\"license\": \"Apache-2.0\"")
    sourceMapText should include("\"arkadia\"")
    sourceMapText should include("\"license\": \"MIT\"")
  }
}
