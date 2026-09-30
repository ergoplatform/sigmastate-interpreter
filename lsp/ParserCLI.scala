package sigmastate.lsp

import sigmastate.lang.SigmaParser
import fastparse._
import scala.io.Source
import java.io.{PrintWriter, StringWriter}

/** Simple CLI wrapper around SigmaParser for LSP integration.
  * Reads ErgoScript source from stdin or file, parses it, and outputs diagnostics as JSON.
  */
object ParserCLI {

  case class Diagnostic(
    line: Int,
    column: Int,
    message: String,
    severity: String // "error" or "warning"
  )

  def parseSource(source: String): Either[List[Diagnostic], String] = {
    try {
      // Try to parse as expression
      val exprResult = fastparse.parse(source, SigmaParser.Expr(_))
      exprResult match {
        case Parsed.Success(value, _) =>
          Right(s"Parsed successfully: ${value.getClass.getSimpleName}")
        
        case f @ Parsed.Failure(label, index, extra) =>
          val pos = getLineCol(source, index)
          val diag = Diagnostic(
            line = pos._1,
            column = pos._2,
            message = s"Parse error: expected $label at ${extra.trace().msg}",
            severity = "error"
          )
          Left(List(diag))
      }
    } catch {
      case e: Exception =>
        val diag = Diagnostic(
          line = 0,
          column = 0,
          message = s"Parser exception: ${e.getMessage}",
          severity = "error"
        )
        Left(List(diag))
    }
  }

  def getLineCol(source: String, index: Int): (Int, Int) = {
    val lines = source.take(index).split("\n", -1)
    val line = lines.length - 1
    val col = if (lines.isEmpty) 0 else lines.last.length
    (line, col)
  }

  def diagnosticsToJSON(diags: List[Diagnostic]): String = {
    val diagStrings = diags.map { d =>
      s"""{"line":${d.line},"column":${d.column},"message":"${escapeJSON(d.message)}","severity":"${d.severity}"}"""
    }
    s"""{"diagnostics":[${diagStrings.mkString(",")}]}"""
  }

  def escapeJSON(s: String): String = {
    s.replace("\\", "\\\\")
     .replace("\"", "\\\"")
     .replace("\n", "\\n")
     .replace("\r", "\\r")
     .replace("\t", "\\t")
  }

  def main(args: Array[String]): Unit = {
    val source = if (args.isEmpty) {
      // Read from stdin
      Source.fromInputStream(System.in).mkString
    } else {
      // Read from file
      Source.fromFile(args(0)).mkString
    }

    parseSource(source) match {
      case Left(diagnostics) =>
        println(diagnosticsToJSON(diagnostics))
        System.exit(1)
      
      case Right(success) =>
        println(s"""{"diagnostics":[],"success":"${escapeJSON(success)}"}""")
        System.exit(0)
    }
  }
}
