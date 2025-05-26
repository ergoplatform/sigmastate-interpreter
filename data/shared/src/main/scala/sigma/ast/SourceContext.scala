package sigma.ast

import fastparse.Parsed.Failure

import scala.io.Source

case class SourceContext(line: Int, column: Int, sourceLine: String) {

}

object SourceContext {

  def fromParserIndex(index: Int, input: String): SourceContext = {
    val lines = Source.fromString(input).getLines.toSeq
    if (lines.isEmpty)
      SourceContext(0, 0, "")
    else
      lines.tail
        .scanLeft((0, lines.head.length)) { case ((_, end), line) => (end + 1, end + 1 + line.length) }
        .zip(lines)
        .zipWithIndex
        .find { case (((start, end), _), _) =>  index >= start && index <= end }
        .map {
          case (((start, _), line), lineIndex) =>
            SourceContext(lineIndex + 1, index - start + 1, line)
        }.getOrElse {
          // at least one line in the input
          // point to the last character of the last line
          val lastLine = lines.last
          val iLine = lines.length - 1
          val iCol = lastLine.length - 1
          SourceContext(iLine, iCol, lastLine)
        }
  }

  def fromParserFailure(e: Failure): SourceContext =
    fromParserIndex(e.index , e.extra.input.slice(0, e.extra.input.length))
}
