package sigma.lsp

import org.eclipse.lsp4j._
import sigma.compiler.{CompilerSettings, SigmaCompiler}
import sigma.ast.{SourceContext, TransformingSigmaBuilder}
import sigma.exceptions.{CompilerException, BinderException, TyperException, BuilderException, GraphBuildingException}
import sigmastate.lang.parsers.ParserException
import sigmastate.interpreter.Interpreter.ScriptEnv

import scala.util.{Try, Success, Failure}

/**
 * Provides diagnostic information (errors, warnings) for ErgoScript code.
 * 
 * Uses the existing SigmaCompiler to parse and typecheck code,
 * then converts any exceptions to LSP Diagnostic objects.
 */
object DiagnosticsProvider {

  // Use mainnet prefix by default (0x00), can be made configurable
  private val NetworkPrefix: Byte = 0x00
  
  // Empty environment for compilation
  private val EmptyEnv: ScriptEnv = Map.empty

  /**
   * Get diagnostics (errors) for the given ErgoScript code.
   * 
   * @param code The ErgoScript source code
   * @param uri The document URI (for error reporting)
   * @return List of Diagnostic objects representing any errors
   */
  def getDiagnostics(code: String, uri: String): List[Diagnostic] = {
    if (code.trim.isEmpty) {
      return List.empty
    }

    // Create a fresh compiler instance
    val settings = CompilerSettings(
      networkPrefix = NetworkPrefix,
      builder = TransformingSigmaBuilder,
      lowerMethodCalls = true
    )
    val compiler = SigmaCompiler(settings)

    Try {
      // Just typecheck - we don't need the full compilation for diagnostics
      compiler.typecheck(EmptyEnv, code)
    } match {
      case Success(_) => 
        List.empty // No errors
        
      case Failure(e: ParserException) =>
        List(createDiagnostic(
          cleanMessage(e.getMessage),
          e.source,
          DiagnosticSeverity.Error,
          "parser"
        ))
        
      case Failure(e: BinderException) =>
        List(createDiagnostic(
          cleanMessage(e.getMessage),
          e.source,
          DiagnosticSeverity.Error,
          "binder"
        ))
        
      case Failure(e: TyperException) =>
        List(createDiagnostic(
          cleanMessage(e.getMessage),
          e.source,
          DiagnosticSeverity.Error,
          "typer"
        ))
        
      case Failure(e: BuilderException) =>
        List(createDiagnostic(
          cleanMessage(e.getMessage),
          e.source,
          DiagnosticSeverity.Error,
          "builder"
        ))
        
      case Failure(e: GraphBuildingException) =>
        List(createDiagnostic(
          cleanMessage(e.getMessage),
          e.source,
          DiagnosticSeverity.Error,
          "graph-builder"
        ))
        
      case Failure(e: CompilerException) =>
        List(createDiagnostic(
          cleanMessage(e.getMessage),
          e.source,
          DiagnosticSeverity.Error,
          "compiler"
        ))
        
      case Failure(e) =>
        // Unknown error - show at the beginning of the file
        val range = new Range(new Position(0, 0), new Position(0, 10))
        List(new Diagnostic(
          range,
          s"Error: ${e.getMessage}",
          DiagnosticSeverity.Error,
          "ergoscript"
        ))
    }
  }

  /**
   * Create a Diagnostic from compiler exception info.
   */
  private def createDiagnostic(
    message: String,
    source: Option[SourceContext],
    severity: DiagnosticSeverity,
    errorSource: String
  ): Diagnostic = {
    val range = source match {
      case Some(ctx) =>
        // LSP uses 0-based line/column, SourceContext uses 1-based
        val startLine = math.max(0, ctx.line - 1)
        val startCol = math.max(0, ctx.column - 1)
        // Estimate end column based on source line
        val endCol = math.min(startCol + 20, ctx.sourceLine.length)
        new Range(
          new Position(startLine, startCol),
          new Position(startLine, endCol)
        )
      case None =>
        new Range(new Position(0, 0), new Position(0, 10))
    }
    
    val diagnostic = new Diagnostic(range, message, severity, "ergoscript")
    diagnostic.setCode(errorSource)
    diagnostic
  }

  /**
   * Clean up error messages for display.
   * Removes excessive formatting that's already in the SourceContext.
   */
  private def cleanMessage(message: String): String = {
    // The CompilerException.getMessage includes line info and source line
    // We want to extract just the core message for the LSP diagnostic
    val lines = message.split("\n").toList
    
    // Try to find the actual error message (usually after the source line indicator)
    lines.filterNot { line =>
      line.trim.isEmpty ||
      line.trim.startsWith("line ") ||
      line.trim.matches("^\\^+$") ||
      line.trim.matches("^\\s*\\^\\s*$")
    }.headOption.getOrElse(message)
  }
}
