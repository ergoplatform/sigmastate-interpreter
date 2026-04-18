package sigma.lsp

import org.eclipse.lsp4j.launch.LSPLauncher
import java.io.{InputStream, OutputStream}

/**
 * Main entry point for the ErgoScript Language Server.
 * 
 * Supports multiple modes:
 * - `--stdio`: Start LSP server communicating via stdin/stdout (for editors)
 * - `--check <file>`: Check a file for errors (CLI mode)
 * - `--completions <prefix>`: Show completions for a prefix (CLI mode)
 * - `--help`: Show usage information
 */
object Main {
  
  val VERSION = "0.1.0"
  
  def main(args: Array[String]): Unit = {
    args.toList match {
      case "--stdio" :: Nil => 
        startServer(System.in, System.out)
        
      case "--check" :: file :: Nil =>
        checkFile(file)
        
      case "--completions" :: prefix :: Nil =>
        showCompletions(prefix)
        
      case "--hover" :: word :: Nil =>
        showHover(word)
        
      case "--version" :: Nil =>
        println(s"ErgoScript Language Server v$VERSION")
        
      case "--help" :: Nil | Nil =>
        printHelp()
        
      case unknown =>
        System.err.println(s"Unknown arguments: ${unknown.mkString(" ")}")
        System.err.println("Use --help for usage information")
        System.exit(1)
    }
  }

  /**
   * Start the LSP server for communication with editors.
   * Uses stdin/stdout for JSON-RPC communication.
   */
  def startServer(in: InputStream, out: OutputStream): Unit = {
    val server = new ErgoScriptLanguageServer()
    val launcher = LSPLauncher.createServerLauncher(server, in, out)
    
    val client = launcher.getRemoteProxy
    server.connect(client)
    
    // Block until shutdown
    launcher.startListening().get()
  }

  /**
   * Check a file for syntax/type errors.
   * Exits with code 1 if errors are found.
   */
  def checkFile(path: String): Unit = {
    val file = new java.io.File(path)
    if (!file.exists()) {
      System.err.println(s"Error: File not found: $path")
      System.exit(1)
    }
    
    val source = scala.io.Source.fromFile(path)
    val code = try source.mkString finally source.close()
    
    val diagnostics = DiagnosticsProvider.getDiagnostics(code, path)
    
    if (diagnostics.isEmpty) {
      println(s"✓ $path: No errors found")
    } else {
      println(s"✗ $path: ${diagnostics.size} error(s) found")
      diagnostics.foreach { d =>
        val line = d.getRange.getStart.getLine + 1
        val col = d.getRange.getStart.getCharacter + 1
        val severity = d.getSeverity.toString.toLowerCase
        println(s"  [$severity] Line $line:$col - ${d.getMessage}")
      }
      System.exit(1)
    }
  }

  /**
   * Show available completions for a given prefix.
   */
  def showCompletions(prefix: String): Unit = {
    println(s"Completions for '$prefix':")
    println("-" * 50)
    
    val completions = CompletionProvider.getCompletions(prefix, 0, 0)
    
    if (completions.isEmpty) {
      println("  (no completions found)")
    } else {
      completions.foreach { c =>
        val kind = c.getKind.toString.toLowerCase
        val detail = Option(c.getDetail).getOrElse("")
        println(f"  ${c.getLabel}%-25s [$kind%-10s] $detail")
      }
    }
  }

  /**
   * Show hover documentation for a word.
   */
  def showHover(word: String): Unit = {
    HoverProvider.getHover(word) match {
      case Some(hover) =>
        println(hover.getContents.getRight.getValue)
      case None =>
        println(s"No documentation found for '$word'")
    }
  }

  def printHelp(): Unit = {
    println(s"""
      |ErgoScript Language Server v$VERSION
      |
      |A Language Server Protocol implementation for ErgoScript,
      |the smart contract language of the Ergo blockchain.
      |
      |USAGE:
      |  ergoscript-lsp [OPTIONS]
      |
      |OPTIONS:
      |  --stdio              Start LSP server (for editor integration)
      |  --check <file>       Check file for errors
      |  --completions <prefix>  Show completions for prefix
      |  --hover <word>       Show documentation for word
      |  --version            Show version information
      |  --help               Show this help message
      |
      |EXAMPLES:
      |  # Start LSP server for VS Code
      |  java -jar ergoscript-lsp.jar --stdio
      |
      |  # Check a file for errors
      |  java -jar ergoscript-lsp.jar --check mycontract.es
      |
      |  # See Box methods
      |  java -jar ergoscript-lsp.jar --completions "SELF."
      |
      |For more information, visit:
      |  https://github.com/ScorexFoundation/sigmastate-interpreter
    """.stripMargin)
  }
}
