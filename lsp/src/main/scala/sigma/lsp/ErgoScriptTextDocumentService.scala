package sigma.lsp

import org.eclipse.lsp4j._
import org.eclipse.lsp4j.services.TextDocumentService
import org.eclipse.lsp4j.jsonrpc.messages.{Either => JEither}
import java.util.concurrent.CompletableFuture
import scala.jdk.CollectionConverters._

/**
 * Handles text document related LSP requests.
 * 
 * Implements:
 * - Document synchronization (open, change, close, save)
 * - Completion (autocomplete)
 * - Hover (documentation on mouse hover)
 * - Definition (go to definition)
 * - Signature help (function parameter hints)
 */
class ErgoScriptTextDocumentService(server: ErgoScriptLanguageServer) extends TextDocumentService {

  // ============ Document Sync ============

  override def didOpen(params: DidOpenTextDocumentParams): Unit = {
    val uri = params.getTextDocument.getUri
    val content = params.getTextDocument.getText
    server.updateDocument(uri, content)
  }

  override def didChange(params: DidChangeTextDocumentParams): Unit = {
    val uri = params.getTextDocument.getUri
    // We use Full sync, so there's only one change with the full content
    val changes = params.getContentChanges
    if (!changes.isEmpty) {
      val content = changes.get(0).getText
      server.updateDocument(uri, content)
    }
  }

  override def didClose(params: DidCloseTextDocumentParams): Unit = {
    server.removeDocument(params.getTextDocument.getUri)
  }

  override def didSave(params: DidSaveTextDocumentParams): Unit = {
    // Re-check on save if text is provided
    val text = params.getText
    if (text != null) {
      server.updateDocument(params.getTextDocument.getUri, text)
    }
  }

  // ============ Completion (Autocomplete) ============

  override def completion(params: CompletionParams): CompletableFuture[JEither[java.util.List[CompletionItem], CompletionList]] = {
    CompletableFuture.supplyAsync { () =>
      val uri = params.getTextDocument.getUri
      val line = params.getPosition.getLine
      val character = params.getPosition.getCharacter

      val content = server.getDocument(uri).getOrElse("")
      val prefix = getPrefix(content, line, character)

      val items = CompletionProvider.getCompletions(prefix, line, character)
      JEither.forLeft(items.asJava)
    }
  }

  /**
   * Extract the prefix before cursor for completion.
   * Handles cases like "SELF.", "INPUTS(0).", etc.
   */
  private def getPrefix(content: String, line: Int, character: Int): String = {
    val lines = content.split("\n", -1)
    if (line < lines.length) {
      val currentLine = lines(line)
      if (character <= currentLine.length) {
        val beforeCursor = currentLine.substring(0, character)
        
        // Check if we're after a dot (member access)
        val dotIndex = beforeCursor.lastIndexOf('.')
        if (dotIndex >= 0 && dotIndex == beforeCursor.length - 1) {
          // Cursor is right after a dot, find the identifier before it
          val beforeDot = beforeCursor.substring(0, dotIndex).trim
          val word = extractLastIdentifier(beforeDot)
          s"$word."
        } else if (dotIndex >= 0) {
          // We're typing after a dot
          val beforeDot = beforeCursor.substring(0, dotIndex).trim
          val word = extractLastIdentifier(beforeDot)
          val afterDot = beforeCursor.substring(dotIndex + 1)
          s"$word.$afterDot"
        } else {
          // Top-level completion
          extractLastIdentifier(beforeCursor)
        }
      } else ""
    } else ""
  }

  /**
   * Extract the last identifier from a string.
   */
  private def extractLastIdentifier(s: String): String = {
    val pattern = """([a-zA-Z_][a-zA-Z0-9_]*)$""".r
    pattern.findFirstIn(s).getOrElse("")
  }

  // ============ Hover ============

  override def hover(params: HoverParams): CompletableFuture[Hover] = {
    CompletableFuture.supplyAsync { () =>
      val uri = params.getTextDocument.getUri
      val line = params.getPosition.getLine
      val character = params.getPosition.getCharacter

      val content = server.getDocument(uri).getOrElse("")
      val word = getWordAtPosition(content, line, character)

      HoverProvider.getHover(word).orNull
    }
  }

  /**
   * Get the word at the given position.
   */
  private def getWordAtPosition(content: String, line: Int, character: Int): String = {
    val lines = content.split("\n", -1)
    if (line < lines.length) {
      val currentLine = lines(line)
      val wordChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"
      
      // Find word start
      var start = math.min(character, currentLine.length - 1)
      if (start < 0) return ""
      
      while (start > 0 && wordChars.contains(currentLine(start - 1))) {
        start -= 1
      }
      
      // Find word end
      var end = character
      while (end < currentLine.length && wordChars.contains(currentLine(end))) {
        end += 1
      }
      
      if (start < end && start < currentLine.length) {
        currentLine.substring(start, math.min(end, currentLine.length))
      } else ""
    } else ""
  }

  // ============ Definition ============

  override def definition(params: DefinitionParams): CompletableFuture[JEither[java.util.List[_ <: Location], java.util.List[_ <: LocationLink]]] = {
    CompletableFuture.supplyAsync { () =>
      val uri = params.getTextDocument.getUri
      val line = params.getPosition.getLine
      val character = params.getPosition.getCharacter

      val content = server.getDocument(uri).getOrElse("")
      val word = getWordAtPosition(content, line, character)

      val locations = DefinitionProvider.getDefinition(content, word, uri)
      JEither.forLeft(locations.asJava)
    }
  }

  // ============ Signature Help ============

  override def signatureHelp(params: SignatureHelpParams): CompletableFuture[SignatureHelp] = {
    CompletableFuture.supplyAsync { () =>
      val uri = params.getTextDocument.getUri
      val line = params.getPosition.getLine
      val character = params.getPosition.getCharacter

      val content = server.getDocument(uri).getOrElse("")
      
      SignatureProvider.getSignatureHelp(content, line, character).orNull
    }
  }
}
