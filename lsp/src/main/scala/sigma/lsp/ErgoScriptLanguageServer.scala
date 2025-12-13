package sigma.lsp

import org.eclipse.lsp4j._
import org.eclipse.lsp4j.services._
import java.util.concurrent.CompletableFuture
import scala.collection.mutable
import scala.jdk.CollectionConverters._

/**
 * ErgoScript Language Server implementation.
 * * Provides language intelligence features for ErgoScript:
 * - Syntax and type error diagnostics
 * - Autocomplete for global variables, functions, and methods
 * - Hover documentation
 * - Go to definition
 * - Signature help
 */
class ErgoScriptLanguageServer extends LanguageServer {
  
  private var client: LanguageClient = _
  private val documents = mutable.Map[String, String]()
  private lazy val textDocumentService = new ErgoScriptTextDocumentService(this)
  private lazy val workspaceService = new ErgoScriptWorkspaceService()

  /**
   * Connect to the language client (editor).
   */
  def connect(languageClient: LanguageClient): Unit = {
    this.client = languageClient
  }

  /**
   * Get the connected client for sending notifications.
   */
  def getClient: LanguageClient = client

  /**
   * Get document content by URI.
   */
  def getDocument(uri: String): Option[String] = documents.get(uri)

  /**
   * Update document content and publish diagnostics.
   */
  def updateDocument(uri: String, content: String): Unit = {
    documents(uri) = content
    publishDiagnostics(uri, content)
  }

  /**
   * Remove document from tracking.
   */
  def removeDocument(uri: String): Unit = {
    documents.remove(uri)
    // Clear diagnostics when document is closed
    if (client != null) {
      client.publishDiagnostics(
        new PublishDiagnosticsParams(uri, java.util.Collections.emptyList())
      )
    }
  }

  /**
   * Publish diagnostics (errors/warnings) for a document.
   */
  private def publishDiagnostics(uri: String, content: String): Unit = {
    if (client != null) {
      val diagnostics = DiagnosticsProvider.getDiagnostics(content, uri)
      client.publishDiagnostics(
        new PublishDiagnosticsParams(uri, diagnostics.asJava)
      )
    }
  }

  /**
   * Initialize the language server.
   * Returns server capabilities to inform the client what features are supported.
   */
  override def initialize(params: InitializeParams): CompletableFuture[InitializeResult] = {
    val capabilities = new ServerCapabilities()
    
    // Document sync - get notified of file open/change/close
    capabilities.setTextDocumentSync(TextDocumentSyncKind.Full)
    
    // Autocomplete
    val completionOptions = new CompletionOptions()
    completionOptions.setTriggerCharacters(java.util.Arrays.asList(".", "(", "["))
    completionOptions.setResolveProvider(false)
    capabilities.setCompletionProvider(completionOptions)
    
    // Hover documentation
    capabilities.setHoverProvider(true)
    
    // Go to definition
    capabilities.setDefinitionProvider(true)
    
    // Signature help (function parameter hints)
    val signatureOptions = new SignatureHelpOptions()
    signatureOptions.setTriggerCharacters(java.util.Arrays.asList("(", ","))
    capabilities.setSignatureHelpProvider(signatureOptions)

    val serverInfo = new ServerInfo("ErgoScript Language Server", Main.VERSION)
    
    CompletableFuture.completedFuture(new InitializeResult(capabilities, serverInfo))
  }

  /**
   * Called after initialization is complete.
   */
  override def initialized(params: InitializedParams): Unit = {
    // Server is ready
  }

  /**
   * Shutdown the server.
   */
  override def shutdown(): CompletableFuture[Object] = {
    CompletableFuture.completedFuture(null)
  }

  /**
   * Exit the server process.
   */
  override def exit(): Unit = {
    System.exit(0)
  }

  /**
   * FIX: Explicitly override cancelProgress to prevent "Duplicate RPC method" error.
   * This resolves a conflict between the Java default method and Scala's generated bridge method.
   */
  override def cancelProgress(params: WorkDoneProgressCancelParams): Unit = {
    // No-op implementation is fine
  }

  override def getTextDocumentService: TextDocumentService = textDocumentService

  override def getWorkspaceService: WorkspaceService = workspaceService
}