package sigma.lsp

import org.eclipse.lsp4j._
import org.eclipse.lsp4j.services.WorkspaceService

/**
 * Handles workspace-level LSP requests.
 * 
 * Currently a minimal implementation that can be extended
 * to support workspace-wide features like:
 * - Configuration changes
 * - File system watching
 * - Workspace symbols
 */
class ErgoScriptWorkspaceService extends WorkspaceService {
  
  /**
   * Handle configuration changes from the client.
   */
  override def didChangeConfiguration(params: DidChangeConfigurationParams): Unit = {
    // TODO: Handle configuration changes (e.g., network prefix, analysis settings)
  }

  /**
   * Handle file system changes.
   */
  override def didChangeWatchedFiles(params: DidChangeWatchedFilesParams): Unit = {
    // TODO: Handle file system changes if needed
  }
}
