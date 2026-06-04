import * as path from 'path';
import * as fs from 'fs';
import { workspace, ExtensionContext, window, commands } from 'vscode';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

export function activate(context: ExtensionContext) {
  console.log('ErgoScript extension is activating...');

  // Find the LSP server JAR
  const serverPath = getServerPath(context);
  
  if (!serverPath) {
    window.showErrorMessage(
      'ErgoScript LSP server JAR not found. Please build the server first:\n' +
      'cd sigmastate-interpreter && sbt "lsp/assembly"'
    );
    return;
  }

  console.log(`Using LSP server at: ${serverPath}`);

  // Find Java executable
  const javaPath = getJavaPath();
  
  if (!javaPath) {
    window.showErrorMessage(
      'Java not found. Please install Java 11+ and ensure it is in your PATH.'
    );
    return;
  }

  console.log(`Using Java at: ${javaPath}`);

  // Server options - spawn the JAR with Java
  const serverOptions: ServerOptions = {
    run: {
      command: javaPath,
      args: ['-jar', serverPath, '--stdio']
    },
    debug: {
      command: javaPath,
      args: ['-jar', serverPath, '--stdio']
    }
  };

  // Client options
  const clientOptions: LanguageClientOptions = {
    documentSelector: [
      { scheme: 'file', language: 'ergoscript' },
      { scheme: 'untitled', language: 'ergoscript' }
    ],
    synchronize: {
      fileEvents: workspace.createFileSystemWatcher('**/*.{es,ergo,ergoscript}')
    },
    outputChannelName: 'ErgoScript Language Server'
  };

  // Create the language client
  client = new LanguageClient(
    'ergoscript',
    'ErgoScript Language Server',
    serverOptions,
    clientOptions
  );

  // Register restart command
  const restartCommand = commands.registerCommand('ergoscript.restartServer', async () => {
    if (client) {
      await client.stop();
      await client.start();
      window.showInformationMessage('ErgoScript Language Server restarted');
    }
  });
  context.subscriptions.push(restartCommand);

  // Start the client
  client.start().then(() => {
    console.log('ErgoScript Language Server started successfully');
    window.showInformationMessage('ErgoScript Language Server is ready!');
  }).catch((error) => {
    console.error('Failed to start ErgoScript Language Server:', error);
    window.showErrorMessage(`Failed to start ErgoScript LSP: ${error.message}`);
  });

  context.subscriptions.push(client);
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}

/**
 * Find the LSP server JAR file
 */
function getServerPath(context: ExtensionContext): string | undefined {
  // 1. Check user configuration
  const config = workspace.getConfiguration('ergoscript');
  const configuredPath = config.get<string>('lsp.serverPath');
  if (configuredPath && fs.existsSync(configuredPath)) {
    return configuredPath;
  }

  // 2. Check extension's server directory
  const bundledServer = path.join(context.extensionPath, 'server', 'ergoscript-lsp.jar');
  if (fs.existsSync(bundledServer)) {
    return bundledServer;
  }

  // 3. Check relative to workspace (development mode)
  const workspaceFolders = workspace.workspaceFolders;
  if (workspaceFolders) {
    for (const folder of workspaceFolders) {
      // Check if we're in the sigmastate-interpreter project
      const devServerPath = path.join(
        folder.uri.fsPath,
        'lsp', 'target', 'scala-2.13', 'ergoscript-lsp.jar'
      );
      if (fs.existsSync(devServerPath)) {
        return devServerPath;
      }
      
      // Also check parent directory (if opened lsp/vscode-extension directly)
      const parentServerPath = path.join(
        folder.uri.fsPath,
        '..', 'target', 'scala-2.13', 'ergoscript-lsp.jar'
      );
      if (fs.existsSync(parentServerPath)) {
        return parentServerPath;
      }
    }
  }

  return undefined;
}

/**
 * Find Java executable
 */
function getJavaPath(): string | undefined {
  // 1. Check user configuration
  const config = workspace.getConfiguration('ergoscript');
  const configuredJavaHome = config.get<string>('java.home');
  if (configuredJavaHome) {
    const javaExe = path.join(configuredJavaHome, 'bin', 'java');
    if (fs.existsSync(javaExe)) {
      return javaExe;
    }
  }

  // 2. Check common macOS Homebrew locations FIRST (since /usr/bin/java often doesn't work on Mac)
  const homebrewLocations = [
    '/opt/homebrew/opt/openjdk/bin/java',           // Apple Silicon
    '/usr/local/opt/openjdk/bin/java',              // Intel Mac
    '/opt/homebrew/opt/openjdk@11/bin/java',
    '/opt/homebrew/opt/openjdk@17/bin/java',
    '/opt/homebrew/opt/openjdk@21/bin/java'
  ];
  
  for (const javaPath of homebrewLocations) {
    if (fs.existsSync(javaPath)) {
      return javaPath;
    }
  }

  // 3. Check JAVA_HOME environment variable
  const javaHome = process.env.JAVA_HOME;
  if (javaHome) {
    const javaExe = path.join(javaHome, 'bin', 'java');
    if (fs.existsSync(javaExe)) {
      return javaExe;
    }
  }

  // 4. Last resort - return undefined to show proper error
  return undefined;
}
