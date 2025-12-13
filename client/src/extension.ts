import * as path from "path";
import { ExtensionContext, window, workspace } from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

let client: LanguageClient;

export function activate(context: ExtensionContext) {
  console.log("ErgoScript LSP Extension activated");

  // Path to the language server executable
  const serverModule = context.asAbsolutePath(
    path.join("../server", "dist", "server.js")
  );

  // If the extension is launched in debug mode then the debug server options are used
  // Otherwise the run options are used
  const serverOptions: ServerOptions = {
    run: { module: serverModule, transport: TransportKind.ipc },
    debug: {
      module: serverModule,
      transport: TransportKind.ipc,
      options: { execArgv: ["--nolazy", "--inspect=6009"] },
    },
  };

  // Options to control the language client
  const clientOptions: LanguageClientOptions = {
    // Register the server for ErgoScript files
    documentSelector: [{ scheme: "file", language: "ergoscript" }],
    synchronize: {
      // Notify the server about file changes to .es files contained in the workspace
      fileEvents: workspace.createFileSystemWatcher("**/.{es}"),
    },
  };

  // Create the language client and start the server
  client = new LanguageClient(
    "ergoscriptLanguageServer",
    "ErgoScript Language Server",
    serverOptions,
    clientOptions
  );

  // Start the client. This will also launch the server process
  client.start();
  console.log("ErgoScript LSP started");

  context.subscriptions.push(client);
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
