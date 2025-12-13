/**
 * VS Code Extension for ErgoScript
 * 
 * Activates the language client and connects to the language server.
 */

import * as path from 'path';
import { workspace, ExtensionContext } from 'vscode';

import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
    // The server is implemented in node
    const serverModule = context.asAbsolutePath(
        path.join('..', 'server', 'out', 'server.js')
    );

    // The debug options for the server
    const debugOptions = { execArgv: ['--nolazy', '--inspect=6009'] };

    // If the extension is launched in debug mode then the debug server options are used
    // Otherwise the run options are used
    const serverOptions: ServerOptions = {
        run: { module: serverModule, transport: TransportKind.ipc },
        debug: {
            module: serverModule,
            transport: TransportKind.ipc,
            options: debugOptions
        }
    };

    // Options to control the language client
    const clientOptions: LanguageClientOptions = {
        // Register the server for ErgoScript documents
        documentSelector: [{ scheme: 'file', language: 'ergoscript' }],
        synchronize: {
            // Notify the server about file changes to '.es' files contained in the workspace
            fileEvents: workspace.createFileSystemWatcher('**/.es')
        }
    };

    // Create the language client and start the client.
    client = new LanguageClient(
        'ergoscriptLanguageServer',
        'ErgoScript Language Server',
        serverOptions,
        clientOptions
    );

    // Start the client. This will also launch the server
    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
