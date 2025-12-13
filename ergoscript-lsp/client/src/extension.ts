/**
 * VS Code Extension for ErgoScript LSP
 * Activates the language server when .es files are opened
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
    console.log('ErgoScript extension activating...');

    // Server module path
    const serverModule = context.asAbsolutePath(
        path.join('..', 'server', 'dist', 'index.js')
    );

    // Server options
    const serverOptions: ServerOptions = {
        run: {
            module: serverModule,
            transport: TransportKind.ipc
        },
        debug: {
            module: serverModule,
            transport: TransportKind.ipc,
            options: {
                execArgv: ['--nolazy', '--inspect=6009']
            }
        }
    };

    // Client options
    const clientOptions: LanguageClientOptions = {
        documentSelector: [
            { scheme: 'file', language: 'ergoscript' }
        ],
        synchronize: {
            fileEvents: workspace.createFileSystemWatcher('**/*.{es,ergo}')
        }
    };

    // Create and start the language client
    client = new LanguageClient(
        'ergoscriptLanguageServer',
        'ErgoScript Language Server',
        serverOptions,
        clientOptions
    );

    // Start the client (also starts the server)
    client.start();

    console.log('ErgoScript extension activated');
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
