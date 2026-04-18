/**
 * ErgoScript Language Server
 * MVP implementation for IDE and AI-agent support
 */

import {
    createConnection,
    TextDocuments,
    ProposedFeatures,
    InitializeParams,
    TextDocumentSyncKind,
    InitializeResult,
    HoverParams
} from 'vscode-languageserver/node';

import { TextDocument } from 'vscode-languageserver-textdocument';
import { validateDocument } from './diagnostics';
import { provideHover } from './hover';
import { tokenize } from './parser';

// Create LSP connection
const connection = createConnection(ProposedFeatures.all);

// Document manager
const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

// Server initialization
connection.onInitialize((params: InitializeParams): InitializeResult => {
    connection.console.log('ErgoScript Language Server initializing...');

    return {
        capabilities: {
            textDocumentSync: TextDocumentSyncKind.Incremental,
            hoverProvider: true
        }
    };
});

connection.onInitialized(() => {
    connection.console.log('ErgoScript Language Server initialized');
});

// Document change handling
documents.onDidChangeContent(change => {
    validateDocumentAndSendDiagnostics(change.document);

    // For AI agent support: log tokenized output
    const tokens = tokenize(change.document.getText());
    connection.console.log(`Document tokenized: ${tokens.length} tokens`);

    // Output structured token data for AI agents (can be consumed via LSP or logs)
    if (tokens.length > 0) {
        const structuredOutput = {
            uri: change.document.uri,
            version: change.document.version,
            tokens: tokens.map(t => ({
                type: t.type,
                value: t.value,
                position: { line: t.line, column: t.column }
            }))
        };

        // This structured output can be consumed by AI agents
        connection.console.log(`Structured analysis: ${JSON.stringify(structuredOutput, null, 2)}`);
    }
});

// Document open handling
documents.onDidOpen(event => {
    connection.console.log(`Document opened: ${event.document.uri}`);
    validateDocumentAndSendDiagnostics(event.document);
});

// Validate document and send diagnostics
function validateDocumentAndSendDiagnostics(document: TextDocument): void {
    const diagnostics = validateDocument(document);
    connection.sendDiagnostics({ uri: document.uri, diagnostics });
}

// Hover provider
connection.onHover((params: HoverParams) => {
    const document = documents.get(params.textDocument.uri);
    if (!document) {
        return null;
    }

    const source = document.getText();
    const hover = provideHover(source, params.position.line, params.position.character);

    return hover;
});

// Document close handling
documents.onDidClose(event => {
    connection.sendDiagnostics({ uri: event.document.uri, diagnostics: [] });
});

// Listen on the connection
documents.listen(connection);
connection.listen();

connection.console.log('ErgoScript Language Server started');
