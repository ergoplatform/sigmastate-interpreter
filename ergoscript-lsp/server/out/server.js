"use strict";
/**
 * ErgoScript Language Server
 *
 * Provides IDE features for ErgoScript:
 * - Autocomplete
 * - Diagnostics (error checking)
 * - Hover information
 * - Syntax highlighting
 */
Object.defineProperty(exports, "__esModule", { value: true });
const node_1 = require("vscode-languageserver/node");
const vscode_languageserver_textdocument_1 = require("vscode-languageserver-textdocument");
const builtins_1 = require("./utils/builtins");
// Create a connection for the server
const connection = (0, node_1.createConnection)(node_1.ProposedFeatures.all);
// Create a simple text document manager
const documents = new node_1.TextDocuments(vscode_languageserver_textdocument_1.TextDocument);
let hasConfigurationCapability = false;
let hasWorkspaceFolderCapability = false;
let hasDiagnosticRelatedInformationCapability = false;
connection.onInitialize((params) => {
    const capabilities = params.capabilities;
    hasConfigurationCapability = !!(capabilities.workspace && !!capabilities.workspace.configuration);
    hasWorkspaceFolderCapability = !!(capabilities.workspace && !!capabilities.workspace.workspaceFolders);
    hasDiagnosticRelatedInformationCapability = !!(capabilities.textDocument &&
        capabilities.textDocument.publishDiagnostics &&
        capabilities.textDocument.publishDiagnostics.relatedInformation);
    const result = {
        capabilities: {
            textDocumentSync: node_1.TextDocumentSyncKind.Incremental,
            completionProvider: {
                resolveProvider: true,
                triggerCharacters: ['.', '(']
            },
            hoverProvider: true
        }
    };
    if (hasWorkspaceFolderCapability) {
        result.capabilities.workspace = {
            workspaceFolders: {
                supported: true
            }
        };
    }
    return result;
});
connection.onInitialized(() => {
    if (hasConfigurationCapability) {
        connection.client.register(node_1.DidChangeConfigurationNotification.type, undefined);
    }
    if (hasWorkspaceFolderCapability) {
        connection.workspace.onDidChangeWorkspaceFolders(_event => {
            connection.console.log('Workspace folder change event received.');
        });
    }
});
// Document settings cache
const documentSettings = new Map();
connection.onDidChangeConfiguration(change => {
    if (hasConfigurationCapability) {
        documentSettings.clear();
    }
    else {
        // Global settings changed
    }
    // Revalidate all open text documents
    documents.all().forEach(validateTextDocument);
});
// Only keep settings for open documents
documents.onDidClose(e => {
    documentSettings.delete(e.document.uri);
});
// The content of a text document has changed
documents.onDidChangeContent(change => {
    validateTextDocument(change.document);
});
async function validateTextDocument(textDocument) {
    const text = textDocument.getText();
    const diagnostics = [];
    // Simple validation: check for common errors
    const lines = text.split('\n');
    for (let i = 0; i < lines.length; i++) {
        const line = lines[i];
        // Check for undefined variables (simple heuristic)
        const varMatch = /\b([a-z][a-zA-Z0-9_]*)\b/g;
        let match;
        while ((match = varMatch.exec(line)) !== null) {
            const varName = match[1];
            // Skip keywords and built-ins
            if (builtins_1.ERGOSCRIPT_KEYWORDS.includes(varName) ||
                builtins_1.ERGOSCRIPT_BUILTINS.some(b => b.name === varName) ||
                builtins_1.CONTEXT_VARIABLES.some(v => v.name === varName)) {
                continue;
            }
            // Check if variable is defined earlier in the document
            const beforeText = text.substring(0, textDocument.offsetAt({ line: i, character: match.index }));
            const valPattern = new RegExp(`\\bval\\s+${varName}\\b`);
            const defPattern = new RegExp(`\\bdef\\s+${varName}\\b`);
            if (!valPattern.test(beforeText) && !defPattern.test(beforeText)) {
                // Might be undefined - create warning
                const diagnostic = {
                    severity: node_1.DiagnosticSeverity.Warning,
                    range: {
                        start: { line: i, character: match.index },
                        end: { line: i, character: match.index + varName.length }
                    },
                    message: `'${varName}' might be undefined`,
                    source: 'ergoscript'
                };
                diagnostics.push(diagnostic);
            }
        }
        // Check for type mismatches (simple check)
        if (line.includes('val') && line.includes(':') && line.includes('=')) {
            const typeAnnotation = line.match(/:\s*(\w+)\s*=/);
            if (typeAnnotation) {
                const declaredType = typeAnnotation[1];
                // Check if it's a valid type
                if (!builtins_1.ERGOSCRIPT_TYPES.some(t => t.name === declaredType)) {
                    const index = line.indexOf(declaredType);
                    const diagnostic = {
                        severity: node_1.DiagnosticSeverity.Error,
                        range: {
                            start: { line: i, character: index },
                            end: { line: i, character: index + declaredType.length }
                        },
                        message: `Unknown type '${declaredType}'`,
                        source: 'ergoscript'
                    };
                    diagnostics.push(diagnostic);
                }
            }
        }
    }
    connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });
}
// Autocomplete
connection.onCompletion((_textDocumentPosition) => {
    const completions = [];
    // Add keywords
    builtins_1.ERGOSCRIPT_KEYWORDS.forEach(keyword => {
        completions.push({
            label: keyword,
            kind: node_1.CompletionItemKind.Keyword,
            data: { type: 'keyword', value: keyword }
        });
    });
    // Add built-in functions
    builtins_1.ERGOSCRIPT_BUILTINS.forEach(builtin => {
        completions.push({
            label: builtin.name,
            kind: node_1.CompletionItemKind.Function,
            detail: builtin.signature,
            documentation: builtin.documentation,
            data: { type: 'builtin', value: builtin.name }
        });
    });
    // Add context variables
    builtins_1.CONTEXT_VARIABLES.forEach(variable => {
        completions.push({
            label: variable.name,
            kind: node_1.CompletionItemKind.Variable,
            detail: variable.type,
            documentation: variable.documentation,
            data: { type: 'context', value: variable.name }
        });
    });
    // Add types
    builtins_1.ERGOSCRIPT_TYPES.forEach(type => {
        completions.push({
            label: type.name,
            kind: node_1.CompletionItemKind.Class,
            detail: type.description,
            data: { type: 'type', value: type.name }
        });
    });
    return completions;
});
// Completion item resolution
connection.onCompletionResolve((item) => {
    return item;
});
// Hover information
connection.onHover((params) => {
    const document = documents.get(params.textDocument.uri);
    if (!document) {
        return null;
    }
    const text = document.getText();
    const offset = document.offsetAt(params.position);
    // Get word at position
    const wordRange = getWordRangeAtPosition(text, offset);
    if (!wordRange) {
        return null;
    }
    const word = text.substring(wordRange.start, wordRange.end);
    // Check if it's a built-in function
    const builtin = builtins_1.ERGOSCRIPT_BUILTINS.find(b => b.name === word);
    if (builtin) {
        return {
            contents: {
                kind: node_1.MarkupKind.Markdown,
                value: `**${builtin.name}**\n\n\`\`\`ergoscript\n${builtin.signature}\n\`\`\`\n\n${builtin.documentation}`
            }
        };
    }
    // Check if it's a context variable
    const contextVar = builtins_1.CONTEXT_VARIABLES.find(v => v.name === word);
    if (contextVar) {
        return {
            contents: {
                kind: node_1.MarkupKind.Markdown,
                value: `**${contextVar.name}**: \`${contextVar.type}\`\n\n${contextVar.documentation}`
            }
        };
    }
    // Check if it's a type
    const type = builtins_1.ERGOSCRIPT_TYPES.find(t => t.name === word);
    if (type) {
        return {
            contents: {
                kind: node_1.MarkupKind.Markdown,
                value: `**${type.name}**\n\n${type.description}`
            }
        };
    }
    return null;
});
function getWordRangeAtPosition(text, offset) {
    let start = offset;
    let end = offset;
    // Find start of word
    while (start > 0 && /[a-zA-Z0-9_]/.test(text[start - 1])) {
        start--;
    }
    // Find end of word
    while (end < text.length && /[a-zA-Z0-9_]/.test(text[end])) {
        end++;
    }
    if (start === end) {
        return null;
    }
    return { start, end };
}
// Make the text document manager listen on the connection
documents.listen(connection);
// Listen on the connection
connection.listen();
//# sourceMappingURL=server.js.map