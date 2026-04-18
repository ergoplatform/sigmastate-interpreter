"use strict";
/**
 * Completion Provider for ErgoScript LSP
 *
 * Provides autocomplete suggestions for keywords, functions, variables, and types.
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.CompletionProvider = void 0;
const node_1 = require("vscode-languageserver/node");
const builtins_1 = require("../utils/builtins");
class CompletionProvider {
    /**
     * Provide completion items at the given position
     */
    provideCompletionItems(_params) {
        const completions = [];
        // Add keywords
        builtins_1.ERGOSCRIPT_KEYWORDS.forEach(keyword => {
            completions.push({
                label: keyword,
                kind: node_1.CompletionItemKind.Keyword,
                detail: 'Keyword',
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
                insertText: `${builtin.name}($1)`,
                insertTextFormat: 2, // Snippet format
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
        // Add box properties
        builtins_1.BOX_PROPERTIES.forEach(prop => {
            completions.push({
                label: prop.name,
                kind: node_1.CompletionItemKind.Property,
                detail: prop.type,
                documentation: prop.doc,
                data: { type: 'box-property', value: prop.name }
            });
        });
        // Add collection methods
        builtins_1.COLLECTION_METHODS.forEach(method => {
            completions.push({
                label: method.name,
                kind: node_1.CompletionItemKind.Method,
                detail: method.signature,
                documentation: method.doc,
                data: { type: 'collection-method', value: method.name }
            });
        });
        // Add numeric methods
        builtins_1.NUMERIC_METHODS.forEach(method => {
            completions.push({
                label: method.name,
                kind: node_1.CompletionItemKind.Method,
                detail: method.signature,
                documentation: method.doc,
                data: { type: 'numeric-method', value: method.name }
            });
        });
        return completions;
    }
    /**
     * Resolve additional information for a completion item
     */
    resolveCompletionItem(item) {
        // Could add more detailed documentation here
        return item;
    }
}
exports.CompletionProvider = CompletionProvider;
//# sourceMappingURL=completion.js.map