/**
 * Completion Provider for ErgoScript LSP
 * 
 * Provides autocomplete suggestions for keywords, functions, variables, and types.
 */

import { CompletionItem, CompletionItemKind, TextDocumentPositionParams } from 'vscode-languageserver/node';
import { ERGOSCRIPT_KEYWORDS, ERGOSCRIPT_BUILTINS, ERGOSCRIPT_TYPES, CONTEXT_VARIABLES, BOX_PROPERTIES, COLLECTION_METHODS, NUMERIC_METHODS } from '../utils/builtins';

export class CompletionProvider {

    /**
     * Provide completion items at the given position
     */
    provideCompletionItems(_params: TextDocumentPositionParams): CompletionItem[] {
        const completions: CompletionItem[] = [];

        // Add keywords
        ERGOSCRIPT_KEYWORDS.forEach(keyword => {
            completions.push({
                label: keyword,
                kind: CompletionItemKind.Keyword,
                detail: 'Keyword',
                data: { type: 'keyword', value: keyword }
            });
        });

        // Add built-in functions
        ERGOSCRIPT_BUILTINS.forEach(builtin => {
            completions.push({
                label: builtin.name,
                kind: CompletionItemKind.Function,
                detail: builtin.signature,
                documentation: builtin.documentation,
                insertText: `${builtin.name}($1)`,
                insertTextFormat: 2, // Snippet format
                data: { type: 'builtin', value: builtin.name }
            });
        });

        // Add context variables
        CONTEXT_VARIABLES.forEach(variable => {
            completions.push({
                label: variable.name,
                kind: CompletionItemKind.Variable,
                detail: variable.type,
                documentation: variable.documentation,
                data: { type: 'context', value: variable.name }
            });
        });

        // Add types
        ERGOSCRIPT_TYPES.forEach(type => {
            completions.push({
                label: type.name,
                kind: CompletionItemKind.Class,
                detail: type.description,
                data: { type: 'type', value: type.name }
            });
        });

        // Add box properties
        BOX_PROPERTIES.forEach(prop => {
            completions.push({
                label: prop.name,
                kind: CompletionItemKind.Property,
                detail: prop.type,
                documentation: prop.doc,
                data: { type: 'box-property', value: prop.name }
            });
        });

        // Add collection methods
        COLLECTION_METHODS.forEach(method => {
            completions.push({
                label: method.name,
                kind: CompletionItemKind.Method,
                detail: method.signature,
                documentation: method.doc,
                data: { type: 'collection-method', value: method.name }
            });
        });

        // Add numeric methods
        NUMERIC_METHODS.forEach(method => {
            completions.push({
                label: method.name,
                kind: CompletionItemKind.Method,
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
    resolveCompletionItem(item: CompletionItem): CompletionItem {
        // Could add more detailed documentation here
        return item;
    }
}
