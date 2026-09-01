/**
 * Hover Provider for ErgoScript LSP
 * 
 * Provides hover information showing type signatures and documentation.
 */

import { Hover, MarkupKind, TextDocumentPositionParams } from 'vscode-languageserver/node';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { ERGOSCRIPT_BUILTINS, ERGOSCRIPT_TYPES, CONTEXT_VARIABLES, BOX_PROPERTIES } from '../utils/builtins';

export class HoverProvider {

    /**
     * Provide hover information at the given position
     */
    provideHover(params: TextDocumentPositionParams, document: TextDocument): Hover | null {
        const text = document.getText();
        const offset = document.offsetAt(params.position);

        // Get word at position
        const wordRange = this.getWordRangeAtPosition(text, offset);
        if (!wordRange) {
            return null;
        }

        const word = text.substring(wordRange.start, wordRange.end);

        // Check if it's a built-in function
        const builtin = ERGOSCRIPT_BUILTINS.find(b => b.name === word);
        if (builtin) {
            return {
                contents: {
                    kind: MarkupKind.Markdown,
                    value: [
                        `**${builtin.name}**`,
                        '',
                        '```ergoscript',
                        builtin.signature,
                        '```',
                        '',
                        builtin.documentation
                    ].join('\n')
                }
            };
        }

        // Check if it's a context variable
        const contextVar = CONTEXT_VARIABLES.find(v => v.name === word);
        if (contextVar) {
            return {
                contents: {
                    kind: MarkupKind.Markdown,
                    value: [
                        `**${contextVar.name}**: \`${contextVar.type}\``,
                        '',
                        contextVar.documentation
                    ].join('\n')
                }
            };
        }

        // Check if it's a type
        const type = ERGOSCRIPT_TYPES.find(t => t.name === word);
        if (type) {
            return {
                contents: {
                    kind: MarkupKind.Markdown,
                    value: [
                        `**${type.name}**`,
                        '',
                        type.description,
                        '',
                        type.properties ? `**Properties:** ${type.properties.join(', ')}` : ''
                    ].filter(Boolean).join('\n')
                }
            };
        }

        // Check if it's a box property
        const boxProp = BOX_PROPERTIES.find(p => p.name === word);
        if (boxProp) {
            return {
                contents: {
                    kind: MarkupKind.Markdown,
                    value: [
                        `**${boxProp.name}**: \`${boxProp.type}\``,
                        '',
                        boxProp.doc
                    ].join('\n')
                }
            };
        }

        return null;
    }

    private getWordRangeAtPosition(text: string, offset: number): { start: number; end: number } | null {
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
}
