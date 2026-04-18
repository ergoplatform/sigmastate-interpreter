"use strict";
/**
 * Hover Provider for ErgoScript LSP
 *
 * Provides hover information showing type signatures and documentation.
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.HoverProvider = void 0;
const node_1 = require("vscode-languageserver/node");
const builtins_1 = require("../utils/builtins");
class HoverProvider {
    /**
     * Provide hover information at the given position
     */
    provideHover(params, document) {
        const text = document.getText();
        const offset = document.offsetAt(params.position);
        // Get word at position
        const wordRange = this.getWordRangeAtPosition(text, offset);
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
        const contextVar = builtins_1.CONTEXT_VARIABLES.find(v => v.name === word);
        if (contextVar) {
            return {
                contents: {
                    kind: node_1.MarkupKind.Markdown,
                    value: [
                        `**${contextVar.name}**: \`${contextVar.type}\``,
                        '',
                        contextVar.documentation
                    ].join('\n')
                }
            };
        }
        // Check if it's a type
        const type = builtins_1.ERGOSCRIPT_TYPES.find(t => t.name === word);
        if (type) {
            return {
                contents: {
                    kind: node_1.MarkupKind.Markdown,
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
        const boxProp = builtins_1.BOX_PROPERTIES.find(p => p.name === word);
        if (boxProp) {
            return {
                contents: {
                    kind: node_1.MarkupKind.Markdown,
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
    getWordRangeAtPosition(text, offset) {
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
exports.HoverProvider = HoverProvider;
//# sourceMappingURL=hover.js.map