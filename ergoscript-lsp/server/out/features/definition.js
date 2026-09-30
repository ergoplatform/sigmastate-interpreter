"use strict";
/**
 * Definition Provider for ErgoScript LSP
 *
 * Provides go-to-definition functionality.
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.DefinitionProvider = void 0;
const node_1 = require("vscode-languageserver/node");
class DefinitionProvider {
    /**
     * Provide definition location for symbol at position
     */
    provideDefinition(params, document) {
        const text = document.getText();
        const offset = document.offsetAt(params.position);
        // Get word at position
        const wordRange = this.getWordRangeAtPosition(text, offset);
        if (!wordRange) {
            return null;
        }
        const word = text.substring(wordRange.start, wordRange.end);
        // Find variable declaration
        const valPattern = new RegExp(`\\bval\\s+(${word})\\b`, 'g');
        const defPattern = new RegExp(`\\bdef\\s+(${word})\\b`, 'g');
        let match;
        // Search for val declaration
        while ((match = valPattern.exec(text)) !== null) {
            const position = document.positionAt(match.index + 4); // Skip 'val '
            return node_1.Location.create(document.uri, {
                start: position,
                end: { line: position.line, character: position.character + word.length }
            });
        }
        // Search for def declaration
        while ((match = defPattern.exec(text)) !== null) {
            const position = document.positionAt(match.index + 4); // Skip 'def '
            return node_1.Location.create(document.uri, {
                start: position,
                end: { line: position.line, character: position.character + word.length }
            });
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
exports.DefinitionProvider = DefinitionProvider;
//# sourceMappingURL=definition.js.map