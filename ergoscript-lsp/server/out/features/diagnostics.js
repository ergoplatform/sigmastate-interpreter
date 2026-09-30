"use strict";
/**
 * Diagnostics Provider for ErgoScript LSP
 *
 * Provides real-time error checking and warnings.
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.DiagnosticsProvider = void 0;
const node_1 = require("vscode-languageserver/node");
const parser_1 = require("../parser/parser");
const typeChecker_1 = require("../analyzer/typeChecker");
const validator_1 = require("../analyzer/validator");
const builtins_1 = require("../utils/builtins");
class DiagnosticsProvider {
    /**
     * Provide diagnostics for a document
     */
    provideDiagnostics(textDocument) {
        const diagnostics = [];
        const text = textDocument.getText();
        // Try to parse the document
        try {
            const parser = new parser_1.Parser(text);
            const ast = parser.parse();
            // Type checking
            const typeChecker = new typeChecker_1.TypeChecker();
            typeChecker.check(ast);
            // Semantic validation
            const validator = new validator_1.Validator(typeChecker.getSymbolTable());
            const errors = validator.validate(ast);
            // Convert validation errors to diagnostics
            for (const error of errors) {
                const severity = error.severity === 'error' ? node_1.DiagnosticSeverity.Error :
                    error.severity === 'warning' ? node_1.DiagnosticSeverity.Warning :
                        node_1.DiagnosticSeverity.Information;
                diagnostics.push({
                    severity,
                    range: {
                        start: { line: error.line || 0, character: error.column || 0 },
                        end: { line: error.line || 0, character: (error.column || 0) + 10 }
                    },
                    message: error.message,
                    source: 'ergoscript'
                });
            }
        }
        catch (parseError) {
            // Parse error - add diagnostic
            diagnostics.push({
                severity: node_1.DiagnosticSeverity.Error,
                range: {
                    start: { line: 0, character: 0 },
                    end: { line: 0, character: 10 }
                },
                message: `Parse error: ${parseError}`,
                source: 'ergoscript'
            });
        }
        // Simple heuristic checks
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
                // Check if variable is defined earlier
                const beforeText = text.substring(0, textDocument.offsetAt({ line: i, character: match.index }));
                const valPattern = new RegExp(`\\bval\\s+${varName}\\b`);
                const defPattern = new RegExp(`\\bdef\\s+${varName}\\b`);
                if (!valPattern.test(beforeText) && !defPattern.test(beforeText)) {
                    diagnostics.push({
                        severity: node_1.DiagnosticSeverity.Warning,
                        range: {
                            start: { line: i, character: match.index },
                            end: { line: i, character: match.index + varName.length }
                        },
                        message: `'${varName}' might be undefined`,
                        source: 'ergoscript'
                    });
                }
            }
            // Check for unknown types
            if (line.includes('val') && line.includes(':') && line.includes('=')) {
                const typeAnnotation = line.match(/:\s*(\w+)\s*=/);
                if (typeAnnotation) {
                    const declaredType = typeAnnotation[1];
                    if (!builtins_1.ERGOSCRIPT_TYPES.some(t => t.name === declaredType)) {
                        const index = line.indexOf(declaredType);
                        diagnostics.push({
                            severity: node_1.DiagnosticSeverity.Error,
                            range: {
                                start: { line: i, character: index },
                                end: { line: i, character: index + declaredType.length }
                            },
                            message: `Unknown type '${declaredType}'`,
                            source: 'ergoscript'
                        });
                    }
                }
            }
        }
        return diagnostics;
    }
}
exports.DiagnosticsProvider = DiagnosticsProvider;
//# sourceMappingURL=diagnostics.js.map