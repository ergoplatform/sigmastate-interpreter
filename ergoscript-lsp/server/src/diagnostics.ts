/**
 * Diagnostics provider for ErgoScript
 * Validates syntax and reports errors
 */

import { Diagnostic, DiagnosticSeverity } from 'vscode-languageserver/node';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { extractBraces, BraceInfo } from './parser';

/**
 * Validate document and return diagnostics
 */
export function validateDocument(document: TextDocument): Diagnostic[] {
    const diagnostics: Diagnostic[] = [];
    const text = document.getText();

    // Check for unmatched braces, parentheses, and brackets
    const braceErrors = checkBraceMatching(text);
    diagnostics.push(...braceErrors);

    // Check for consecutive operators (malformed expressions)
    const operatorErrors = checkConsecutiveOperators(text, document);
    diagnostics.push(...operatorErrors);

    return diagnostics;
}

/**
 * Check for unmatched braces, parentheses, and brackets
 */
function checkBraceMatching(source: string): Diagnostic[] {
    const diagnostics: Diagnostic[] = [];
    const braces = extractBraces(source);
    const stack: BraceInfo[] = [];

    const pairs: Record<string, string> = {
        '(': ')',
        '{': '}',
        '[': ']'
    };

    for (const brace of braces) {
        if (brace.type === '(' || brace.type === '{' || brace.type === '[') {
            // Opening brace
            stack.push(brace);
        } else {
            // Closing brace
            if (stack.length === 0) {
                // Unmatched closing brace
                diagnostics.push({
                    severity: DiagnosticSeverity.Error,
                    range: {
                        start: { line: brace.line, character: brace.column },
                        end: { line: brace.line, character: brace.column + 1 }
                    },
                    message: `Unmatched closing '${brace.type}'`,
                    source: 'ergoscript'
                });
            } else {
                const opening = stack.pop()!;
                const expectedClosing = pairs[opening.type];

                if (brace.type !== expectedClosing) {
                    // Mismatched brace
                    diagnostics.push({
                        severity: DiagnosticSeverity.Error,
                        range: {
                            start: { line: brace.line, character: brace.column },
                            end: { line: brace.line, character: brace.column + 1 }
                        },
                        message: `Expected '${expectedClosing}' to match '${opening.type}' at line ${opening.line + 1}, but found '${brace.type}'`,
                        source: 'ergoscript'
                    });
                }
            }
        }
    }

    // Check for unclosed opening braces
    for (const unclosed of stack) {
        diagnostics.push({
            severity: DiagnosticSeverity.Error,
            range: {
                start: { line: unclosed.line, character: unclosed.column },
                end: { line: unclosed.line, character: unclosed.column + 1 }
            },
            message: `Unclosed '${unclosed.type}'`,
            source: 'ergoscript'
        });
    }

    return diagnostics;
}

/**
 * Check for consecutive operators (malformed expressions)
 */
function checkConsecutiveOperators(source: string, document: TextDocument): Diagnostic[] {
    const diagnostics: Diagnostic[] = [];
    const lines = source.split('\n');

    const operators = /[+\-*/<>=!&|]{1,2}/g;

    for (let lineNum = 0; lineNum < lines.length; lineNum++) {
        const line = lines[lineNum];

        // Skip comments
        const commentIndex = line.indexOf('//');
        const checkLine = commentIndex >= 0 ? line.substring(0, commentIndex) : line;

        // Find consecutive operators (simplified check)
        const consecutiveOps = /([+\-*/<>=!&|])\s*([+\-*/<>=!&|])/g;
        let match;

        while ((match = consecutiveOps.exec(checkLine)) !== null) {
            const op1 = match[1];
            const op2 = match[2];

            // Allow valid multi-char operators and some patterns
            const validPatterns = [
                '==', '!=', '<=', '>=', '&&', '||', // Multi-char operators
                '!', // Unary not
            ];

            const combined = op1 + op2;
            if (!validPatterns.includes(combined) &&
                !(op1 === '-' && op2 !== '=') && // Allow unary minus
                !(op1 === '!' && op2 === '=')) { // Allow !=

                diagnostics.push({
                    severity: DiagnosticSeverity.Warning,
                    range: {
                        start: { line: lineNum, character: match.index },
                        end: { line: lineNum, character: match.index + match[0].length }
                    },
                    message: `Possibly malformed expression: consecutive operators '${op1}' and '${op2}'`,
                    source: 'ergoscript'
                });
            }
        }
    }

    return diagnostics;
}
