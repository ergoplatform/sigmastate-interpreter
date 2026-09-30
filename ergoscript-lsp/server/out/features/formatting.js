"use strict";
/**
 * Signature Help Provider for ErgoScript LSP
 *
 * Provides parameter hints for function calls.
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.SignatureProvider = void 0;
const builtins_1 = require("../utils/builtins");
class SignatureProvider {
    /**
     * Provide signature help at the given position
     */
    provideSignatureHelp(params, document) {
        const text = document.getText();
        const offset = document.offsetAt(params.position);
        // Find the function call we're inside
        const functionCall = this.findFunctionCall(text, offset);
        if (!functionCall) {
            return null;
        }
        // Find the built-in function
        const builtin = builtins_1.ERGOSCRIPT_BUILTINS.find(b => b.name === functionCall.name);
        if (!builtin) {
            return null;
        }
        // Parse parameters from signature
        const paramMatch = builtin.signature.match(/\((.*?)\)/);
        if (!paramMatch) {
            return null;
        }
        const paramList = paramMatch[1].split(',').map(p => p.trim());
        const parameters = paramList.map(param => ({
            label: param,
            documentation: `Parameter: ${param}`
        }));
        const signature = {
            label: builtin.signature,
            documentation: builtin.documentation,
            parameters
        };
        return {
            signatures: [signature],
            activeSignature: 0,
            activeParameter: functionCall.paramIndex
        };
    }
    findFunctionCall(text, offset) {
        // Walk backwards to find opening parenthesis
        let parenCount = 0;
        let commaCount = 0;
        let i = offset - 1;
        while (i >= 0) {
            const char = text[i];
            if (char === ')') {
                parenCount++;
            }
            else if (char === '(') {
                if (parenCount === 0) {
                    // Found the opening paren
                    // Now find the function name
                    let nameEnd = i;
                    while (i > 0 && /\s/.test(text[i - 1])) {
                        i--;
                    }
                    let nameStart = i;
                    while (nameStart > 0 && /[a-zA-Z0-9_]/.test(text[nameStart - 1])) {
                        nameStart--;
                    }
                    const name = text.substring(nameStart, nameEnd).trim();
                    return { name, paramIndex: commaCount };
                }
                parenCount--;
            }
            else if (char === ',' && parenCount === 0) {
                commaCount++;
            }
            i--;
        }
        return null;
    }
}
exports.SignatureProvider = SignatureProvider;
//# sourceMappingURL=formatting.js.map