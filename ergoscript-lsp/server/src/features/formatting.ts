/**
 * Signature Help Provider for ErgoScript LSP
 * 
 * Provides parameter hints for function calls.
 */

import { SignatureHelp, SignatureInformation, ParameterInformation, TextDocumentPositionParams } from 'vscode-languageserver/node';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { ERGOSCRIPT_BUILTINS } from '../utils/builtins';

export class SignatureProvider {

    /**
     * Provide signature help at the given position
     */
    provideSignatureHelp(params: TextDocumentPositionParams, document: TextDocument): SignatureHelp | null {
        const text = document.getText();
        const offset = document.offsetAt(params.position);

        // Find the function call we're inside
        const functionCall = this.findFunctionCall(text, offset);
        if (!functionCall) {
            return null;
        }

        // Find the built-in function
        const builtin = ERGOSCRIPT_BUILTINS.find(b => b.name === functionCall.name);
        if (!builtin) {
            return null;
        }

        // Parse parameters from signature
        const paramMatch = builtin.signature.match(/\((.*?)\)/);
        if (!paramMatch) {
            return null;
        }

        const params = paramMatch[1].split(',').map(p => p.trim());
        const parameters: ParameterInformation[] = params.map(param => ({
            label: param,
            documentation: `Parameter: ${param}`
        }));

        const signature: SignatureInformation = {
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

    private findFunctionCall(text: string, offset: number): { name: string; paramIndex: number } | null {
        // Walk backwards to find opening parenthesis
        let parenCount = 0;
        let commaCount = 0;
        let i = offset - 1;

        while (i >= 0) {
            const char = text[i];

            if (char === ')') {
                parenCount++;
            } else if (char === '(') {
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
            } else if (char === ',' && parenCount === 0) {
                commaCount++;
            }

            i--;
        }

        return null;
    }
}
