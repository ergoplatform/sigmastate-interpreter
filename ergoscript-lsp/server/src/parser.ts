/**
 * Simple token-based parser for ErgoScript
 * Provides tokenization and basic structural analysis for LSP features
 */

export interface Token {
    type: 'keyword' | 'identifier' | 'operator' | 'literal' | 'punctuation' | 'comment' | 'whitespace';
    value: string;
    line: number;
    column: number;
}

export interface BraceInfo {
    type: '(' | ')' | '{' | '}' | '[' | ']';
    line: number;
    column: number;
}

// ErgoScript keywords
const KEYWORDS = new Set([
    // Core types
    'SigmaProp', 'Box', 'Coll', 'Option', 'AvlTree',
    // Primitive types
    'Int', 'Long', 'BigInt', 'Boolean', 'Byte', 'Short', 'String',
    // Keywords
    'val', 'if', 'else', 'true', 'false',
    // Context variables
    'SELF', 'HEIGHT', 'INPUTS', 'OUTPUTS', 'CONTEXT',
    // Common functions
    'sigmaProp', 'anyOf', 'allOf', 'atLeast',
    'proveDlog', 'proveDHTuple',
    // Box properties
    'tokens', 'value', 'propositionBytes', 'id', 'creationInfo',
    // Collection operations
    'size', 'exists', 'forall', 'fold', 'map', 'filter'
]);

/**
 * Tokenize ErgoScript source code
 */
export function tokenize(source: string): Token[] {
    const tokens: Token[] = [];
    const lines = source.split('\n');

    for (let lineNum = 0; lineNum < lines.length; lineNum++) {
        const line = lines[lineNum];
        let column = 0;

        while (column < line.length) {
            const char = line[column];

            // Skip whitespace
            if (/\s/.test(char)) {
                column++;
                continue;
            }

            // Single-line comment
            if (char === '/' && line[column + 1] === '/') {
                tokens.push({
                    type: 'comment',
                    value: line.substring(column),
                    line: lineNum,
                    column
                });
                break;
            }

            // Multi-line comment start
            if (char === '/' && line[column + 1] === '*') {
                const commentStart = column;
                column += 2;
                let commentValue = '/*';

                // Find end of comment (simplified - doesn't handle multi-line)
                while (column < line.length - 1) {
                    if (line[column] === '*' && line[column + 1] === '/') {
                        commentValue += '*/';
                        column += 2;
                        break;
                    }
                    commentValue += line[column];
                    column++;
                }

                tokens.push({
                    type: 'comment',
                    value: commentValue,
                    line: lineNum,
                    column: commentStart
                });
                continue;
            }

            // Punctuation and operators
            if ('(){}[],;'.includes(char)) {
                tokens.push({
                    type: 'punctuation',
                    value: char,
                    line: lineNum,
                    column
                });
                column++;
                continue;
            }

            // Operators
            if ('+-*/<>=!&|'.includes(char)) {
                let op = char;
                // Check for multi-char operators
                if (column + 1 < line.length) {
                    const next = line[column + 1];
                    if ((char === '=' && next === '=') ||
                        (char === '!' && next === '=') ||
                        (char === '<' && next === '=') ||
                        (char === '>' && next === '=') ||
                        (char === '&' && next === '&') ||
                        (char === '|' && next === '|')) {
                        op += next;
                        column++;
                    }
                }
                tokens.push({
                    type: 'operator',
                    value: op,
                    line: lineNum,
                    column
                });
                column++;
                continue;
            }

            // Numbers
            if (/\d/.test(char)) {
                let num = '';
                const startCol = column;
                while (column < line.length && /[\d.]/.test(line[column])) {
                    num += line[column];
                    column++;
                }
                // Check for L suffix (Long literal)
                if (column < line.length && line[column] === 'L') {
                    num += 'L';
                    column++;
                }
                tokens.push({
                    type: 'literal',
                    value: num,
                    line: lineNum,
                    column: startCol
                });
                continue;
            }

            // Identifiers and keywords
            if (/[a-zA-Z_]/.test(char)) {
                let word = '';
                const startCol = column;
                while (column < line.length && /[a-zA-Z0-9_]/.test(line[column])) {
                    word += line[column];
                    column++;
                }

                const tokenType = KEYWORDS.has(word) ? 'keyword' : 'identifier';
                tokens.push({
                    type: tokenType,
                    value: word,
                    line: lineNum,
                    column: startCol
                });
                continue;
            }

            // String literals
            if (char === '"' || char === "'") {
                const quote = char;
                let str = quote;
                const startCol = column;
                column++;

                while (column < line.length && line[column] !== quote) {
                    if (line[column] === '\\' && column + 1 < line.length) {
                        str += line[column] + line[column + 1];
                        column += 2;
                    } else {
                        str += line[column];
                        column++;
                    }
                }

                if (column < line.length) {
                    str += quote;
                    column++;
                }

                tokens.push({
                    type: 'literal',
                    value: str,
                    line: lineNum,
                    column: startCol
                });
                continue;
            }

            // Unknown character - skip
            column++;
        }
    }

    return tokens;
}

/**
 * Extract brace/parenthesis/bracket information for syntax checking
 */
export function extractBraces(source: string): BraceInfo[] {
    const braces: BraceInfo[] = [];
    const lines = source.split('\n');

    for (let lineNum = 0; lineNum < lines.length; lineNum++) {
        const line = lines[lineNum];
        let inString = false;
        let inComment = false;

        for (let col = 0; col < line.length; col++) {
            const char = line[col];

            // Handle comments
            if (!inString && char === '/' && line[col + 1] === '/') {
                break; // Rest of line is comment
            }
            if (!inString && char === '/' && line[col + 1] === '*') {
                inComment = true;
                col++;
                continue;
            }
            if (inComment && char === '*' && line[col + 1] === '/') {
                inComment = false;
                col++;
                continue;
            }

            // Handle strings
            if (!inComment && (char === '"' || char === "'")) {
                if (!inString) {
                    inString = true;
                } else if (col === 0 || line[col - 1] !== '\\') {
                    inString = false;
                }
                continue;
            }

            // Extract braces outside strings and comments
            if (!inString && !inComment && '(){}[]'.includes(char)) {
                braces.push({
                    type: char as '(' | ')' | '{' | '}' | '[' | ']',
                    line: lineNum,
                    column: col
                });
            }
        }
    }

    return braces;
}

/**
 * Get keyword at position (for hover support)
 */
export function getWordAtPosition(source: string, line: number, column: number): string | null {
    const lines = source.split('\n');
    if (line < 0 || line >= lines.length) {
        return null;
    }

    const lineText = lines[line];
    if (column < 0 || column >= lineText.length) {
        return null;
    }

    // Find word boundaries
    let start = column;
    let end = column;

    while (start > 0 && /[a-zA-Z0-9_]/.test(lineText[start - 1])) {
        start--;
    }

    while (end < lineText.length && /[a-zA-Z0-9_]/.test(lineText[end])) {
        end++;
    }

    const word = lineText.substring(start, end);
    return word.length > 0 ? word : null;
}
