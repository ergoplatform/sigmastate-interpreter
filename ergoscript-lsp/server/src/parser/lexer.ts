/**
 * ErgoScript Lexer (Tokenizer)
 * 
 * Converts ErgoScript source code into tokens for parsing.
 */

export enum TokenType {
    // Keywords
    VAL = 'VAL',
    DEF = 'DEF',
    IF = 'IF',
    ELSE = 'ELSE',
    TRUE = 'TRUE',
    FALSE = 'FALSE',
    RETURN = 'RETURN',

    // Identifiers and literals
    IDENTIFIER = 'IDENTIFIER',
    NUMBER = 'NUMBER',
    STRING = 'STRING',

    // Operators
    PLUS = 'PLUS',
    MINUS = 'MINUS',
    STAR = 'STAR',
    SLASH = 'SLASH',
    PERCENT = 'PERCENT',

    // Comparison
    EQ = 'EQ',
    NEQ = 'NEQ',
    LT = 'LT',
    GT = 'GT',
    LTE = 'LTE',
    GTE = 'GTE',

    // Logical
    AND = 'AND',
    OR = 'OR',
    NOT = 'NOT',

    // Bitwise
    BITAND = 'BITAND',
    BITOR = 'BITOR',
    BITXOR = 'BITXOR',

    // Delimiters
    LPAREN = 'LPAREN',
    RPAREN = 'RPAREN',
    LBRACE = 'LBRACE',
    RBRACE = 'RBRACE',
    LBRACKET = 'LBRACKET',
    RBRACKET = 'RBRACKET',

    // Punctuation
    DOT = 'DOT',
    COMMA = 'COMMA',
    COLON = 'COLON',
    SEMICOLON = 'SEMICOLON',
    ARROW = 'ARROW',
    FATARROW = 'FATARROW',
    ASSIGN = 'ASSIGN',

    // Special
    EOF = 'EOF',
    NEWLINE = 'NEWLINE',
    COMMENT = 'COMMENT'
}

export interface Token {
    type: TokenType;
    value: string;
    line: number;
    column: number;
    offset: number;
}

const KEYWORDS: Record<string, TokenType> = {
    'val': TokenType.VAL,
    'def': TokenType.DEF,
    'if': TokenType.IF,
    'else': TokenType.ELSE,
    'true': TokenType.TRUE,
    'false': TokenType.FALSE,
    'return': TokenType.RETURN
};

export class Lexer {
    private input: string;
    private position: number = 0;
    private line: number = 1;
    private column: number = 1;

    constructor(input: string) {
        this.input = input;
    }

    tokenize(): Token[] {
        const tokens: Token[] = [];

        while (this.position < this.input.length) {
            const token = this.nextToken();
            if (token.type !== TokenType.COMMENT) {
                tokens.push(token);
            }
        }

        tokens.push({
            type: TokenType.EOF,
            value: '',
            line: this.line,
            column: this.column,
            offset: this.position
        });

        return tokens;
    }

    private nextToken(): Token {
        this.skipWhitespace();

        if (this.position >= this.input.length) {
            return this.makeToken(TokenType.EOF, '');
        }

        const char = this.input[this.position];

        // Comments
        if (char === '/' && this.peek() === '/') {
            return this.readLineComment();
        }
        if (char === '/' && this.peek() === '*') {
            return this.readBlockComment();
        }

        // Numbers
        if (this.isDigit(char)) {
            return this.readNumber();
        }

        // Strings
        if (char === '"') {
            return this.readString();
        }

        // Identifiers and keywords
        if (this.isAlpha(char) || char === '_') {
            return this.readIdentifier();
        }

        // Two-character operators
        if (char === '=' && this.peek() === '=') {
            return this.readTwoChar(TokenType.EQ);
        }
        if (char === '!' && this.peek() === '=') {
            return this.readTwoChar(TokenType.NEQ);
        }
        if (char === '<' && this.peek() === '=') {
            return this.readTwoChar(TokenType.LTE);
        }
        if (char === '>' && this.peek() === '=') {
            return this.readTwoChar(TokenType.GTE);
        }
        if (char === '&' && this.peek() === '&') {
            return this.readTwoChar(TokenType.AND);
        }
        if (char === '|' && this.peek() === '|') {
            return this.readTwoChar(TokenType.OR);
        }
        if (char === '=' && this.peek() === '>') {
            return this.readTwoChar(TokenType.FATARROW);
        }
        if (char === '-' && this.peek() === '>') {
            return this.readTwoChar(TokenType.ARROW);
        }

        // Single-character tokens
        switch (char) {
            case '+': return this.readSingleChar(TokenType.PLUS);
            case '-': return this.readSingleChar(TokenType.MINUS);
            case '*': return this.readSingleChar(TokenType.STAR);
            case '/': return this.readSingleChar(TokenType.SLASH);
            case '%': return this.readSingleChar(TokenType.PERCENT);
            case '<': return this.readSingleChar(TokenType.LT);
            case '>': return this.readSingleChar(TokenType.GT);
            case '!': return this.readSingleChar(TokenType.NOT);
            case '&': return this.readSingleChar(TokenType.BITAND);
            case '|': return this.readSingleChar(TokenType.BITOR);
            case '^': return this.readSingleChar(TokenType.BITXOR);
            case '(': return this.readSingleChar(TokenType.LPAREN);
            case ')': return this.readSingleChar(TokenType.RPAREN);
            case '{': return this.readSingleChar(TokenType.LBRACE);
            case '}': return this.readSingleChar(TokenType.RBRACE);
            case '[': return this.readSingleChar(TokenType.LBRACKET);
            case ']': return this.readSingleChar(TokenType.RBRACKET);
            case '.': return this.readSingleChar(TokenType.DOT);
            case ',': return this.readSingleChar(TokenType.COMMA);
            case ':': return this.readSingleChar(TokenType.COLON);
            case ';': return this.readSingleChar(TokenType.SEMICOLON);
            case '=': return this.readSingleChar(TokenType.ASSIGN);
            default:
                throw new Error(`Unexpected character: ${char} at line ${this.line}, column ${this.column}`);
        }
    }

    private readNumber(): Token {
        const start = this.position;
        const startLine = this.line;
        const startColumn = this.column;

        while (this.position < this.input.length && this.isDigit(this.input[this.position])) {
            this.advance();
        }

        // Handle decimal point
        if (this.position < this.input.length && this.input[this.position] === '.') {
            this.advance();
            while (this.position < this.input.length && this.isDigit(this.input[this.position])) {
                this.advance();
            }
        }

        const value = this.input.substring(start, this.position);
        return {
            type: TokenType.NUMBER,
            value,
            line: startLine,
            column: startColumn,
            offset: start
        };
    }

    private readString(): Token {
        const start = this.position;
        const startLine = this.line;
        const startColumn = this.column;

        this.advance(); // Skip opening quote

        while (this.position < this.input.length && this.input[this.position] !== '"') {
            if (this.input[this.position] === '\\') {
                this.advance(); // Skip escape character
            }
            this.advance();
        }

        this.advance(); // Skip closing quote

        const value = this.input.substring(start, this.position);
        return {
            type: TokenType.STRING,
            value,
            line: startLine,
            column: startColumn,
            offset: start
        };
    }

    private readIdentifier(): Token {
        const start = this.position;
        const startLine = this.line;
        const startColumn = this.column;

        while (this.position < this.input.length &&
            (this.isAlphaNumeric(this.input[this.position]) || this.input[this.position] === '_')) {
            this.advance();
        }

        const value = this.input.substring(start, this.position);
        const type = KEYWORDS[value] || TokenType.IDENTIFIER;

        return {
            type,
            value,
            line: startLine,
            column: startColumn,
            offset: start
        };
    }

    private readLineComment(): Token {
        const start = this.position;
        const startLine = this.line;
        const startColumn = this.column;

        while (this.position < this.input.length && this.input[this.position] !== '\n') {
            this.advance();
        }

        const value = this.input.substring(start, this.position);
        return {
            type: TokenType.COMMENT,
            value,
            line: startLine,
            column: startColumn,
            offset: start
        };
    }

    private readBlockComment(): Token {
        const start = this.position;
        const startLine = this.line;
        const startColumn = this.column;

        this.advance(); // Skip /
        this.advance(); // Skip *

        while (this.position < this.input.length - 1) {
            if (this.input[this.position] === '*' && this.input[this.position + 1] === '/') {
                this.advance();
                this.advance();
                break;
            }
            this.advance();
        }

        const value = this.input.substring(start, this.position);
        return {
            type: TokenType.COMMENT,
            value,
            line: startLine,
            column: startColumn,
            offset: start
        };
    }

    private readSingleChar(type: TokenType): Token {
        const token = this.makeToken(type, this.input[this.position]);
        this.advance();
        return token;
    }

    private readTwoChar(type: TokenType): Token {
        const value = this.input.substring(this.position, this.position + 2);
        const token = this.makeToken(type, value);
        this.advance();
        this.advance();
        return token;
    }

    private makeToken(type: TokenType, value: string): Token {
        return {
            type,
            value,
            line: this.line,
            column: this.column,
            offset: this.position
        };
    }

    private skipWhitespace(): void {
        while (this.position < this.input.length) {
            const char = this.input[this.position];
            if (char === ' ' || char === '\t' || char === '\r' || char === '\n') {
                this.advance();
            } else {
                break;
            }
        }
    }

    private advance(): void {
        if (this.input[this.position] === '\n') {
            this.line++;
            this.column = 1;
        } else {
            this.column++;
        }
        this.position++;
    }

    private peek(): string {
        if (this.position + 1 < this.input.length) {
            return this.input[this.position + 1];
        }
        return '';
    }

    private isDigit(char: string): boolean {
        return char >= '0' && char <= '9';
    }

    private isAlpha(char: string): boolean {
        return (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z');
    }

    private isAlphaNumeric(char: string): boolean {
        return this.isAlpha(char) || this.isDigit(char);
    }
}
