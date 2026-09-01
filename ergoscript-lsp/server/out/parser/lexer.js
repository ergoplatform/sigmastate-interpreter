"use strict";
/**
 * ErgoScript Lexer (Tokenizer)
 *
 * Converts ErgoScript source code into tokens for parsing.
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.Lexer = exports.TokenType = void 0;
var TokenType;
(function (TokenType) {
    // Keywords
    TokenType["VAL"] = "VAL";
    TokenType["DEF"] = "DEF";
    TokenType["IF"] = "IF";
    TokenType["ELSE"] = "ELSE";
    TokenType["TRUE"] = "TRUE";
    TokenType["FALSE"] = "FALSE";
    TokenType["RETURN"] = "RETURN";
    // Identifiers and literals
    TokenType["IDENTIFIER"] = "IDENTIFIER";
    TokenType["NUMBER"] = "NUMBER";
    TokenType["STRING"] = "STRING";
    // Operators
    TokenType["PLUS"] = "PLUS";
    TokenType["MINUS"] = "MINUS";
    TokenType["STAR"] = "STAR";
    TokenType["SLASH"] = "SLASH";
    TokenType["PERCENT"] = "PERCENT";
    // Comparison
    TokenType["EQ"] = "EQ";
    TokenType["NEQ"] = "NEQ";
    TokenType["LT"] = "LT";
    TokenType["GT"] = "GT";
    TokenType["LTE"] = "LTE";
    TokenType["GTE"] = "GTE";
    // Logical
    TokenType["AND"] = "AND";
    TokenType["OR"] = "OR";
    TokenType["NOT"] = "NOT";
    // Bitwise
    TokenType["BITAND"] = "BITAND";
    TokenType["BITOR"] = "BITOR";
    TokenType["BITXOR"] = "BITXOR";
    // Delimiters
    TokenType["LPAREN"] = "LPAREN";
    TokenType["RPAREN"] = "RPAREN";
    TokenType["LBRACE"] = "LBRACE";
    TokenType["RBRACE"] = "RBRACE";
    TokenType["LBRACKET"] = "LBRACKET";
    TokenType["RBRACKET"] = "RBRACKET";
    // Punctuation
    TokenType["DOT"] = "DOT";
    TokenType["COMMA"] = "COMMA";
    TokenType["COLON"] = "COLON";
    TokenType["SEMICOLON"] = "SEMICOLON";
    TokenType["ARROW"] = "ARROW";
    TokenType["FATARROW"] = "FATARROW";
    TokenType["ASSIGN"] = "ASSIGN";
    // Special
    TokenType["EOF"] = "EOF";
    TokenType["NEWLINE"] = "NEWLINE";
    TokenType["COMMENT"] = "COMMENT";
})(TokenType || (exports.TokenType = TokenType = {}));
const KEYWORDS = {
    'val': TokenType.VAL,
    'def': TokenType.DEF,
    'if': TokenType.IF,
    'else': TokenType.ELSE,
    'true': TokenType.TRUE,
    'false': TokenType.FALSE,
    'return': TokenType.RETURN
};
class Lexer {
    constructor(input) {
        this.position = 0;
        this.line = 1;
        this.column = 1;
        this.input = input;
    }
    tokenize() {
        const tokens = [];
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
    nextToken() {
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
    readNumber() {
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
    readString() {
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
    readIdentifier() {
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
    readLineComment() {
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
    readBlockComment() {
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
    readSingleChar(type) {
        const token = this.makeToken(type, this.input[this.position]);
        this.advance();
        return token;
    }
    readTwoChar(type) {
        const value = this.input.substring(this.position, this.position + 2);
        const token = this.makeToken(type, value);
        this.advance();
        this.advance();
        return token;
    }
    makeToken(type, value) {
        return {
            type,
            value,
            line: this.line,
            column: this.column,
            offset: this.position
        };
    }
    skipWhitespace() {
        while (this.position < this.input.length) {
            const char = this.input[this.position];
            if (char === ' ' || char === '\t' || char === '\r' || char === '\n') {
                this.advance();
            }
            else {
                break;
            }
        }
    }
    advance() {
        if (this.input[this.position] === '\n') {
            this.line++;
            this.column = 1;
        }
        else {
            this.column++;
        }
        this.position++;
    }
    peek() {
        if (this.position + 1 < this.input.length) {
            return this.input[this.position + 1];
        }
        return '';
    }
    isDigit(char) {
        return char >= '0' && char <= '9';
    }
    isAlpha(char) {
        return (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z');
    }
    isAlphaNumeric(char) {
        return this.isAlpha(char) || this.isDigit(char);
    }
}
exports.Lexer = Lexer;
//# sourceMappingURL=lexer.js.map