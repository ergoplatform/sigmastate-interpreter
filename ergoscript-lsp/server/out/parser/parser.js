"use strict";
/**
 * ErgoScript Parser
 *
 * Parses tokens into an Abstract Syntax Tree (AST).
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.Parser = void 0;
const lexer_1 = require("./lexer");
class Parser {
    constructor(input) {
        this.current = 0;
        const lexer = new lexer_1.Lexer(input);
        this.tokens = lexer.tokenize();
    }
    parse() {
        const body = [];
        while (!this.isAtEnd()) {
            if (this.check(lexer_1.TokenType.EOF))
                break;
            const stmt = this.statement();
            if (stmt) {
                body.push(stmt);
            }
        }
        return {
            type: 'Program',
            body
        };
    }
    statement() {
        try {
            if (this.match(lexer_1.TokenType.VAL)) {
                return this.valDeclaration();
            }
            if (this.match(lexer_1.TokenType.DEF)) {
                return this.defDeclaration();
            }
            return this.expressionStatement();
        }
        catch (error) {
            // Error recovery: skip to next statement
            this.synchronize();
            return null;
        }
    }
    valDeclaration() {
        const name = this.consume(lexer_1.TokenType.IDENTIFIER, 'Expected variable name').value;
        let typeAnnotation;
        if (this.match(lexer_1.TokenType.COLON)) {
            typeAnnotation = this.typeAnnotation();
        }
        this.consume(lexer_1.TokenType.ASSIGN, 'Expected = after variable name');
        const value = this.expression();
        return {
            type: 'ValDeclaration',
            name,
            typeAnnotation,
            value
        };
    }
    defDeclaration() {
        const name = this.consume(lexer_1.TokenType.IDENTIFIER, 'Expected function name').value;
        this.consume(lexer_1.TokenType.LPAREN, 'Expected ( after function name');
        const params = [];
        if (!this.check(lexer_1.TokenType.RPAREN)) {
            do {
                const paramName = this.consume(lexer_1.TokenType.IDENTIFIER, 'Expected parameter name').value;
                this.consume(lexer_1.TokenType.COLON, 'Expected : after parameter name');
                const paramType = this.typeAnnotation();
                params.push({ name: paramName, type: paramType });
            } while (this.match(lexer_1.TokenType.COMMA));
        }
        this.consume(lexer_1.TokenType.RPAREN, 'Expected ) after parameters');
        let returnType;
        if (this.match(lexer_1.TokenType.COLON)) {
            returnType = this.typeAnnotation();
        }
        this.consume(lexer_1.TokenType.ASSIGN, 'Expected = before function body');
        const body = this.expression();
        return {
            type: 'DefDeclaration',
            name,
            params,
            returnType,
            body
        };
    }
    expressionStatement() {
        return this.expression();
    }
    expression() {
        return this.logicalOr();
    }
    logicalOr() {
        let expr = this.logicalAnd();
        while (this.match(lexer_1.TokenType.OR)) {
            const operator = this.previous().value;
            const right = this.logicalAnd();
            expr = {
                type: 'BinaryExpression',
                operator,
                left: expr,
                right
            };
        }
        return expr;
    }
    logicalAnd() {
        let expr = this.equality();
        while (this.match(lexer_1.TokenType.AND)) {
            const operator = this.previous().value;
            const right = this.equality();
            expr = {
                type: 'BinaryExpression',
                operator,
                left: expr,
                right
            };
        }
        return expr;
    }
    equality() {
        let expr = this.comparison();
        while (this.match(lexer_1.TokenType.EQ, lexer_1.TokenType.NEQ)) {
            const operator = this.previous().value;
            const right = this.comparison();
            expr = {
                type: 'BinaryExpression',
                operator,
                left: expr,
                right
            };
        }
        return expr;
    }
    comparison() {
        let expr = this.addition();
        while (this.match(lexer_1.TokenType.GT, lexer_1.TokenType.GTE, lexer_1.TokenType.LT, lexer_1.TokenType.LTE)) {
            const operator = this.previous().value;
            const right = this.addition();
            expr = {
                type: 'BinaryExpression',
                operator,
                left: expr,
                right
            };
        }
        return expr;
    }
    addition() {
        let expr = this.multiplication();
        while (this.match(lexer_1.TokenType.PLUS, lexer_1.TokenType.MINUS)) {
            const operator = this.previous().value;
            const right = this.multiplication();
            expr = {
                type: 'BinaryExpression',
                operator,
                left: expr,
                right
            };
        }
        return expr;
    }
    multiplication() {
        let expr = this.unary();
        while (this.match(lexer_1.TokenType.STAR, lexer_1.TokenType.SLASH, lexer_1.TokenType.PERCENT)) {
            const operator = this.previous().value;
            const right = this.unary();
            expr = {
                type: 'BinaryExpression',
                operator,
                left: expr,
                right
            };
        }
        return expr;
    }
    unary() {
        if (this.match(lexer_1.TokenType.NOT, lexer_1.TokenType.MINUS)) {
            const operator = this.previous().value;
            const argument = this.unary();
            return {
                type: 'UnaryExpression',
                operator,
                argument
            };
        }
        return this.postfix();
    }
    postfix() {
        let expr = this.primary();
        while (true) {
            if (this.match(lexer_1.TokenType.DOT)) {
                const property = this.consume(lexer_1.TokenType.IDENTIFIER, 'Expected property name');
                expr = {
                    type: 'MemberExpression',
                    object: expr,
                    property: { type: 'Identifier', name: property.value }
                };
            }
            else if (this.match(lexer_1.TokenType.LPAREN)) {
                const args = [];
                if (!this.check(lexer_1.TokenType.RPAREN)) {
                    do {
                        args.push(this.expression());
                    } while (this.match(lexer_1.TokenType.COMMA));
                }
                this.consume(lexer_1.TokenType.RPAREN, 'Expected ) after arguments');
                expr = {
                    type: 'CallExpression',
                    callee: expr,
                    arguments: args
                };
            }
            else if (this.match(lexer_1.TokenType.LBRACKET)) {
                // Type parameter or array access
                const index = this.expression();
                this.consume(lexer_1.TokenType.RBRACKET, 'Expected ] after index');
                expr = {
                    type: 'CallExpression',
                    callee: expr,
                    arguments: [index]
                };
            }
            else {
                break;
            }
        }
        return expr;
    }
    primary() {
        if (this.match(lexer_1.TokenType.TRUE)) {
            return { type: 'BooleanLiteral', value: true };
        }
        if (this.match(lexer_1.TokenType.FALSE)) {
            return { type: 'BooleanLiteral', value: false };
        }
        if (this.match(lexer_1.TokenType.NUMBER)) {
            return { type: 'NumberLiteral', value: parseFloat(this.previous().value) };
        }
        if (this.match(lexer_1.TokenType.STRING)) {
            return { type: 'StringLiteral', value: this.previous().value };
        }
        if (this.match(lexer_1.TokenType.IDENTIFIER)) {
            return { type: 'Identifier', name: this.previous().value };
        }
        if (this.match(lexer_1.TokenType.LPAREN)) {
            const expr = this.expression();
            this.consume(lexer_1.TokenType.RPAREN, 'Expected ) after expression');
            return expr;
        }
        if (this.match(lexer_1.TokenType.LBRACE)) {
            return this.blockExpression();
        }
        if (this.match(lexer_1.TokenType.IF)) {
            return this.ifExpression();
        }
        throw new Error(`Unexpected token: ${this.peek().value}`);
    }
    blockExpression() {
        const statements = [];
        while (!this.check(lexer_1.TokenType.RBRACE) && !this.isAtEnd()) {
            const stmt = this.statement();
            if (stmt) {
                statements.push(stmt);
            }
        }
        this.consume(lexer_1.TokenType.RBRACE, 'Expected } after block');
        return {
            type: 'BlockExpression',
            statements
        };
    }
    ifExpression() {
        this.consume(lexer_1.TokenType.LPAREN, 'Expected ( after if');
        const condition = this.expression();
        this.consume(lexer_1.TokenType.RPAREN, 'Expected ) after condition');
        const consequent = this.expression();
        let alternate;
        if (this.match(lexer_1.TokenType.ELSE)) {
            alternate = this.expression();
        }
        return {
            type: 'IfExpression',
            condition,
            consequent,
            alternate
        };
    }
    typeAnnotation() {
        const typeName = this.consume(lexer_1.TokenType.IDENTIFIER, 'Expected type name').value;
        let typeParams;
        if (this.match(lexer_1.TokenType.LBRACKET)) {
            typeParams = [];
            do {
                typeParams.push(this.typeAnnotation());
            } while (this.match(lexer_1.TokenType.COMMA));
            this.consume(lexer_1.TokenType.RBRACKET, 'Expected ] after type parameters');
        }
        return {
            type: 'TypeAnnotation',
            typeName,
            typeParams
        };
    }
    // Helper methods
    match(...types) {
        for (const type of types) {
            if (this.check(type)) {
                this.advance();
                return true;
            }
        }
        return false;
    }
    check(type) {
        if (this.isAtEnd())
            return false;
        return this.peek().type === type;
    }
    advance() {
        if (!this.isAtEnd())
            this.current++;
        return this.previous();
    }
    isAtEnd() {
        return this.peek().type === lexer_1.TokenType.EOF;
    }
    peek() {
        return this.tokens[this.current];
    }
    previous() {
        return this.tokens[this.current - 1];
    }
    consume(type, message) {
        if (this.check(type))
            return this.advance();
        throw new Error(`${message} at line ${this.peek().line}`);
    }
    synchronize() {
        this.advance();
        while (!this.isAtEnd()) {
            if (this.previous().type === lexer_1.TokenType.SEMICOLON)
                return;
            switch (this.peek().type) {
                case lexer_1.TokenType.VAL:
                case lexer_1.TokenType.DEF:
                case lexer_1.TokenType.IF:
                case lexer_1.TokenType.RETURN:
                    return;
            }
            this.advance();
        }
    }
}
exports.Parser = Parser;
//# sourceMappingURL=parser.js.map