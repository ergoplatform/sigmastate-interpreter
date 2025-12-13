/**
 * ErgoScript Parser
 * 
 * Parses tokens into an Abstract Syntax Tree (AST).
 */

import { Token, TokenType, Lexer } from './lexer';
import * as AST from './ast';

export class Parser {
    private tokens: Token[];
    private current: number = 0;

    constructor(input: string) {
        const lexer = new Lexer(input);
        this.tokens = lexer.tokenize();
    }

    parse(): AST.Program {
        const body: AST.Statement[] = [];

        while (!this.isAtEnd()) {
            if (this.check(TokenType.EOF)) break;
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

    private statement(): AST.Statement | null {
        try {
            if (this.match(TokenType.VAL)) {
                return this.valDeclaration();
            }
            if (this.match(TokenType.DEF)) {
                return this.defDeclaration();
            }
            return this.expressionStatement();
        } catch (error) {
            // Error recovery: skip to next statement
            this.synchronize();
            return null;
        }
    }

    private valDeclaration(): AST.ValDeclaration {
        const name = this.consume(TokenType.IDENTIFIER, 'Expected variable name').value;

        let typeAnnotation: AST.TypeAnnotation | undefined;
        if (this.match(TokenType.COLON)) {
            typeAnnotation = this.typeAnnotation();
        }

        this.consume(TokenType.ASSIGN, 'Expected = after variable name');
        const value = this.expression();

        return {
            type: 'ValDeclaration',
            name,
            typeAnnotation,
            value
        };
    }

    private defDeclaration(): AST.DefDeclaration {
        const name = this.consume(TokenType.IDENTIFIER, 'Expected function name').value;

        this.consume(TokenType.LPAREN, 'Expected ( after function name');
        const params: AST.Parameter[] = [];

        if (!this.check(TokenType.RPAREN)) {
            do {
                const paramName = this.consume(TokenType.IDENTIFIER, 'Expected parameter name').value;
                this.consume(TokenType.COLON, 'Expected : after parameter name');
                const paramType = this.typeAnnotation();
                params.push({ name: paramName, type: paramType });
            } while (this.match(TokenType.COMMA));
        }

        this.consume(TokenType.RPAREN, 'Expected ) after parameters');

        let returnType: AST.TypeAnnotation | undefined;
        if (this.match(TokenType.COLON)) {
            returnType = this.typeAnnotation();
        }

        this.consume(TokenType.ASSIGN, 'Expected = before function body');
        const body = this.expression();

        return {
            type: 'DefDeclaration',
            name,
            params,
            returnType,
            body
        };
    }

    private expressionStatement(): AST.Expression {
        return this.expression();
    }

    private expression(): AST.Expression {
        return this.logicalOr();
    }

    private logicalOr(): AST.Expression {
        let expr = this.logicalAnd();

        while (this.match(TokenType.OR)) {
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

    private logicalAnd(): AST.Expression {
        let expr = this.equality();

        while (this.match(TokenType.AND)) {
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

    private equality(): AST.Expression {
        let expr = this.comparison();

        while (this.match(TokenType.EQ, TokenType.NEQ)) {
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

    private comparison(): AST.Expression {
        let expr = this.addition();

        while (this.match(TokenType.GT, TokenType.GTE, TokenType.LT, TokenType.LTE)) {
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

    private addition(): AST.Expression {
        let expr = this.multiplication();

        while (this.match(TokenType.PLUS, TokenType.MINUS)) {
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

    private multiplication(): AST.Expression {
        let expr = this.unary();

        while (this.match(TokenType.STAR, TokenType.SLASH, TokenType.PERCENT)) {
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

    private unary(): AST.Expression {
        if (this.match(TokenType.NOT, TokenType.MINUS)) {
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

    private postfix(): AST.Expression {
        let expr = this.primary();

        while (true) {
            if (this.match(TokenType.DOT)) {
                const property = this.consume(TokenType.IDENTIFIER, 'Expected property name');
                expr = {
                    type: 'MemberExpression',
                    object: expr,
                    property: { type: 'Identifier', name: property.value }
                };
            } else if (this.match(TokenType.LPAREN)) {
                const args: AST.Expression[] = [];
                if (!this.check(TokenType.RPAREN)) {
                    do {
                        args.push(this.expression());
                    } while (this.match(TokenType.COMMA));
                }
                this.consume(TokenType.RPAREN, 'Expected ) after arguments');
                expr = {
                    type: 'CallExpression',
                    callee: expr,
                    arguments: args
                };
            } else if (this.match(TokenType.LBRACKET)) {
                // Type parameter or array access
                const index = this.expression();
                this.consume(TokenType.RBRACKET, 'Expected ] after index');
                expr = {
                    type: 'CallExpression',
                    callee: expr,
                    arguments: [index]
                };
            } else {
                break;
            }
        }

        return expr;
    }

    private primary(): AST.Expression {
        if (this.match(TokenType.TRUE)) {
            return { type: 'BooleanLiteral', value: true };
        }
        if (this.match(TokenType.FALSE)) {
            return { type: 'BooleanLiteral', value: false };
        }
        if (this.match(TokenType.NUMBER)) {
            return { type: 'NumberLiteral', value: parseFloat(this.previous().value) };
        }
        if (this.match(TokenType.STRING)) {
            return { type: 'StringLiteral', value: this.previous().value };
        }
        if (this.match(TokenType.IDENTIFIER)) {
            return { type: 'Identifier', name: this.previous().value };
        }
        if (this.match(TokenType.LPAREN)) {
            const expr = this.expression();
            this.consume(TokenType.RPAREN, 'Expected ) after expression');
            return expr;
        }
        if (this.match(TokenType.LBRACE)) {
            return this.blockExpression();
        }
        if (this.match(TokenType.IF)) {
            return this.ifExpression();
        }

        throw new Error(`Unexpected token: ${this.peek().value}`);
    }

    private blockExpression(): AST.BlockExpression {
        const statements: AST.Statement[] = [];

        while (!this.check(TokenType.RBRACE) && !this.isAtEnd()) {
            const stmt = this.statement();
            if (stmt) {
                statements.push(stmt);
            }
        }

        this.consume(TokenType.RBRACE, 'Expected } after block');

        return {
            type: 'BlockExpression',
            statements
        };
    }

    private ifExpression(): AST.IfExpression {
        this.consume(TokenType.LPAREN, 'Expected ( after if');
        const condition = this.expression();
        this.consume(TokenType.RPAREN, 'Expected ) after condition');

        const consequent = this.expression();

        let alternate: AST.Expression | undefined;
        if (this.match(TokenType.ELSE)) {
            alternate = this.expression();
        }

        return {
            type: 'IfExpression',
            condition,
            consequent,
            alternate
        };
    }

    private typeAnnotation(): AST.TypeAnnotation {
        const typeName = this.consume(TokenType.IDENTIFIER, 'Expected type name').value;

        let typeParams: AST.TypeAnnotation[] | undefined;
        if (this.match(TokenType.LBRACKET)) {
            typeParams = [];
            do {
                typeParams.push(this.typeAnnotation());
            } while (this.match(TokenType.COMMA));
            this.consume(TokenType.RBRACKET, 'Expected ] after type parameters');
        }

        return {
            type: 'TypeAnnotation',
            typeName,
            typeParams
        };
    }

    // Helper methods
    private match(...types: TokenType[]): boolean {
        for (const type of types) {
            if (this.check(type)) {
                this.advance();
                return true;
            }
        }
        return false;
    }

    private check(type: TokenType): boolean {
        if (this.isAtEnd()) return false;
        return this.peek().type === type;
    }

    private advance(): Token {
        if (!this.isAtEnd()) this.current++;
        return this.previous();
    }

    private isAtEnd(): boolean {
        return this.peek().type === TokenType.EOF;
    }

    private peek(): Token {
        return this.tokens[this.current];
    }

    private previous(): Token {
        return this.tokens[this.current - 1];
    }

    private consume(type: TokenType, message: string): Token {
        if (this.check(type)) return this.advance();
        throw new Error(`${message} at line ${this.peek().line}`);
    }

    private synchronize(): void {
        this.advance();

        while (!this.isAtEnd()) {
            if (this.previous().type === TokenType.SEMICOLON) return;

            switch (this.peek().type) {
                case TokenType.VAL:
                case TokenType.DEF:
                case TokenType.IF:
                case TokenType.RETURN:
                    return;
            }

            this.advance();
        }
    }
}
