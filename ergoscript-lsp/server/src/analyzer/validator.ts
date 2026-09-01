/**
 * Semantic Validator for ErgoScript
 * 
 * Performs semantic validation beyond type checking.
 */

import * as AST from '../parser/ast';
import { SymbolTable } from './symbols';

export interface ValidationError {
    message: string;
    line?: number;
    column?: number;
    severity: 'error' | 'warning' | 'info';
}

export class Validator {
    private errors: ValidationError[] = [];
    private symbolTable: SymbolTable;

    constructor(symbolTable: SymbolTable) {
        this.symbolTable = symbolTable;
    }

    validate(program: AST.Program): ValidationError[] {
        this.errors = [];

        for (const statement of program.body) {
            this.validateStatement(statement);
        }

        return this.errors;
    }

    private validateStatement(stmt: AST.Statement): void {
        if (stmt.type === 'ValDeclaration') {
            this.validateValDeclaration(stmt);
        } else if (stmt.type === 'DefDeclaration') {
            this.validateDefDeclaration(stmt);
        } else {
            this.validateExpression(stmt as AST.Expression);
        }
    }

    private validateValDeclaration(decl: AST.ValDeclaration): void {
        // Check if variable is already defined
        if (this.symbolTable.isDefined(decl.name)) {
            this.addWarning(`Variable '${decl.name}' is already defined`);
        }

        // Validate value expression
        this.validateExpression(decl.value);
    }

    private validateDefDeclaration(decl: AST.DefDeclaration): void {
        // Check if function is already defined
        if (this.symbolTable.isDefined(decl.name)) {
            this.addWarning(`Function '${decl.name}' is already defined`);
        }

        // Validate body
        this.validateExpression(decl.body);
    }

    private validateExpression(expr: AST.Expression): void {
        switch (expr.type) {
            case 'Identifier':
                this.validateIdentifier(expr);
                break;
            case 'BinaryExpression':
                this.validateBinaryExpression(expr);
                break;
            case 'UnaryExpression':
                this.validateUnaryExpression(expr);
                break;
            case 'CallExpression':
                this.validateCallExpression(expr);
                break;
            case 'MemberExpression':
                this.validateMemberExpression(expr);
                break;
            case 'IfExpression':
                this.validateIfExpression(expr);
                break;
            case 'BlockExpression':
                this.validateBlockExpression(expr);
                break;
        }
    }

    private validateIdentifier(expr: AST.Identifier): void {
        if (!this.symbolTable.isDefined(expr.name)) {
            this.addWarning(`'${expr.name}' might be undefined`, expr.loc?.start.line, expr.loc?.start.column);
        }
    }

    private validateBinaryExpression(expr: AST.BinaryExpression): void {
        this.validateExpression(expr.left);
        this.validateExpression(expr.right);
    }

    private validateUnaryExpression(expr: AST.UnaryExpression): void {
        this.validateExpression(expr.argument);
    }

    private validateCallExpression(expr: AST.CallExpression): void {
        this.validateExpression(expr.callee);
        for (const arg of expr.arguments) {
            this.validateExpression(arg);
        }
    }

    private validateMemberExpression(expr: AST.MemberExpression): void {
        this.validateExpression(expr.object);
    }

    private validateIfExpression(expr: AST.IfExpression): void {
        this.validateExpression(expr.condition);
        this.validateExpression(expr.consequent);
        if (expr.alternate) {
            this.validateExpression(expr.alternate);
        }
    }

    private validateBlockExpression(expr: AST.BlockExpression): void {
        for (const stmt of expr.statements) {
            this.validateStatement(stmt);
        }
    }

    private addError(message: string, line?: number, column?: number): void {
        this.errors.push({ message, line, column, severity: 'error' });
    }

    private addWarning(message: string, line?: number, column?: number): void {
        this.errors.push({ message, line, column, severity: 'warning' });
    }

    private addInfo(message: string, line?: number, column?: number): void {
        this.errors.push({ message, line, column, severity: 'info' });
    }
}
