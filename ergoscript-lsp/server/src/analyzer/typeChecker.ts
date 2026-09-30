/**
 * Type Checker for ErgoScript
 * 
 * Performs type inference and type checking on the AST.
 */

import * as AST from '../parser/ast';
import { ErgoType, getType, isCompatible, IntType, BooleanType, UnknownType, isNumericType } from '../utils/ergoTypes';
import { SymbolTable } from './symbols';

export class TypeChecker {
    private symbolTable: SymbolTable;

    constructor() {
        this.symbolTable = new SymbolTable();
    }

    check(program: AST.Program): Map<AST.ASTNode, ErgoType> {
        const typeMap = new Map<AST.ASTNode, ErgoType>();

        for (const statement of program.body) {
            this.checkStatement(statement, typeMap);
        }

        return typeMap;
    }

    private checkStatement(stmt: AST.Statement, typeMap: Map<AST.ASTNode, ErgoType>): void {
        if (stmt.type === 'ValDeclaration') {
            this.checkValDeclaration(stmt, typeMap);
        } else if (stmt.type === 'DefDeclaration') {
            this.checkDefDeclaration(stmt, typeMap);
        } else {
            this.inferType(stmt as AST.Expression, typeMap);
        }
    }

    private checkValDeclaration(decl: AST.ValDeclaration, typeMap: Map<AST.ASTNode, ErgoType>): void {
        const valueType = this.inferType(decl.value, typeMap);

        // If type annotation exists, check compatibility
        if (decl.typeAnnotation) {
            const declaredType = this.resolveTypeAnnotation(decl.typeAnnotation);
            if (!isCompatible(valueType, declaredType)) {
                // Type mismatch - would report error
            }
            typeMap.set(decl, declaredType);
            this.symbolTable.define({
                name: decl.name,
                type: declaredType,
                kind: 'variable'
            });
        } else {
            typeMap.set(decl, valueType);
            this.symbolTable.define({
                name: decl.name,
                type: valueType,
                kind: 'variable'
            });
        }
    }

    private checkDefDeclaration(decl: AST.DefDeclaration, typeMap: Map<AST.ASTNode, ErgoType>): void {
        // Push new scope for function
        this.symbolTable.pushScope();

        // Add parameters to scope
        for (const param of decl.params) {
            const paramType = this.resolveTypeAnnotation(param.type);
            this.symbolTable.define({
                name: param.name,
                type: paramType,
                kind: 'parameter'
            });
        }

        // Infer body type
        const bodyType = this.inferType(decl.body, typeMap);

        // Check return type if specified
        if (decl.returnType) {
            const declaredReturnType = this.resolveTypeAnnotation(decl.returnType);
            if (!isCompatible(bodyType, declaredReturnType)) {
                // Type mismatch - would report error
            }
        }

        this.symbolTable.popScope();

        // Define function in outer scope
        const returnType = decl.returnType ? this.resolveTypeAnnotation(decl.returnType) : bodyType;
        this.symbolTable.define({
            name: decl.name,
            type: returnType,
            kind: 'function'
        });

        typeMap.set(decl, returnType);
    }

    private inferType(expr: AST.Expression, typeMap: Map<AST.ASTNode, ErgoType>): ErgoType {
        let type: ErgoType;

        switch (expr.type) {
            case 'NumberLiteral':
                type = IntType;
                break;

            case 'BooleanLiteral':
                type = BooleanType;
                break;

            case 'StringLiteral':
                type = getType('String');
                break;

            case 'Identifier':
                const symbol = this.symbolTable.lookup(expr.name);
                type = symbol ? symbol.type : UnknownType;
                break;

            case 'BinaryExpression':
                type = this.inferBinaryExpression(expr, typeMap);
                break;

            case 'UnaryExpression':
                type = this.inferUnaryExpression(expr, typeMap);
                break;

            case 'CallExpression':
                type = this.inferCallExpression(expr, typeMap);
                break;

            case 'MemberExpression':
                type = this.inferMemberExpression(expr, typeMap);
                break;

            case 'IfExpression':
                type = this.inferIfExpression(expr, typeMap);
                break;

            case 'BlockExpression':
                type = this.inferBlockExpression(expr, typeMap);
                break;

            default:
                type = UnknownType;
        }

        typeMap.set(expr, type);
        return type;
    }

    private inferBinaryExpression(expr: AST.BinaryExpression, typeMap: Map<AST.ASTNode, ErgoType>): ErgoType {
        const leftType = this.inferType(expr.left, typeMap);
        const rightType = this.inferType(expr.right, typeMap);

        // Arithmetic operators
        if (['+', '-', '*', '/', '%'].includes(expr.operator)) {
            if (isNumericType(leftType) && isNumericType(rightType)) {
                return leftType; // Return left type for now
            }
        }

        // Comparison operators
        if (['<', '>', '<=', '>=', '==', '!='].includes(expr.operator)) {
            return BooleanType;
        }

        // Logical operators
        if (['&&', '||'].includes(expr.operator)) {
            return BooleanType;
        }

        return UnknownType;
    }

    private inferUnaryExpression(expr: AST.UnaryExpression, typeMap: Map<AST.ASTNode, ErgoType>): ErgoType {
        const argType = this.inferType(expr.argument, typeMap);

        if (expr.operator === '!') {
            return BooleanType;
        }
        if (expr.operator === '-') {
            return argType;
        }

        return UnknownType;
    }

    private inferCallExpression(expr: AST.CallExpression, typeMap: Map<AST.ASTNode, ErgoType>): ErgoType {
        // Infer callee type
        this.inferType(expr.callee, typeMap);

        // Infer argument types
        for (const arg of expr.arguments) {
            this.inferType(arg, typeMap);
        }

        // For now, return unknown - would need function signature lookup
        return UnknownType;
    }

    private inferMemberExpression(expr: AST.MemberExpression, typeMap: Map<AST.ASTNode, ErgoType>): ErgoType {
        const objectType = this.inferType(expr.object, typeMap);

        // Would lookup property type based on object type
        // For now, return unknown
        return UnknownType;
    }

    private inferIfExpression(expr: AST.IfExpression, typeMap: Map<AST.ASTNode, ErgoType>): ErgoType {
        this.inferType(expr.condition, typeMap);
        const consequentType = this.inferType(expr.consequent, typeMap);

        if (expr.alternate) {
            const alternateType = this.inferType(expr.alternate, typeMap);
            // Return common type
            return isCompatible(consequentType, alternateType) ? consequentType : UnknownType;
        }

        return consequentType;
    }

    private inferBlockExpression(expr: AST.BlockExpression, typeMap: Map<AST.ASTNode, ErgoType>): ErgoType {
        this.symbolTable.pushScope();

        let lastType: ErgoType = getType('Unit');
        for (const stmt of expr.statements) {
            this.checkStatement(stmt, typeMap);
            if (stmt.type !== 'ValDeclaration' && stmt.type !== 'DefDeclaration') {
                lastType = this.inferType(stmt as AST.Expression, typeMap);
            }
        }

        this.symbolTable.popScope();
        return lastType;
    }

    private resolveTypeAnnotation(annotation: AST.TypeAnnotation): ErgoType {
        return getType(annotation.typeName);
    }

    getSymbolTable(): SymbolTable {
        return this.symbolTable;
    }
}
