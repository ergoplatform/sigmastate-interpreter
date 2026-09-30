"use strict";
/**
 * Type Checker for ErgoScript
 *
 * Performs type inference and type checking on the AST.
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.TypeChecker = void 0;
const ergoTypes_1 = require("../utils/ergoTypes");
const symbols_1 = require("./symbols");
class TypeChecker {
    constructor() {
        this.symbolTable = new symbols_1.SymbolTable();
    }
    check(program) {
        const typeMap = new Map();
        for (const statement of program.body) {
            this.checkStatement(statement, typeMap);
        }
        return typeMap;
    }
    checkStatement(stmt, typeMap) {
        if (stmt.type === 'ValDeclaration') {
            this.checkValDeclaration(stmt, typeMap);
        }
        else if (stmt.type === 'DefDeclaration') {
            this.checkDefDeclaration(stmt, typeMap);
        }
        else {
            this.inferType(stmt, typeMap);
        }
    }
    checkValDeclaration(decl, typeMap) {
        const valueType = this.inferType(decl.value, typeMap);
        // If type annotation exists, check compatibility
        if (decl.typeAnnotation) {
            const declaredType = this.resolveTypeAnnotation(decl.typeAnnotation);
            if (!(0, ergoTypes_1.isCompatible)(valueType, declaredType)) {
                // Type mismatch - would report error
            }
            typeMap.set(decl, declaredType);
            this.symbolTable.define({
                name: decl.name,
                type: declaredType,
                kind: 'variable'
            });
        }
        else {
            typeMap.set(decl, valueType);
            this.symbolTable.define({
                name: decl.name,
                type: valueType,
                kind: 'variable'
            });
        }
    }
    checkDefDeclaration(decl, typeMap) {
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
            if (!(0, ergoTypes_1.isCompatible)(bodyType, declaredReturnType)) {
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
    inferType(expr, typeMap) {
        let type;
        switch (expr.type) {
            case 'NumberLiteral':
                type = ergoTypes_1.IntType;
                break;
            case 'BooleanLiteral':
                type = ergoTypes_1.BooleanType;
                break;
            case 'StringLiteral':
                type = (0, ergoTypes_1.getType)('String');
                break;
            case 'Identifier':
                const symbol = this.symbolTable.lookup(expr.name);
                type = symbol ? symbol.type : ergoTypes_1.UnknownType;
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
                type = ergoTypes_1.UnknownType;
        }
        typeMap.set(expr, type);
        return type;
    }
    inferBinaryExpression(expr, typeMap) {
        const leftType = this.inferType(expr.left, typeMap);
        const rightType = this.inferType(expr.right, typeMap);
        // Arithmetic operators
        if (['+', '-', '*', '/', '%'].includes(expr.operator)) {
            if ((0, ergoTypes_1.isNumericType)(leftType) && (0, ergoTypes_1.isNumericType)(rightType)) {
                return leftType; // Return left type for now
            }
        }
        // Comparison operators
        if (['<', '>', '<=', '>=', '==', '!='].includes(expr.operator)) {
            return ergoTypes_1.BooleanType;
        }
        // Logical operators
        if (['&&', '||'].includes(expr.operator)) {
            return ergoTypes_1.BooleanType;
        }
        return ergoTypes_1.UnknownType;
    }
    inferUnaryExpression(expr, typeMap) {
        const argType = this.inferType(expr.argument, typeMap);
        if (expr.operator === '!') {
            return ergoTypes_1.BooleanType;
        }
        if (expr.operator === '-') {
            return argType;
        }
        return ergoTypes_1.UnknownType;
    }
    inferCallExpression(expr, typeMap) {
        // Infer callee type
        this.inferType(expr.callee, typeMap);
        // Infer argument types
        for (const arg of expr.arguments) {
            this.inferType(arg, typeMap);
        }
        // For now, return unknown - would need function signature lookup
        return ergoTypes_1.UnknownType;
    }
    inferMemberExpression(expr, typeMap) {
        const objectType = this.inferType(expr.object, typeMap);
        // Would lookup property type based on object type
        // For now, return unknown
        return ergoTypes_1.UnknownType;
    }
    inferIfExpression(expr, typeMap) {
        this.inferType(expr.condition, typeMap);
        const consequentType = this.inferType(expr.consequent, typeMap);
        if (expr.alternate) {
            const alternateType = this.inferType(expr.alternate, typeMap);
            // Return common type
            return (0, ergoTypes_1.isCompatible)(consequentType, alternateType) ? consequentType : ergoTypes_1.UnknownType;
        }
        return consequentType;
    }
    inferBlockExpression(expr, typeMap) {
        this.symbolTable.pushScope();
        let lastType = (0, ergoTypes_1.getType)('Unit');
        for (const stmt of expr.statements) {
            this.checkStatement(stmt, typeMap);
            if (stmt.type !== 'ValDeclaration' && stmt.type !== 'DefDeclaration') {
                lastType = this.inferType(stmt, typeMap);
            }
        }
        this.symbolTable.popScope();
        return lastType;
    }
    resolveTypeAnnotation(annotation) {
        return (0, ergoTypes_1.getType)(annotation.typeName);
    }
    getSymbolTable() {
        return this.symbolTable;
    }
}
exports.TypeChecker = TypeChecker;
//# sourceMappingURL=typeChecker.js.map