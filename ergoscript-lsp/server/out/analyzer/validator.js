"use strict";
/**
 * Semantic Validator for ErgoScript
 *
 * Performs semantic validation beyond type checking.
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.Validator = void 0;
class Validator {
    constructor(symbolTable) {
        this.errors = [];
        this.symbolTable = symbolTable;
    }
    validate(program) {
        this.errors = [];
        for (const statement of program.body) {
            this.validateStatement(statement);
        }
        return this.errors;
    }
    validateStatement(stmt) {
        if (stmt.type === 'ValDeclaration') {
            this.validateValDeclaration(stmt);
        }
        else if (stmt.type === 'DefDeclaration') {
            this.validateDefDeclaration(stmt);
        }
        else {
            this.validateExpression(stmt);
        }
    }
    validateValDeclaration(decl) {
        // Check if variable is already defined
        if (this.symbolTable.isDefined(decl.name)) {
            this.addWarning(`Variable '${decl.name}' is already defined`);
        }
        // Validate value expression
        this.validateExpression(decl.value);
    }
    validateDefDeclaration(decl) {
        // Check if function is already defined
        if (this.symbolTable.isDefined(decl.name)) {
            this.addWarning(`Function '${decl.name}' is already defined`);
        }
        // Validate body
        this.validateExpression(decl.body);
    }
    validateExpression(expr) {
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
    validateIdentifier(expr) {
        if (!this.symbolTable.isDefined(expr.name)) {
            this.addWarning(`'${expr.name}' might be undefined`, expr.loc?.start.line, expr.loc?.start.column);
        }
    }
    validateBinaryExpression(expr) {
        this.validateExpression(expr.left);
        this.validateExpression(expr.right);
    }
    validateUnaryExpression(expr) {
        this.validateExpression(expr.argument);
    }
    validateCallExpression(expr) {
        this.validateExpression(expr.callee);
        for (const arg of expr.arguments) {
            this.validateExpression(arg);
        }
    }
    validateMemberExpression(expr) {
        this.validateExpression(expr.object);
    }
    validateIfExpression(expr) {
        this.validateExpression(expr.condition);
        this.validateExpression(expr.consequent);
        if (expr.alternate) {
            this.validateExpression(expr.alternate);
        }
    }
    validateBlockExpression(expr) {
        for (const stmt of expr.statements) {
            this.validateStatement(stmt);
        }
    }
    addError(message, line, column) {
        this.errors.push({ message, line, column, severity: 'error' });
    }
    addWarning(message, line, column) {
        this.errors.push({ message, line, column, severity: 'warning' });
    }
    addInfo(message, line, column) {
        this.errors.push({ message, line, column, severity: 'info' });
    }
}
exports.Validator = Validator;
//# sourceMappingURL=validator.js.map