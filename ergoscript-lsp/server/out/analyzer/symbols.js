"use strict";
/**
 * Symbol Table for ErgoScript
 *
 * Manages symbols (variables, functions) and their types during analysis.
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.SymbolTable = void 0;
class SymbolTable {
    constructor() {
        this.scopes = [];
        this.pushScope(); // Global scope
    }
    pushScope() {
        this.scopes.push(new Map());
    }
    popScope() {
        this.scopes.pop();
    }
    define(symbol) {
        const currentScope = this.currentScope();
        currentScope.set(symbol.name, symbol);
    }
    lookup(name) {
        // Search from innermost to outermost scope
        for (let i = this.scopes.length - 1; i >= 0; i--) {
            const symbol = this.scopes[i].get(name);
            if (symbol) {
                return symbol;
            }
        }
        return undefined;
    }
    isDefined(name) {
        return this.lookup(name) !== undefined;
    }
    getSymbolsInScope() {
        const symbols = [];
        for (const scope of this.scopes) {
            symbols.push(...scope.values());
        }
        return symbols;
    }
    currentScope() {
        return this.scopes[this.scopes.length - 1];
    }
}
exports.SymbolTable = SymbolTable;
//# sourceMappingURL=symbols.js.map