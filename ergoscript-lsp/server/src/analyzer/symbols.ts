/**
 * Symbol Table for ErgoScript
 * 
 * Manages symbols (variables, functions) and their types during analysis.
 */

import { ErgoType } from '../utils/ergoTypes';

export interface Symbol {
    name: string;
    type: ErgoType;
    kind: 'variable' | 'function' | 'parameter';
    line?: number;
    column?: number;
}

export class SymbolTable {
    private scopes: Map<string, Symbol>[] = [];

    constructor() {
        this.pushScope(); // Global scope
    }

    pushScope(): void {
        this.scopes.push(new Map());
    }

    popScope(): void {
        this.scopes.pop();
    }

    define(symbol: Symbol): void {
        const currentScope = this.currentScope();
        currentScope.set(symbol.name, symbol);
    }

    lookup(name: string): Symbol | undefined {
        // Search from innermost to outermost scope
        for (let i = this.scopes.length - 1; i >= 0; i--) {
            const symbol = this.scopes[i].get(name);
            if (symbol) {
                return symbol;
            }
        }
        return undefined;
    }

    isDefined(name: string): boolean {
        return this.lookup(name) !== undefined;
    }

    getSymbolsInScope(): Symbol[] {
        const symbols: Symbol[] = [];
        for (const scope of this.scopes) {
            symbols.push(...scope.values());
        }
        return symbols;
    }

    private currentScope(): Map<string, Symbol> {
        return this.scopes[this.scopes.length - 1];
    }
}
