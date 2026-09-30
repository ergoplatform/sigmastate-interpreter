/**
 * ErgoScript AST (Abstract Syntax Tree) Node Definitions
 */

export type ASTNode =
    | Program
    | ValDeclaration
    | DefDeclaration
    | BlockExpression
    | BinaryExpression
    | UnaryExpression
    | CallExpression
    | MemberExpression
    | IfExpression
    | LambdaExpression
    | Identifier
    | NumberLiteral
    | BooleanLiteral
    | StringLiteral
    | TypeAnnotation;

export interface Position {
    line: number;
    column: number;
    offset: number;
}

export interface SourceLocation {
    start: Position;
    end: Position;
}

export interface BaseNode {
    type: string;
    loc?: SourceLocation;
}

export interface Program extends BaseNode {
    type: 'Program';
    body: Statement[];
}

export type Statement = ValDeclaration | DefDeclaration | Expression;

export interface ValDeclaration extends BaseNode {
    type: 'ValDeclaration';
    name: string;
    typeAnnotation?: TypeAnnotation;
    value: Expression;
}

export interface DefDeclaration extends BaseNode {
    type: 'DefDeclaration';
    name: string;
    params: Parameter[];
    returnType?: TypeAnnotation;
    body: Expression;
}

export interface Parameter {
    name: string;
    type: TypeAnnotation;
}

export type Expression =
    | BlockExpression
    | BinaryExpression
    | UnaryExpression
    | CallExpression
    | MemberExpression
    | IfExpression
    | LambdaExpression
    | Identifier
    | NumberLiteral
    | BooleanLiteral
    | StringLiteral;

export interface BlockExpression extends BaseNode {
    type: 'BlockExpression';
    statements: Statement[];
}

export interface BinaryExpression extends BaseNode {
    type: 'BinaryExpression';
    operator: string;
    left: Expression;
    right: Expression;
}

export interface UnaryExpression extends BaseNode {
    type: 'UnaryExpression';
    operator: string;
    argument: Expression;
}

export interface CallExpression extends BaseNode {
    type: 'CallExpression';
    callee: Expression;
    arguments: Expression[];
}

export interface MemberExpression extends BaseNode {
    type: 'MemberExpression';
    object: Expression;
    property: Identifier;
}

export interface IfExpression extends BaseNode {
    type: 'IfExpression';
    condition: Expression;
    consequent: Expression;
    alternate?: Expression;
}

export interface LambdaExpression extends BaseNode {
    type: 'LambdaExpression';
    params: Parameter[];
    body: Expression;
}

export interface Identifier extends BaseNode {
    type: 'Identifier';
    name: string;
}

export interface NumberLiteral extends BaseNode {
    type: 'NumberLiteral';
    value: number;
}

export interface BooleanLiteral extends BaseNode {
    type: 'BooleanLiteral';
    value: boolean;
}

export interface StringLiteral extends BaseNode {
    type: 'StringLiteral';
    value: string;
}

export interface TypeAnnotation extends BaseNode {
    type: 'TypeAnnotation';
    typeName: string;
    typeParams?: TypeAnnotation[];
}
