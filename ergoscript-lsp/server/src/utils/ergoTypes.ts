/**
 * ErgoScript Type System
 * 
 * Defines the type system for ErgoScript including primitives,
 * complex types, and type operations.
 */

export enum ErgoTypeKind {
    Primitive = 'Primitive',
    Collection = 'Collection',
    Option = 'Option',
    Tuple = 'Tuple',
    Box = 'Box',
    SigmaProp = 'SigmaProp',
    GroupElement = 'GroupElement',
    AvlTree = 'AvlTree',
    Header = 'Header',
    Context = 'Context',
    Unknown = 'Unknown'
}

export interface ErgoType {
    kind: ErgoTypeKind;
    name: string;
    description?: string;
    typeParams?: ErgoType[];
}

// Primitive types
export const IntType: ErgoType = {
    kind: ErgoTypeKind.Primitive,
    name: 'Int',
    description: '32-bit signed integer'
};

export const LongType: ErgoType = {
    kind: ErgoTypeKind.Primitive,
    name: 'Long',
    description: '64-bit signed integer'
};

export const BigIntType: ErgoType = {
    kind: ErgoTypeKind.Primitive,
    name: 'BigInt',
    description: '256-bit signed integer'
};

export const UnsignedBigIntType: ErgoType = {
    kind: ErgoTypeKind.Primitive,
    name: 'UnsignedBigInt',
    description: '256-bit unsigned integer'
};

export const BooleanType: ErgoType = {
    kind: ErgoTypeKind.Primitive,
    name: 'Boolean',
    description: 'Logical type with values true and false'
};

export const ByteType: ErgoType = {
    kind: ErgoTypeKind.Primitive,
    name: 'Byte',
    description: '8-bit signed integer'
};

export const ShortType: ErgoType = {
    kind: ErgoTypeKind.Primitive,
    name: 'Short',
    description: '16-bit signed integer'
};

export const StringType: ErgoType = {
    kind: ErgoTypeKind.Primitive,
    name: 'String',
    description: 'String of characters'
};

export const UnitType: ErgoType = {
    kind: ErgoTypeKind.Primitive,
    name: 'Unit',
    description: 'Type with a single value ()'
};

// Special types
export const BoxType: ErgoType = {
    kind: ErgoTypeKind.Box,
    name: 'Box',
    description: 'UTXO box containing value, tokens, and registers'
};

export const SigmaPropType: ErgoType = {
    kind: ErgoTypeKind.SigmaProp,
    name: 'SigmaProp',
    description: 'Sigma proposition that can be proven via zero-knowledge proof'
};

export const GroupElementType: ErgoType = {
    kind: ErgoTypeKind.GroupElement,
    name: 'GroupElement',
    description: 'Elliptic curve point'
};

export const AvlTreeType: ErgoType = {
    kind: ErgoTypeKind.AvlTree,
    name: 'AvlTree',
    description: 'Authenticated dynamic dictionary digest'
};

export const HeaderType: ErgoType = {
    kind: ErgoTypeKind.Header,
    name: 'Header',
    description: 'Block header data'
};

export const ContextType: ErgoType = {
    kind: ErgoTypeKind.Context,
    name: 'Context',
    description: 'Execution context with blockchain and transaction data'
};

export const UnknownType: ErgoType = {
    kind: ErgoTypeKind.Unknown,
    name: 'Unknown',
    description: 'Unknown type'
};

// Type constructors
export function CollType(elementType: ErgoType): ErgoType {
    return {
        kind: ErgoTypeKind.Collection,
        name: `Coll[${elementType.name}]`,
        description: `Collection of ${elementType.name}`,
        typeParams: [elementType]
    };
}

export function OptionType(valueType: ErgoType): ErgoType {
    return {
        kind: ErgoTypeKind.Option,
        name: `Option[${valueType.name}]`,
        description: `Optional ${valueType.name}`,
        typeParams: [valueType]
    };
}

export function TupleType(types: ErgoType[]): ErgoType {
    const typeNames = types.map(t => t.name).join(', ');
    return {
        kind: ErgoTypeKind.Tuple,
        name: `(${typeNames})`,
        description: `Tuple of ${typeNames}`,
        typeParams: types
    };
}

// Type registry
export const ERGO_TYPES: Map<string, ErgoType> = new Map([
    ['Int', IntType],
    ['Long', LongType],
    ['BigInt', BigIntType],
    ['UnsignedBigInt', UnsignedBigIntType],
    ['Boolean', BooleanType],
    ['Byte', ByteType],
    ['Short', ShortType],
    ['String', StringType],
    ['Unit', UnitType],
    ['Box', BoxType],
    ['SigmaProp', SigmaPropType],
    ['GroupElement', GroupElementType],
    ['AvlTree', AvlTreeType],
    ['Header', HeaderType],
    ['Context', ContextType]
]);

// Type operations
export function getType(typeName: string): ErgoType {
    return ERGO_TYPES.get(typeName) || UnknownType;
}

export function isNumericType(type: ErgoType): boolean {
    return ['Int', 'Long', 'BigInt', 'UnsignedBigInt', 'Byte', 'Short'].includes(type.name);
}

export function isCompatible(type1: ErgoType, type2: ErgoType): boolean {
    if (type1.name === type2.name) {
        return true;
    }

    // Numeric type compatibility
    if (isNumericType(type1) && isNumericType(type2)) {
        return true; // Allow implicit conversions
    }

    // Collection compatibility
    if (type1.kind === ErgoTypeKind.Collection && type2.kind === ErgoTypeKind.Collection) {
        if (type1.typeParams && type2.typeParams) {
            return isCompatible(type1.typeParams[0], type2.typeParams[0]);
        }
    }

    // Option compatibility
    if (type1.kind === ErgoTypeKind.Option && type2.kind === ErgoTypeKind.Option) {
        if (type1.typeParams && type2.typeParams) {
            return isCompatible(type1.typeParams[0], type2.typeParams[0]);
        }
    }

    return false;
}

export function formatType(type: ErgoType): string {
    return type.name;
}
