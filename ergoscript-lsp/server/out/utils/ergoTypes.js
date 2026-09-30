"use strict";
/**
 * ErgoScript Type System
 *
 * Defines the type system for ErgoScript including primitives,
 * complex types, and type operations.
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.ERGO_TYPES = exports.UnknownType = exports.ContextType = exports.HeaderType = exports.AvlTreeType = exports.GroupElementType = exports.SigmaPropType = exports.BoxType = exports.UnitType = exports.StringType = exports.ShortType = exports.ByteType = exports.BooleanType = exports.UnsignedBigIntType = exports.BigIntType = exports.LongType = exports.IntType = exports.ErgoTypeKind = void 0;
exports.CollType = CollType;
exports.OptionType = OptionType;
exports.TupleType = TupleType;
exports.getType = getType;
exports.isNumericType = isNumericType;
exports.isCompatible = isCompatible;
exports.formatType = formatType;
var ErgoTypeKind;
(function (ErgoTypeKind) {
    ErgoTypeKind["Primitive"] = "Primitive";
    ErgoTypeKind["Collection"] = "Collection";
    ErgoTypeKind["Option"] = "Option";
    ErgoTypeKind["Tuple"] = "Tuple";
    ErgoTypeKind["Box"] = "Box";
    ErgoTypeKind["SigmaProp"] = "SigmaProp";
    ErgoTypeKind["GroupElement"] = "GroupElement";
    ErgoTypeKind["AvlTree"] = "AvlTree";
    ErgoTypeKind["Header"] = "Header";
    ErgoTypeKind["Context"] = "Context";
    ErgoTypeKind["Unknown"] = "Unknown";
})(ErgoTypeKind || (exports.ErgoTypeKind = ErgoTypeKind = {}));
// Primitive types
exports.IntType = {
    kind: ErgoTypeKind.Primitive,
    name: 'Int',
    description: '32-bit signed integer'
};
exports.LongType = {
    kind: ErgoTypeKind.Primitive,
    name: 'Long',
    description: '64-bit signed integer'
};
exports.BigIntType = {
    kind: ErgoTypeKind.Primitive,
    name: 'BigInt',
    description: '256-bit signed integer'
};
exports.UnsignedBigIntType = {
    kind: ErgoTypeKind.Primitive,
    name: 'UnsignedBigInt',
    description: '256-bit unsigned integer'
};
exports.BooleanType = {
    kind: ErgoTypeKind.Primitive,
    name: 'Boolean',
    description: 'Logical type with values true and false'
};
exports.ByteType = {
    kind: ErgoTypeKind.Primitive,
    name: 'Byte',
    description: '8-bit signed integer'
};
exports.ShortType = {
    kind: ErgoTypeKind.Primitive,
    name: 'Short',
    description: '16-bit signed integer'
};
exports.StringType = {
    kind: ErgoTypeKind.Primitive,
    name: 'String',
    description: 'String of characters'
};
exports.UnitType = {
    kind: ErgoTypeKind.Primitive,
    name: 'Unit',
    description: 'Type with a single value ()'
};
// Special types
exports.BoxType = {
    kind: ErgoTypeKind.Box,
    name: 'Box',
    description: 'UTXO box containing value, tokens, and registers'
};
exports.SigmaPropType = {
    kind: ErgoTypeKind.SigmaProp,
    name: 'SigmaProp',
    description: 'Sigma proposition that can be proven via zero-knowledge proof'
};
exports.GroupElementType = {
    kind: ErgoTypeKind.GroupElement,
    name: 'GroupElement',
    description: 'Elliptic curve point'
};
exports.AvlTreeType = {
    kind: ErgoTypeKind.AvlTree,
    name: 'AvlTree',
    description: 'Authenticated dynamic dictionary digest'
};
exports.HeaderType = {
    kind: ErgoTypeKind.Header,
    name: 'Header',
    description: 'Block header data'
};
exports.ContextType = {
    kind: ErgoTypeKind.Context,
    name: 'Context',
    description: 'Execution context with blockchain and transaction data'
};
exports.UnknownType = {
    kind: ErgoTypeKind.Unknown,
    name: 'Unknown',
    description: 'Unknown type'
};
// Type constructors
function CollType(elementType) {
    return {
        kind: ErgoTypeKind.Collection,
        name: `Coll[${elementType.name}]`,
        description: `Collection of ${elementType.name}`,
        typeParams: [elementType]
    };
}
function OptionType(valueType) {
    return {
        kind: ErgoTypeKind.Option,
        name: `Option[${valueType.name}]`,
        description: `Optional ${valueType.name}`,
        typeParams: [valueType]
    };
}
function TupleType(types) {
    const typeNames = types.map(t => t.name).join(', ');
    return {
        kind: ErgoTypeKind.Tuple,
        name: `(${typeNames})`,
        description: `Tuple of ${typeNames}`,
        typeParams: types
    };
}
// Type registry
exports.ERGO_TYPES = new Map([
    ['Int', exports.IntType],
    ['Long', exports.LongType],
    ['BigInt', exports.BigIntType],
    ['UnsignedBigInt', exports.UnsignedBigIntType],
    ['Boolean', exports.BooleanType],
    ['Byte', exports.ByteType],
    ['Short', exports.ShortType],
    ['String', exports.StringType],
    ['Unit', exports.UnitType],
    ['Box', exports.BoxType],
    ['SigmaProp', exports.SigmaPropType],
    ['GroupElement', exports.GroupElementType],
    ['AvlTree', exports.AvlTreeType],
    ['Header', exports.HeaderType],
    ['Context', exports.ContextType]
]);
// Type operations
function getType(typeName) {
    return exports.ERGO_TYPES.get(typeName) || exports.UnknownType;
}
function isNumericType(type) {
    return ['Int', 'Long', 'BigInt', 'UnsignedBigInt', 'Byte', 'Short'].includes(type.name);
}
function isCompatible(type1, type2) {
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
function formatType(type) {
    return type.name;
}
//# sourceMappingURL=ergoTypes.js.map