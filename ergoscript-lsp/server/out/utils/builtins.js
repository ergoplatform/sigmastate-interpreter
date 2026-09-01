"use strict";
/**
 * ErgoScript Built-in Definitions
 *
 * Defines all built-in keywords, functions, types, and context variables
 * for autocomplete and hover information.
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.NUMERIC_METHODS = exports.COLLECTION_METHODS = exports.BOX_PROPERTIES = exports.ERGOSCRIPT_TYPES = exports.CONTEXT_VARIABLES = exports.ERGOSCRIPT_BUILTINS = exports.ERGOSCRIPT_KEYWORDS = void 0;
exports.ERGOSCRIPT_KEYWORDS = [
    'val', 'def', 'if', 'else', 'true', 'false', 'return'
];
exports.ERGOSCRIPT_BUILTINS = [
    {
        name: 'blake2b256',
        signature: '(data: Coll[Byte]) => Coll[Byte]',
        documentation: 'Computes Blake2b256 hash of the input bytes. Returns 32-byte hash.'
    },
    {
        name: 'sha256',
        signature: '(data: Coll[Byte]) => Coll[Byte]',
        documentation: 'Computes SHA-256 hash of the input bytes. Returns 32-byte hash.'
    },
    {
        name: 'proveDlog',
        signature: '(value: GroupElement) => SigmaProp',
        documentation: 'Creates a sigma proposition for discrete logarithm proof. Used for signature verification.'
    },
    {
        name: 'proveDHTuple',
        signature: '(g: GroupElement, h: GroupElement, u: GroupElement, v: GroupElement) => SigmaProp',
        documentation: 'Creates a sigma proposition for Diffie-Hellman tuple proof.'
    },
    {
        name: 'sigmaProp',
        signature: '(condition: Boolean) => SigmaProp',
        documentation: 'Converts a boolean condition to a SigmaProp. Used to combine boolean and cryptographic conditions.'
    },
    {
        name: 'atLeast',
        signature: '(k: Int, props: Coll[SigmaProp]) => SigmaProp',
        documentation: 'Creates a threshold signature requiring at least k of the provided sigma propositions to be proven.'
    },
    {
        name: 'allOf',
        signature: '(props: Coll[Boolean]) => Boolean',
        documentation: 'Returns true if all conditions in the collection are true.'
    },
    {
        name: 'anyOf',
        signature: '(props: Coll[Boolean]) => Boolean',
        documentation: 'Returns true if any condition in the collection is true.'
    },
    {
        name: 'serialize',
        signature: '[T](value: T) => Coll[Byte]',
        documentation: 'Serializes any value to bytes using ErgoTree serialization.'
    },
    {
        name: 'deserializeTo',
        signature: '[T](bytes: Coll[Byte]) => T',
        documentation: 'Deserializes bytes to a value of the specified type.'
    },
    {
        name: 'byteArrayToBigInt',
        signature: '(bytes: Coll[Byte]) => BigInt',
        documentation: 'Converts a byte array to a BigInt value (big-endian).'
    },
    {
        name: 'byteArrayToLong',
        signature: '(bytes: Coll[Byte]) => Long',
        documentation: 'Converts a byte array to a Long value (big-endian).'
    },
    {
        name: 'longToByteArray',
        signature: '(value: Long) => Coll[Byte]',
        documentation: 'Converts a Long value to a byte array (big-endian).'
    },
    {
        name: 'getVar',
        signature: '[T](id: Byte) => Option[T]',
        documentation: 'Extracts a context variable by id and type. Returns None if variable does not exist.'
    },
    {
        name: 'unsignedBigInt',
        signature: '(value: String) => UnsignedBigInt',
        documentation: 'Creates an unsigned 256-bit integer from a string representation.'
    }
];
exports.CONTEXT_VARIABLES = [
    {
        name: 'HEIGHT',
        type: 'Int',
        documentation: 'Current blockchain height (block number). Shortcut for CONTEXT.HEIGHT.'
    },
    {
        name: 'SELF',
        type: 'Box',
        documentation: 'The box whose proposition is currently being evaluated. Shortcut for CONTEXT.SELF.'
    },
    {
        name: 'INPUTS',
        type: 'Coll[Box]',
        documentation: 'Collection of all input boxes in the current transaction. Shortcut for CONTEXT.INPUTS.'
    },
    {
        name: 'OUTPUTS',
        type: 'Coll[Box]',
        documentation: 'Collection of all output boxes in the current transaction. Shortcut for CONTEXT.OUTPUTS.'
    },
    {
        name: 'CONTEXT',
        type: 'Context',
        documentation: 'The full context object containing all blockchain and transaction data.'
    }
];
exports.ERGOSCRIPT_TYPES = [
    {
        name: 'Int',
        description: '32-bit signed integer'
    },
    {
        name: 'Long',
        description: '64-bit signed integer'
    },
    {
        name: 'BigInt',
        description: '256-bit signed integer'
    },
    {
        name: 'UnsignedBigInt',
        description: '256-bit unsigned integer with modular arithmetic support'
    },
    {
        name: 'Boolean',
        description: 'Logical type with values true and false'
    },
    {
        name: 'Byte',
        description: '8-bit signed integer'
    },
    {
        name: 'Short',
        description: '16-bit signed integer'
    },
    {
        name: 'String',
        description: 'String of characters'
    },
    {
        name: 'Unit',
        description: 'Type with a single value ()'
    },
    {
        name: 'Box',
        description: 'UTXO box containing value, tokens, and registers',
        properties: ['value', 'propositionBytes', 'bytes', 'id', 'tokens', 'creationInfo', 'R4', 'R5', 'R6', 'R7', 'R8', 'R9']
    },
    {
        name: 'SigmaProp',
        description: 'Sigma proposition that can be proven via zero-knowledge proof',
        properties: ['propBytes', 'isProven']
    },
    {
        name: 'GroupElement',
        description: 'Elliptic curve point',
        properties: ['getEncoded', 'exp', 'multiply', 'negate']
    },
    {
        name: 'AvlTree',
        description: 'Authenticated dynamic dictionary digest',
        properties: ['digest', 'enabledOperations', 'keyLength', 'valueLengthOpt']
    },
    {
        name: 'Header',
        description: 'Block header data',
        properties: ['id', 'version', 'parentId', 'timestamp', 'height', 'minerPk']
    },
    {
        name: 'PreHeader',
        description: 'Pre-header data (fields known before mining)',
        properties: ['version', 'parentId', 'timestamp', 'height', 'minerPk']
    },
    {
        name: 'Context',
        description: 'Execution context with blockchain and transaction data',
        properties: ['HEIGHT', 'SELF', 'INPUTS', 'OUTPUTS', 'dataInputs', 'headers', 'preHeader', 'minerPubKey']
    },
    {
        name: 'Coll',
        description: 'Generic collection type. Example: Coll[Int], Coll[Byte]'
    },
    {
        name: 'Option',
        description: 'Optional value type. Example: Option[Int]',
        properties: ['isDefined', 'isEmpty', 'get', 'getOrElse']
    }
];
// Box property completions
exports.BOX_PROPERTIES = [
    { name: 'value', type: 'Long', doc: 'Monetary value in NanoErg' },
    { name: 'propositionBytes', type: 'Coll[Byte]', doc: 'Serialized guarding script' },
    { name: 'bytes', type: 'Coll[Byte]', doc: 'Serialized box content' },
    { name: 'id', type: 'Coll[Byte]', doc: 'Blake2b256 hash of box content' },
    { name: 'tokens', type: 'Coll[(Coll[Byte], Long)]', doc: 'Collection of tokens (tokenId, amount)' },
    { name: 'creationInfo', type: '(Int, Coll[Byte])', doc: 'Creation height and transaction info' },
    { name: 'R4', type: 'Option[T]', doc: 'Register R4 (optional)' },
    { name: 'R5', type: 'Option[T]', doc: 'Register R5 (optional)' },
    { name: 'R6', type: 'Option[T]', doc: 'Register R6 (optional)' },
    { name: 'R7', type: 'Option[T]', doc: 'Register R7 (optional)' },
    { name: 'R8', type: 'Option[T]', doc: 'Register R8 (optional)' },
    { name: 'R9', type: 'Option[T]', doc: 'Register R9 (optional)' }
];
// Collection methods
exports.COLLECTION_METHODS = [
    { name: 'map', signature: '[A, B](f: A => B) => Coll[B]', doc: 'Apply function to each element' },
    { name: 'filter', signature: '[A](f: A => Boolean) => Coll[A]', doc: 'Keep elements matching predicate' },
    { name: 'fold', signature: '[A, B](zero: B, f: (B, A) => B) => B', doc: 'Fold left with accumulator' },
    { name: 'exists', signature: '[A](f: A => Boolean) => Boolean', doc: 'Check if any element matches' },
    { name: 'forall', signature: '[A](f: A => Boolean) => Boolean', doc: 'Check if all elements match' },
    { name: 'size', signature: '=> Int', doc: 'Number of elements in collection' },
    { name: 'slice', signature: '(from: Int, until: Int) => Coll[T]', doc: 'Extract sub-collection' },
    { name: 'indexOf', signature: '[A](elem: A, from: Int) => Int', doc: 'Find index of element' },
    { name: 'zip', signature: '[A, B](that: Coll[B]) => Coll[(A, B)]', doc: 'Combine two collections' }
];
// Numeric methods
exports.NUMERIC_METHODS = [
    { name: 'toByte', signature: '=> Byte', doc: 'Convert to Byte' },
    { name: 'toShort', signature: '=> Short', doc: 'Convert to Short' },
    { name: 'toInt', signature: '=> Int', doc: 'Convert to Int' },
    { name: 'toLong', signature: '=> Long', doc: 'Convert to Long' },
    { name: 'toBigInt', signature: '=> BigInt', doc: 'Convert to BigInt' },
    { name: 'toBytes', signature: '=> Coll[Byte]', doc: 'Convert to byte array (big-endian)' },
    { name: 'toBits', signature: '=> Coll[Boolean]', doc: 'Convert to bit array' }
];
//# sourceMappingURL=builtins.js.map