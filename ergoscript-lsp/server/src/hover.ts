/**
 * Hover provider for ErgoScript keywords
 * Provides documentation when hovering over known keywords
 */

import { Hover, MarkupKind } from 'vscode-languageserver/node';
import { getWordAtPosition } from './parser';

/**
 * Keyword documentation map
 */
const KEYWORD_DOCS: Record<string, string> = {
    'SigmaProp': `**SigmaProp** - Sigma Proposition

The fundamental return type for ErgoScript contracts. Represents a cryptographic condition that must be satisfied to spend a box.

SigmaProp encapsulates zero-knowledge proofs and evaluates to true/false during verification.

Example: \`sigmaProp(HEIGHT > 100000)\``,

    'HEIGHT': `**HEIGHT** - Blockchain Height

A context variable representing the current block height of the Ergo blockchain.

Used for time-locked contracts and temporal conditions.

Example: \`HEIGHT > 500000\` (spendable after block 500000)`,

    'OUTPUTS': `**OUTPUTS** - Transaction Outputs

Collection of new boxes (UTXOs) created by the current transaction in Ergo's eUTXO model.

Access with round parentheses: \`OUTPUTS(0)\` for first output.

Example: \`OUTPUTS(0).value >= 1000000\` (first output has at least 1 ERG)`,

    'INPUTS': `**INPUTS** - Transaction Inputs

Collection of boxes being consumed/spent in the current transaction.

Access with round parentheses: \`INPUTS(0)\` for first input.

Example: \`INPUTS.size > 1\` (transaction has multiple inputs)`,

    'SELF': `**SELF** - Current Box

Reference to the box currently being spent. Provides access to the box's properties.

Example: \`SELF.value\` (value of current box in nanoERG)`,

    'tokens': `**tokens** - Token Registry

Property of a box containing the tokens held by that box.

Returns a collection of (tokenId, amount) pairs.

Example: \`SELF.tokens(0)._1 == myTokenId\` (check first token ID)`,

    'value': `**value** - Box Value

The ERG amount stored in a box, denominated in nanoERG (1 ERG = 1,000,000,000 nanoERG).

Example: \`OUTPUTS(0).value >= SELF.value\` (output preserves input value)`,

    'sigmaProp': `**sigmaProp** - Convert to SigmaProp

Function that converts a boolean condition into a SigmaProp.

Every ErgoScript contract must return a SigmaProp.

Example: \`sigmaProp(condition)\``,

    'proveDlog': `**proveDlog** - Discrete Logarithm Proof

Creates a SigmaProp requiring knowledge of the discrete logarithm (private key) for a given public key.

Used for standard public key authentication.

Example: \`proveDlog(ownerPubKey)\``,

    'anyOf': `**anyOf** - Logical OR

Requires that at least one of the provided conditions is satisfied.

Example: \`anyOf(Coll(condition1, condition2))\``,

    'allOf': `**allOf** - Logical AND

Requires that all provided conditions are satisfied.

Example: \`allOf(Coll(condition1, condition2))\``,

    'atLeast': `**atLeast** - Threshold Signature

Requires at least N out of M conditions to be satisfied (threshold signature).

Example: \`atLeast(2, Coll(sig1, sig2, sig3))\` (2-of-3 multisig)`,

    'CONTEXT': `**CONTEXT** - Transaction Context

Provides access to the full transaction context including headers, data inputs, and other metadata.

Example: \`CONTEXT.headers\` (blockchain headers)`,

    'Box': `**Box** - UTXO Box Type

Represents an unspent transaction output in Ergo's eUTXO model.

Each box contains: value (ERG), tokens, registers, and a guarding proposition.`,

    'Coll': `**Coll** - Collection Type

Represents an immutable collection of elements.

Supports operations like: size, map, filter, fold, exists, forall.

Example: \`Coll(1, 2, 3).size == 3\``,

    'size': `**size** - Collection Size

Returns the number of elements in a collection.

Example: \`INPUTS.size\` (number of input boxes)`,

    'exists': `**exists** - Collection Exists

Returns true if at least one element in the collection satisfies the predicate.

Example: \`INPUTS.exists({ (box: Box) => box.value > 1000000 })\``,

    'forall': `**forall** - Collection For All

Returns true if all elements in the collection satisfy the predicate.

Example: \`OUTPUTS.forall({ (box: Box) => box.value > 0 })\``,

    'fold': `**fold** - Collection Fold/Reduce

Reduces a collection to a single value by applying a binary operation.

Example: \`INPUTS.fold(0L, { (acc: Long, box: Box) => acc + box.value })\``,

    'map': `**map** - Collection Map

Transforms each element of a collection using a function.

Example: \`INPUTS.map({ (box: Box) => box.value })\``,

    'filter': `**filter** - Collection Filter

Returns a new collection containing only elements that satisfy the predicate.

Example: \`INPUTS.filter({ (box: Box) => box.value > 1000000 })\``
};

/**
 * Provide hover information for a position in the document
 */
export function provideHover(source: string, line: number, character: number): Hover | null {
    const word = getWordAtPosition(source, line, character);

    if (!word) {
        return null;
    }

    const documentation = KEYWORD_DOCS[word];

    if (!documentation) {
        return null;
    }

    return {
        contents: {
            kind: MarkupKind.Markdown,
            value: documentation
        }
    };
}
