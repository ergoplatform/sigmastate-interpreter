[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/interpreter/shared/src/main/scala/org/ergoplatform/ErgoBox.scala)

# ErgoBox Class

The `ErgoBox` class represents a box (also known as a coin or an unspent output) in the Ergo blockchain. A box is a state element locked by some proposition (ErgoTree). It is associated with some monetary value (arbitrary, but with predefined precision, so integer arithmetic is used to work with the value), and also a guarding script (aka proposition) to protect the box from unauthorized opening. 

In Ergo, a box is just a collection of registers, some with mandatory types and semantics, others could be used by applications in any way. The `ErgoBox` class adds additional fields in addition to amount and proposition (which are stored in the registers R0 and R1). Namely, register R2 contains additional tokens (a sequence of pairs (token identifier, value)). Register R3 contains height when block got included into the blockchain and also transaction identifier and box index in the transaction outputs. Registers R4-R9 are free for arbitrary usage.

A transaction is unsealing a box. As a box can not be open twice, any further valid transaction can not be linked to the same box.

The `ErgoBox` class has the following fields:
- `value`: amount of money associated with the box
- `ergoTree`: guarding script, which should be evaluated to true in order to open this box
- `additionalTokens`: secondary tokens the box contains
- `additionalRegisters`: additional registers the box can carry over
- `transactionId`: id of transaction which created the box
- `index`: index of the box (from 0 to total number of boxes the transaction with transactionId created - 1)
- `creationHeight`: height when a transaction containing the box was created.

The class has the following methods:
- `get(identifier: RegisterId)`: returns the value of the register with the given identifier, or None if the register is not found.
- `toCandidate`: converts this box to `ErgoBoxCandidate` by forgetting transaction reference data (transactionId, index).
- `equals(arg: Any)`: returns true if the given object is an `ErgoBox` with the same id as this box.
- `hashCode()`: returns the hash code of the box id.
- `toString()`: returns a string representation of the box.

The `ErgoBox` class also has several companion objects:
- `BoxId`: a type alias for `ADKey`, which is the type of the box id.
- `TokenId`: a type alias for `Digest32Coll`, which is the type of the token id.
- `Token`: a tuple of a token id and a long value.
- `MaxBoxSize`: the maximum size of a box.
- `STokenType`: the type of a token.
- `STokensRegType`: the type of the register containing additional tokens.
- `SReferenceRegType`: the type of the register containing reference to transaction and output id where the box was created.
- `Amount`: a type alias for `Long`, which is the type of the box value.
- `RegisterId`: a trait representing a register identifier.
- `MandatoryRegisterId`: a trait representing a mandatory register identifier.
- `NonMandatoryRegisterId`: a trait representing a non-mandatory register identifier.
- `R0`, `R1`, `R2`, `R3`, `R4`, `R5`, `R6`, `R7`, `R8`, `R9`: objects representing register identifiers.
- `ValueRegId`, `ScriptRegId`, `TokensRegId`, `ReferenceRegId`: objects representing mandatory register identifiers.
- `MaxTokens`: the maximum number of tokens that can be stored in a box.
- `maxRegisters`: the maximum number of registers that can be stored in a box.
- `mandatoryRegisters`: a sequence of mandatory register identifiers.
- `nonMandatoryRegisters`: a sequence of non-mandatory register identifiers.
- `startingNonMandatoryIndex`: the index of the first non-mandatory register.
- `allRegisters`: a sequence of all register identifiers.
- `registerByName`: a map from register name to register identifier.
- `registerByIndex(index: Int)`: returns the register identifier with the given index.
- `findRegisterByIndex(i: Int)`: returns the register identifier with the given index, or None if the index is out of range.
- `sigmaSerializer`: a serializer for `ErgoBox`.
## Questions: 
 1. What is the purpose of the `ErgoBox` class?
- The `ErgoBox` class represents a box (or coin) in a UTXO-based cryptocurrency, which contains a monetary value, a guarding script, additional tokens, and additional registers.

2. What are the mandatory and non-mandatory registers in an `ErgoBox`?
- The mandatory registers in an `ErgoBox` are `R0` (monetary value), `R1` (guarding script), `R2` (secondary tokens), and `R3` (reference to transaction and output id where the box was created). The non-mandatory registers are `R4` to `R9`.

3. What is the purpose of the `sigmaSerializer` object in the `ErgoBox` companion object?
- The `sigmaSerializer` object is used to serialize and deserialize `ErgoBox` objects to and from bytes, respectively.