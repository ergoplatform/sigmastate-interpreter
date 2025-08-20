[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/interpreter/shared/src/main/scala/org/ergoplatform/ErgoBoxCandidate.scala)

# ErgoBoxCandidate

The `ErgoBoxCandidate` class is a representation of an unspent transaction output (UTXO) in the Ergo blockchain. It is similar to the `ErgoBox` class, but it does not contain the transaction ID and index, which are calculated after the transaction is formed. 

The class has the following fields:
- `value`: the amount of money associated with the box.
- `ergoTree`: the guarding script, which should be evaluated to true in order to open this box.
- `creationHeight`: the height when a transaction containing the box was created. This height is declared by the user and should not exceed the height of the block containing the transaction with this box.
- `additionalTokens`: secondary tokens the box contains.
- `additionalRegisters`: additional registers the box can carry over.

The `ErgoBoxCandidate` class has several methods:
- `proposition`: transforms the `ergoTree` to a proposition, substituting the constants if the constant segregation flag is set.
- `propositionBytes`: returns the serialized bytes of the guarding `ErgoTree`.
- `bytesWithNoRef`: returns the serialized bytes of this box without transaction reference data (transaction ID and index).
- `toBox(txId: ModifierId, boxIndex: Short)`: creates a new `ErgoBox` based on this candidate using the given transaction reference data.
- `get(identifier: RegisterId)`: extracts register by ID.

The `ErgoBoxCandidate` class also has a `tokens` field, which is a map of additional tokens stored in the box, merged into a map. This method is not used in `ErgoTree` and serialization and is not part of consensus.

The `ErgoBoxCandidate` class is serialized using the `ErgoBoxCandidate.serializer` object, which is a `SigmaSerializer` that serializes the `ErgoBoxCandidate` object to bytes. The `serializer` object has two methods: `serialize` and `parse`. The `serialize` method serializes the `ErgoBoxCandidate` object to bytes, while the `parse` method deserializes the bytes to an `ErgoBoxCandidate` object.

The `ErgoBoxCandidate` class is an important part of the Ergo blockchain, as it represents the UTXOs that can be spent in transactions. It is used in the larger project to manage the state of the blockchain and to ensure that transactions are valid.
## Questions: 
 1. What is the purpose of the `ErgoBoxCandidate` class?
- The `ErgoBoxCandidate` class contains the same fields as `org.ergoplatform.ErgoBox`, except for the transaction id and index, which will be calculated after full transaction formation.

2. What is the `proposition` method used for?
- The `proposition` method returns the guarding script of the `ErgoBoxCandidate` as a `SigmaPropValue`, which should be evaluated to true in order to open this box.

3. What is the purpose of the `tokens` method?
- The `tokens` method returns a map of additional tokens stored in the box, merged into a Map. This method is not used in ErgoTree and serialization, and is not part of consensus.