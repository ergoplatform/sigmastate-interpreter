[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/.autodoc/docs/json/graph-ir/src/main/scala/special/sigma)

The `SigmaDslUnit.scala` file is part of the Sigma project, which is a smart contract language for the blockchain. It provides a set of traits that represent different types of data and operations that can be used in Sigma contracts. These traits serve as building blocks for creating complex smart contracts on the blockchain using the Sigma language.

For instance, the `BigInt` trait defines methods for performing arithmetic operations on big integers, such as addition, subtraction, multiplication, division, and modulo. Developers can use these methods to define custom data types and operations, and to create sophisticated contract logic that can be executed on the blockchain.

```scala
val a: BigInt = ...
val b: BigInt = ...
val sum: BigInt = a + b
val product: BigInt = a * b
```

The `Box` trait defines methods for working with transaction outputs, including retrieving the ID, value, and proposition bytes of a box, as well as its creation information and tokens. This can be useful for developers to create conditions that must be satisfied for a transaction to be valid.

```scala
val outputBox: Box = ...
val boxId: Array[Byte] = outputBox.id
val boxValue: Long = outputBox.value
val boxPropositionBytes: Array[Byte] = outputBox.propositionBytes
```

The `Context` trait defines methods for working with the current transaction context, including retrieving the inputs, outputs, and data inputs of the transaction, as well as the current block height and various other fields. This allows developers to create contracts that depend on the current state of the blockchain.

```scala
val ctx: Context = ...
val inputs: Coll[Box] = ctx.inputs
val outputs: Coll[Box] = ctx.outputs
val dataInputs: Coll[Box] = ctx.dataInputs
val currentHeight: Int = ctx.height
```

The `SigmaDslBuilder` trait defines methods for building Sigma propositions and performing various cryptographic operations, including hashing, signature verification, and point decoding. This enables developers to create secure and verifiable contracts.

```scala
val sigmaDslBuilder: SigmaDslBuilder = ...
val message: Array[Byte] = ...
val hash: Array[Byte] = sigmaDslBuilder.blake2b256(message)
val signature: Array[Byte] = ...
val publicKey: GroupElement = ...
val isValid: Boolean = sigmaDslBuilder.verifySignature(message, publicKey, signature)
```

In the `wrappers` subfolder, the `WrappersModule.scala` file provides a set of wrappers for various types and operations used throughout the project. These wrappers simplify the code and make it more readable and maintainable. By using these wrappers, developers can write more concise and maintainable code, and avoid common pitfalls and errors that may arise when working with low-level APIs.

Overall, the code in this folder and its subfolder provides essential building blocks for creating complex smart contracts on the blockchain using the Sigma language. Developers can use these traits and their methods to define custom data types and operations, and to create sophisticated contract logic that can be executed on the blockchain.
