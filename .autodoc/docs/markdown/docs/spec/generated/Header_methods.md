[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/docs/spec/generated/Header_methods.tex)

This file contains a set of methods that are used to retrieve various properties of a blockchain header. A header is a data structure that contains metadata about a block in a blockchain. It includes information such as the block's timestamp, the hash of the previous block, and the root hash of the Merkle tree of transactions in the block.

The methods in this file are used to retrieve specific properties of a header. For example, the `Header.version` method returns the version number of the protocol used to create the block, while the `Header.timestamp` method returns the timestamp of the block.

Each method takes no parameters and returns a specific type of data. For example, the `Header.stateRoot` method returns an `AvlTree` object, which is a data structure used to represent a Merkle tree. The `Header.powDistance` method returns a `BigInt` object, which is a large integer used to represent the proof-of-work difficulty of the block.

These methods are used throughout the larger project to retrieve information about blocks in the blockchain. For example, they may be used by other modules to verify the validity of a block or to calculate the total difficulty of the blockchain. 

Example usage:

```
val header: Header = // get header object from somewhere
val version: Byte = header.version
val timestamp: Long = header.timestamp
val stateRoot: AvlTree = header.stateRoot
```
## Questions: 
 1. What is the purpose of the Header class?
- The Header class contains methods that return various properties of a block header, such as the version, timestamp, and proof-of-work information.

2. What type of data does the Header.stateRoot method return?
- The Header.stateRoot method returns an AvlTree object.

3. What is the purpose of the Header.votes method?
- The Header.votes method returns the votes that were cast for this block by validators in the network.