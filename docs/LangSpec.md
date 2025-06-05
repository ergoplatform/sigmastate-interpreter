# ErgoScript Language Description

### Introduction

ErgoScript is a language to write contracts for [Ergo
blockchain](https://ergoplatform.org). ErgoScript contracts can be compiled to
[ErgoTrees](https://ergoplatform.org/docs/ErgoTree.pdf), typed abstract syntax treeы which are serialized and stored
in UTXOs.

A good starting point to writing contracts is to use [ErgoScript by
Example](https://github.com/ergoplatform/ergoscript-by-example), with [Ergo
Playgrounds](https://github.com/ergoplatform/ergo-playgrounds) or
[Appkit](https://github.com/ergoplatform/ergo-appkit) or [Fleet](https://github.com/fleet-sdk) .

ErgoScript compiler is
[published](https://mvnrepository.com/artifact/org.scorexfoundation/sigma-state)
as a library which is cross compiled for Java 7 and Java 8+ and thus can be used
from any JVM language and also on Android and JavaFX platforms. It is also cross-compiled to JavaScript using Scala.js.

The following example shows how source code of ErgoScript contract can be used
to create new transaction using
[Appkit](https://github.com/ergoplatform/ergo-appkit), see [full
example](https://github.com/aslesarenko/ergo-appkit-examples/blob/master/java-examples/src/main/java/org/ergoplatform/appkit/examples/FreezeCoin.java)
for details.

```java
// To create transaction we use a builder obtained from the context
// the builder keeps relationship with the context to access necessary blockchain data.
UnsignedTransactionBuilder txB = ctx.newTxBuilder();

// create new box using new builder obtained from the transaction builder
// in this case we compile new ErgoContract from source ErgoScript code
OutBox newBox = txB.outBoxBuilder()
        .value(amountToPay)
        .contract(ctx.compileContract(
                ConstantsBuilder.create()
                        .item("freezeDeadline", ctx.getHeight() + newBoxDelay)
                        .item("pkOwner", prover.getP2PKAddress().pubkey())
                        .build(),
                "{ " +
                "  val deadlinePassed = sigmaProp(HEIGHT > freezeDeadline)" +
                "  deadlinePassed && pkOwner " +
                "}"))
        .build();
```

The contract is given as the string literal which contains the block of `val`
declarations followed by the logical expression. The expression defines the all
possible conditions to spend the box. The contract can also contain _named
constants_ (which cannot be represented as literals in the source code). 
In the example `freezeDeadline` and `pkOwner` are named constants. The concrete
values of named constants should be given to the compiler (see `compileContract`
method)

The following sections describe ErgoScript and its operations. 

#### ErgoScript language features overview

- syntax borrowed from Scala but simplified
- standard syntax and semantics for well known constructs (operations, code blocks, if branches etc.)
- high-order language with first-class lambdas which are used in collection operations
- call-by-value (eager evaluation)
- statically typed with local type inference
- blocks are expressions 
- semicolon inference in blocks
- type constructors: Pair, Coll, Option

#### Operations and constructs overview

- Binary operations: `>, <, >=, <=, +, -, &&, ||, ==, !=, |, &, *, /, %, ^, ++`
- predefined primitives: `deserializeTo`, `serialize`, `blake2b256`, `byteArrayToBigInt`, `proveDlog` etc. 
- val declarations: `val h = blake2b256(pubkey)`
- if-then-else clause: `if (x > 0) 1 else 0`
- collection literals: `Coll(1, 2, 3, 4)`
- generic high-order collection operations: `map`, `filter`, `fold`, `exists`, `forall`, etc.
- accessing fields of any predefined types: `box.value`
- method invocation for predefined types: `coll.map({ x => x + 1 })`
- function invocations (predefined and user defined): `proveDlog(pubkey)` 
- user defined function declarations: `def isProven(pk: GroupElement) = proveDlog(pk).isProven`
- lambdas and high-order methods: `OUTPUTS.exists { (out: Box) => out.value >= minToRaise }`

#### Data types 

In ErgoScript, everything is an object in the sense that we can call member functions and properties on any variable.
Some of the types can have a special internal representation - for example, numbers and booleans can be
represented as primitive values at runtime - but to the user they look like ordinary classes.
NOTE: in ErgoScript we use *type*, *class* and *trait* as synonyms, we prefer *type* when talking about primitive values and
*trait* or *class* when talking about methods.

Type Name        |   Description
-----------------|------------------------------
`Any`            | a supertype of any other type (not used directly in ErgoScript)
`Unit`           | a type with a single value `()`
`Boolean`        | a type with two logical values `true` and `false`
`Byte`           | 8 bit signed integer
`Short`          | 16 bit signed integer
`Int`            | 32 bit signed integer
`Long`           | 64 bit signed integer
`BigInt`         | 256 bit signed integer
`UnsignedBigInt` | 256 bit unsigned integer
`SigmaProp`      | a type representing a _sigma proposition_ which can be verified by executing a Sigma protocol with zero-knowledge proof of knowledge. Every contract should return a value of this type.
`AvlTree`        | represents a digest of authenticated dynamic dictionary and can be used to verify proofs of operations performed on the dictionary
`GroupElement`   | elliptic curve points
`Box`            | a box containing a monetary value (in NanoErgs), tokens and registers along with a guarding proposition
`Option[T]`      | a container which either have some value of type `T` or none.
`Coll[T]`        | a collection of arbitrary length with all values of type `T` 
`(T1,T2)`        | a pair of values where T1, T2 can be different types

The type constructors `Coll`, `(_,_)` can be used to construct complex
types as in the following example.
```scala
{
  val keyValues = OUTPUTS(0).R4[Coll[(Int, (Byte, Long))]].get
  ...
}
```

#### Literal syntax and Constants

Literals are used to introduce values of some types directly in program text
like in the following example:
```
 val unit: Unit = ()                           // unit constant
 val long: Int = 10                            // interger value literal
 val bool: Boolean = true                      // logical literal
 val arr = Coll(1, 2, 3)                       // constructs a collection with given items
 val str = "abc"                               // string of characters 
 val ubi = unsignedBigInt("508473958676860")   // unsigned big integer
```
Note that many types don't have literal syntax and their values are introduced 
by applying operations, for example `deserialize` function can be used to introduce
a constant of any type by using Base64 encoded string (See [predefined function](#PredefinedFunctions)).

### Data Types
<a name="DataTypes"></a>

#### Primitive Types

Below we specify methods of pre-defined types using Scala-like declaration of
classes. Note, the `Boolean` type doesn't have pre-defined methods in addition
to the standard operations.

Note, ErgoScript doesn't allow to define new `class` types, however it has many
pre-defined classes with methods defined below.

Every numeric type has the following methods.
```scala
/** Base supertype for all numeric types. */
class Numeric {
  /** Convert this Numeric value to Byte. 
   * @throws ArithmeticException if overflow happens. 
   */
  def toByte: Byte
  
  /** Convert this Numeric value to Short. 
   * @throws ArithmeticException if overflow happens. 
   */
  def toShort: Short
  
  /** Convert this Numeric value to Int. 
   * @throws ArithmeticException if overflow happens. 
   */
  def toInt: Int
  
  /** Convert this Numeric value to Long. 
   * @throws ArithmeticException if overflow happens. 
   */
  def toLong: Long
  
  /** Convert this Numeric value to BigInt. */
  def toBigInt: BigInt

  /** Convert this Numeric value to UnsignedBigInt. */
  def toUnsignedBigInt: UnsignedBigInt

  /** Returns a big-endian representation of this value in a collection of bytes.
    * For example, the `Int` value `0x12131415` would yield the
    * collection of bytes [0x12, 0x13, 0x14, 0x15]
    */
  def toBytes(x: T): Coll[Byte]

  /**
   * Returns a big-endian binary representation of this value as boolean array.
   */
  def toBits(x: T): Coll[Boolean]

  /**
    * @return a numeric value which is inverse of `x` (every bit, including sign, is flipped)
    */
  def bitwiseInverse(x: T): T

  /**
    * @return a numeric value which is `this | that`
    */
  def bitwiseOr(x: T, y: T): T

  /**
    * @return a numeric value which is `this && that`
    */
  def bitwiseAnd(x: T, y: T): T

  /**
    * @return a numeric value which is `this xor that`
    */
  def bitwiseXor(x: T, y: T): T

  /**
    * @return a value which is (this << n). The shift distance, n, may be negative,
    *         in which case this method performs a right shift. (Computes floor(this * 2n).)
    */
  def shiftLeft(x: T, bits: Int): T

  /**
    * @return a value which is (this >> n). Sign extension is performed. The shift distance, n,
    *         may be negative, in which case this method performs a left shift. (Computes floor(this / 2n).)
    */
  def shiftRight(x: T, bits: Int): T
 
}
```

The only exception for conversions is about BigInt to and from UnsignedBigInt. To convert from signed big int to unsigned, use
`.toUnsigned` method to convert signed big integer to unsigned, or `.toUnsignedMod(m)` to convert modulo `m` (and modulo
 operation is cryptographic, i.e. always returns positive number modulo `m`). To convert from unsigned big int to signed,
 use `.toSigned`.

All the predefined numeric types inherit Numeric class and its methods.
They can be thought as being pre-defined like the following.

```scala 
class Byte extends Numeric
class Short extends Numeric
class Int extends Numeric
class Long extends Numeric
class BigInt extends Numeric
class UnsignedBigInt extends Numeric
```

examples: 

```
val b = (126 + 1).toByte  // max byte value
b.bitwiseInverse == (-128).toByte // min byte value
```

```
val x = 4.toByte
val y = 2
x.shiftLeft(y) == 16.toByte
```

```
val l = getVar[Long](1).get
val ba = l.toBytes
Global.fromBigEndianBytes[Long](ba) == l
```

In addition to all the methods from above, UnsignedBigInt type has support for modular arithmetics, namely, `modInverse`,
`plusMod`, `subtractMod`, `multiplyMod`, `mod`. Examples:

```
val bi1 = unsignedBigInt("2")
val bi2 = unsignedBigInt("4")
val m = unsignedBigInt("575879797")
bi1.subtractMod(bi2, m) > 0
```

```
val bi = unsignedBigInt("248486720836984554860790790898080606")
val m = unsignedBigInt("575879797")
bi.mod(m) < bi
```

```
val bi = unsignedBigInt("248486720836984554860790790898080606")
val m = unsignedBigInt("575879797")
bi.modInverse(m) > 0 // returns multiplicative inverse of bi mod m
```


#### Context Data 

Every script is executed in a context, which is a collection of data available
for operations in the script. The context data is available using the `CONTEXT`
variable which is of pre-defined class `Context` which is shown below. 

There are also shortcut variables which are available in every script to
simplify access to the most commonly used context data.

Variable          |  Type               | Shortcut for ...
------------------|---------------------|----------------------
`HEIGHT`          | `Int`               | `CONTEXT.HEIGHT`
`SELF`            | `Box`               | `CONTEXT.SELF` 
`INPUTS`          | `Coll[Box]`         | `CONTEXT.INPUTS`  
`OUTPUTS`         | `Coll[Box]`         | `CONTEXT.OUTPUTS` 

The following listing shows the methods of pre-defined `Context`, `Header`,
`PreHeader` types.

```scala
/** Represents data available in ErgoScript using `CONTEXT` global variable */
class Context {
  /** Height (block number) of the block which is currently being validated. */
  def HEIGHT: Int
  
  /** Box whose proposition is being currently executing */
  def SELF: Box

  /** Zero based index in `inputs` of `selfBox` */
  def selfBoxIndex: Int

  /** A collection of inputs of the current transaction, the transaction where
    * selfBox is one of the inputs. 
    */
  def INPUTS: Coll[Box]
  
  /** A collection of data inputs of the current transaction. Data inputs are
    * not going to be spent and thus don't participate in transaction validation
    * as `INPUTS`, but data boxes are available in guarding propositions of
    * `INPUTS` and thus can be used in spending logic.
    */
  def dataInputs: Coll[Box]
  
  /** A collection of outputs of the current transaction. */
  def OUTPUTS: Coll[Box]
  
  /** Authenticated dynamic dictionary digest representing Utxo state before
    * current state. 
    */
  def LastBlockUtxoRootHash: AvlTree
  
  /** A fixed number of last block headers in descending order (first header is
    * the newest one) 
    */
  def headers: Coll[Header]

  /** Header fields that are known before the block is mined. */
  def preHeader: PreHeader

  /** Bytes of encoded miner's public key. 
    * Same as `preHeader.minerPk.getEncoded`
    */
  def minerPubKey: Coll[Byte]

  /** Extracts Context variable by id and type.
    * ErgoScript is typed, so accessing a the variables is an operation which involves
    * some expected type given in brackets. Thus `getVar[Int](id)` expression should
    * evaluate to a valid value of the `Option[Int]` type.
    */
  def getVar[T](id: Byte): Option[T]

  /**
    * A variant of `getVar` to extract a context variable by id and type from any input
    */
  def getVarFromInput[T](inputIndex: Short, id: Byte): Option[T]

  /** Returns new $coll with elements in reversed order.
    *
    *  @return A new $coll with all elements of this $coll in reversed order.
    */
  def reverse: Coll[T]
 
}

/** Represents data of the block headers available in scripts. */
class Header {  
  
  /** Bytes representation of ModifierId of this Header */
  def id: Coll[Byte]

  /** Block version, to be increased on every soft and hardfork. */
  def version: Byte
  
  /** Id of parent block (as bytes) */
  def parentId: Coll[Byte] // 
  
  /** Hash of ADProofs for transactions in a block */
  def ADProofsRoot: Coll[Byte] // Digest32. Can we build AvlTree out of it? 

  /** AvlTree) of a state after block application */
  def stateRoot: Coll[Byte]  // ADDigest  //33 bytes! extra byte with tree height here!

  /** Root hash (for a Merkle tree) of transactions in a block. */
  def transactionsRoot: Coll[Byte]  // Digest32

  /** Block timestamp (in milliseconds since beginning of Unix Epoch) */
  def timestamp: Long

  /** Current difficulty in a compressed view.
    * NOTE: actually it is unsigned Int*/
  def nBits: Long

  /** Block height */
  def height: Int

  /** Root hash of extension section (Digest32) */
  def extensionRoot: Coll[Byte]

  /** Miner public key. Should be used to collect block rewards.
    * Part of Autolykos solution (pk). 
    */
  def minerPk: GroupElement
  
  /** Nonce value found by the miner. Part of Autolykos solution (n). */
  def powNonce: Coll[Byte]
  
  /** Miner votes for changing system parameters. */
  def votes: Coll[Byte]
  
  /** Validate header's proof-of-work */
  def checkPow: Boolean

}

/** Only header fields that can be predicted by a miner. */
class PreHeader { 
  /** Block version, to be increased on every soft and hardfork. */
  def version: Byte

  /** Id of parent block */
  def parentId: Coll[Byte] // ModifierId

  /** Block timestamp (in milliseconds since beginning of Unix Epoch) */
  def timestamp: Long

  /** Current difficulty in a compressed view.
    * NOTE: actually it is 32-bit unsigned Int */
  def nBits: Long

  /** Block height */
  def height: Int

  /** Miner's public key. Should be used to collect block rewards. */
  def minerPk: GroupElement

  /** Miner votes for changing system parameters. */
  def votes: Coll[Byte]
}

```

#### Box type

Box represents a unit of storage in Ergo blockchain. It contains 10 registers
(indexed 0-9). First 4 are mandatory and the others are optional.

```scala
/** Representation of Ergo boxes used during execution of ErgoTree operations. */
class Box {
  /** Box monetary value in NanoErg */
  def value: Long 
  
  /** Blake2b256 hash of this box's content, basically equals to
    * `blake2b256(bytes)` 
    */
  def id: Coll[Byte] 

  /** Serialized bytes of guarding script, which should be evaluated to true in
    * order to open this box. 
    */
  def propositionBytes: Coll[Byte] 
  
  /** Serialized bytes of this box's content, including proposition bytes. */
  def bytes: Coll[Byte] 
  
  /** Serialized bytes of this box's content, excluding transactionId and index
    * of output. 
    */
  def bytesWithoutRef: Coll[Byte]
    
  /** If `tx` is a transaction which generated this box, then `creationInfo._1`
    * is a height of the tx's block. The `creationInfo._2` is a serialized
    * transaction identifier followed by box index in the transaction outputs.
    */
  def creationInfo: (Int, Coll[Byte]) 
  
  /** Synonym of R2 obligatory register */
  def tokens: Coll[(Coll[Byte], Long)] 
  
  /** Extracts register by id and type.
    * ErgoScript is typed, so accessing a register is an operation which involves some
    * expected type given in brackets. Thus `SELF.R4[Int]` expression should evaluate to a
    * valid value of the `Option[Int]` type.
    *
    * For example `val x = SELF.R4[Int]` expects the
    * register, if it is present, to have type `Int`. At runtime the corresponding type
    * descriptor is passed as `implicit t: RType[T]` parameter of `getReg` method and
    * checked against the actual value of the register.
    *
    * There are three cases:
    * 1) If the register doesn't exist.
    *   Then `val x = SELF.R4[Int]` succeeds and returns the None value, which conforms to
    *   any value of type `Option[T]` for any T. (In the example above T is equal to
    *   `Int`). Calling `x.get` fails when x is equal to None, but `x.isDefined`
    *   succeeds and returns `false`.
    * 2) If the register contains a value `v` of type `Int`.
    *   Then `val x = SELF.R4[Int]` succeeds and returns `Some(v)`, which is a valid value
    *   of type `Option[Int]`. In this case, calling `x.get` succeeds and returns the
    *   value `v` of type `Int`. Calling `x.isDefined` returns `true`.
    * 3) If the register contains a value `v` of type T other then `Int`.
    *   Then `val x = SELF.R4[Int]` fails, because there is no way to return a valid value
    *   of type `Option[Int]`. The value of register is present, so returning it as None
    *   would break the typed semantics of registers collection.
    *
    * In some use cases one register may have values of different types. To access such
    * register an additional register can be used as a tag.
    *
    * <pre class="stHighlight">
    *   val tagOpt = SELF.R5[Int]
    *   val res = if (tagOpt.isDefined) {
    *     val tag = tagOpt.get
    *     if (tag == 1) {
    *       val x = SELF.R4[Int].get
    *       // compute res using value x is of type Int
    *     } else if (tag == 2) {
    *       val x = SELF.R4[GroupElement].get
    *       // compute res using value x is of type GroupElement
    *     } else if (tag == 3) {
    *       val x = SELF.R4[ Array[Byte] ].get
    *       // compute res using value x of type Array[Byte]
    *     } else {
    *       // compute `res` when `tag` is not 1, 2 or 3
    *     }
    *   }
    *   else {
    *     // compute value of res when register is not present
    *   }
    * </pre>
    *
    * @param i zero-based identifier of the register.
    * @tparam T expected type of the register.
    * @return Some(value) if the register is defined AND has the given type.
    *         None otherwise
    * @throws InvalidType exception when the type of the register value is
    *                                   different from T.
    */
  def Ri[T]: Option[T]
}
```

Besides properties, every box can have up to 10 numbered registers.
The following syntax is supported to access registers on box objects:
```
// access R3 register, check that its value of type Int and return it
box.R3[Int].get         
 
// check that value of R3 is defined and has type Int
box.R3[Int].isDefined    

// access R3 register, if it is defined and of type Int then return it, 
// if not of type Int then throw exception, 
// if not defined then return `d`
box.R3[Int].getOrElse(d) 
```

#### GroupElement
```scala
/** Base class for points on elliptic curves. */
class GroupElement {
  /** Exponentiate this <code>GroupElement</code> to the given number.
    * @param k The power.
    * @return <code>this to the power of k</code>.
    */
  def exp(k: BigInt): GroupElement
  
  /** Exponentiate this <code>GroupElement</code> to the given unsigned 256 bit integer.
    * @param k The power.
    * @return <code>this to the power of k</code>.
    */
  def expUnsigned(k: UnsignedBigInt): GroupElement

  /** Group operation. */
  def multiply(that: GroupElement): GroupElement

  /** Inverse element in the group. */
  def negate: GroupElement

  /** Get an encoding of the point value.
    *
    * @return the point encoding
    */
  def getEncoded: Coll[Byte]
}
```

#### SigmaProp
```scala
/** Proposition which can be proven and verified by sigma protocol. */
trait SigmaProp {
  /** Serialized bytes of this sigma proposition.
    * In order to have comparisons like  `box.propositionBytes == prop.propBytes`
    * this SigmaProp is converted to ErgoTree as:
    * 1. prop converted to [[SigmaPropConstant]]
    * 2. new ErgoTree created with with ErgoTree.DefaultHeader, EmptyConstant and SigmaPropConstant as the root
    * 
    * Thus obtained ErgoTree is serialized using DefaultSerializer and compared with `box.propositionBytes`.
    * Here, propBytes uses very specific ErgoTree with header == 0, and hence ErgoTree.version == 0.
    * However, `box.propositionBytes`, contain bytes of ErgoTree from `box`, and version of that
    * tree depends on how the `box` was created.
    * A better way to compare `box.propositionBytes` and `prop.propBytes` is to use the following code:
    * ```
    * val propBytes = prop.propBytes
    * val treeBytes = box.propositionBytes
    * if (treeBytes(0) == 0) {
    *   treeBytes == propBytes
    * } else {
    *   // offset = 1 + <number of VLQ encoded bytes to store propositionBytes.size>
    *   val offset = if (propositionBytes.size > 127) 3 else 2
    *   propBytes.slice(1, propBytes.size) == propositionBytes.slice(offset, propositionBytes.size) 
    * }
    * ```
    */
  def propBytes: Coll[Byte]

  /** Logical AND between this SigmaProp and the `other` SigmaProp.
    * This constructs a new CAND node of a sigma tree with two children. */
  def &&(other: SigmaProp): SigmaProp

  /** Logical AND between this `SigmaProp` and the `Boolean` value on the right.
    * The boolean value will be wrapped into `SigmaProp` using the `sigmaProp` function.
    * This constructs a new CAND node of a sigma tree with two children. */
  def &&(other: Boolean): SigmaProp

  /** Logical OR between this SigmaProp and the other SigmaProp.
    * This constructs a new COR node of sigma tree with two children. */
  def ||(other: SigmaProp): SigmaProp

  /** Logical OR between this `SigmaProp` and the `Boolean` value on the right.
    * The boolean value will be wrapped into `SigmaProp` using the `sigmaProp` function.
    * This constructs a new COR node of a sigma tree with two children. */
  def ||(other: Boolean): SigmaProp
}
```

#### AvlTree

```scala
/** Type of data which efficiently authenticates potentially huge dataset having key-value dictionary interface.
  * Only root hash of dynamic AVL+ tree, tree height, key length, optional value length, and access flags are stored
  * in an instance of the datatype.
  *
  * Please note that standard hash function from `scorex.crypto.hash` is used, and height is stored along with root hash of
  * the tree, thus `digest` size is always CryptoConstants.hashLength + 1 bytes.
  */
class AvlTree {
  /** Returns digest of the state represented by this tree.
    * Authenticated tree digest = root hash bytes ++ tree height
    */
  def digest: Coll[Byte]

  /** Flags of enabled operations packed in single byte.
    * isInsertAllowed == (enabledOperations & 0x01) != 0
    * isUpdateAllowed == (enabledOperations & 0x02) != 0
    * isRemoveAllowed == (enabledOperations & 0x04) != 0
    */
  def enabledOperations: Byte

  /** All the elements under the tree have the same length of the keys */
  def keyLength: Int
  
  /** If non-empty, all the values under the tree are of the same length. */
  def valueLengthOpt: Option[Int]

  /** Checks if Insert operation is allowed for this tree instance. */
  def isInsertAllowed: Boolean

  /** Checks if Update operation is allowed for this tree instance. */
  def isUpdateAllowed: Boolean

  /** Checks if Remove operation is allowed for this tree instance. */
  def isRemoveAllowed: Boolean

  /** Replace digest of this tree producing a new tree.
    * Since AvlTree is immutable, this tree instance remains unchanged.
    * @param newDigest   a new digest
    * @return a copy of this AvlTree instance where `this.digest` replaced by
    *         `newDigest`
    */
  def updateDigest(newDigest: Coll[Byte]): AvlTree

  /** Enable/disable operations of this tree producing a new tree.
    * Since AvlTree is immutable, `this` tree instance remains unchanged.
    * @param newOperations a new flags which specify available operations on a
    *                      new tree.
    * @return              a copy of this AvlTree instance where
    *                      `this.enabledOperations` replaced by `newOperations`
    */
  def updateOperations(newOperations: Byte): AvlTree

  /** Checks if an entry with key `key` exists in this tree using proof `proof`.
    * Throws exception if proof is incorrect.
    *
    * @note CAUTION! Does not support multiple keys check, use [[getMany]] instead.
    * Return `true` if a leaf with the key `key` exists
    * Return `false` if leaf with provided key does not exist.
    * @param key    a key of an element of this authenticated dictionary.
    * @param proof data to reconstruct part of the tree enough to perform the check
    */
  def contains(key: Coll[Byte], proof: Coll[Byte]): Boolean

  /** Perform a lookup of key `key` in this tree using proof `proof`.
    * Throws exception if proof is incorrect
    *
    * @note CAUTION! Does not support multiple keys check, use [[getMany]] instead.
    * Return Some(bytes) of leaf with key `key` if it exists
    * Return None if leaf with provided key does not exist.
    * @param key    a key of an element of this authenticated dictionary.
    * @param proof data to reconstruct part of the tree enough to get the value
    *              by the key
    */
  def get(key: Coll[Byte], proof: Coll[Byte]): Option[Coll[Byte]]

  /** Perform a lookup of many keys `keys` in this tree using proof `proof`.
    *
    * @note CAUTION! Keys must be ordered the same way they were in lookup
    * before proof was generated.
    * For each key return Some(bytes) of leaf if it exists and None if is doesn't.
    * @param keys  keys of elements of this authenticated dictionary.
    * @param proof data to reconstruct part of the tree enough to get the values
    *              by the keys
    */
  def getMany(keys: Coll[Coll[Byte]], proof: Coll[Byte]): Coll[Option[Coll[Byte]]]

  /** Perform insertions of key-value entries into this tree using proof `proof`.
    * Throws exception if proof is incorrect
    *
    * @note CAUTION! Pairs must be ordered the same way they were in insert ops
    * before proof was generated.
    * Return Some(newTree) if successful
    * Return None if operations were not performed.
    * @param operations collection of key-value pairs to insert in this
    *                   authenticated dictionary.
    * @param proof data to reconstruct part of the tree
    */
  def insert(operations: Coll[(Coll[Byte], Coll[Byte])], proof: Coll[Byte]): Option[AvlTree]

  /** Perform updates of key-value entries into this tree using proof `proof`.
    * Throws exception if proof is incorrect
    *
    * @note CAUTION! Pairs must be ordered the same way they were in update ops
    * before proof was generated.
    * Return Some(newTree) if successful
    * Return None if operations were not performed.
    * @param operations collection of key-value pairs to update in this
    *                   authenticated dictionary.
    * @param proof      subtree which is enough to check operations
    */
  def update(operations: Coll[(Coll[Byte], Coll[Byte])], proof: Coll[Byte]): Option[AvlTree]


 /** Perform insertions or updates of key-value entries into this tree using proof `proof`.
   * Throws exception if proof is incorrect
   *
   * @note CAUTION! Pairs must be ordered the same way they were in ops
   * before proof was generated.
   * Return Some(newTree) if successful
   * Return None if operations were not performed.
   * @param operations collection of key-value pairs to insert or update in this
   *                   authenticated dictionary.
   * @param proof subtree which is enough to check operations
   */
 def insertOrUpdate(operations: Coll[(Coll[Byte], Coll[Byte])], proof: Coll[Byte]): Option[AvlTree]

  /** Perform removal of entries into this tree using proof `proof`.
    * Throws exception if proof is incorrect
    * Return Some(newTree) if successful
    * Return None if operations were not performed.
    *
    * @note CAUTION! Keys must be ordered the same way they were in remove ops
    * before proof was generated.
    * @param operations collection of keys to remove from this authenticated
    *                   dictionary.
    * @param proof      data to reconstruct part of the tree
    */
  def remove(operations: Coll[Coll[Byte]], proof: Coll[Byte]): Option[AvlTree]
}
```

#### Option[T]

```scala
/** Represents optional values. Instances of `Option`
 *  are either an instance of `Some(x)` or the value `None`.
 */
class Option[A] {
  /** Returns true if the option is an instance of Some(value), false otherwise. 
   */
  def isDefined: Boolean;

  /** Returns true if the option is None, false otherwise. 
   */
  def isEmpty: Boolean;
  
  /** Returns the option's value if the option is nonempty, otherwise
    * return the result of evaluating `default`.
    * NOTE: the `default` is evaluated lazily.
    *
    * @param default  the default expression.
    */
  def getOrElse[B](default: B): B  

  /** Returns the option's value.
   *  @note The option must be nonempty.
   *  @throws InterpreterException if the option is empty.
   */
  def get: A

  /** Returns a Some containing the result of applying $f to this option's
   * value if this option is nonempty.
   * Otherwise return None.
   *
   * @note This is similar to `flatMap` except here, $f does not need to wrap its result in an $option.
   *
   * @param  f   the function to apply
   * @since  2.0
   * @see flatMap
   */
  def map[B](f: A => B): Option[B]

  
  /** Returns this option if it is nonempty '''and''' applying the predicate $p to
   * this option's value returns true. Otherwise, return $none.
   *
   * @param  p   the predicate used for testing.
   * @since  2.0
   */
  def filter(p: A => Boolean): Option[A]
}
```

#### Coll[T]

```scala
/** Indexed (zero-based) collection of elements of type `A` 
  * @tparam A the collection element type
  */
class Coll[A] {
  /** The number of elements in the collection */
  def size: Int
  
  /** The element at given index.
   *  Indices start at `0`; `xs.apply(0)` is the first element of collection `xs`.
   *  Note the indexing syntax `xs(i)` is a shorthand for `xs.apply(i)`.
   *
   *  @param    i   the index
   *  @return       the element at the given index
   *  @throws       ArrayIndexOutOfBoundsException if `i < 0` or `length <= i`
   */
  def apply(i: Int): A
  
  /** The element of the collection or default value. 
   * If an index is out of bounds (`i < 0 || i >= length`) then `default` value is returned.
   *  @param    i   the index
   *  @return       the element at the given index or default value if index is out or bounds
   */
  def getOrElse(i: Int, default: A): A
  
  /** Builds a new collection by applying a function to all elements of this collection.
   *
   *  @param f      the function to apply to each element.
   *  @tparam B     the element type of the returned collection.
   *  @return       a new collection of type `Coll[B]` resulting from applying the given function
   *                `f` to each element of this collection and collecting the results.
   */
  def map[B](f: A => B): Coll[B]

  /** For this collection (x0, ..., xN) and other collection (y0, ..., yM)
   * produces a collection ((x0, y0), ..., (xK, yK)) where K = min(N, M) 
   */
  def zip[B](ys: Coll[B]): Coll[(A, B)]

  /** Tests whether a predicate holds for at least one element of this collection.
   *  @param   p     the predicate used to test elements.
   *  @return        `true` if the given predicate `p` is satisfied by at least one element of this collection, otherwise `false`
   */
  def exists(p: A => Boolean): Boolean
  
  /** Tests whether a predicate holds for all elements of this collection.
   *  @param   p   the predicate used to test elements.
   *  @return      `true` if this collection is empty or the given predicate `p`
   *               holds for all elements of this collection, otherwise `false`.
   */
  def forall(p: A => Boolean): Boolean
  
  /** Selects all elements of this collection which satisfy a predicate.
   *  @param p     the predicate used to test elements.
   *  @return      a new collection consisting of all elements of this collection that satisfy the given
   *               predicate `p`. The order of the elements is preserved.
   */
  def filter(p: A => Boolean): Coll[A]
  
  /** Applies a binary operator to a start value and all elements of this collection,
   *  going left to right.
   *
   *  @param   z    the start value.
   *  @param   op   the binary operator.
   *  @tparam  B    the result type of the binary operator.
   *  @return  the result of inserting `op` between consecutive elements of this collection,
   *           going left to right with the start value `z` on the left:
   *           {{{
   *             op(...op(z, x_1), x_2, ..., x_n)
   *           }}}
   *           where `x,,1,,, ..., x,,n,,` are the elements of this collection.
   *           Returns `z` if this collection is empty.
   */
  def fold[B](z: B, op: (B, A) => B): B

  /** Produces the range of all indices of this collection [0 .. size-1] */
  def indices: Coll[Int]

  /**
    * Builds a new collection by applying a function to all elements of this collection
    * and using the elements of the resulting collections.
    *
    * @param f the function to apply to each element.
    * @tparam B the element type of the returned collection.
    * @return a new collection of type `Coll[B]` resulting from applying the given collection-valued function
    *         `f` to each element of this collection and concatenating the results.
    */
  def flatMap[B](f: A => Coll[B]): Coll[B]

  /** Produces a new collection where a slice of elements in this collection is replaced by another sequence.
    *
    *  @param  from     the index of the first replaced element
    *  @param  patch    the replacement sequence
    *  @param  replaced the number of elements to drop in the original collection
    *  @return          a new collection consisting of all elements of this collection
    *                   except that `replaced` elements starting from `from` are replaced by `patch`.
    */
  def patch(from: Int, patch: Coll[A], replaced: Int): Coll[A]

  /** A copy of this collection with one single replaced element.
    *  @param  index  the position of the replacement
    *  @param  elem   the replacing element
    *  @return a new collection which is a copy of this collection with the element at position `index` replaced by `elem`.
    *  @throws IndexOutOfBoundsException if `index` does not satisfy `0 <= index < length`.
    */
  def updated(index: Int, elem: A): Coll[A]

  /** Returns a copy of this collection where elements at `indexes` are replaced
    * with `values`. 
    */
  def updateMany(indexes: Coll[Int], values: Coll[A]): Coll[A]

  /** Selects an interval of elements.  The returned collection is made up
   *  of all elements `x` which satisfy the invariant:
   *  {{{
   *    from <= indexOf(x) < until
   *  }}}
   *  @param from   the lowest index to include from this collection.
   *  @param until  the lowest index to EXCLUDE from this collection.
   */
  def slice(from: Int, until: Int): Coll[A]
  
  /** Puts the elements of other collection after the elements of this
    * collection (concatenation of 2 collections).
    */
  def append(other: Coll[A]): Coll[A]
  
  /** Finds index of first occurrence of some value in this collection after or
    * at some start index.
    *  @param   elem   the element value to search for.
    *  @param   from   the start index
    *  @return  the index `>= from` of the first element of this collection that is equal (as determined by `==`)
    *           to `elem`, or `-1`, if none exists.
    */
  def indexOf(elem: A, from: Int): Int

  /** The element at given index or None if there is no such element. Indices start at `0`.
    *
    *  @param  i       the index
    *  @return         the element at the given index, or None if there is no such element
    */
  def get(i: Int): Option[A]

  /**
    * @return true if first elements of this collection form given `ys` collection, false otherwise.
    *         E.g. [1,2,3] starts with [1,2]
    */
  def startsWith(ys: Coll[A]): Boolean

  /**
    * @return true if last elements of this collection form given `ys` collection, false otherwise.
    *         E.g. [1,2,3] ends with [2,3]
    */
  def endsWith(ys: Coll[A]): Boolean
  
  
}
```

Each item can be accessed by constant index, for example:
```
val myOutput = OUTPUTS(0)
val myInput = INPUTS(0)
```

or, if you want to get None instead of exception: 
```
val elemOpt = coll.get(0)
if (elemOpt.isDefined) {
  val elem = elemOpt.get
}
```

Any collection have the `size` property which returns the number of elements in
the collection.

```
val size = OUTPUTS.size
```

The following script check an existence of some element in the collection
satisfying some predicate (condition)

```
val ok = OUTPUTS.exists { (box: Box) => box.value > 1000 }
``` 

### Global functions

There are some functions which do not belong to other types, thus they put under `Global` type. Those functions are:


```
{
  /** The generator g of the group is an element of the group such that, when written
    * multiplicative form, every element of the group is a power of g.
    * @return the generator of this Dlog group
    */
  def groupGenerator: GroupElement

  /**
    * @return NBits-encoded approximate representation of given big integer,
    *         see (https://bitcoin.stackexchange.com/questions/57184/what-does-the-nbits-value-represent)
    *         for NBits format details
    */
  def encodeNbits(bi: BigInt): Long

  /**
    * @return big integer decoded from NBits value provided,
    *         see (https://bitcoin.stackexchange.com/questions/57184/what-does-the-nbits-value-represent)
    *         for format details
    */
  def decodeNbits(l: Long): BigInt
  
  
  /** Serializes the given `value` into bytes using the default serialization format. */
  def serialize[T](value: T)(implicit cT: RType[T]): Coll[Byte]

  /** Returns a byte-wise XOR of the two collections of bytes. */
  def xor(l: Coll[Byte], r: Coll[Byte]): Coll[Byte]

  /** Calculates value of a custom Autolykos 2 hash function 
    * @param k - k parameter of Autolykos 2 (number of inputs in k-sum problem)
    * @param msg - message to calculate Autolykos hash 2 for
    * @param nonce - used to pad the message to get Proof-of-Work hash function output with desirable properties
    * @param h - PoW protocol specific padding for table uniqueness (e.g. block height in Ergo)
    */
  def powHit(k: Int, msg: Coll[Byte], nonce: Coll[Byte], h: Coll[Byte], N: Int): UnsignedBigInt

  /** Deserializes provided `bytes` into a value of type `T`. **/
  def deserializeTo[T](bytes: Coll[Byte])(implicit cT: RType[T]): T

  /** Returns a number decoded from provided big-endian bytes array. */
  def fromBigEndianBytes[T](bytes: Coll[Byte])(implicit cT: RType[T]): T
}
```

examples:

```
val h = getVar[Header](21).get
val n = h.nBits
val target = Global.decodeNbits(n)
```

```
val src = getVar[Coll[Short]](21).get
val ba = Global.serialize(src)
val restored = Global.deserializeTo[Coll[Short]](ba)
src == restored
```


### Predefined functions
<a name="PredefinedFunctions"></a>

ErgoScript standard library include predefined functions that can be called 
without prior declaration. 

The following function declarations are automatically imported into any script:

```scala 
/** Returns true if all the elements in collection are true. */
def allOf(conditions: Coll[Boolean]): Boolean

/** Returns true if at least on element of the conditions is true */
def anyOf(conditions: Coll[Boolean]): Boolean

/** Similar to allOf, but performing logical XOR operation instead of `&&` */
def xorOf(conditions: Coll[Boolean]): Boolean 

/** Returns SigmaProp value which can be ZK proven to be true 
 * if at least k properties can be proven to be true. 
 */
def atLeast(k: Int, properties: Coll[SigmaProp]): SigmaProp
    
/** Embedding of Boolean values to SigmaProp values. As an example, this
 * operation allows boolean expressions to be used as arguments of
 * `atLeast(..., sigmaProp(myCondition), ...)` operation.
 */
def sigmaProp(condition: Boolean): SigmaProp
        
/** Cryptographic hash function Blake2b256 (See scorex.crypto.hash.Blake2b256) */
def blake2b256(input: Coll[Byte]): Coll[Byte]

/** Cryptographic hash function Sha256 (See scorex.crypto.hash.Sha256) */
def sha256(input: Coll[Byte]): Coll[Byte]

/** Create an instance of type T from bytes of its wrapped type. 
See https://github.com/ScorexFoundation/sigmastate-interpreter/pull/979 for more details */
def deserializeTo[T](input: Coll[Byte]): T

/** Create BigInt from a collection of bytes. */
def byteArrayToBigInt(input: Coll[Byte]): BigInt

/** Create Long from a collection of bytes. */
def byteArrayToLong(input: Coll[Byte]): Long  

/** Returns bytes representation of Long value. */
def longToByteArray(input: Long): Coll[Byte]

/** Convert bytes representation of group element (ECPoint) 
  * to a new value of GroupElement (using
  * org.bouncycastle.math.ec.ECCurve.decodePoint())
  */
def decodePoint(bytes: Coll[Byte]): GroupElement 


/** Extracts Context variable from SELF input by id and type.
  * ErgoScript is typed, so accessing a the variables is an operation which involves
  * some expected type given in brackets. Thus `getVar[Int](id)` expression should
  * evaluate to a valid value of the `Option[Int]` type.
  *
  * For example `val x = getVar[Int](10)` expects the variable, if it is present, to have
  * type `Int`. 
  *
  * There are three cases:
  * 1) If the variable doesn't exist.
  *   Then `val x = getVar[Int](id)` succeeds and returns the None value, which conforms to
  *   any value of type `Option[T]` for any T. (In the example above T is equal to
  *   `Int`). Calling `x.get` fails when x is equal to None, but `x.isDefined`
  *   succeeds and returns `false`.
  * 2) If the variable contains a value `v` of type `Int`.
  *   Then `val x = getVar[Int](id)` succeeds and returns `Some(v)`, which is a valid value
  *   of type `Option[Int]`. In this case, calling `x.get` succeeds and returns the
  *   value `v` of type `Int`. Calling `x.isDefined` returns `true`.
  * 3) If the variable contains a value `v` of type T other then `Int`.
  *   Then `val x = getVar[Int](id)` fails, because there is no way to return a valid value
  *   of type `Option[Int]`. The value of variable is present, so returning it as None
  *   would break the typed semantics of variables collection.
  *
  * In some use cases one variable may have values of different types. To access such
  * variable an additional variable can be used as a tag.
  *
  * <pre class="stHighlight">
  *   val tagOpt = getVar[Int](id)
  *   val res = if (tagOpt.isDefined) {
  *     val tag = tagOpt.get
  *     if (tag == 1) {
  *       val x = getVar[Int](id2).get
  *       // compute res when value x is of type Int
  *     } else if (tag == 2) {
  *       val x = getVar[GroupElement](id2).get
  *       // compute res when value x is of type GroupElement
  *     } else if (tag == 3) {
  *       val x = getVar[ Array[Byte] ](id2).get
  *       // compute res when value x of type Array[Byte]
  *     } else {
  *       // compute `res` when `tag` is not 1, 2 or 3
  *     }
  *   }
  *   else {
  *     // compute value of res when the variable is not present
  *   }
  * </pre>
  *
  * @param id zero-based identifier of the variable.
  * @tparam T expected type of the variable.
  * @return Some(value) if the variable is defined in the context AND has the given type.
  *         None otherwise
  * @throws InvalidType exception when the type of the variable value is
  *                                   different from cT.
  */
def getVar[T](tag: Int): Option[T]

/** Extracts Context variable from any input by input index, variable id and variable type.
  * Unlike getVar, it is not throwing exception when expected type does not match real type of the variable.
  * Thus it can be used to get context variable from self without exception, using selfBoxIndex, e.g. 
  * <pre class="stHighlight">
  *   {
  *       val idx = CONTEXT.selfBoxIndex
  *       sigmaProp(CONTEXT.getVarFromInput[Int](idx.toShort, 1.toByte).get == 5)
  *   }
  * </pre>
  */
def getVarFromInput[T](inputId: Short, varId: Byte): Option[T]

/** Construct a new SigmaProp value representing public key of Diffie Hellman
  * signature protocol. When executed as part of Sigma protocol allow to provide
  * for a verifier a zero-knowledge proof of secret knowledge.
  */
def proveDHTuple(g: GroupElement, h: GroupElement, 
                 u: GroupElement, v: GroupElement): SigmaProp
                 
/** Construct a new SigmaProp value representing public key of discrete
  * logarithm signature protocol. When executed as part of Sigma protocol allow
  * to provide for a verifier a zero-knowledge proof of secret knowledge.
  */
def proveDlog(value: GroupElement): SigmaProp

/** Transforms Base16 encoded string literal into constant of type BigInt.
  * It is a compile-time operation and only string literal (constant) can be its
  * argument.
  */
def bigInt(input: String): BigInt

/** Transforms Base16 encoded string literal into constant of type UnsignedBigInt.
  * It is a compile-time operation and only string literal (constant) can be its
  * argument.
  */
def unsignedBigInt(input: String): UnsignedBigInt

/** Transforms Base16 encoded string literal into constant of type Coll[Byte].
  * It is a compile-time operation and only string literal (constant) can be its
  * argument.
  */
def fromBase16(input: String): Coll[Byte]

/** Transforms Base58 encoded string literal into constant of type Coll[Byte].
  * It is a compile-time operation and only string literal (constant) can be its
  * argument.
  */
def fromBase58(input: String): Coll[Byte]

/** Transforms Base64 encoded string literal into constant of type Coll[Byte].
  * It is a compile-time operation and only string literal (constant) can be its
  * argument.
  */
def fromBase64(input: String): Coll[Byte]

/** It is executed in compile time. The compiler takes Base58 encoding of public
  * key as String literal and create GroupElement constant. Then the compiler
  * used this constant to construct proveDlog public key out of it.
  */
def PK(input: String): SigmaProp
    
/** Deserializes values from Base58 encoded binary data at compile time into a
  * value of type T.
  */
def deserialize[T](string: String): T

/**  
 *
 * Extracts, deserializes and executes a script contained in the context variable specified
 * by id. Returns the result of the script execution in the current context.
 * Throws an exception if the result type of the execution doesn't conform to the return
 * type specified.
 * 
 * @param id context variable holding the serialized script to execute
 * @tparam T expected type of the variable and return type.
 * @return result of the executed script
 * @throws InvalidType exception when the result type of the execution value is 
 *                     different from T. 
 */
def executeFromVar[T](id: Byte): T

/**
 * 
 * Extracts, deserializes and executes a script contained in the SELF register 
 * indicated by id. Returns the result of the script execution in the current context
 * An exception is thrown if the result type of the execution doesn't conform to the
 * return type specified and if an invalid register is specified.
 * 
 * @param id register id holding the serialized script to execute
 * @tparam T expected type of the register and return type.
 * @return result of the executed script or default value
 * @throws InterpreterException when a script reduces to false
 * @throws InvalidType exception when the result type of the execution value is 
 *                     different from T.
 */
def executeFromSelfReg[T](id: Int): T

/**
 * 
 * Extracts, deserializes and executes a script contained in the SELF register 
 * indicated by id. Returns the result of the script execution in the current context
 * or default value, if the register is unavailable. An exception is thrown if the result 
 * type of the execution doesn't conform to the return type specified and if an invalid 
 * register is specified.
 * 
 * @param id register id holding the serialized script to execute
 * @param default value returned if the register is unavailable
 * @tparam T expected type of the register and return type.
 * @return result of the executed script or default value
 * @throws InterpreterException when a script reduces to false
 * @throws InvalidType exception when the result type of the execution value is 
 *                     different from T.
 */
def executeFromSelfRegWithDefault[T](id: Int, default: T): T

/**
  * Transforms serialized bytes of ErgoTree with segregated constants by
  * replacing constants at given positions with new values. This operation allow
  * to use serialized scripts as pre-defined templates.

  * The typical usage is "check that output box have proposition equal to given
  * script bytes, where minerPk (constants(0)) is replaced with currentMinerPk".
  * Each constant in original scriptBytes have SType serialized before actual
  * data (see ConstantSerializer). During substitution each value from newValues
  * is checked to be an instance of the corresponding type. This means, the
  * constants during substitution cannot change their types.
  *
  * @param scriptBytes serialized ErgoTree with ConstantSegregationFlag set to 1.
  * @param positions zero based indexes in ErgoTree.constants array which should
  *                  be replaced with new values
  * @param newValues new values to be injected into the corresponding positions
  *                  in ErgoTree.constants array
  * @return original scriptBytes array where only specified constants are
  *         replaced and all other bytes remain exactly the same
  */
def substConstants[T](scriptBytes: Coll[Byte], positions: Coll[Int], newValues: Coll[T]): Coll[Byte]
```

## Known Limitations

* You can use methods added in 6.0 and new `UnsignedBigInt` type only within an ErgoTree with version >= 3 
* 6.0 methods and other changes can be found in [EIP-50](https://github.com/ergoplatform/eips/pull/100)
* You can't put values of `Option[]`, `Header`, `UnsignedBigInt` types into registers or context extension variables to 
avoid versioning issues with 5.0 clients. To bypass the limitation, you can write typed value as bytes and call `Global.deserialize` in a script 
to get an instance of those types.  
* Higher order lambdas supported since 6.0 (you need to use ErgoTree v3 to have them parsed), see a higher order lambda example @ 
[https://github.com/ergoplatform/sigmastate-interpreter/blob/b754e143cf38ed86d95698ede744a470dfa053d6/sigmastate/src/test/scala/special/sigma/SigmaDslSpecification.scala#L10040](https://github.com/ergoplatform/sigmastate-interpreter/blob/b754e143cf38ed86d95698ede744a470dfa053d6/sigmastate/src/test/scala/special/sigma/SigmaDslSpecification.scala#L10040)


## Examples

See [white paper for examples](https://ergoplatform.org/docs/ErgoScript.pdf)
