package sigma

import sigma.ast.SType

import java.math.BigInteger
import sigma.data._

/**
  * Functions defined for 256-bit signed integers
  * */
trait BigInt {
  /** Convert this BigInt value to Byte.
    * @throws ArithmeticException if overflow happens.
    */
  def toByte: Byte

  /** Convert this BigInt value to Short.
    * @throws ArithmeticException if overflow happens.
    */
  def toShort: Short

  /** Convert this BigInt value to Int.
    * @throws ArithmeticException if overflow happens.
    */
  def toInt: Int

  /** Convert this BigInt value to Int.
    * @throws ArithmeticException if overflow happens.
    */
  def toLong: Long

  /** Returns a big-endian representation of this BigInt in a collection of bytes.
    * For example, the value {@code 0x1213141516171819} would yield the
    * byte array {@code {0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19}}.
    * @since 2.0
    */
  def toBytes: Coll[Byte]

  /** Absolute value of this numeric value.
    * @since 2.0
    */
  def toAbs: BigInt

  /** Compares this numeric with that numeric for order.  Returns a negative integer, zero, or a positive integer as the
    * `this` is less than, equal to, or greater than `that`.
    */
  def compareTo(that: BigInt): Int

  /** Returns the signum function of this BigInt.
    *
    * @return -1, 0 or 1 as the value of this BigInt is negative, zero or
    *         positive.
    */
  def signum: Int

  /** Returns a BigInt whose value is {@code (this + that)}.
    *
    * @param  that value to be added to this BigInt.
    * @return { @code this + that}
    */
  def add(that: BigInt): BigInt
  def +(that: BigInt): BigInt = add(that)

  /** Returns a BigInt whose value is {@code (this - that)}.
    *
    * @param  that value to be subtracted from this BigInt.
    * @return { @code this - that}
    */
  def subtract(that: BigInt): BigInt
  def -(that: BigInt): BigInt = subtract(that)

  /** Returns a BigInt whose value is {@code (this * that)}.
    *
    * @implNote An implementation may offer better algorithmic
    *           performance when { @code that == this}.
    * @param  that value to be multiplied by this BigInt.
    * @return { @code this * that}
    */
  def multiply(that: BigInt): BigInt
  def *(that: BigInt): BigInt = multiply(that)

  /** Returns a BigInt whose value is {@code (this / that)}.
    *
    * @param  that value by which this BigInt is to be divided.
    * @return { @code this / that}
    * @throws ArithmeticException if { @code that} is zero.
    */
  def divide(that: BigInt): BigInt
  def /(that: BigInt): BigInt = divide(that)

  /**
    * Returns a BigInt whose value is {@code (this mod m}).  This method
    * differs from {@code remainder} in that it always returns a
    * <i>non-negative</i> BigInteger.
    *
    * @param  m the modulus.
    * @return { @code this mod m}
    * @throws ArithmeticException { @code m} &le; 0
    * @see #remainder
    */
  def mod(m: BigInt): BigInt
  def %(m: BigInt): BigInt = mod(m)

  /**
    * Returns a BigInt whose value is {@code (this % that)}.
    *
    * @param  that value by which this BigInt is to be divided, and the
    *             remainder computed.
    * @return { @code this % that}
    * @throws ArithmeticException if { @code that} is zero.
    */
  def remainder(that: BigInt): BigInt

  /**
    * Returns the minimum of this BigInteger and {@code val}.
    *
    * @param  that value with which the minimum is to be computed.
    * @return the BigInteger whose value is the lesser of this BigInteger and
    *         { @code val}.  If they are equal, either may be returned.
    */
  def min(that: BigInt): BigInt

  /**
    * Returns the maximum of this BigInteger and {@code val}.
    *
    * @param  that value with which the maximum is to be computed.
    * @return the BigInteger whose value is the greater of this and
    *         { @code val}.  If they are equal, either may be returned.
    */
  def max(that: BigInt): BigInt

  /** Returns a BigInt whose value is {@code (-this)}.
    * @return { @code -this}
    */
  def negate(): BigInt

  /** Returns a BigInteger whose value is `(this & that)`.  (This
    * method returns a negative BigInteger if and only if `this` and `that` are
    * both negative.)
    *
    * @param that value to be AND'ed with this BigInteger.
    * @return `this & that`
    */
  def and(that: BigInt): BigInt
  def &(that: BigInt): BigInt = and(that)

  /** Returns a BigInteger whose value is `(this | that)`.  (This
    * method returns a negative BigInteger if and only if either `this` or `that`` is
    * negative.)
    *
    * @param that value to be OR'ed with this BigInteger.
    * @return `this | that`
    */
  def or(that: BigInt): BigInt
  def |(that: BigInt): BigInt = or(that)

  /**
    * @return a big integer whose value is `this xor that`.
    *         This method returns a negative BigInteger if and only if exactly one of this and val are negative.
    */
  def xor(that: BigInt): BigInt

  /**
    * @return a 256-bit signed integer whose value is (this << n). `n` should be in 0..255 range (inclusive).
    */
  def shiftLeft(n: Int): BigInt

  /**
    * @return a 256-bit signed integer whose value is (this >> n). `n` should be in 0..255 range (inclusive).
    */
  def shiftRight(n: Int): BigInt
}

/** Base class for points on elliptic curves. */
trait GroupElement {
  /** Checks if the provided element is an identity element. */
  def isIdentity: Boolean

  /** Exponentiate this <code>GroupElement</code> to the given number.
    * @param k The power.
    * @return <code>this to the power of k</code>.
    * @since 2.0
    */
  def exp(k: BigInt): GroupElement

  /** Group operation. */
  def multiply(that: GroupElement): GroupElement

  /** Inverse element in the group. */
  def negate: GroupElement

  /**
    * Get an encoding of the point value.
    *
    * @return the point encoding
    */
  def getEncoded: Coll[Byte]
}

/** Proposition which can be proven and verified by sigma protocol. */
trait SigmaProp {
  def isValid: Boolean
  /** Serialized bytes of this sigma proposition taken as ErgoTree and then serialized. */
  def propBytes: Coll[Byte]

  /** Logical AND between this SigmaProp and other SigmaProp.
    * This constructs a new CAND node of sigma tree with two children. */
  def &&(other: SigmaProp): SigmaProp

  /** Logical OR between this SigmaProp and other SigmaProp.
    * This constructs a new COR node of sigma tree with two children. */
  def ||(other: SigmaProp): SigmaProp
}

/** Represents any value paired with type descriptor. */
trait AnyValue {
  /** The data value wrapped by this instance. */
  def value: Any
  /** The type descriptor of the `value` instance. */
  def tVal: RType[Any]
}

/** Runtime representation of Ergo boxes used during execution of ErgoTree operations.
  * @see [[org.ergoplatform.ErgoBox]]
  */
trait Box {
  /** Blake2b256 hash of this box's content, basically equals to `blake2b256(bytes)` */
  def id: Coll[Byte]

  /** Mandatory: Monetary value, in Ergo tokens (NanoErg unit of measure)*/
  def value: Long

  /** Serialized bytes of guarding script, which should be evaluated to true in order to
    * open this box. (aka spend it in a transaction)*/
  def propositionBytes: Coll[Byte]

  /** Serialized bytes of this box's content, including proposition bytes. */
  def bytes: Coll[Byte]

  /** Serialized bytes of this box's content, excluding transactionId and index of output. */
  def bytesWithoutRef: Coll[Byte]

  def registers: Coll[AnyValue]

  /** Extracts register by id and type.
    * ErgoScript is typed, so accessing a register is an operation which involves some
    * expected type given in brackets. Thus `SELF.R4[Int]` expression should evaluate to a
    * valid value of the `Option[Int]` type.
    *
    * For example `val x = SELF.R4[Int]` expects the
    * register, if it is present, to have type `Int`. At runtime the corresponding type
    * descriptor is passed as `cT` parameter.
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
    * @throws `sigmastate.eval.InvalidType` exception when the type of the register value is
    *                                   different from cT.
    * @since 2.0
    */
  def getReg[T](i: Int)(implicit cT: RType[T]): Option[T]

  /** Mandatory: Monetary value, in Ergo tokens */
  def R0[T](implicit cT:RType[T]): Option[T] = this.getReg[T](0)

  /** Mandatory: Guarding script */
  def R1[T](implicit cT:RType[T]): Option[T] = this.getReg[T](1)

  /** Mandatory: Secondary tokens */
  def R2[T](implicit cT:RType[T]): Option[T] = this.getReg[T](2)

  /** Mandatory: Reference to transaction and output id where the box was created */
  def R3[T](implicit cT:RType[T]): Option[T] = this.getReg[T](3)

  /** Non-mandatory register */
  def R4[T](implicit cT:RType[T]): Option[T] = this.getReg[T](4)
  /** Non-mandatory register */
  def R5[T](implicit cT:RType[T]): Option[T] = this.getReg[T](5)
  /** Non-mandatory register */
  def R6[T](implicit cT:RType[T]): Option[T] = this.getReg[T](6)
  /** Non-mandatory register */
  def R7[T](implicit cT:RType[T]): Option[T] = this.getReg[T](7)
  /** Non-mandatory register */
  def R8[T](implicit cT:RType[T]): Option[T] = this.getReg[T](8)
  /** Non-mandatory register */
  def R9[T](implicit cT:RType[T]): Option[T] = this.getReg[T](9)

  /** Secondary tokens */
  def tokens: Coll[(Coll[Byte], Long)]

  /** If `tx` is a transaction which generated this box, then `creationInfo._1` is a height of the tx's block.
    *  The `creationInfo._2` is a serialized transaction identifier followed by box index in the transaction outputs.
    */
  def creationInfo: (Int, Coll[Byte])

  /** Extracts register as Coll[Byte], deserializes it to script and then executes this script in the current context.
    * The original Coll[Byte] of the script is available as getReg[Coll[Byte]](id)
    * @param regId identifier of the register
    * @tparam T result type of the deserialized script.
    * @throws IllegalArgumentException if the actual script type doesn't conform to `T`
    * @return result of the script execution in the current context
    * @since Mainnet
    */
  def executeFromRegister[T](regId: Byte)(implicit cT:RType[T]): T

  override def toString = s"Box(id=$id; value=$value; regs=$registers)"
}

/** Type of data which efficiently authenticates potentially huge dataset having key-value dictionary interface.
  * Only root hash of dynamic AVL+ tree, tree height, key length, optional value length, and access flags are stored
  * in an instance of the datatype.
  *
  * Please note that standard hash function from `scorex.crypto.hash` is used, and height is stored along with root hash of
  * the tree, thus `digest` size is always CryptoConstants.hashLength + 1 bytes.
  *
  * This interface is used as runtime representation of the AvlTree type of ErgoTree.
  */
trait AvlTree {
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
    * @return a copy of this AvlTree instance where `this.digest` replaced by `newDigest`
    */
  def updateDigest(newDigest: Coll[Byte]): AvlTree

  /** Enable/disable operations of this tree producing a new tree.
    * Since AvlTree is immutable, `this` tree instance remains unchanged.
    * @param newOperations  a new flags which specify available operations on a new tree.
    * @return               a copy of this AvlTree instance where `this.enabledOperations`
    *                       replaced by `newOperations`
    */
  def updateOperations(newOperations: Byte): AvlTree
}

/** Only header fields that can be predicted by a miner. */
trait PreHeader {
  /** Block version, to be increased on every soft and hardfork. */
  def version: Byte

  /** Id of parent block */
  def parentId: Coll[Byte] // ModifierId

  /** Block timestamp (in milliseconds since beginning of Unix Epoch) */
  def timestamp: Long

  /** Current difficulty in a compressed view.
    * NOTE: actually it is unsigned Int*/
  def nBits: Long  // actually it is unsigned Int

  /** Block height */
  def height: Int

  /** Miner public key. Should be used to collect block rewards. */
  def minerPk: GroupElement

  /** Miner votes for changing system parameters. */
  def votes: Coll[Byte]
}

/** Represents data of the block header available in Sigma propositions. */
trait Header {
  /** Bytes representation of ModifierId of this Header */
  def id: Coll[Byte]

  /** Block version, to be increased on every soft and hardfork. */
  def version: Byte

  /** Bytes representation of ModifierId of the parent block */
  def parentId: Coll[Byte] //

  /** Hash of ADProofs for transactions in a block */
  def ADProofsRoot: Coll[Byte] // Digest32. Can we build AvlTree out of it?

  /** AvlTree of a state after block application */
  def stateRoot: AvlTree

  /** Root hash (for a Merkle tree) of transactions in a block. */
  def transactionsRoot: Coll[Byte]  // Digest32

  /** Block timestamp (in milliseconds since beginning of Unix Epoch) */
  def timestamp: Long

  /** Current difficulty in a compressed view.
    * NOTE: actually it is unsigned Int*/
  def nBits: Long

  /** Block height */
  def height: Int

  /** Root hash of extension section */
  def extensionRoot: Coll[Byte] // Digest32

  /** Miner public key. Should be used to collect block rewards.
    * Part of Autolykos solution. */
  def minerPk: GroupElement

  /** One-time public key. Prevents revealing of miners secret. */
  def powOnetimePk: GroupElement

  /** nonce */
  def powNonce: Coll[Byte]

  /** Distance between pseudo-random number, corresponding to nonce `powNonce` and a secret,
    * corresponding to `minerPk`. The lower `powDistance` is, the harder it was to find this solution. */
  def powDistance: BigInt

  /** Miner votes for changing system parameters. */
  def votes: Coll[Byte] //3 bytes

  /** Bytes which are coming from future versions of the protocol, so
    * their meaning is not known to current version of Sigma, but they
    * are stored to get the same id as future version users.
    */
  def unparsedBytes: Coll[Byte]

  /**
    * @return header bytes without proof of work, a PoW is generated over them
    */
  def serializeWithoutPoW: Coll[Byte]

  /**
    * @return result of header's proof-of-work validation
    */
  def checkPow: Boolean

}

/** Runtime representation of Context ErgoTree type.
  * Represents data available in Sigma language using `CONTEXT` global variable.
  */
trait Context {
  def builder: SigmaDslBuilder

  /** A collection of outputs of the current transaction. */
  def OUTPUTS: Coll[Box]

  /** A collection of inputs of the current transaction, the transaction where selfBox is one of the inputs. */
  def INPUTS: Coll[Box]

  /** A collection of inputs of the current transaction that will not be spent. */
  def dataInputs: Coll[Box]

  /** Height (block number) of the block which is currently being validated. */
  def HEIGHT: Int

  /** Box whose proposition is being currently executing */
  def SELF: Box

  /** Zero based index in `inputs` of `selfBox`. */
  def selfBoxIndex: Int

  /** Authenticated dynamic dictionary digest representing Utxo state before current state. */
  def LastBlockUtxoRootHash: AvlTree

  /** A fixed number of last block headers in descending order (first header is the newest one) */
  def headers: Coll[Header]

  /** Fields of a new block header, that can be predicted by a miner before block's formation */
  def preHeader: PreHeader

  /** Bytes of encoded miner's public key.
    * Same as `preHeader.minerPk.getEncoded`
    */
  def minerPubKey: Coll[Byte]

  /** Extracts Context variable by id and type.
    * ErgoScript is typed, so accessing a the variables is an operation which involves
    * some expected type given in brackets. Thus `getVar[Int](id)` expression should
    * evaluate to a valid value of the `Option[Int]` type.
    *
    * For example `val x = getVar[Int](10)` expects the variable, if it is present, to have
    * type `Int`. At runtime the corresponding type descriptor is passed as `cT`
    * parameter.
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
    *       // compute res using value x is of type Int
    *     } else if (tag == 2) {
    *       val x = getVar[GroupElement](id2).get
    *       // compute res using value x is of type GroupElement
    *     } else if (tag == 3) {
    *       val x = getVar[ Array[Byte] ](id2).get
    *       // compute res using value x of type Array[Byte]
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
    * @throws `sigmastate.eval.InvalidType` exception when the type of the variable value is
    *                                   different from cT.
    */
  def getVar[T](id: Byte)(implicit cT: RType[T]): Option[T]

  /**
    * A variant of `getVar` to extract a context variable by id and type from any input
    *
    * @param inputId - input index
    * @param id - context variable id
    * @tparam T - expected type of the variable
    * @return Some(value) if the variable is defined in the context AND has the given type.
    *         None otherwise
    */
  def getVarFromInput[T](inputId: Short, id: Byte)(implicit cT: RType[T]): Option[T]

  def vars: Coll[AnyValue]

  /** Maximum version of ErgoTree currently activated on the network.
    * See [[ErgoLikeContext]] class for details. */
  def activatedScriptVersion: Byte

  /** The version of ErgoTree currently executed by interpreter.
    * See [[ErgoLikeContext]] class for details. */
  def currentErgoTreeVersion: Byte
}

/** Shortcut declarations for methods of global object (aka [[SigmaDslBuilder]]).
  * Every methods delegates to the corresponding method of `builder`, look there for details.
  */
trait SigmaContract {
  /** Returns Global instance (aka Builder) which implements all global operations. */
  def builder: SigmaDslBuilder

  def Collection[T](items: T*)(implicit cT: RType[T]): Coll[T] = this.builder.Colls.fromItems[T](items:_*)

  /** !!! all methods should delegate to builder */
  def atLeast(bound: Int, props: Coll[SigmaProp]): SigmaProp = this.builder.atLeast(bound, props)

  def allOf(conditions: Coll[Boolean]): Boolean = this.builder.allOf(conditions)
  def allZK(conditions: Coll[SigmaProp]): SigmaProp = this.builder.allZK(conditions)

  def anyOf(conditions: Coll[Boolean]): Boolean = this.builder.anyOf(conditions)
  def anyZK(conditions: Coll[SigmaProp]): SigmaProp = this.builder.anyZK(conditions)

  def xorOf(conditions: Coll[Boolean]): Boolean = this.builder.xorOf(conditions)

  def sigmaProp(b: Boolean): SigmaProp = this.builder.sigmaProp(b)

  def blake2b256(bytes: Coll[Byte]): Coll[Byte] = this.builder.blake2b256(bytes)
  def sha256(bytes: Coll[Byte]): Coll[Byte] = this.builder.sha256(bytes)

  def byteArrayToBigInt(bytes: Coll[Byte]): BigInt = this.builder.byteArrayToBigInt(bytes)
  def longToByteArray(l: Long): Coll[Byte] = this.builder.longToByteArray(l)
  def byteArrayToLong(bytes: Coll[Byte]): Long = this.builder.byteArrayToLong(bytes)

  def proveDlog(g: GroupElement): SigmaProp = this.builder.proveDlog(g)
  def proveDHTuple(g: GroupElement, h: GroupElement, u: GroupElement, v: GroupElement): SigmaProp =
    this.builder.proveDHTuple(g, h, u, v)

  def groupGenerator: GroupElement = this.builder.groupGenerator

  def decodePoint(encoded: Coll[Byte]): GroupElement = this.builder.decodePoint(encoded)

  def substConstants[T](scriptBytes: Coll[Byte],
      positions: Coll[Int],
      newValues: Coll[T]): Coll[Byte] = this.builder.substConstants(scriptBytes, positions, newValues)
}

/** Runtime representation of SGlobal ErgoTree type.
  * The only instance of SGlobal type can be referenced as `Global` variable in ErgoScript.
  * It is represented as [[org.ergoplatform.Global]] node of ErgoTree, which evaluates to
  * the default singleton instance of this interface.
  *
  * CostingSigmaDslBuilder object serves as the default singleton instance of Global
  * object, which implements global ErgoTree functions.
  *
  * @see SGlobal.WrappedType, CostingSigmaDslBuilder
  */
trait SigmaDslBuilder {

  /** Access to collection operations. */
  def Colls: CollBuilder

  /**
    * Logical threshold operation.
    * AtLeast has two inputs: integer `bound`` and a collection of `props` same as in anyZK/allZK.
    * @param bound  number of props which should be proven in order to satisfy verifier
    * @param props  a collection of sigma propositions of which at least the `bound` number should be proved.
    * @return THRESHOLD sigma protocol proposition wrapped in SigmaProp value.
    */
  def atLeast(bound: Int, props: Coll[SigmaProp]): SigmaProp

  /** @return true if all the elements in collection are true. */
  def allOf(conditions: Coll[Boolean]): Boolean

  /** Returns a sigma proposition which is proven when ALL the propositions in the `conditions` are proven.
    * @param conditions a collection of propositions
    * @return AND sigma protocol proposition
    */
  def allZK(conditions: Coll[SigmaProp]): SigmaProp

  /** Returns true if at least one element in the `conditions` is true, otherwise false. */
  def anyOf(conditions: Coll[Boolean]): Boolean

  /** Returns a sigma proposition which is proven when at least one of the propositions in the `conditions` is proven.
    * @param conditions a collection of propositions
    * @return OR sigma protocol proposition
    */
  def anyZK(conditions: Coll[SigmaProp]): SigmaProp

  /** Similar to `allOf`, but performing logical XOR operation between all conditions. */
  def xorOf(conditions: Coll[Boolean]): Boolean

  /** Creates trivial sigma proposition with the given underlying Boolean value.
    * @param b boolean value to be wrapped into SigmaProp
    * @return sigma proposition with can be combined with other SigmaProp values
    */
  def sigmaProp(b: Boolean): SigmaProp

  /** Calculate Blake2b256 hash from the input `bytes`. */
  def blake2b256(bytes: Coll[Byte]): Coll[Byte]

  /** Calculate Sha256 hash from the input `bytes`.*/
  def sha256(bytes: Coll[Byte]): Coll[Byte]

  /** Convert big-endian `bytes` representation (Coll[Byte]) to the corresponding BigInt value.
    * @param bytes collection of bytes in big-endian format
    */
  def byteArrayToBigInt(bytes: Coll[Byte]): BigInt

  /** Converts Long value `l` to the big-endian bytes representation. */
  def longToByteArray(l: Long): Coll[Byte]

  /** Convert big-endian `bytes` representation (Coll[Byte]) to the corresponding Long value. */
  def byteArrayToLong(bytes: Coll[Byte]): Long

  /** Creates a new SigmaProp value representing public key of the discrete logarithm
    * signature protocol.
    * @param  g  an element of the elliptic curve group which serves as the public key
    */
  def proveDlog(g: GroupElement): SigmaProp

  /** Creates a new SigmaProp value representing sigma proposition of the Diffie Hellman
    * signature protocol. Common input: (g,h,u,v)
    */
  def proveDHTuple(g: GroupElement, h: GroupElement, u: GroupElement, v: GroupElement): SigmaProp

  /** The generator g of the group is an element of the group such that, when written
    * multiplicative form, every element of the group is a power of g.
    * @return the generator of this Dlog group
    */
  def groupGenerator: GroupElement

  /**
    * Transforms serialized bytes of ErgoTree with segregated constants by replacing constants
    * at given positions with new values. This operation allow to use serialized scripts as
    * pre-defined templates.
    * The typical usage is "check that output box have proposition equal to given script bytes,
    * where minerPk (constants(0)) is replaced with currentMinerPk".
    * Each constant in original scriptBytes have SType serialized before actual data (see ConstantSerializer).
    * During substitution each value from newValues is checked to be an instance of the corresponding type.
    * This means, the constants during substitution cannot change their types.
    *
    * @param scriptBytes serialized ErgoTree with ConstantSegregationFlag set to 1.
    * @param positions zero based indexes in ErgoTree.constants array which should be replaced with new values
    * @param newValues new values to be injected into the corresponding positions in ErgoTree.constants array
    * @return original scriptBytes array where only specified constants are replaced and all other bytes remain exactly the same
    */
  def substConstants[T](scriptBytes: Coll[Byte], positions: Coll[Int], newValues: Coll[T]): Coll[Byte]

  /** Decodes the given bytes to the corresponding GroupElement using default serialization.
    * @param encoded serialized bytes of some GroupElement value
    * @see GroupElementSerializer
    */
  def decodePoint(encoded: Coll[Byte]): GroupElement

  /** Create DSL big integer from existing `java.math.BigInteger`*/
  def BigInt(n: BigInteger): BigInt

  /** Extract `java.math.BigInteger` from DSL's `BigInt` type*/
  def toBigInteger(n: BigInt): BigInteger

  /** Construct a new authenticated dictionary with given parameters and tree root digest. */
  def avlTree(operationFlags: Byte, digest: Coll[Byte], keyLength: Int, valueLengthOpt: Option[Int]): AvlTree

  /** Serializes the given `value` into bytes using the default serialization format. */
  def serialize[T](value: T)(implicit cT: RType[T]): Coll[Byte]

  /** Returns a byte-wise XOR of the two collections of bytes. */
  def xor(l: Coll[Byte], r: Coll[Byte]): Coll[Byte]

  /** Returns a number decoded from provided big-endian bytes array. */
  def fromBigEndianBytes[T](bytes: Coll[Byte])(implicit cT: RType[T]): T
}

