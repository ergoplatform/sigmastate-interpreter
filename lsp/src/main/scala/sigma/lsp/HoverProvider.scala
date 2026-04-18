package sigma.lsp

import org.eclipse.lsp4j._

/**
 * Provides hover documentation for ErgoScript symbols.
 * 
 * When the user hovers over a symbol (variable, function, keyword, type),
 * this provider returns comprehensive Markdown documentation.
 */
object HoverProvider {

  /**
   * Get hover documentation for a word.
   * 
   * @param word The word being hovered over
   * @return Optional Hover with documentation
   */
  def getHover(word: String): Option[Hover] = {
    documentation.get(word).map { md =>
      new Hover(new MarkupContent(MarkupKind.MARKDOWN, md))
    }
  }

  // ============ Documentation Map ============
  private val documentation: Map[String, String] = Map(
    
    // ============ Global Variables ============
    
    "SELF" -> """
      |## SELF
      |**Type:** `Box`
      |
      |The current input box being spent in this transaction.
      |
      |This is the box whose spending condition (ErgoTree) is currently being evaluated.
      |In multi-input transactions, each input is evaluated separately with its own SELF.
      |
      |### Properties
      |```ergoscript
      |SELF.value           // Long - value in nanoERG
      |SELF.tokens          // Coll[(Coll[Byte], Long)] - tokens
      |SELF.propositionBytes // Coll[Byte] - serialized script
      |SELF.id              // Coll[Byte] - unique 32-byte ID
      |SELF.R4[T]           // Option[T] - user register R4
      |```
      |
      |### Example
      |```ergoscript
      |{
      |  val myValue = SELF.value
      |  val myTokens = SELF.tokens
      |  val ownerPk = SELF.R4[SigmaProp].get
      |  
      |  // Only owner can spend, and must preserve value
      |  ownerPk && sigmaProp(OUTPUTS(0).value >= myValue)
      |}
      |```
    """.stripMargin,

    "INPUTS" -> """
      |## INPUTS
      |**Type:** `Coll[Box]`
      |
      |All input boxes being spent in this transaction.
      |
      |### Example
      |```ergoscript
      |{
      |  // Sum of all input values
      |  val totalInput = INPUTS.fold(0L, { (acc: Long, box: Box) => acc + box.value })
      |  
      |  // Get first input
      |  val firstBox = INPUTS(0)
      |  
      |  // Number of inputs
      |  val count = INPUTS.size
      |  
      |  // Check if all inputs have same script
      |  val allSameScript = INPUTS.forall({ (box: Box) => 
      |    box.propositionBytes == SELF.propositionBytes 
      |  })
      |  
      |  sigmaProp(totalInput > 0)
      |}
      |```
    """.stripMargin,

    "OUTPUTS" -> """
      |## OUTPUTS
      |**Type:** `Coll[Box]`
      |
      |All output boxes created by this transaction.
      |
      |### Example
      |```ergoscript
      |{
      |  // Sum of all output values
      |  val totalOutput = OUTPUTS.fold(0L, { (acc: Long, box: Box) => acc + box.value })
      |  
      |  // First output (typically recipient)
      |  val recipientBox = OUTPUTS(0)
      |  
      |  // Check output has same script as input (self-replication)
      |  val preserved = OUTPUTS(0).propositionBytes == SELF.propositionBytes
      |  
      |  // Verify exactly 2 outputs
      |  sigmaProp(OUTPUTS.size == 2)
      |}
      |```
    """.stripMargin,

    "HEIGHT" -> """
      |## HEIGHT
      |**Type:** `Int`
      |
      |Current blockchain height (block number).
      |
      |The height of the block in which this transaction will be included.
      |Useful for time-locked contracts.
      |
      |### Example
      |```ergoscript
      |{
      |  // Time-locked: can only spend after block 500000
      |  val unlockHeight = 500000
      |  sigmaProp(HEIGHT > unlockHeight)
      |}
      |
      |{
      |  // Deadline: must spend before block 600000
      |  val deadline = 600000
      |  sigmaProp(HEIGHT < deadline)
      |}
      |
      |{
      |  // Either owner can spend, or anyone after deadline
      |  val ownerPk = SELF.R4[SigmaProp].get
      |  val deadline = SELF.R5[Int].get
      |  
      |  ownerPk || sigmaProp(HEIGHT > deadline)
      |}
      |```
    """.stripMargin,

    "CONTEXT" -> """
      |## CONTEXT
      |**Type:** `Context`
      |
      |Full transaction execution context.
      |
      |Provides access to all transaction data including inputs, outputs,
      |data inputs, headers, and context extension variables.
      |
      |### Properties
      |```ergoscript
      |CONTEXT.dataInputs   // Coll[Box] - read-only input boxes
      |CONTEXT.headers      // Coll[Header] - last 10 block headers
      |CONTEXT.preHeader    // PreHeader - current block info
      |CONTEXT.INPUTS       // Coll[Box] - same as INPUTS
      |CONTEXT.OUTPUTS      // Coll[Box] - same as OUTPUTS
      |CONTEXT.HEIGHT       // Int - same as HEIGHT
      |CONTEXT.SELF         // Box - same as SELF
      |CONTEXT.selfBoxIndex // Int - index of SELF in INPUTS
      |```
      |
      |### Example
      |```ergoscript
      |{
      |  // Use data input for oracle price
      |  val oracleBox = CONTEXT.dataInputs(0)
      |  val price = oracleBox.R4[Long].get
      |  
      |  // Get context extension variable
      |  val secret = getVar[Coll[Byte]](0).get
      |  
      |  sigmaProp(SELF.value > price)
      |}
      |```
    """.stripMargin,

    // ============ Global Functions ============

    "sigmaProp" -> """
      |## sigmaProp
      |**Type:** `(Boolean) => SigmaProp`
      |
      |Converts a Boolean condition to a SigmaProp (spending condition).
      |
      |⚠️ **Important:** All ErgoScript contracts must return `SigmaProp`.
      |Use this function to convert Boolean logic to a spendable proposition.
      |
      |### Example
      |```ergoscript
      |{
      |  // Simple condition: value must be greater than 1 ERG
      |  sigmaProp(SELF.value > 1000000000L)
      |}
      |
      |{
      |  // Combined with signature requirement
      |  val ownerPk = SELF.R4[SigmaProp].get
      |  val heightOk = HEIGHT > 100
      |  
      |  sigmaProp(heightOk) && ownerPk
      |}
      |```
      |
      |### Notes
      |- Boolean conditions wrapped in `sigmaProp` become "trivial" propositions
      |- Can be combined with actual signatures using `&&` and `||`
      |- Returns a "true" proposition if condition is true, "false" if false
    """.stripMargin,

    "atLeast" -> """
      |## atLeast
      |**Type:** `(Int, Coll[SigmaProp]) => SigmaProp`
      |
      |Threshold signature: requires at least `k` of `n` signatures.
      |
      |Used for multi-signature (multisig) wallets and voting schemes.
      |
      |### Parameters
      |- `k: Int` - Minimum number of required signatures
      |- `props: Coll[SigmaProp]` - Collection of public keys / propositions
      |
      |### Example
      |```ergoscript
      |{
      |  // 2-of-3 multisig
      |  val pk1 = SELF.R4[SigmaProp].get
      |  val pk2 = SELF.R5[SigmaProp].get
      |  val pk3 = SELF.R6[SigmaProp].get
      |  
      |  atLeast(2, Coll(pk1, pk2, pk3))
      |}
      |
      |{
      |  // 3-of-5 voting
      |  val voters = SELF.R4[Coll[SigmaProp]].get
      |  atLeast(3, voters)
      |}
      |```
      |
      |### Notes
      |- More efficient than chaining `||` for threshold signatures
      |- If k > n, the proposition is always false
      |- If k <= 0, the proposition is always true
    """.stripMargin,

    "blake2b256" -> """
      |## blake2b256
      |**Type:** `(Coll[Byte]) => Coll[Byte]`
      |
      |Computes the Blake2b-256 cryptographic hash of input bytes.
      |
      |Blake2b is a fast and secure hash function. The output is always 32 bytes.
      |
      |### Example
      |```ergoscript
      |{
      |  // Hash preimage scheme
      |  val preimage = getVar[Coll[Byte]](0).get
      |  val expectedHash = SELF.R4[Coll[Byte]].get
      |  
      |  sigmaProp(blake2b256(preimage) == expectedHash)
      |}
      |
      |{
      |  // Hash the box proposition
      |  val scriptHash = blake2b256(SELF.propositionBytes)
      |}
      |```
    """.stripMargin,

    "sha256" -> """
      |## sha256
      |**Type:** `(Coll[Byte]) => Coll[Byte]`
      |
      |Computes the SHA-256 cryptographic hash of input bytes.
      |
      |Output is always 32 bytes. Compatible with Bitcoin's hash function.
      |
      |### Example
      |```ergoscript
      |{
      |  val message = SELF.R4[Coll[Byte]].get
      |  val hash = sha256(message)
      |  sigmaProp(hash(0) == 0.toByte)  // Hash must start with zero
      |}
      |```
    """.stripMargin,

    "proveDlog" -> """
      |## proveDlog
      |**Type:** `(GroupElement) => SigmaProp`
      |
      |Creates a Schnorr signature proposition.
      |
      |The proposition can be proven by providing a signature that demonstrates
      |knowledge of the discrete logarithm (private key) of the group element (public key).
      |
      |### Example
      |```ergoscript
      |{
      |  // Single signature requirement
      |  val ownerPubKey = SELF.R4[GroupElement].get
      |  proveDlog(ownerPubKey)
      |}
      |
      |{
      |  // 2-of-2 with group elements
      |  val pk1 = SELF.R4[GroupElement].get
      |  val pk2 = SELF.R5[GroupElement].get
      |  
      |  proveDlog(pk1) && proveDlog(pk2)
      |}
      |```
      |
      |### Notes
      |- This is the fundamental signature primitive in ErgoScript
      |- Based on Schnorr signatures over secp256k1 curve
      |- More efficient than using SigmaProp directly
    """.stripMargin,

    "proveDHTuple" -> """
      |## proveDHTuple
      |**Type:** `(GroupElement, GroupElement, GroupElement, GroupElement) => SigmaProp`
      |
      |Creates a Diffie-Hellman tuple proposition.
      |
      |Proves knowledge of `x` such that `u = g^x` and `v = h^x` without revealing `x`.
      |
      |### Parameters
      |- `g: GroupElement` - First generator
      |- `h: GroupElement` - Second generator
      |- `u: GroupElement` - g^x
      |- `v: GroupElement` - h^x
      |
      |### Example
      |```ergoscript
      |{
      |  val g = SELF.R4[GroupElement].get
      |  val h = SELF.R5[GroupElement].get
      |  val u = SELF.R6[GroupElement].get
      |  val v = SELF.R7[GroupElement].get
      |  
      |  proveDHTuple(g, h, u, v)
      |}
      |```
      |
      |### Notes
      |- Used for advanced cryptographic protocols
      |- Enables ring signatures and mixers
    """.stripMargin,

    // ============ Box Properties ============

    "value" -> """
      |## Box.value
      |**Type:** `Long`
      |
      |Monetary value stored in the box, in **nanoERG**.
      |
      |1 ERG = 1,000,000,000 nanoERG (10^9)
      |
      |### Example
      |```ergoscript
      |{
      |  val oneErg = 1000000000L
      |  val minValue = oneErg / 10  // 0.1 ERG
      |  
      |  // Box must have at least 1 ERG
      |  sigmaProp(SELF.value >= oneErg)
      |}
      |
      |{
      |  // Preserve value: output >= input
      |  sigmaProp(OUTPUTS(0).value >= SELF.value)
      |}
      |
      |{
      |  // Calculate total value
      |  val totalIn = INPUTS.fold(0L, { (acc: Long, box: Box) => acc + box.value })
      |  val totalOut = OUTPUTS.fold(0L, { (acc: Long, box: Box) => acc + box.value })
      |  
      |  sigmaProp(totalIn >= totalOut)  // No value creation
      |}
      |```
    """.stripMargin,

    "tokens" -> """
      |## Box.tokens
      |**Type:** `Coll[(Coll[Byte], Long)]`
      |
      |Collection of tokens stored in the box.
      |
      |Each token is a tuple of:
      |- Token ID: `Coll[Byte]` (32 bytes)
      |- Amount: `Long`
      |
      |### Example
      |```ergoscript
      |{
      |  // Check if box has any tokens
      |  val hasTokens = SELF.tokens.size > 0
      |  
      |  // Get first token
      |  val (tokenId, amount) = SELF.tokens(0)
      |  
      |  // Preserve tokens in output
      |  sigmaProp(OUTPUTS(0).tokens == SELF.tokens)
      |}
      |
      |{
      |  // Find specific token
      |  val targetTokenId = SELF.R4[Coll[Byte]].get
      |  val hasToken = SELF.tokens.exists({ (t: (Coll[Byte], Long)) => 
      |    t._1 == targetTokenId 
      |  })
      |  
      |  sigmaProp(hasToken)
      |}
      |```
    """.stripMargin,

    "propositionBytes" -> """
      |## Box.propositionBytes
      |**Type:** `Coll[Byte]`
      |
      |Serialized spending condition (ErgoTree) of the box.
      |
      |This is the compiled script that controls how the box can be spent.
      |
      |### Example
      |```ergoscript
      |{
      |  // Self-replication: output must have same script
      |  sigmaProp(OUTPUTS(0).propositionBytes == SELF.propositionBytes)
      |}
      |
      |{
      |  // All outputs must go to same script
      |  val sameScript = OUTPUTS.forall({ (box: Box) => 
      |    box.propositionBytes == SELF.propositionBytes 
      |  })
      |  
      |  sigmaProp(sameScript)
      |}
      |
      |{
      |  // Hash of the script for identification
      |  val scriptHash = blake2b256(SELF.propositionBytes)
      |}
      |```
    """.stripMargin,

    // ============ Collection Methods ============

    "map" -> """
      |## Coll.map
      |**Type:** `((A) => B) => Coll[B]`
      |
      |Transforms each element of the collection using a function.
      |
      |### Example
      |```ergoscript
      |{
      |  // Get values of all inputs
      |  val values: Coll[Long] = INPUTS.map({ (box: Box) => box.value })
      |  
      |  // Double all values
      |  val doubled = values.map({ (v: Long) => v * 2 })
      |  
      |  // Extract token IDs
      |  val tokenIds = SELF.tokens.map({ (t: (Coll[Byte], Long)) => t._1 })
      |  
      |  sigmaProp(values.size > 0)
      |}
      |```
    """.stripMargin,

    "filter" -> """
      |## Coll.filter
      |**Type:** `((A) => Boolean) => Coll[A]`
      |
      |Keeps only elements that satisfy the predicate.
      |
      |### Example
      |```ergoscript
      |{
      |  // Find boxes with value > 1 ERG
      |  val bigBoxes = INPUTS.filter({ (box: Box) => box.value > 1000000000L })
      |  
      |  // Find boxes with tokens
      |  val withTokens = INPUTS.filter({ (box: Box) => box.tokens.size > 0 })
      |  
      |  // Filter positive values
      |  val positives = values.filter({ (v: Long) => v > 0 })
      |  
      |  sigmaProp(bigBoxes.size > 0)
      |}
      |```
    """.stripMargin,

    "fold" -> """
      |## Coll.fold
      |**Type:** `(B, (B, A) => B) => B`
      |
      |Reduces collection to a single value using an accumulator function.
      |
      |### Parameters
      |- Initial value (zero/identity)
      |- Accumulator function: (accumulated, current) => new accumulated
      |
      |### Example
      |```ergoscript
      |{
      |  // Sum all input values
      |  val total: Long = INPUTS.fold(0L, { (acc: Long, box: Box) => acc + box.value })
      |  
      |  // Count boxes with tokens
      |  val count: Int = INPUTS.fold(0, { (acc: Int, box: Box) => 
      |    if (box.tokens.size > 0) acc + 1 else acc 
      |  })
      |  
      |  // Logical AND of conditions
      |  val allValid: Boolean = conditions.fold(true, { (acc: Boolean, c: Boolean) => 
      |    acc && c 
      |  })
      |  
      |  sigmaProp(total > 0)
      |}
      |```
    """.stripMargin,

    "exists" -> """
      |## Coll.exists
      |**Type:** `((A) => Boolean) => Boolean`
      |
      |Returns true if any element satisfies the predicate.
      |
      |Short-circuits: stops as soon as a match is found.
      |
      |### Example
      |```ergoscript
      |{
      |  // Check if any input has > 1 ERG
      |  val hasLargeBox = INPUTS.exists({ (box: Box) => box.value > 1000000000L })
      |  
      |  // Check if specific token exists
      |  val targetToken = SELF.R4[Coll[Byte]].get
      |  val hasToken = SELF.tokens.exists({ (t: (Coll[Byte], Long)) => 
      |    t._1 == targetToken 
      |  })
      |  
      |  sigmaProp(hasLargeBox)
      |}
      |```
    """.stripMargin,

    "forall" -> """
      |## Coll.forall
      |**Type:** `((A) => Boolean) => Boolean`
      |
      |Returns true if all elements satisfy the predicate.
      |
      |Short-circuits: stops as soon as a non-match is found.
      |
      |### Example
      |```ergoscript
      |{
      |  // All inputs must have value > 0
      |  val allPositive = INPUTS.forall({ (box: Box) => box.value > 0 })
      |  
      |  // All outputs go to same script
      |  val allSameScript = OUTPUTS.forall({ (box: Box) => 
      |    box.propositionBytes == SELF.propositionBytes 
      |  })
      |  
      |  sigmaProp(allPositive && allSameScript)
      |}
      |```
    """.stripMargin,

    // ============ Option Methods ============

    "get" -> """
      |## Option.get
      |**Type:** `T`
      |
      |Gets the value from the Option.
      |
      |⚠️ **Warning:** Throws an exception if the Option is empty (None).
      |Consider using `getOrElse` for safer access.
      |
      |### Example
      |```ergoscript
      |{
      |  // Get register value (assumes it exists)
      |  val ownerPk: SigmaProp = SELF.R4[SigmaProp].get
      |  
      |  // Get context variable
      |  val secret: Coll[Byte] = getVar[Coll[Byte]](0).get
      |  
      |  ownerPk
      |}
      |```
    """.stripMargin,

    "getOrElse" -> """
      |## Option.getOrElse
      |**Type:** `(T) => T`
      |
      |Gets the value or returns a default if empty.
      |
      |Safer than `.get` - won't throw if Option is empty.
      |
      |### Example
      |```ergoscript
      |{
      |  // Get deadline or default to 0 (always valid)
      |  val deadline: Int = SELF.R5[Int].getOrElse(0)
      |  
      |  // Get amount or default to 0
      |  val amount: Long = SELF.R6[Long].getOrElse(0L)
      |  
      |  sigmaProp(HEIGHT > deadline)
      |}
      |```
    """.stripMargin,

    // ============ Keywords ============

    "val" -> """
      |## val
      |**Keyword**
      |
      |Declares an immutable value binding.
      |
      |Values in ErgoScript cannot be reassigned after declaration.
      |
      |### Example
      |```ergoscript
      |{
      |  val myValue: Long = SELF.value
      |  val oneErg = 1000000000L
      |  val isLarge = myValue > oneErg
      |  
      |  sigmaProp(isLarge)
      |}
      |```
    """.stripMargin,

    "def" -> """
      |## def
      |**Keyword**
      |
      |Declares a function.
      |
      |### Example
      |```ergoscript
      |{
      |  def isLargeBox(box: Box): Boolean = box.value > 1000000000L
      |  
      |  def sumValues(boxes: Coll[Box]): Long = 
      |    boxes.fold(0L, { (acc: Long, box: Box) => acc + box.value })
      |  
      |  val total = sumValues(INPUTS)
      |  sigmaProp(total > 0)
      |}
      |```
    """.stripMargin,

    "if" -> """
      |## if
      |**Keyword**
      |
      |Conditional expression. In ErgoScript, `if` is an expression that returns a value.
      |
      |### Example
      |```ergoscript
      |{
      |  val amount = if (SELF.value > 1000000000L) 
      |    SELF.value 
      |  else 
      |    0L
      |  
      |  val condition = if (HEIGHT > 100) 
      |    sigmaProp(true) 
      |  else 
      |    SELF.R4[SigmaProp].get
      |  
      |  condition
      |}
      |```
    """.stripMargin,

    // ============ Types ============

    "Boolean" -> """
      |## Boolean
      |**Type**
      |
      |Boolean type with values `true` and `false`.
      |
      |### Operators
      |- `&&` - logical AND
      |- `||` - logical OR
      |- `!` - logical NOT
      |
      |### Example
      |```ergoscript
      |{
      |  val a: Boolean = true
      |  val b: Boolean = HEIGHT > 100
      |  val c = a && b
      |  val d = a || !b
      |  
      |  sigmaProp(c)
      |}
      |```
    """.stripMargin,

    "Long" -> """
      |## Long
      |**Type**
      |
      |64-bit signed integer.
      |
      |Used for monetary values (nanoERG) and token amounts.
      |
      |### Range
      |- Minimum: -9,223,372,036,854,775,808
      |- Maximum: 9,223,372,036,854,775,807
      |
      |### Example
      |```ergoscript
      |{
      |  val oneErg: Long = 1000000000L
      |  val value: Long = SELF.value
      |  val sum = value + oneErg
      |  val diff = value - 100L
      |  val product = value * 2L
      |  val quotient = value / 10L
      |  val remainder = value % 7L
      |  
      |  sigmaProp(value > 0L)
      |}
      |```
    """.stripMargin,

    "Int" -> """
      |## Int
      |**Type**
      |
      |32-bit signed integer.
      |
      |Used for indices, heights, and counts.
      |
      |### Range
      |- Minimum: -2,147,483,648
      |- Maximum: 2,147,483,647
      |
      |### Example
      |```ergoscript
      |{
      |  val height: Int = HEIGHT
      |  val count: Int = INPUTS.size
      |  val index: Int = 0
      |  
      |  sigmaProp(height > 100)
      |}
      |```
    """.stripMargin,

    "BigInt" -> """
      |## BigInt
      |**Type**
      |
      |Arbitrary precision signed integer.
      |
      |Used for cryptographic calculations and large numbers.
      |
      |### Example
      |```ergoscript
      |{
      |  val bytes = SELF.R4[Coll[Byte]].get
      |  val bigNum: BigInt = byteArrayToBigInt(bytes)
      |  
      |  sigmaProp(bigNum > 0)
      |}
      |```
    """.stripMargin,

    "SigmaProp" -> """
      |## SigmaProp
      |**Type**
      |
      |Cryptographic spending condition (sigma proposition).
      |
      |This is the return type of all ErgoScript contracts. Represents
      |a condition that must be satisfied to spend a box.
      |
      |### Creating SigmaProps
      |```ergoscript
      |sigmaProp(booleanCondition)  // From Boolean
      |proveDlog(groupElement)       // Schnorr signature
      |proveDHTuple(g, h, u, v)      // DH tuple
      |atLeast(k, props)             // Threshold
      |```
      |
      |### Operators
      |- `&&` - AND (both must be satisfied)
      |- `||` - OR (at least one must be satisfied)
      |
      |### Example
      |```ergoscript
      |{
      |  val ownerPk: SigmaProp = SELF.R4[SigmaProp].get
      |  val heightOk: SigmaProp = sigmaProp(HEIGHT > 100)
      |  
      |  ownerPk && heightOk  // Both signature and height condition
      |}
      |```
    """.stripMargin,

    "Box" -> """
      |## Box
      |**Type**
      |
      |UTXO (Unspent Transaction Output) box.
      |
      |A box contains ERG value, tokens, a spending condition (script),
      |and up to 10 registers for custom data.
      |
      |### Properties
      |```ergoscript
      |box.value           // Long - nanoERG amount
      |box.tokens          // Coll[(Coll[Byte], Long)] - tokens
      |box.propositionBytes // Coll[Byte] - spending script
      |box.bytes           // Coll[Byte] - full serialized box
      |box.id              // Coll[Byte] - unique identifier
      |box.creationInfo    // (Int, Coll[Byte]) - height and tx ID
      |box.R4[T] ... R9[T] // Option[T] - user registers
      |```
    """.stripMargin,

    "Coll" -> """
      |## Coll[T]
      |**Type**
      |
      |Immutable collection (array) of elements of type T.
      |
      |### Creating Collections
      |```ergoscript
      |Coll(1, 2, 3)           // Coll[Int]
      |Coll(pk1, pk2, pk3)     // Coll[SigmaProp]
      |```
      |
      |### Methods
      |```ergoscript
      |coll.size               // Int - number of elements
      |coll(index)             // T - element at index
      |coll.map(f)             // transform elements
      |coll.filter(p)          // keep matching elements
      |coll.fold(z, f)         // reduce to single value
      |coll.exists(p)          // any element matches?
      |coll.forall(p)          // all elements match?
      |coll.slice(from, until) // sub-collection
      |coll.append(other)      // concatenate
      |```
    """.stripMargin,

    "Option" -> """
      |## Option[T]
      |**Type**
      |
      |Optional value that may or may not be present.
      |
      |Used for register access and context variables.
      |
      |### Values
      |- `Some(value)` - contains a value
      |- `None` - empty
      |
      |### Methods
      |```ergoscript
      |opt.get              // T - get value (throws if empty)
      |opt.getOrElse(default) // T - get or default
      |opt.isDefined        // Boolean - has value?
      |opt.isEmpty          // Boolean - is empty?
      |opt.map(f)           // transform if present
      |opt.filter(p)        // keep if matches
      |```
      |
      |### Example
      |```ergoscript
      |{
      |  val maybeValue: Option[Long] = SELF.R4[Long]
      |  val value: Long = maybeValue.getOrElse(0L)
      |  
      |  sigmaProp(value > 0)
      |}
      |```
    """.stripMargin,

    "GroupElement" -> """
      |## GroupElement
      |**Type**
      |
      |Elliptic curve point on secp256k1.
      |
      |Used for public keys and cryptographic operations.
      |
      |### Operations
      |```ergoscript
      |ge.exp(bigInt)       // Exponentiation: ge^n
      |ge.multiply(other)   // Point multiplication
      |ge.negate            // Negation: -ge
      |ge.getEncoded        // Coll[Byte] - 33-byte encoding
      |```
      |
      |### Example
      |```ergoscript
      |{
      |  val pubKey: GroupElement = SELF.R4[GroupElement].get
      |  proveDlog(pubKey)
      |}
      |```
    """.stripMargin,

    "AvlTree" -> """
      |## AvlTree
      |**Type**
      |
      |Authenticated AVL+ tree for verifiable data structures.
      |
      |Used for proving membership/non-membership in large datasets
      |without including the full data on-chain.
      |
      |### Methods
      |```ergoscript
      |tree.digest          // Coll[Byte] - root hash
      |tree.enabledOperations // Byte - allowed operations
      |tree.keyLength       // Int - key size
      |tree.valueLengthOpt  // Option[Int] - value size
      |tree.contains(key, proof) // verify membership
      |tree.get(key, proof) // get value with proof
      |tree.insert(entries, proof) // insert with proof
      |tree.update(entries, proof) // update with proof
      |tree.remove(keys, proof) // remove with proof
      |```
    """.stripMargin
  )
}
