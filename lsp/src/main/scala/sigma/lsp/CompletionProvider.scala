package sigma.lsp

import org.eclipse.lsp4j._

/**
 * Provides code completion (autocomplete) for ErgoScript.
 * 
 * Supports:
 * - Global variables (SELF, INPUTS, OUTPUTS, HEIGHT, etc.)
 * - Global functions (sigmaProp, atLeast, blake2b256, etc.)
 * - Box methods (value, tokens, propositionBytes, R4, etc.)
 * - Collection methods (map, filter, fold, exists, etc.)
 * - Option methods (get, getOrElse, isDefined, etc.)
 * - Keywords (val, def, if, else, etc.)
 * - Types (Boolean, Int, Long, Box, Coll, etc.)
 */
object CompletionProvider {

  /**
   * Get completions for the given context.
   * 
   * @param prefix The text before the cursor (e.g., "SELF." or "val x")
   * @param line Current line number (0-based)
   * @param character Current column number (0-based)
   * @return List of completion items
   */
  def getCompletions(prefix: String, line: Int, character: Int): List[CompletionItem] = {
    val lowerPrefix = prefix.toLowerCase

    // Determine what kind of completions to provide
    val items = if (prefix.endsWith(".")) {
      // Member access - determine type from the identifier before the dot
      val beforeDot = prefix.dropRight(1).trim.toUpperCase
      getMemberCompletions(beforeDot)
    } else if (prefix.contains(".")) {
      // Typing after a dot - filter member completions
      val parts = prefix.split("\\.", 2)
      val objName = parts(0).toUpperCase
      val partialMember = parts(1).toLowerCase
      getMemberCompletions(objName).filter(_.getLabel.toLowerCase.startsWith(partialMember))
    } else {
      // Top-level completions
      getTopLevelCompletions(lowerPrefix)
    }

    items
  }

  /**
   * Get member completions for an object/type.
   */
  private def getMemberCompletions(objName: String): List[CompletionItem] = {
    objName match {
      case "SELF" => boxMethods
      case s if s.startsWith("INPUTS") || s.startsWith("OUTPUTS") => 
        if (s.contains("(")) boxMethods else collectionMethods ++ boxMethods
      case "CONTEXT" => contextMethods
      case s if s.startsWith("R") && s.length <= 3 => optionMethods
      case s if s.contains("TOKENS") => collectionMethods
      case s if s.endsWith("PROP") => sigmaPropMethods
      case _ => 
        // Could be any object - show common methods
        boxMethods ++ collectionMethods ++ optionMethods ++ sigmaPropMethods
    }
  }

  /**
   * Get top-level completions, optionally filtered by prefix.
   */
  private def getTopLevelCompletions(prefix: String): List[CompletionItem] = {
    val all = globalVariables ++ globalFunctions ++ keywords ++ types
    if (prefix.isEmpty) all
    else all.filter(_.getLabel.toLowerCase.startsWith(prefix))
  }

  // ============ Global Variables ============
  private val globalVariables: List[CompletionItem] = List(
    completion("SELF", "Box", "Current input box being spent", CompletionItemKind.Variable, 
      "The input box that is currently being evaluated for spending."),
    completion("INPUTS", "Coll[Box]", "All input boxes in transaction", CompletionItemKind.Variable,
      "Collection of all input boxes being spent in this transaction."),
    completion("OUTPUTS", "Coll[Box]", "All output boxes in transaction", CompletionItemKind.Variable,
      "Collection of all output boxes created by this transaction."),
    completion("HEIGHT", "Int", "Current blockchain height", CompletionItemKind.Variable,
      "The height of the block in which this transaction is included."),
    completion("CONTEXT", "Context", "Transaction context", CompletionItemKind.Variable,
      "Full transaction context including inputs, outputs, headers, and more."),
    completion("LastBlockUtxoRootHash", "AvlTree", "UTXO set digest of last block", CompletionItemKind.Variable,
      "AVL+ tree digest of the UTXO set after the previous block."),
    completion("minerPubKey", "Coll[Byte]", "Miner's public key", CompletionItemKind.Variable,
      "Public key of the miner who mined this block."),
  )

  // ============ Global Functions ============
  private val globalFunctions: List[CompletionItem] = List(
    completionWithSnippet("sigmaProp", "(Boolean) => SigmaProp", "Convert Boolean to SigmaProp", 
      CompletionItemKind.Function, "sigmaProp($1)", 
      "Converts a Boolean condition to a SigmaProp (spending condition)."),
    completionWithSnippet("atLeast", "(Int, Coll[SigmaProp]) => SigmaProp", "Threshold signature (k-of-n)", 
      CompletionItemKind.Function, "atLeast($1, Coll($2))",
      "Requires at least k of n signatures. Used for multi-signature wallets."),
    completionWithSnippet("blake2b256", "(Coll[Byte]) => Coll[Byte]", "Blake2b-256 hash", 
      CompletionItemKind.Function, "blake2b256($1)",
      "Computes the Blake2b-256 cryptographic hash of the input bytes."),
    completionWithSnippet("sha256", "(Coll[Byte]) => Coll[Byte]", "SHA-256 hash", 
      CompletionItemKind.Function, "sha256($1)",
      "Computes the SHA-256 cryptographic hash of the input bytes."),
    completionWithSnippet("byteArrayToBigInt", "(Coll[Byte]) => BigInt", "Convert bytes to BigInt", 
      CompletionItemKind.Function, "byteArrayToBigInt($1)",
      "Converts a byte array to a BigInt. The bytes are interpreted as a signed big-endian integer."),
    completionWithSnippet("longToByteArray", "(Long) => Coll[Byte]", "Convert Long to bytes", 
      CompletionItemKind.Function, "longToByteArray($1)",
      "Converts a Long value to an 8-byte big-endian byte array."),
    completionWithSnippet("proveDlog", "(GroupElement) => SigmaProp", "Prove discrete log knowledge", 
      CompletionItemKind.Function, "proveDlog($1)",
      "Creates a proposition that can be proven by knowing the discrete logarithm (private key)."),
    completionWithSnippet("proveDHTuple", "(GroupElement, GroupElement, GroupElement, GroupElement) => SigmaProp", 
      "Prove Diffie-Hellman tuple", CompletionItemKind.Function, "proveDHTuple($1, $2, $3, $4)",
      "Creates a proposition provable by knowing the exponent x in g^x = u and h^x = v."),
    completionWithSnippet("decodePoint", "(Coll[Byte]) => GroupElement", "Decode elliptic curve point", 
      CompletionItemKind.Function, "decodePoint($1)",
      "Decodes a 33-byte compressed elliptic curve point."),
    completionWithSnippet("allOf", "(Coll[Boolean]) => Boolean", "Logical AND of all elements", 
      CompletionItemKind.Function, "allOf($1)",
      "Returns true if all elements in the collection are true."),
    completionWithSnippet("anyOf", "(Coll[Boolean]) => Boolean", "Logical OR of all elements", 
      CompletionItemKind.Function, "anyOf($1)",
      "Returns true if any element in the collection is true."),
    completionWithSnippet("xor", "(Coll[Byte], Coll[Byte]) => Coll[Byte]", "XOR of two byte arrays", 
      CompletionItemKind.Function, "xor($1, $2)",
      "Performs bitwise XOR of two byte arrays of equal length."),
    completionWithSnippet("min", "(T, T) => T", "Minimum of two values", 
      CompletionItemKind.Function, "min($1, $2)",
      "Returns the smaller of two values."),
    completionWithSnippet("max", "(T, T) => T", "Maximum of two values", 
      CompletionItemKind.Function, "max($1, $2)",
      "Returns the larger of two values."),
    completionWithSnippet("PK", "(String) => SigmaProp", "Create SigmaProp from address", 
      CompletionItemKind.Function, "PK(\"$1\")",
      "Creates a SigmaProp from an Ergo address string."),
    completionWithSnippet("fromBase16", "(String) => Coll[Byte]", "Decode hex string", 
      CompletionItemKind.Function, "fromBase16(\"$1\")",
      "Decodes a hexadecimal string to bytes."),
    completionWithSnippet("fromBase64", "(String) => Coll[Byte]", "Decode base64 string", 
      CompletionItemKind.Function, "fromBase64(\"$1\")",
      "Decodes a Base64 string to bytes."),
    completionWithSnippet("getVar", "(Byte) => Option[T]", "Get context extension variable", 
      CompletionItemKind.Function, "getVar[$1]($2)",
      "Gets a variable from the context extension by its ID."),
  )

  // ============ Box Methods ============
  private val boxMethods: List[CompletionItem] = List(
    completion("value", "Long", "Monetary value in nanoERG", CompletionItemKind.Property,
      "The monetary value stored in the box, in nanoERG (1 ERG = 10^9 nanoERG)."),
    completion("propositionBytes", "Coll[Byte]", "Serialized spending condition (ErgoTree)", CompletionItemKind.Property,
      "The serialized ErgoTree that defines the spending condition for this box."),
    completion("bytes", "Coll[Byte]", "Serialized box bytes", CompletionItemKind.Property,
      "The full serialized bytes of the box."),
    completion("bytesWithoutRef", "Coll[Byte]", "Box bytes without box reference", CompletionItemKind.Property,
      "Box bytes excluding the transaction reference."),
    completion("id", "Coll[Byte]", "Unique box identifier", CompletionItemKind.Property,
      "The unique 32-byte identifier of the box (Blake2b256 hash of box bytes)."),
    completion("creationInfo", "(Int, Coll[Byte])", "Block height and tx id of creation", CompletionItemKind.Property,
      "A tuple containing the block height when the box was created and the transaction ID."),
    completion("tokens", "Coll[(Coll[Byte], Long)]", "Tokens stored in box", CompletionItemKind.Property,
      "Collection of tokens stored in this box. Each token is a pair of (tokenId, amount)."),
    completion("R0", "Option[T]", "Register R0 (always value)", CompletionItemKind.Property,
      "Register R0, which always contains the box value."),
    completion("R1", "Option[T]", "Register R1 (always script)", CompletionItemKind.Property,
      "Register R1, which always contains the proposition bytes."),
    completion("R2", "Option[T]", "Register R2 (always tokens)", CompletionItemKind.Property,
      "Register R2, which always contains the tokens."),
    completion("R3", "Option[T]", "Register R3 (always creationInfo)", CompletionItemKind.Property,
      "Register R3, which always contains the creation info."),
    completion("R4", "Option[T]", "Register R4 (user-defined)", CompletionItemKind.Property,
      "User-defined register R4. Access with `.get` or `.getOrElse(default)`."),
    completion("R5", "Option[T]", "Register R5 (user-defined)", CompletionItemKind.Property,
      "User-defined register R5. Access with `.get` or `.getOrElse(default)`."),
    completion("R6", "Option[T]", "Register R6 (user-defined)", CompletionItemKind.Property,
      "User-defined register R6. Access with `.get` or `.getOrElse(default)`."),
    completion("R7", "Option[T]", "Register R7 (user-defined)", CompletionItemKind.Property,
      "User-defined register R7. Access with `.get` or `.getOrElse(default)`."),
    completion("R8", "Option[T]", "Register R8 (user-defined)", CompletionItemKind.Property,
      "User-defined register R8. Access with `.get` or `.getOrElse(default)`."),
    completion("R9", "Option[T]", "Register R9 (user-defined)", CompletionItemKind.Property,
      "User-defined register R9. Access with `.get` or `.getOrElse(default)`."),
  )

  // ============ Collection Methods ============
  private val collectionMethods: List[CompletionItem] = List(
    completion("size", "Int", "Number of elements", CompletionItemKind.Property,
      "The number of elements in the collection."),
    completionWithSnippet("apply", "(Int) => A", "Get element at index", CompletionItemKind.Method,
      "($1)", "Gets the element at the specified index. Syntax: coll(index)."),
    completionWithSnippet("getOrElse", "(Int, A) => A", "Get element or default", CompletionItemKind.Method,
      "getOrElse($1, $2)", "Gets element at index or returns default if out of bounds."),
    completionWithSnippet("map", "((A) => B) => Coll[B]", "Transform each element", CompletionItemKind.Method,
      "map({ (x: $1) => $2 })", "Transforms each element using the provided function."),
    completionWithSnippet("filter", "((A) => Boolean) => Coll[A]", "Keep elements matching predicate", 
      CompletionItemKind.Method, "filter({ (x: $1) => $2 })", 
      "Keeps only elements for which the predicate returns true."),
    completionWithSnippet("fold", "(B, (B, A) => B) => B", "Reduce collection to single value", 
      CompletionItemKind.Method, "fold($1, { (acc: $2, x: $3) => $4 })",
      "Reduces the collection to a single value using an accumulator function."),
    completionWithSnippet("exists", "((A) => Boolean) => Boolean", "Check if any element matches", 
      CompletionItemKind.Method, "exists({ (x: $1) => $2 })",
      "Returns true if any element satisfies the predicate."),
    completionWithSnippet("forall", "((A) => Boolean) => Boolean", "Check if all elements match", 
      CompletionItemKind.Method, "forall({ (x: $1) => $2 })",
      "Returns true if all elements satisfy the predicate."),
    completionWithSnippet("slice", "(Int, Int) => Coll[A]", "Get sub-collection", CompletionItemKind.Method,
      "slice($1, $2)", "Gets elements from start index (inclusive) to end index (exclusive)."),
    completionWithSnippet("append", "(Coll[A]) => Coll[A]", "Concatenate collections", CompletionItemKind.Method,
      "append($1)", "Concatenates this collection with another."),
    completionWithSnippet("indexOf", "(A, Int) => Int", "Find element index", CompletionItemKind.Method,
      "indexOf($1, $2)", "Finds the index of the first occurrence starting from the given index."),
    completionWithSnippet("zip", "(Coll[B]) => Coll[(A, B)]", "Pair elements with another collection", 
      CompletionItemKind.Method, "zip($1)",
      "Pairs each element with the corresponding element from another collection."),
    completionWithSnippet("flatMap", "((A) => Coll[B]) => Coll[B]", "Map and flatten", CompletionItemKind.Method,
      "flatMap({ (x: $1) => $2 })", "Maps each element to a collection and flattens the result."),
    completionWithSnippet("indices", "Coll[Int]", "Collection of indices", CompletionItemKind.Property,
      "indices", "Returns a collection of indices from 0 to size-1."),
    completionWithSnippet("patch", "(Int, Coll[A], Int) => Coll[A]", "Replace elements", CompletionItemKind.Method,
      "patch($1, $2, $3)", "Replaces elements starting at index with elements from another collection."),
    completionWithSnippet("updated", "(Int, A) => Coll[A]", "Update element at index", CompletionItemKind.Method,
      "updated($1, $2)", "Returns a new collection with the element at index replaced."),
    completionWithSnippet("updateMany", "(Coll[Int], Coll[A]) => Coll[A]", "Update multiple elements", 
      CompletionItemKind.Method, "updateMany($1, $2)",
      "Updates multiple elements at the specified indices."),
  )

  // ============ Option Methods ============
  private val optionMethods: List[CompletionItem] = List(
    completion("get", "T", "Get value (throws if None)", CompletionItemKind.Method,
      "Gets the value. Throws an exception if the Option is empty."),
    completionWithSnippet("getOrElse", "(T) => T", "Get value or default", CompletionItemKind.Method,
      "getOrElse($1)", "Gets the value or returns the default if empty."),
    completion("isDefined", "Boolean", "Check if value exists", CompletionItemKind.Property,
      "Returns true if the Option contains a value."),
    completion("isEmpty", "Boolean", "Check if no value", CompletionItemKind.Property,
      "Returns true if the Option is empty."),
    completionWithSnippet("map", "((T) => R) => Option[R]", "Transform value if present", CompletionItemKind.Method,
      "map({ (x: $1) => $2 })", "Applies a function to the value if present."),
    completionWithSnippet("filter", "((T) => Boolean) => Option[T]", "Keep value if matches", 
      CompletionItemKind.Method, "filter({ (x: $1) => $2 })",
      "Returns the Option if the predicate is true, otherwise empty."),
  )

  // ============ SigmaProp Methods ============
  private val sigmaPropMethods: List[CompletionItem] = List(
    completion("propBytes", "Coll[Byte]", "Serialized proposition", CompletionItemKind.Property,
      "The serialized bytes of this SigmaProp."),
    completion("isValid", "Boolean", "Check if proposition is satisfied", CompletionItemKind.Property,
      "Returns true if this SigmaProp is satisfied (for testing only)."),
  )

  // ============ Context Methods ============
  private val contextMethods: List[CompletionItem] = List(
    completion("dataInputs", "Coll[Box]", "Read-only input boxes", CompletionItemKind.Property,
      "Collection of read-only data input boxes."),
    completion("headers", "Coll[Header]", "Recent block headers", CompletionItemKind.Property,
      "Collection of the last 10 block headers."),
    completion("preHeader", "PreHeader", "Current block info", CompletionItemKind.Property,
      "Pre-header of the block being mined."),
    completion("INPUTS", "Coll[Box]", "All input boxes", CompletionItemKind.Property,
      "All input boxes in the transaction."),
    completion("OUTPUTS", "Coll[Box]", "All output boxes", CompletionItemKind.Property,
      "All output boxes in the transaction."),
    completion("HEIGHT", "Int", "Current blockchain height", CompletionItemKind.Property,
      "Height of the block in which this transaction is included."),
    completion("SELF", "Box", "Current input box", CompletionItemKind.Property,
      "The input box currently being evaluated."),
    completion("selfBoxIndex", "Int", "Index of SELF in INPUTS", CompletionItemKind.Property,
      "The index of SELF in the INPUTS collection."),
    completion("LastBlockUtxoRootHash", "AvlTree", "UTXO set digest", CompletionItemKind.Property,
      "AVL+ tree digest of the UTXO set."),
    completion("minerPubKey", "Coll[Byte]", "Miner public key", CompletionItemKind.Property,
      "Public key of the miner."),
    completionWithSnippet("getVar", "(Byte) => Option[T]", "Get context extension variable", CompletionItemKind.Method,
      "getVar[$1]($2)", "Gets a variable from the context extension."),
  )

  // ============ Keywords ============
  private val keywords: List[CompletionItem] = List(
    completionKeyword("val", "Immutable value declaration"),
    completionKeyword("def", "Function declaration"),
    completionKeyword("if", "Conditional expression"),
    completionKeyword("else", "Alternative branch"),
    completionKeyword("true", "Boolean true"),
    completionKeyword("false", "Boolean false"),
    completionKeyword("None", "Empty Option value"),
    completionKeyword("Some", "Option with value"),
  )

  // ============ Types ============
  private val types: List[CompletionItem] = List(
    completionType("Boolean", "True/false type"),
    completionType("Byte", "8-bit signed integer"),
    completionType("Short", "16-bit signed integer"),
    completionType("Int", "32-bit signed integer"),
    completionType("Long", "64-bit signed integer"),
    completionType("BigInt", "Arbitrary precision integer"),
    completionType("SigmaProp", "Cryptographic spending condition"),
    completionType("GroupElement", "Elliptic curve point"),
    completionType("Box", "UTXO box"),
    completionType("Coll", "Collection type"),
    completionType("Option", "Optional value"),
    completionType("AvlTree", "Authenticated AVL+ tree"),
    completionType("Header", "Block header"),
    completionType("PreHeader", "Pre-header of block being mined"),
    completionType("Context", "Transaction context"),
  )

  // ============ Helper Functions ============

  private def completion(
    label: String,
    detail: String,
    doc: String,
    kind: CompletionItemKind,
    fullDoc: String = ""
  ): CompletionItem = {
    val item = new CompletionItem(label)
    item.setKind(kind)
    item.setDetail(detail)
    val docContent = if (fullDoc.nonEmpty) fullDoc else doc
    item.setDocumentation(new MarkupContent(MarkupKind.MARKDOWN, docContent))
    item
  }

  private def completionWithSnippet(
    label: String,
    detail: String,
    doc: String,
    kind: CompletionItemKind,
    snippet: String,
    fullDoc: String = ""
  ): CompletionItem = {
    val item = completion(label, detail, doc, kind, fullDoc)
    item.setInsertText(snippet)
    item.setInsertTextFormat(InsertTextFormat.Snippet)
    item
  }

  private def completionKeyword(label: String, doc: String): CompletionItem = {
    val item = new CompletionItem(label)
    item.setKind(CompletionItemKind.Keyword)
    item.setDocumentation(new MarkupContent(MarkupKind.MARKDOWN, doc))
    item
  }

  private def completionType(label: String, doc: String): CompletionItem = {
    val item = new CompletionItem(label)
    item.setKind(CompletionItemKind.Class)
    item.setDocumentation(new MarkupContent(MarkupKind.MARKDOWN, doc))
    item
  }
}
