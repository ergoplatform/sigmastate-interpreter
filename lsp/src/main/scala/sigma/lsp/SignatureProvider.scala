package sigma.lsp

import org.eclipse.lsp4j._
import scala.jdk.CollectionConverters._

/**
 * Provides signature help (parameter hints) for ErgoScript functions.
 * 
 * When the user types a function call like `atLeast(`, this provider
 * shows the function signature and highlights the current parameter.
 */
object SignatureProvider {

  /**
   * Get signature help for the current position.
   * 
   * @param content Document content
   * @param line Current line (0-based)
   * @param character Current column (0-based)
   * @return Optional SignatureHelp
   */
  def getSignatureHelp(content: String, line: Int, character: Int): Option[SignatureHelp] = {
    val lines = content.split("\n", -1)
    if (line >= lines.length) return None
    
    val currentLine = lines(line)
    val beforeCursor = currentLine.substring(0, math.min(character, currentLine.length))
    
    // Find function name before the opening parenthesis
    findFunctionCall(beforeCursor).flatMap { case (funcName, activeParam) =>
      signatures.get(funcName).map { info =>
        createSignatureHelp(info, activeParam)
      }
    }
  }

  /**
   * Find the function being called and the active parameter index.
   */
  private def findFunctionCall(beforeCursor: String): Option[(String, Int)] = {
    // Find the last unclosed parenthesis
    var parenDepth = 0
    var funcStart = -1
    var commaCount = 0
    
    var i = beforeCursor.length - 1
    while (i >= 0 && funcStart < 0) {
      beforeCursor(i) match {
        case ')' | ']' | '}' => parenDepth += 1
        case '(' =>
          if (parenDepth == 0) {
            // Found the opening paren, now find the function name
            val beforeParen = beforeCursor.substring(0, i).trim
            val funcPattern = """([a-zA-Z_][a-zA-Z0-9_]*)$""".r
            funcPattern.findFirstIn(beforeParen).foreach { name =>
              funcStart = i
            }
          } else {
            parenDepth -= 1
          }
        case '[' | '{' => 
          if (parenDepth > 0) parenDepth -= 1
        case ',' => 
          if (parenDepth == 0) commaCount += 1
        case _ =>
      }
      i -= 1
    }
    
    if (funcStart >= 0) {
      val beforeParen = beforeCursor.substring(0, funcStart).trim
      val funcPattern = """([a-zA-Z_][a-zA-Z0-9_]*)$""".r
      funcPattern.findFirstIn(beforeParen).map { name =>
        (name, commaCount)
      }
    } else {
      None
    }
  }

  /**
   * Create SignatureHelp from SignatureInfo.
   */
  private def createSignatureHelp(info: SignatureInfo, activeParam: Int): SignatureHelp = {
    val sigInfo = new SignatureInformation(info.label)
    sigInfo.setDocumentation(new MarkupContent(MarkupKind.MARKDOWN, info.documentation))
    sigInfo.setParameters(
      info.parameters.map { p =>
        val paramInfo = new ParameterInformation(p.label)
        paramInfo.setDocumentation(new MarkupContent(MarkupKind.MARKDOWN, p.documentation))
        paramInfo
      }.asJava
    )
    
    val help = new SignatureHelp()
    help.setSignatures(java.util.Arrays.asList(sigInfo))
    help.setActiveSignature(0)
    help.setActiveParameter(math.min(activeParam, info.parameters.size - 1))
    help
  }

  // ============ Signature Definitions ============
  
  case class SignatureInfo(
    label: String,
    documentation: String,
    parameters: List[ParameterInfo]
  )
  
  case class ParameterInfo(label: String, documentation: String)

  private val signatures: Map[String, SignatureInfo] = Map(
    
    "sigmaProp" -> SignatureInfo(
      "sigmaProp(condition: Boolean): SigmaProp",
      "Converts a Boolean condition to a SigmaProp spending condition.",
      List(
        ParameterInfo("condition: Boolean", "The Boolean condition that must be true to spend the box.")
      )
    ),
    
    "atLeast" -> SignatureInfo(
      "atLeast(k: Int, props: Coll[SigmaProp]): SigmaProp",
      "Threshold signature requiring at least `k` of `n` signatures.",
      List(
        ParameterInfo("k: Int", "Minimum number of required signatures."),
        ParameterInfo("props: Coll[SigmaProp]", "Collection of public keys / propositions.")
      )
    ),
    
    "blake2b256" -> SignatureInfo(
      "blake2b256(input: Coll[Byte]): Coll[Byte]",
      "Computes Blake2b-256 hash. Returns 32 bytes.",
      List(
        ParameterInfo("input: Coll[Byte]", "Bytes to hash.")
      )
    ),
    
    "sha256" -> SignatureInfo(
      "sha256(input: Coll[Byte]): Coll[Byte]",
      "Computes SHA-256 hash. Returns 32 bytes.",
      List(
        ParameterInfo("input: Coll[Byte]", "Bytes to hash.")
      )
    ),
    
    "byteArrayToBigInt" -> SignatureInfo(
      "byteArrayToBigInt(bytes: Coll[Byte]): BigInt",
      "Converts byte array to BigInt (signed, big-endian).",
      List(
        ParameterInfo("bytes: Coll[Byte]", "Bytes to convert. Interpreted as signed big-endian.")
      )
    ),
    
    "longToByteArray" -> SignatureInfo(
      "longToByteArray(value: Long): Coll[Byte]",
      "Converts Long to 8-byte big-endian array.",
      List(
        ParameterInfo("value: Long", "Long value to convert.")
      )
    ),
    
    "proveDlog" -> SignatureInfo(
      "proveDlog(pk: GroupElement): SigmaProp",
      "Creates Schnorr signature proposition.",
      List(
        ParameterInfo("pk: GroupElement", "Public key (group element) to prove knowledge of.")
      )
    ),
    
    "proveDHTuple" -> SignatureInfo(
      "proveDHTuple(g: GroupElement, h: GroupElement, u: GroupElement, v: GroupElement): SigmaProp",
      "Creates Diffie-Hellman tuple proposition. Proves knowledge of x where u=g^x and v=h^x.",
      List(
        ParameterInfo("g: GroupElement", "First generator."),
        ParameterInfo("h: GroupElement", "Second generator."),
        ParameterInfo("u: GroupElement", "g^x where x is the secret."),
        ParameterInfo("v: GroupElement", "h^x where x is the secret.")
      )
    ),
    
    "decodePoint" -> SignatureInfo(
      "decodePoint(bytes: Coll[Byte]): GroupElement",
      "Decodes 33-byte compressed elliptic curve point.",
      List(
        ParameterInfo("bytes: Coll[Byte]", "33-byte compressed point encoding.")
      )
    ),
    
    "allOf" -> SignatureInfo(
      "allOf(conditions: Coll[Boolean]): Boolean",
      "Logical AND of all elements. Returns true if all are true.",
      List(
        ParameterInfo("conditions: Coll[Boolean]", "Collection of Boolean conditions.")
      )
    ),
    
    "anyOf" -> SignatureInfo(
      "anyOf(conditions: Coll[Boolean]): Boolean",
      "Logical OR of all elements. Returns true if any is true.",
      List(
        ParameterInfo("conditions: Coll[Boolean]", "Collection of Boolean conditions.")
      )
    ),
    
    "xor" -> SignatureInfo(
      "xor(left: Coll[Byte], right: Coll[Byte]): Coll[Byte]",
      "Bitwise XOR of two byte arrays of equal length.",
      List(
        ParameterInfo("left: Coll[Byte]", "First byte array."),
        ParameterInfo("right: Coll[Byte]", "Second byte array (same length).")
      )
    ),
    
    "min" -> SignatureInfo(
      "min[T](a: T, b: T): T",
      "Returns the smaller of two values.",
      List(
        ParameterInfo("a: T", "First value."),
        ParameterInfo("b: T", "Second value.")
      )
    ),
    
    "max" -> SignatureInfo(
      "max[T](a: T, b: T): T",
      "Returns the larger of two values.",
      List(
        ParameterInfo("a: T", "First value."),
        ParameterInfo("b: T", "Second value.")
      )
    ),
    
    "PK" -> SignatureInfo(
      "PK(address: String): SigmaProp",
      "Creates SigmaProp from an Ergo address string.",
      List(
        ParameterInfo("address: String", "Ergo address (starts with 9 for mainnet, 3 for testnet).")
      )
    ),
    
    "fromBase16" -> SignatureInfo(
      "fromBase16(hex: String): Coll[Byte]",
      "Decodes hexadecimal string to bytes.",
      List(
        ParameterInfo("hex: String", "Hexadecimal string (e.g., \"deadbeef\").")
      )
    ),
    
    "fromBase64" -> SignatureInfo(
      "fromBase64(base64: String): Coll[Byte]",
      "Decodes Base64 string to bytes.",
      List(
        ParameterInfo("base64: String", "Base64-encoded string.")
      )
    ),
    
    "getVar" -> SignatureInfo(
      "getVar[T](id: Byte): Option[T]",
      "Gets context extension variable by ID.",
      List(
        ParameterInfo("id: Byte", "Variable ID (0-127).")
      )
    ),
    
    // Collection methods
    "map" -> SignatureInfo(
      "coll.map[B](f: A => B): Coll[B]",
      "Transforms each element using the function.",
      List(
        ParameterInfo("f: A => B", "Transformation function.")
      )
    ),
    
    "filter" -> SignatureInfo(
      "coll.filter(p: A => Boolean): Coll[A]",
      "Keeps only elements matching the predicate.",
      List(
        ParameterInfo("p: A => Boolean", "Predicate function.")
      )
    ),
    
    "fold" -> SignatureInfo(
      "coll.fold[B](zero: B, op: (B, A) => B): B",
      "Reduces collection to single value.",
      List(
        ParameterInfo("zero: B", "Initial accumulator value."),
        ParameterInfo("op: (B, A) => B", "Accumulator function.")
      )
    ),
    
    "exists" -> SignatureInfo(
      "coll.exists(p: A => Boolean): Boolean",
      "Returns true if any element matches.",
      List(
        ParameterInfo("p: A => Boolean", "Predicate function.")
      )
    ),
    
    "forall" -> SignatureInfo(
      "coll.forall(p: A => Boolean): Boolean",
      "Returns true if all elements match.",
      List(
        ParameterInfo("p: A => Boolean", "Predicate function.")
      )
    ),
    
    "slice" -> SignatureInfo(
      "coll.slice(from: Int, until: Int): Coll[A]",
      "Gets sub-collection from index `from` (inclusive) to `until` (exclusive).",
      List(
        ParameterInfo("from: Int", "Start index (inclusive)."),
        ParameterInfo("until: Int", "End index (exclusive).")
      )
    ),
    
    "append" -> SignatureInfo(
      "coll.append(other: Coll[A]): Coll[A]",
      "Concatenates two collections.",
      List(
        ParameterInfo("other: Coll[A]", "Collection to append.")
      )
    ),
    
    "getOrElse" -> SignatureInfo(
      "coll.getOrElse(index: Int, default: A): A",
      "Gets element at index or returns default if out of bounds.",
      List(
        ParameterInfo("index: Int", "Element index."),
        ParameterInfo("default: A", "Default value if index out of bounds.")
      )
    ),
    
    "indexOf" -> SignatureInfo(
      "coll.indexOf(elem: A, from: Int): Int",
      "Finds index of element starting from given index. Returns -1 if not found.",
      List(
        ParameterInfo("elem: A", "Element to find."),
        ParameterInfo("from: Int", "Starting index for search.")
      )
    ),
    
    "zip" -> SignatureInfo(
      "coll.zip[B](other: Coll[B]): Coll[(A, B)]",
      "Pairs elements with corresponding elements from another collection.",
      List(
        ParameterInfo("other: Coll[B]", "Collection to zip with.")
      )
    ),
    
    "flatMap" -> SignatureInfo(
      "coll.flatMap[B](f: A => Coll[B]): Coll[B]",
      "Maps each element to a collection and flattens the result.",
      List(
        ParameterInfo("f: A => Coll[B]", "Function returning collection.")
      )
    ),
    
    "Coll" -> SignatureInfo(
      "Coll[T](items: T*): Coll[T]",
      "Creates a new collection with the given elements.",
      List(
        ParameterInfo("items: T*", "Elements to include in the collection.")
      )
    ),
    
    "Some" -> SignatureInfo(
      "Some[T](value: T): Option[T]",
      "Creates an Option containing the given value.",
      List(
        ParameterInfo("value: T", "Value to wrap in Some.")
      )
    )
  )
}
