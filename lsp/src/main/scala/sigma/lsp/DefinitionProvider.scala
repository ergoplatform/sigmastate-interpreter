package sigma.lsp

import org.eclipse.lsp4j._

/**
 * Provides "Go to Definition" functionality for ErgoScript.
 * 
 * Finds the definition location of variables and functions
 * within the current document.
 */
object DefinitionProvider {

  /**
   * Find the definition of a symbol in the document.
   * 
   * @param content The document content
   * @param word The symbol to find
   * @param uri The document URI
   * @return List of Location objects pointing to definitions
   */
  def getDefinition(content: String, word: String, uri: String): List[Location] = {
    if (word.isEmpty) return List.empty
    
    // Don't look for definitions of built-in globals
    if (isBuiltIn(word)) return List.empty
    
    val lines = content.split("\n", -1)
    
    // Look for different definition patterns
    val valDefinitions = findValDefinitions(lines, word, uri)
    val defDefinitions = findDefDefinitions(lines, word, uri)
    val paramDefinitions = findParamDefinitions(lines, word, uri)
    
    (valDefinitions ++ defDefinitions ++ paramDefinitions).distinct
  }

  /**
   * Check if the word is a built-in symbol.
   */
  private def isBuiltIn(word: String): Boolean = {
    val builtIns = Set(
      // Global variables
      "SELF", "INPUTS", "OUTPUTS", "HEIGHT", "CONTEXT",
      "LastBlockUtxoRootHash", "minerPubKey",
      // Global functions
      "sigmaProp", "atLeast", "blake2b256", "sha256",
      "byteArrayToBigInt", "longToByteArray", "proveDlog",
      "proveDHTuple", "decodePoint", "allOf", "anyOf",
      "xor", "min", "max", "PK", "fromBase16", "fromBase64",
      "getVar",
      // Keywords
      "val", "def", "if", "else", "true", "false", "None", "Some",
      // Types
      "Boolean", "Byte", "Short", "Int", "Long", "BigInt",
      "SigmaProp", "GroupElement", "Box", "Coll", "Option",
      "AvlTree", "Header", "PreHeader", "Context"
    )
    builtIns.contains(word)
  }

  /**
   * Find `val name = ...` definitions.
   */
  private def findValDefinitions(lines: Array[String], word: String, uri: String): List[Location] = {
    // Pattern: val <word> = ... or val <word>: Type = ...
    val pattern = s"""\\bval\\s+($word)\\s*[:=]""".r
    
    lines.zipWithIndex.flatMap { case (line, lineNum) =>
      pattern.findFirstMatchIn(line).map { m =>
        new Location(uri, new Range(
          new Position(lineNum, m.start(1)),
          new Position(lineNum, m.end(1))
        ))
      }
    }.toList
  }

  /**
   * Find `def name(...) = ...` definitions.
   */
  private def findDefDefinitions(lines: Array[String], word: String, uri: String): List[Location] = {
    // Pattern: def <word>( or def <word>[
    val pattern = s"""\\bdef\\s+($word)\\s*[\\[(]""".r
    
    lines.zipWithIndex.flatMap { case (line, lineNum) =>
      pattern.findFirstMatchIn(line).map { m =>
        new Location(uri, new Range(
          new Position(lineNum, m.start(1)),
          new Position(lineNum, m.end(1))
        ))
      }
    }.toList
  }

  /**
   * Find function parameter definitions.
   */
  private def findParamDefinitions(lines: Array[String], word: String, uri: String): List[Location] = {
    // Pattern: (name: Type) or { (name: Type) =>
    val paramPattern = s"""[(,{]\\s*($word)\\s*:""".r
    
    lines.zipWithIndex.flatMap { case (line, lineNum) =>
      paramPattern.findAllMatchIn(line).map { m =>
        new Location(uri, new Range(
          new Position(lineNum, m.start(1)),
          new Position(lineNum, m.end(1))
        ))
      }.toList
    }.toList
  }
}
