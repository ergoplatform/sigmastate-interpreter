package org.ergoplatform.sdk.utils

import sigma.ast.ErgoTree
import sigma.ast.ErgoTree.HeaderType
import sigma.util.Extensions.BooleanOps

/** SDK level utilities and helper methods to work with ErgoTrees. */
object ErgoTreeUtils {
  /** Prints description of the bits in the given ErgoTree header. */
  def explainTreeHeader(header: HeaderType): String = {
    // convert byte to hex
    val byteToHex = (b: Byte) => f"Ox${b & 0xff}%02x"
    val hasSize = ErgoTree.hasSize(header)
    s"""
      |Header: ${byteToHex(header)} (${header.toString})
      |Bit 0: ${header.toByte & 0x01} \\
      |Bit 1: ${(header.toByte & 0x02) >> 1}\t-- ErgoTree version ${ErgoTree.getVersion(header)}
      |Bit 2: ${(header.toByte & 0x04) >> 2} /
      |Bit 3: ${hasSize.toByte} \t-- size of the whole tree is serialized after the header byte
      |Bit 4: ${ErgoTree.isConstantSegregation(header).toByte} \t-- constant segregation is used for this ErgoTree
      |Bit 5: ${header.toByte & 0x20} \t-- reserved (should be 0)
      |Bit 6: ${header.toByte & 0x40} \t-- reserved (should be 0)
      |Bit 7: ${header.toByte & 0x80} \t-- header contains more than 1 byte (default == 0)
      |""".stripMargin
  }
}
