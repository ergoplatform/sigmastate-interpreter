package org.ergoplatform.sdk.utils

import sigma.serialization.{SigmaByteReader, SigmaByteWriter}
import java.nio.charset.StandardCharsets

object SerializationUtils {

  /**
   * Serialize the string as UTF_8.
   *
   * @param s the string to serialize
   * @param w the writer to which the serialized string will be appended
   */
  def serializeString(s: String, w: SigmaByteWriter): Unit = {
    val bytes = s.getBytes(StandardCharsets.UTF_8)
    w.putUInt(bytes.length)
    w.putBytes(bytes)
  }

  /**
   * Parse a string from the reader
   * @param r the reader from which the string will be parsed
   * @return the parsed string
   */
  def parseString(r: SigmaByteReader): String = {
    val length = r.getUInt().toInt
    new String(r.getBytes(length), StandardCharsets.UTF_8)
  }
}
