package org.ergoplatform.validation

import sigma.validation.{ChangedRule, DisabledRule, EnabledRule, ReplacedRule, RuleStatus}
import sigma.serialization.{SigmaByteReader, SigmaByteWriter, SigmaSerializer}

object RuleStatusSerializer extends SigmaSerializer[RuleStatus, RuleStatus] {
  import RuleStatus._

  val FirstRuleId = 1000

  def measureWrittenBytes(block: SigmaByteWriter => Unit): Int = {
    val w = SigmaSerializer.startWriter()
    block(w)
    val bytes = w.toBytes
    bytes.length
  }

  /** The general format for RuleStatuses
    * field      | Format  | #bytes         |  Description
    * ----------------------------------------------------------------------
    * dataSize   | UShort  | 1..2 bytes     | number of bytes for dataBytes
    * statusCode | Byte    | 1 byte         | code of the status type
    * dataBytes  | Bytes   | dataSize bytes | serialized byte if status value
    */
  override def serialize(status: RuleStatus, w: SigmaByteWriter): Unit = status match {
    case EnabledRule | DisabledRule =>
      w.putUShort(0) // zero bytes for dataBytes
      w.put(status.statusCode)
    case ReplacedRule(newRuleId) =>
      val ofs = (newRuleId - FirstRuleId)  // id offset
      val dataSize = measureWrittenBytes(w => w.putUShort(ofs)) // number of bytes to store id offset
      w.putUShort(dataSize)  // size of dataBytes
      w.put(status.statusCode)
      w.putUShort(ofs)       // dataBytes
    case ChangedRule(data) =>
      w.putUShort(data.length)
      w.put(status.statusCode)
      w.putBytes(data)
  }

  override def parse(r: SigmaByteReader): RuleStatus = {
    val dataSize = r.getUShort() // read number of bytes occupied by status data
    val statusType = r.getByte()
    statusType match {
      case EnabledRuleCode =>
        EnabledRule
      case DisabledRuleCode =>
        DisabledRule  // the rule is explicitly disabled
      case ReplacedRuleCode =>
        val newRule = (r.getUShort() + FirstRuleId).toShort // store small offsets using single byte
        ReplacedRule(newRule) // the rule is disabled, but we also have info about new rule
      case ChangedRuleCode =>
        val bytes = r.getBytes(dataSize) // value bytes except statusType
        ChangedRule(bytes)
      case _ =>
        r.position += dataSize // skip status bytes which we don't understand
        ReplacedRule(0)  // unrecognized status code, the old code should process it as soft-fork
    }
  }
}

