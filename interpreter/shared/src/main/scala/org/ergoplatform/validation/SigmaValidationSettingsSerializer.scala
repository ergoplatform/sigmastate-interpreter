package org.ergoplatform.validation

import sigma.serialization.{SerializerException, SigmaByteReader, SigmaByteWriter}
import sigma.serialization.SigmaSerializer
import sigma.util.Extensions.IntOps
import sigma.validation.{MapSigmaValidationSettings, SigmaValidationSettings}

/** The rules are serialized ordered by ruleId.
  * This serializer preserves roundtrip identity `deserialize(serialize(_)) = identity`
  * however it may not preserve `serialize(deserialize(_)) = identity` */
object SigmaValidationSettingsSerializer extends SigmaSerializer[SigmaValidationSettings, SigmaValidationSettings] {

  override def serialize(settings: SigmaValidationSettings, w: SigmaByteWriter): Unit = {
    val rules = settings.toArray.sortBy(_._1)
    w.putUInt(rules.length)
    rules.foreach { case (id, (_, status)) =>
      w.putUShort(id)
      RuleStatusSerializer.serialize(status, w)
    }
  }

  override def parse(r: SigmaByteReader): SigmaValidationSettings = {
    val nRules = r.getUInt().toInt
    // Note, when nRules < 0 as a result of Int overflow, the loop is empty
    val parsed = (0 until nRules).map { _ =>
      val ruleId = r.getUShort().toShortExact
      val status = RuleStatusSerializer.parse(r)
      ruleId -> status
    }
    val map = parsed.map { case (id, status) =>
      val (rule, _) = ValidationRules.currentSettings.get(id)
          .getOrElse(throw SerializerException(s"Rule with id $id is not registered"))
      id -> (rule, status)
    }.toMap
    val res = new MapSigmaValidationSettings(map)
    res
  }
}

