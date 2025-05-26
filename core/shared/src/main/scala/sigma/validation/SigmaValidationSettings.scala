package sigma.validation

/**
  * Configuration of validation. Each `ValidationRule` instance should be
  * implemented as an `object` to facilitate type-safe usage. It then should be
  * registered in `ValidationRules.currentSettings` to be used in the code to
  * perform validation. Added to `currentSettings` the rule has EnabledRule
  * status by default, but only in a current version of the code. Thus, the
  * value `currentSettings` represents the validation settings of the current
  * version of the code. The set of rules in `currentSettings` is fixed in the
  * current version of the code and thus only rule status can be changed (as
  * described below)
  *
  * Old versions of the code don't have access to the rules added in newer
  * versions. The implementation of the specific rule, once released under
  * specific ruleId, should never be changed, hence ruleId denotes that
  * implementation. However, the behavior of rules (released with code) can be
  * altered by changing their status in block extensions section via voting.
  *
  * The status changes are represented in ValidationSettings using the
  * RuleStatus type. Each descendant class represent a particular change in the
  * rule status. Rule ids are used as keys of the status values stored in the
  * block extension section. RuleStatus instances are deserialized from the
  * block extension values. Deserialized (ruleId, status) pairs are joined with
  * the (ruleId,status) pairs in `currentSettings`, and for matching ruleIds the
  * default statues stored in `currentSettings` are replaced with the new
  * statuses obtained from the blockchain. Deserialized (ruleId,status) pairs
  * which don't match with `currentSettings` are ignored.
  *
  * Each rule has associated check of soft-fork condition by implementing
  * `isSoftFork` method. If `isSoftFork` returns true, then ValidationException
  * raised by the rule is interpreted as *soft-fork condition*. Depending on the
  * use case, soft-fork condition allows some operations performed by an old
  * code to succeed which otherwise would fail due to ValidationException raised
  * by the validation rule. One notable use case is Box.ergoTree validation in
  * which old code can skip ValidationExceptions under soft-fork condition (i.e.
  * when isSoftFork returns true), for example when a new opCode is added in the
  * newer version of the protocol, and this fact can be recognized by the old
  * code.
  *
  * @see SoftForkWhenCodeAdded
  */
abstract class SigmaValidationSettings extends Iterable[(Short, (ValidationRule, RuleStatus))] {
  def get(id: Short): Option[(ValidationRule, RuleStatus)]
  def getStatus(id: Short): Option[RuleStatus]
  def updated(id: Short, newStatus: RuleStatus): SigmaValidationSettings
  def isSoftFork(ve: ValidationException): Boolean = isSoftFork(ve.rule.id, ve)
  def isSoftFork(ruleId: Short, ve: ValidationException): Boolean = {
    val infoOpt = get(ruleId)
    infoOpt match {
      case Some((_, ReplacedRule(_))) => true
      case Some((rule, status)) => rule.isSoftFork(this, rule.id, status, ve.args)
      case None => false
    }
  }
}

/** Default representation of validation settings. */
sealed class MapSigmaValidationSettings(private val map: Map[Short, (ValidationRule, RuleStatus)]) extends SigmaValidationSettings {
  override def iterator: Iterator[(Short, (ValidationRule, RuleStatus))] = map.iterator
  override def get(id: Short): Option[(ValidationRule, RuleStatus)] = map.get(id)

  /** HOTSPOT: don't beautify this code */
  override def getStatus(id: Short): Option[RuleStatus] = {
    val statusOpt = map.get(id)
    val res = if (statusOpt.isDefined) Some(statusOpt.get._2) else None
    res
  }
  override def updated(id: Short, newStatus: RuleStatus): MapSigmaValidationSettings = {
    val (rule,_) = map(id)
    new MapSigmaValidationSettings(map.updated(id, (rule, newStatus)))
  }

  override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) || (obj match {
    case that: MapSigmaValidationSettings => map == that.map
    case _ => false
  })

  override def hashCode(): Int = map.hashCode()
}

