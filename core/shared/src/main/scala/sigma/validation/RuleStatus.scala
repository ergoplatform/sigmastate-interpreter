package sigma.validation

/** Base trait for rule status information. */
sealed trait RuleStatus {
  def statusCode: Byte
}
object RuleStatus {
  val EnabledRuleCode: Byte = 1.toByte
  val DisabledRuleCode: Byte = 2.toByte
  val ReplacedRuleCode: Byte = 3.toByte
  val ChangedRuleCode: Byte = 4.toByte
}

/** This is a default status of a rule which is registered in the table
  * and not yet altered by soft-forks.
  */
case object EnabledRule extends RuleStatus {
  val statusCode: Byte = RuleStatus.EnabledRuleCode
}

/** This is a status of a rule which is disabled in current version
  * and not yet altered by soft-forks.
  * The rule can be disabled via block extensions and voting process.
  */
case object DisabledRule extends RuleStatus {
  val statusCode: Byte = RuleStatus.DisabledRuleCode
}

/** The status of the rule which is replaced by a new rule via soft-fork extensions.
  * This is similar to DisabledRule, but in addition require the new rule to be enabled
  * at the same time (i.e. atomically)
  * @see `ValidationSettings.isSoftFork`
  * @param newRuleId  id of a new rule which replaces the rule marked with this status
  */
case class  ReplacedRule(newRuleId: Short) extends RuleStatus {
  val statusCode: Byte = RuleStatus.ReplacedRuleCode
}

/** The status of the rule whose parameters are changed via soft-fork extensions.
  * The same rule can be changed many times via voting.
  * @param newValue  new value of block extension value with key == rule.id
  */
case class ChangedRule(newValue: Array[Byte]) extends RuleStatus {
  val statusCode: Byte = RuleStatus.ChangedRuleCode

  override def hashCode(): Int = java.util.Arrays.hashCode(newValue)

  override def canEqual(that: Any): Boolean = that.isInstanceOf[ChangedRule]

  override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) || (obj match {
    case that: ChangedRule => java.util.Arrays.equals(newValue, that.newValue)
  })
}
