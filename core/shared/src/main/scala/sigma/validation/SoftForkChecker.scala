package sigma.validation

/** Interface implemented by objects capable of checking soft-fork conditions. */
trait SoftForkChecker {
  /** Check soft-fork condition.
    * @param vs       ValidationSettings actualized from blockchain extension sections
    * @param ruleId   id of the rule which raised ValidationException
    * @param status   status of the rule in the blockchain (agreed upon via voting)
    * @param args     arguments of Validation rule with which the rule has risen the exception
    * @return         true if `args` and `status` can be interpreted as valid soft-fork condition.
    */
  def isSoftFork(vs: SigmaValidationSettings, ruleId: Short, status: RuleStatus, args: Seq[Any]): Boolean = false
}

/** Checks that the failed validation rule has ReplacedRule status in block extensions section.
  * This means the rule given by `ruleId` is not used in newer versions of the protocol.
  * Instead it has been replaced by the new rule given by ReplacedRule status.
  */
trait SoftForkWhenReplaced extends SoftForkChecker {
  override def isSoftFork(vs: SigmaValidationSettings,
      ruleId: Short,
      status: RuleStatus,
      args: Seq[Any]): Boolean = (status, args) match {
    case (ReplacedRule(_), _) => true
    case _ => false
  }
}

/** Checks that the unknown `code` is however present in the ChangedRule new value
  * stored in block extensions section. This is interpreted as soft-fork condition,
  * i.e. the unknown `code` is not arbitrary, but explicitly added to the blockchain
  * configuration and implemented in newer versions of the protocol.
  */
trait SoftForkWhenCodeAdded extends SoftForkChecker {
  override def isSoftFork(vs: SigmaValidationSettings,
      ruleId: Short,
      status: RuleStatus,
      args: Seq[Any]): Boolean = (status, args) match {
    case (ChangedRule(newValue), Seq(code: Byte)) => newValue.contains(code)
    case _ => false
  }
}
