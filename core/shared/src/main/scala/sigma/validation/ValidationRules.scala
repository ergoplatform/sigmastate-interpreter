package sigma.validation

import sigma.SigmaException
import sigma.ast.{SGlobal, SOption, TypeCodes}
import sigma.serialization.{ReaderPositionLimitExceeded, SerializerException}
import sigma.util.Extensions.toUByte
import sigma.serialization.TypeSerializer.embeddableIdToType

/** Base class for different validation rules registered in ValidationRules.currentSettings.
  * Each rule is identified by `id` and have a description.
  * Validation logic is implemented by `apply` methods of derived classes.
  */
abstract case class ValidationRule(
    id: Short,
    description: String
) extends SoftForkChecker {
  /** Validation settings defining the status of this rule.
    * @see checkRule()
    */
  protected def settings: SigmaValidationSettings
  /** Whether the status of this rule was checked on first invocation. */
  private var _checked: Boolean = false

  /** Check the rule is registered and enabled.
    * Since it is easy to forget to register new rule, we need to do this check.
    * But because it is hotspot, we do this check only once for each rule.
    * HOTSPOT: executed on every typeCode and opCode during script deserialization
    */
  @inline protected final def checkRule(): Unit = {
    if (!_checked) {
      if (settings.getStatus(this.id).isEmpty) {
        throw new SigmaException(s"ValidationRule $this not found in validation settings")
      }
      _checked = true  // prevent this check on every call (only first call is checked)
    }
    // upon successful return we know the rule is registered with EnabledRule status
  }

  /** Throws ValidationException with the given cause and args.
    * Should be used in all validation rules to unify ValidationException instances
    * which can be thrown (to simplify handling).
    */
  def throwValidationException(cause: Throwable, args: Seq[Any]): Nothing = {
    if (cause.isInstanceOf[ValidationException]) {
      throw cause
    }
    else {
      throw ValidationException(s"Validation failed on $this with args $args", this, args, Option(cause))
    }
  }

}

/** Base class for all exceptions which may be thrown by validation rules.
  * Instances of this class are used as messages to communicate soft-fork information,
  * from the context where the soft-fork condition is detected (such as in ValidationRules),
  * up the stack to the point where it is clear how to handle it.
  * Some messages of this kind are not handled, in which case a new Exception is thrown
  * and this instance should be attached as a `cause` parameter.
  *
  * This exception should typically always come with Some(cause). As result, stack trace is not
  * filled in when this instance is create. The `cause` parameter should be examined for the source
  * of the exception.
  */
case class ValidationException(message: String, rule: ValidationRule, args: Seq[Any], cause: Option[Throwable] = None)
    extends Exception(message, cause.orNull) {
  override def fillInStackTrace(): Throwable = this  // to avoid spending time on recording stack trace
}

/** All validation rules which are used to check soft-forkable conditions. Each validation
  * rule throws a [[org.ergoplatform.validation.ValidationException]]. Each
  * ValidationException can be caught and handled with respect to
  * [[SigmaValidationSettings]], which can be changed by miners via voting.
  * Thus, the behavior of the rules can be overridden without breaking consensus.
  */
object ValidationRules {
  /** The id of the first validation rule. Can be used as the beginning of the rules id range. */
  val FirstRuleId = 1000.toShort

  object CheckPrimitiveTypeCode extends ValidationRule(1007,
    "Check the primitive type code is supported or is added via soft-fork")
      with SoftForkWhenCodeAdded {
    override protected lazy val settings: SigmaValidationSettings = coreSettings

    final def apply[T](code: Byte): Unit = {
      checkRule()
      val ucode = toUByte(code)
      if (ucode <= 0 || ucode >= embeddableIdToType.length) {
        throwValidationException(
          new SerializerException(s"Cannot deserialize primitive type with code $ucode"),
          Array(code))
      }
    }
  }

  object CheckTypeCode extends ValidationRule(1008,
    "Check the non-primitive type code is supported or is added via soft-fork")
      with SoftForkWhenCodeAdded {
    override protected lazy val settings: SigmaValidationSettings = coreSettings

    final def apply[T](typeCode: Byte): Unit = {
      checkRule()
      val ucode = toUByte(typeCode)
      if (ucode > toUByte(SGlobal.typeCode)) {
        throwValidationException(
          new SerializerException(s"Cannot deserialize the non-primitive type with code $ucode"),
          Array(typeCode))
      }
    }
  }

  object CheckSerializableTypeCode extends ValidationRule(1009,
    "Check the data values of the type (given by type code) can be serialized")
      with SoftForkWhenReplaced {
    override protected lazy val settings: SigmaValidationSettings = coreSettings

    /** Creates an exception which is used as a cause when throwing a ValidationException. */
    def throwValidationException(typeCode: Byte): Nothing = {
      val ex = new SerializerException(
        s"Data value of the type with the code ${toUByte(typeCode)} cannot be deserialized.")
      throwValidationException(ex, Array(typeCode))
    }

    final def apply[T](typeCode: Byte): Unit = {
      checkRule()
      val ucode = toUByte(typeCode)
      if (typeCode == SOption.OptionTypeCode || ucode > toUByte(TypeCodes.LastDataType)) {
        // the `typeCode == SOption.OptionTypeCode` condition is added in v5.0 and we
        // throw ValidationException for Option type as well in order to be able to
        // interpret it as soft-fork condition.
        // This will allow to add Option serialization in DataSerializer via v6.0 soft-fork.
        // This is in contrast to v4.x of this rule where Option type is not checked and
        // ordinary SigmaSerializer exception is thrown by the fallback case of DataSerializer.
        // This change is consensus-safe as v4.x and v5.x nodes will both throw exceptions
        // (albeit different ones) while attempting deserialization of an Option value.
        throwValidationException(typeCode)
      }
    }
  }

  object CheckTypeWithMethods extends ValidationRule(1010,
    "Check the type (given by type code) supports methods")
      with SoftForkWhenCodeAdded {
    override protected lazy val settings: SigmaValidationSettings = coreSettings

    final def apply[T](typeCode: Byte, cond: Boolean): Unit = {
      checkRule()
      val ucode = toUByte(typeCode)
      if (!cond) {
        throwValidationException(
          new SerializerException(s"Type with code $ucode doesn't support methods."),
          Array(typeCode))
      }
    }
  }

  /** Introduced in v5.0, this rule it is used in creation of ValidationExceptions, which
    * in turn can be checked for soft-fork condition using `this.isSoftFork`. Thus, this
    * rule can be replaced with a new rule and the limit can be increased.
    */
  object CheckPositionLimit extends ValidationRule(1014,
    "Check that the Reader has not exceeded the position limit.") with SoftForkWhenReplaced {
    override protected lazy val settings: SigmaValidationSettings = coreSettings

    /** Wraps the given cause into [[ValidationException]] and throws it. */
    def throwValidationException(cause: ReaderPositionLimitExceeded): Nothing = {
      throwValidationException(cause, args = Nil)
    }

    /** Throws a [[ValidationException]] with the given parameters. */
    def throwValidationException(position: Int, positionLimit: Int): Nothing = {
      throwValidationException(
        new ReaderPositionLimitExceeded(
          s"SigmaByteReader position limit $positionLimit is reached at position $position",
          position, positionLimit))
    }

    final def apply(position: Int, positionLimit: Int): Unit = {
      checkRule()
      if (position > positionLimit) {
        throwValidationException(position, positionLimit)
      }
    }
  }

  private val ruleSpecs: Seq[ValidationRule] = Seq(
    CheckPrimitiveTypeCode,
    CheckTypeCode,
    CheckSerializableTypeCode,
    CheckTypeWithMethods,
    CheckPositionLimit
  )

  /** Validation settings that correspond to the current version of the ErgoScript implementation.
    * Different version of the code will have a different set of rules here.
    * This variable is globally available and can be use wherever checking of the rules is necessary.
    * This is immutable data structure, it can be augmented with RuleStates from block extension
    * sections of the blockchain, but that augmentation is only available in stateful context.
    */
  val coreSettings: SigmaValidationSettings = new MapSigmaValidationSettings({
    val map = ruleSpecs.map(r => r.id -> (r, EnabledRule)).toMap
    assert(map.size == ruleSpecs.size, s"Duplicate ruleIds ${ruleSpecs.groupBy(_.id).filter(g => g._2.length > 1)}")
    map
  })

  /** Executes the given `block` catching [[ValidationException]] and checking possible
    * soft-fork condition in the context of the given [[SigmaValidationSettings]].
    * If soft-fork condition is recognized the `whenSoftFork` is executed and its result
    * is returned.
    *
    * @param whenSoftFork executed when soft-fork condition is detected
    * @param block        block of code, which may throw [[ValidationException]]
    * @param vs           set of [[SigmaValidationSettings]] which can be used to recognize soft-fork conditions.
    * @return result of `block` if no ValidationException was thrown, or the result of
    * `whenSoftFork` if soft-fork condition is detected.
    * @throws ValidationException if soft-fork condition is not recognized by the given `vs`
    */
  def trySoftForkable[T](whenSoftFork: => T)(block: => T)(implicit vs: SigmaValidationSettings): T = {
    try block
    catch {
      case ve: ValidationException =>
        if (vs.isSoftFork(ve)) whenSoftFork
        else throw ve
    }
  }
}
