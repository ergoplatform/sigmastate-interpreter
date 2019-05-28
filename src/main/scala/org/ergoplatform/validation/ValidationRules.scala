package org.ergoplatform.validation

import sigmastate.eval.IRContext
import sigmastate.serialization.DataSerializer.CheckSerializableTypeCode
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.Values.{Value, SValue, IntValue}
import sigmastate.serialization.{ValueSerializer, OpCodes}
import sigmastate.utxo.DeserializeContext
import sigmastate.lang.exceptions._
import sigmastate.serialization.TypeSerializer.{CheckPrimitiveTypeCode, CheckTypeCode}
import sigmastate.{CheckMethod, SCollection, CheckTypeWithMethods, SType}
import sigma.util.Extensions.ByteOps

/** Base class for different validation rules registered in ValidationRules.currentSettings.
  * Each rule is identified by `id` and have a description.
  * Validation logic is implemented by `apply` methods of derived classes.
  */
case class ValidationRule(
    id: Short,
    description: String
) extends SoftForkChecker {
  /** Can be used in derived classes to implemented validation logic. */
  protected def validate[T](
      condition: => Boolean,
      cause: => Throwable, args: Seq[Any], block: => T): T = {
    val status = ValidationRules.currentSettings.getStatus(this.id)
    status match {
      case None =>
        throw new InterpreterException(s"ValidationRule $this not found in validation settings")
      case Some(DisabledRule) =>
        block  // if the rule is disabled we still need to execute the block of code
      case Some(status) =>
        if (condition)
          block
        else {
          throw new ValidationException(s"Validation failed on $this with args $args", this, args, Option(cause))
        }
    }
  }
}

/** Base class for all exceptions which may be thrown by validation rules.
  * Instances of this class are used as messages to communicate soft-fork information,
  * from the context where the soft-fork condition is detected (such as in ValidationRules),
  * up the stack to the point where it is clear how to handle it.
  * Some messages of this kind are not handled, in which case a new Exception is thrown
  * and this instance should be attached as a `cause` parameter.
  */
case class ValidationException(message: String, rule: ValidationRule, args: Seq[Any], cause: Option[Throwable] = None)
    extends Exception(message, cause.orNull)

object ValidationRules {
  /** The id of the first validation rule. Can be used as the beginning of the rules id range. */
  val FirstRuleId = 1000.toShort

  object CheckDeserializedScriptType extends ValidationRule(FirstRuleId,
    "Deserialized script should have expected type") {
    def apply[T](d: DeserializeContext[_], script: SValue)(block: => T): T =
      validate(d.tpe == script.tpe,
        new InterpreterException(s"Failed context deserialization of $d: \n" +
        s"expected deserialized script to have type ${d.tpe}; got ${script.tpe}"),
        Seq[Any](d, script), block
      )
  }

  object CheckDeserializedScriptIsSigmaProp extends ValidationRule(1001,
    "Deserialized script should have SigmaProp type") {
    def apply[T](root: SValue)(block: => T): T =
      validate(root.tpe.isSigmaProp,
        new SerializerException(s"Failed deserialization, expected deserialized script to have type SigmaProp; got ${root.tpe}"),
        Seq(root), block
      )
  }

  object CheckValidOpCode extends ValidationRule(1002,
    "Check the opcode is supported by registered serializer or is added via soft-fork")
    with SoftForkWhenCodeAdded {
    def apply[T](ser: ValueSerializer[_], opCode: OpCode)(block: => T): T = {
      def msg = s"Cannot find serializer for Value with opCode = LastConstantCode + ${opCode.toUByte - OpCodes.LastConstantCode}"
      def args = Seq(opCode)
      validate(ser != null && ser.opCode == opCode, new InvalidOpCode(msg), args, block)
    }
  }

  object CheckIsSupportedIndexExpression extends ValidationRule(1003,
    "Check the index expression for accessing collection element is supported.") {
    def apply[Ctx <: IRContext, T](ctx: Ctx)(coll: Value[SCollection[_]], i: IntValue, iSym: ctx.Rep[Int])(block: => T): T = {
      def msg = s"Unsupported index expression $i when accessing collection $coll"
      def args = Seq(coll, i)
      validate(ctx.isSupportedIndexExpression(iSym),
        new SigmaException(msg, i.sourceContext.toOption),
        args, block)
    }
  }

  object CheckCostFunc extends ValidationRule(1004,
    "Cost function should contain only operations from specified list.") {
    def apply[Ctx <: IRContext, T](ctx: Ctx)(costF: ctx.Rep[Any => Int])(block: => T): T = {
      def msg = s"Invalid cost function $costF"
      def args = Seq(costF)
      lazy val verification = ctx.verifyCostFunc(ctx.asRep[Any => Int](costF))
      validate(verification.isSuccess,
        verification.toEither.left.get,
        args, block)
    }
  }

  object CheckCalcFunc extends ValidationRule(1005,
    "If SigmaProp.isProven method calls exists in the given function,\n then it is the last operation") {
    def apply[Ctx <: IRContext, T](ctx: Ctx)(calcF: ctx.Rep[ctx.Context => Any])(block: => T): T = {
      def msg = s"Invalid calc function $calcF"
      def args = Seq(calcF)
      lazy val verification = ctx.verifyIsProven(calcF)
      validate(verification.isSuccess,
        verification.toEither.left.get,
        args, block)
    }
  }

  object CheckCostWithContext extends ValidationRule(1006,
    "Contract execution cost in a given context is limited by given maximum value.") {
    def apply[Ctx <: IRContext, T](ctx: Ctx)
        (costingCtx: ctx.Context.SContext, exp: Value[SType],
            costF: ctx.Rep[((ctx.Context, (Int, ctx.Size[ctx.Context]))) => Int], maxCost: Long): Int = {
      def args = Seq(costingCtx, exp, costF, maxCost)
      lazy val estimatedCostTry = ctx.checkCostWithContext(costingCtx, exp, costF, maxCost)
      validate(estimatedCostTry.isSuccess,
        {
          val t = estimatedCostTry.toEither.left.get
          new CosterException(s"Script cannot be executed due to high cost $exp: ", exp.sourceContext.toList.headOption, Some(t))
        },
        args, estimatedCostTry.get)
    }
  }

  object CheckTupleType extends ValidationRule(1007,
    "Supported tuple type.") with SoftForkWhenReplaced {
    def apply[Ctx <: IRContext, T](ctx: Ctx)(e: ctx.Elem[_])(block: => T): T = {
      def msg = s"Invalid tuple type $e"
      lazy val condition = e match {
        case pe: ctx.PairElem[_,_] => true
        case _ => false
      }
      validate(condition, new SigmaException(msg), Seq[ctx.Elem[_]](e), block)
    }
  }

  val ruleSpecs: Seq[ValidationRule] = Seq(
    CheckDeserializedScriptType,
    CheckDeserializedScriptIsSigmaProp,
    CheckValidOpCode,
    CheckIsSupportedIndexExpression,
    CheckCostFunc,
    CheckCalcFunc,
    CheckCostWithContext,
    CheckTupleType,
    CheckPrimitiveTypeCode,
    CheckTypeCode,
    CheckSerializableTypeCode,
    CheckTypeWithMethods,
    CheckMethod
  )

  /** Validation settings that correspond to the current version of the ErgoScript implementation.
    * Different version of the code will have a different set of rules here.
    * This variable is globally available and can be use wherever checking of the rules is necessary.
    * This is immutable data structure, it can be augmented with RuleStates from block extension
    * sections of the blockchain, but that augmentation is only available in stateful context.
    */
  val currentSettings: SigmaValidationSettings = new MapSigmaValidationSettings({
    val map = ruleSpecs.map(r => r.id -> (r, EnabledRule)).toMap
    assert(map.size == ruleSpecs.size, s"Duplicate ruleIds ${ruleSpecs.groupBy(_.id).filter(g => g._2.length > 1)}")
    map
  })

  def trySoftForkable[T](whenSoftFork: => T)(block: => T)(implicit vs: SigmaValidationSettings): T = {
    try block
    catch {
      case ve: ValidationException =>
        if (vs.isSoftFork(ve)) whenSoftFork
        else throw ve
    }
  }
}
