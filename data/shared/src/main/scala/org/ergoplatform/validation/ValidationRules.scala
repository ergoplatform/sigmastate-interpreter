package org.ergoplatform.validation

import sigma.SigmaException
import sigma.ast.{DeserializeContext, ErgoTree, MethodsContainer, SMethod}
import sigma.ast.TypeCodes.LastConstantCode
import sigma.serialization.{InvalidOpCode, SerializerException}
import sigma.util.Extensions.toUByte
import sigma.validation.ValidationRules._
import sigma.validation._
import sigma.ast.ErgoTree.HeaderType
import sigma.ast.syntax._
import sigma.exceptions.InterpreterException
import sigma.serialization.ValueCodes.OpCode
import sigma.serialization.ValueSerializer

/** All validation rules which are used to check soft-forkable conditions. Each validation
  * rule throws a [[org.ergoplatform.validation.ValidationException]]. Each
  * ValidationException can be caught and handled with respect to
  * [[SigmaValidationSettings]], which can be changed by miners via voting.
  * Thus, the behavior of the rules can be overridden without breaking consensus.
  */
object ValidationRules {

  object CheckDeserializedScriptType extends ValidationRule(FirstRuleId,
    "Deserialized script should have expected type") {
    override protected lazy val settings: SigmaValidationSettings = currentSettings

    final def apply[T](d: DeserializeContext[_], script: SValue): Unit = {
      checkRule()
      if (d.tpe != script.tpe) {
        throwValidationException(
          new InterpreterException(s"Failed context deserialization of $d: \n" +
              s"expected deserialized script to have type ${d.tpe}; got ${script.tpe}"),
          Array[Any](d, script))
      }
    }
  }

  object CheckDeserializedScriptIsSigmaProp extends ValidationRule(1001,
    "Deserialized script should have SigmaProp type") {
    override protected lazy val settings: SigmaValidationSettings = currentSettings

    /** @param root candidate node before it is added as a root of ErgoTree */
    final def apply[T](root: SValue): Unit = {
      checkRule()
      if (!root.tpe.isSigmaProp) {
        throwValidationException(
          new SerializerException(s"Failed deserialization, expected deserialized script to have type SigmaProp; got ${root.tpe}"),
          Array[Any](root))
      }
    }
  }

  object CheckValidOpCode extends ValidationRule(1002,
    "Check the opcode is supported by registered serializer or is added via soft-fork")
    with SoftForkWhenCodeAdded {
    override protected lazy val settings: SigmaValidationSettings = currentSettings

    final def apply[T](ser: ValueSerializer[_], opCode: OpCode): Unit = {
      checkRule()
      if (ser == null || ser.opCode != opCode) {
        throwValidationException(
          new InvalidOpCode(s"Cannot find serializer for Value with opCode = LastConstantCode + ${toUByte(opCode) - LastConstantCode}"),
          Array(opCode))
      }
    }
  }

  /** Not used since v5.0.1. */
  object CheckIsSupportedIndexExpression extends ValidationRule(1003,
    "Check the index expression for accessing collection element is supported.") {
    override protected lazy val settings: SigmaValidationSettings = currentSettings
  }

  /** Not used since v5.0.3  */
  object CheckCostFunc extends ValidationRule(1004,
    "Cost function should contain only operations from specified list.") {
    override protected lazy val settings: SigmaValidationSettings = currentSettings
  }

  object CheckCalcFunc extends ValidationRule(1005,
    "If SigmaProp.isProven method calls exists in the given function,\n then it is the last operation") {
    override protected lazy val settings: SigmaValidationSettings = currentSettings
  }

  /** This rule is not use in v5.x, keep the commented code below as a precise
    * documentation of its semantics.
    */
  object CheckTupleType extends ValidationRule(1006,
    "Supported tuple type.") with SoftForkWhenReplaced {
    override protected lazy val settings: SigmaValidationSettings = currentSettings

//    final def apply[Ctx <: IRContext, T](ctx: Ctx)(e: ctx.Elem[_]): Unit = {
//      checkRule()
//      val condition = e match {
//        case _: ctx.PairElem[_,_] => true
//        case _ => false
//      }
//      if (!condition) {
//        throwValidationException(new SigmaException(s"Invalid tuple type $e"), Array[ctx.Elem[_]](e))
//      }
//    }
  }

  object CheckAndGetMethod extends ValidationRule(1011,
    "Check the type has the declared method.") {
    override protected lazy val settings: SigmaValidationSettings = currentSettings

    final def apply[T](objType: MethodsContainer, methodId: Byte): SMethod = {
      checkRule()
      val methodOpt = objType.getMethodById(methodId)
      if (methodOpt.isDefined) methodOpt.get
      else {
        throwValidationException(
          new SerializerException(s"The method with code $methodId doesn't declared in the type $objType."),
          Array[Any](objType, methodId))
      }
    }

    override def isSoftFork(vs: SigmaValidationSettings,
                            ruleId: Short,
                            status: RuleStatus,
                            args: Seq[Any]): Boolean = (status, args) match {
      case (ChangedRule(newValue), Seq(objType: MethodsContainer, methodId: Byte)) =>
        val key = Array(objType.ownerType.typeId, methodId)
        newValue.grouped(2).exists(java.util.Arrays.equals(_, key))
      case _ => false
    }
  }

  object CheckHeaderSizeBit extends ValidationRule(1012,
    "For version greater then 0, size bit should be set.") with SoftForkWhenReplaced {
    override protected lazy val settings: SigmaValidationSettings = currentSettings

    final def apply(header: HeaderType): Unit = {
      checkRule()
      val version = ErgoTree.getVersion(header)
      if (version != 0 && !ErgoTree.hasSize(header)) {
        throwValidationException(
          new SigmaException(s"Invalid ErgoTreeHeader $header, size bit is expected for version $version"),
          Array(header))
      }
    }
  }

  /** Not used since v5.0.3  */
  object CheckCostFuncOperation extends ValidationRule(1013,
    "Check the opcode is allowed in cost function") {
    override protected lazy val settings: SigmaValidationSettings = currentSettings
  }

  /** Not used since v5.0.1  */
  object CheckLoopLevelInCostFunction extends ValidationRule(1015,
    "Check that loop level is not exceeded.") {
    override protected lazy val settings: SigmaValidationSettings = currentSettings
  }

  val ruleSpecs: Seq[ValidationRule] = Seq(
    CheckDeserializedScriptType,
    CheckDeserializedScriptIsSigmaProp,
    CheckValidOpCode,
    CheckIsSupportedIndexExpression,
    CheckCostFunc,
    CheckCalcFunc,
    CheckTupleType,
    CheckPrimitiveTypeCode,
    CheckTypeCode,
    CheckSerializableTypeCode,
    CheckTypeWithMethods,
    CheckAndGetMethod,
    CheckHeaderSizeBit,
    CheckCostFuncOperation,
    CheckPositionLimit,
    CheckLoopLevelInCostFunction
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

}
