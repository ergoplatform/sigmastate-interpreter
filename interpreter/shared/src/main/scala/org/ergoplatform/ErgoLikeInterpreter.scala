package org.ergoplatform

import sigma.ast.SCollection.SByteArray
import sigma.ast._
import sigma.ast.syntax._
import sigmastate.interpreter.Interpreter


/** Base class of verifying interpreter which expects ErgoLikeContext as input of
  * verify method.
  * It implements deserialization of register of SELF box.
  */
class ErgoLikeInterpreter extends Interpreter {

  override type CTX <: ErgoLikeContext

  override def substDeserialize(context: CTX, updateContext: CTX => Unit, node: SValue): Option[SValue] = node match {
      case d: DeserializeRegister[_] =>
        context.boxesToSpend(context.selfIndex).get(d.reg).flatMap { v =>
          v match {
            case eba: EvaluatedValue[SByteArray]@unchecked =>
              val (ctx1, outVal) = deserializeMeasured(context, eba.value.toArray)
              updateContext(ctx1)

              if (outVal.tpe != d.tpe)
                sys.error(s"Failed deserialization of $d: expected deserialized value to have type ${d.tpe}; got ${outVal.tpe}")
              else
                Some(outVal)
            case _ =>
              // this case is not possible because `ErgoBox.get`
              //  returns lookups values from `additionalRegisters` Map with values
              //  of type EvaluatedValue, which are always Constant nodes in practice.
              //  Also, this branch is never executed so can be safely removed, but
              //  we keep it here explicitly for clarity
              None
          }
        }.orElse(d.default)
      case _ => super.substDeserialize(context, updateContext, node)
    }

}