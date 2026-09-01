package sigma.data

import sigma.ast.TypeCodes.LastConstantCode
import supertagged.TaggedType

/** Opcodes of sigma proposition nodes.
  *
  * @see SigmaBoolean.opCode
  */
object SigmaPropCodes {
  object SPCode extends TaggedType[Byte]
  type SPCode = SPCode.Type
  private def newOpCode(shift: Short): SPCode = SPCode @@ (LastConstantCode + shift).toByte

  val AndCode                    : SPCode = newOpCode(38)
  val OrCode                     : SPCode = newOpCode(39)
  val AtLeastCode                : SPCode = newOpCode(40)
  val ProveDlogCode              : SPCode = newOpCode(93)
  val ProveDiffieHellmanTupleCode: SPCode = newOpCode(94)
  val TrivialPropFalseCode       : SPCode = newOpCode(98)
  val TrivialPropTrueCode        : SPCode = newOpCode(99)
}

