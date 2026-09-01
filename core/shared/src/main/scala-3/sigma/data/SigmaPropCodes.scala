package sigma.data

import sigma.ast.TypeCodes.LastConstantCode

/** Opcodes of sigma proposition nodes.
  *
  * @see SigmaBoolean.opCode
  */
object SigmaPropCodes {
  opaque type SPCode = Byte
  
  object SPCode {
    def apply(b: Byte): SPCode = b
  }
  
  private def newOpCode(shift: Short): SPCode = (LastConstantCode + shift).toByte

  val AndCode                    : SPCode = newOpCode(38)
  val OrCode                     : SPCode = newOpCode(39)
  val AtLeastCode                : SPCode = newOpCode(40)
  val ProveDlogCode              : SPCode = newOpCode(93)
  val ProveDiffieHellmanTupleCode: SPCode = newOpCode(94)
  val TrivialPropFalseCode       : SPCode = newOpCode(98)
  val TrivialPropTrueCode        : SPCode = newOpCode(99)
}
