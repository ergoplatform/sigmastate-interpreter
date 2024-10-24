package sigma.ast

import supertagged.TaggedType

/** Encoding of types for serialization. */
object TypeCodes {
  object TypeCode extends TaggedType[Byte]
  type TypeCode = TypeCode.Type

  /** Decoding of types depends on the first byte and in general is a recursive procedure
    * consuming some number of bytes from Reader.
    * All data types are recognized by the first byte falling in the region [FirstDataType .. LastDataType] */
  val FirstDataType: TypeCode = TypeCode @@ 1.toByte

  val LastDataType : TypeCode = TypeCode @@ 111.toByte

  /** SFunc type */
  val FuncType: TypeCode = TypeCode @@ (LastDataType + 1).toByte

  /** We use optimized encoding of constant values to save space in serialization.
    * Since Box registers are stored as Constant nodes we save 1 byte for each register.
    * This is due to convention that Value.opCode falling in [1..LastDataType] region is a constant.
    * Thus, we can just decode an instance of SType and then decode data using DataSerializer.
    *
    * Decoding of constants depends on the first byte and in general is a recursive procedure
    * consuming some number of bytes from Reader.
    * */
  val ConstantCode: Byte = 0.toByte

  /** The last constant code is equal to FirstFuncType which represent generic function type.
    * We use this single code to represent all functional constants, since we don't have enough space in single byte.
    * Subsequent bytes have to be read from Reader in order to decode the type of the function and the corresponding data. */
  val LastConstantCode: Byte = (TypeCodes.LastDataType + 1).toByte
}
