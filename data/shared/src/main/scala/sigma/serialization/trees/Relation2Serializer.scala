package sigma.serialization.trees

import sigma.ast.{SBoolean, SType}
import sigma.serialization.CoreByteWriter.{ArgInfo, Bits, DataInfo, maxBitsInfo}
import sigma.ast._
import sigma.ast.syntax.SValue
import sigma.serialization.OpCodes._
import sigma.serialization.{SigmaByteReader, SigmaByteWriter, ValueSerializer}
import sigma.serialization.ValueSerializer._
import sigma.util.Extensions._

case class Relation2Serializer[S1 <: SType, S2 <: SType, R <: Value[SBoolean.type]]
(override val opDesc: RelationCompanion,
 constructor: (Value[S1], Value[S2]) => Value[SBoolean.type]) extends ValueSerializer[R] {
  import SigmaByteWriter._
  val opCodeInfo: DataInfo[Byte] = ArgInfo("opCode", s"always contains OpCode ${ConcreteCollectionBooleanConstantCode.toUByte}")
  val bitsInfo: DataInfo[Bits] = maxBitsInfo("(l,r)", 2, "two higher bits in a byte")
  val leftArgInfo: DataInfo[SValue] = opDesc.argInfos(0)
  val rightArgInfo: DataInfo[SValue] = opDesc.argInfos(1)

  override def serialize(obj: R, w: SigmaByteWriter): Unit = {
    val typedRel = obj.asInstanceOf[Relation[S1, S2]]
    cases("(left, right)") {
      (typedRel.left, typedRel.right) match {
        case (Constant(left, lTpe), Constant(right, rTpe)) if lTpe == SBoolean && rTpe == SBoolean =>
          when(1, "(Constant(l, Boolean), Constant(r, Boolean))") {
            w.put(ConcreteCollectionBooleanConstantCode, opCodeInfo)
            w.putBits(Array(left.asInstanceOf[Boolean], right.asInstanceOf[Boolean]), bitsInfo)
          }
        case _ =>
          otherwise {
            w.putValue(typedRel.left, leftArgInfo)
            w.putValue(typedRel.right, rightArgInfo)
          }
      }
    }
  }

  /** HOTSPOT: don't beautify this code */
  override def parse(r: SigmaByteReader): R = {
    if (r.peekByte() == ConcreteCollectionBooleanConstantCode) {
      val _ = r.getByte() // skip collection op code
      val booleans = r.getBits(2)
      val firstArg = BooleanConstant.fromBoolean(booleans(0)).asInstanceOf[Value[S1]]
      val secondArg = BooleanConstant.fromBoolean(booleans(1)).asInstanceOf[Value[S2]]
      constructor(firstArg, secondArg).asInstanceOf[R]
    } else {
      val firstArg = r.getValue().asInstanceOf[Value[S1]]
      val secondArg = r.getValue().asInstanceOf[Value[S2]]
      constructor(firstArg, secondArg).asInstanceOf[R]
    }
  }
}
