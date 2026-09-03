package sigma.serialization

import sigma.ast.SCollection.SByteArray
import sigma.ast.SInt
import sigma.ast.SOption.SIntOption
import sigma.serialization.CoreByteWriter.DataInfo
import sigma.ast._
import sigma.ast.syntax._
import sigma.ast.syntax.ValueOps
import SigmaByteWriter._

case class CreateAvlTreeSerializer(
    cons: (ByteValue, Value[SByteArray], IntValue, Value[SIntOption]) => AvlTreeValue
  )
  extends ValueSerializer[CreateAvlTree]
{
  import Operations.CreateAvlTreeInfo._
  override def opDesc = CreateAvlTree
  val operationFlagsInfo: DataInfo[SValue] = operationFlagsArg
  val digestInfo: DataInfo[SValue] = digestArg
  val keyLengthInfo: DataInfo[SValue] = keyLengthArg
  val valueLengthOptInfo: DataInfo[SValue] = valueLengthOptArg

  override protected def getValueChildren(obj: CreateAvlTree): IndexedSeq[Value[SType]] =
    IndexedSeq(obj.operationFlags, obj.digest, obj.keyLength, obj.valueLengthOpt)

  override protected def rebuildValueNode(
      obj: CreateAvlTree,
      children: IndexedSeq[Value[SType]]): Value[SType] =
    cons(
      children(0).asByteValue,
      children(1).asByteArray,
      children(2).asIntValue,
      children(3).asOption[SInt.type])

  override def serialize(obj: CreateAvlTree, w: SigmaByteWriter): Unit = {
    w.putValue(obj.operationFlags, operationFlagsInfo)
    w.putValue(obj.digest, digestInfo)
    w.putValue(obj.keyLength, keyLengthInfo)
    w.putValue(obj.valueLengthOpt, valueLengthOptInfo)
  }

  override def parse(r: SigmaByteReader) = {
    val flags = r.getValue().asByteValue
    val digest = r.getValue().asByteArray
    val keyLength = r.getValue().asIntValue
    val valueLength = r.getValue().asOption[SInt.type]
    cons(flags, digest, keyLength, valueLength)
  }
}
