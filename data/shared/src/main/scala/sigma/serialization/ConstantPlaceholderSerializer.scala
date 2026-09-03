package sigma.serialization

import sigma.ast.SType
import sigma.ast._

case class ConstantPlaceholderSerializer(cons: (Int, SType) => Value[SType])
  extends ValueSerializer[ConstantPlaceholder[SType]] {
  import Operations.ConstantPlaceholderInfo._
  override def opDesc = ConstantPlaceholder

  override protected def getValueChildren(
      obj: ConstantPlaceholder[SType]): IndexedSeq[Value[SType]] = Value.EmptySeq

  override protected def rebuildValueNode(
      obj: ConstantPlaceholder[SType],
      children: IndexedSeq[Value[SType]]): Value[SType] = obj

  override def serialize(obj: ConstantPlaceholder[SType], w: SigmaByteWriter): Unit = {
    w.putUInt(obj.id, indexArg)
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val id = r.getUIntExact
    // NO-FORK: in v5.x getUIntExact may throw Int overflow exception
    // in v4.x r.getUInt().toInt is used and may return negative Int instead of the overflow
    // in which case the constantStore.get will throw ArrayIndexOutOfBoundsException
    val constant = r.constantStore.get(id)
    if (r.resolvePlaceholdersToConstants)
      constant
    else
      cons(id, constant.tpe)
  }
}

