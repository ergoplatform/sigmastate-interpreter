package sigma.serialization

import sigma.ast._

case class ValUseSerializer(cons: (Int, SType) => Value[SType]) extends ValueSerializer[ValUse[SType]] {
  override def opDesc = ValUse

  override protected def getValueChildren(obj: ValUse[SType]): IndexedSeq[Value[SType]] = Value.EmptySeq

  override protected def rebuildValueNode(
      obj: ValUse[SType],
      children: IndexedSeq[Value[SType]]): Value[SType] = obj

  override def serialize(obj: ValUse[SType], w: SigmaByteWriter): Unit = {
    w.putUInt(obj.valId)
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val id = r.getUInt.toInt
    // Note, when id < 0 as a result of Int overflow, the r.valDefTypeStore(id) won't throw
    // and also ValUse node will be created, but then its evaluation will throw (because
    // there will be no ValDef with negative id in the env.
    // However, in general, there is no guarantee that this ValUse will ever be executed
    // as it may be in an `if` branch.
    val tpe = r.valDefTypeStore(id)
    cons(id, tpe)
  }
}

