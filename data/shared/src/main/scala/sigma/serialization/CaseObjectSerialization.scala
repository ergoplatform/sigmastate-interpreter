package sigma.serialization

import sigma.ast.{Height, LastBlockUtxoRootHash, MinerPubkey, SType, Value, ValueCompanion}

case class CaseObjectSerialization[V <: Value[SType]](override val opDesc: ValueCompanion, obj: V)
  extends ValueSerializer[V] {

  override protected def getValueChildren(obj: V): IndexedSeq[Value[SType]] = Value.EmptySeq

  override protected def rebuildValueNode(obj: V, children: IndexedSeq[Value[SType]]): Value[SType] = obj

  override def serialize(obj: V, w: SigmaByteWriter): Unit = ()

  override def parse(r: SigmaByteReader): V = {
    opDesc match {
      case Height => r.wasUsingBlockchainContext = true
      case LastBlockUtxoRootHash => r.wasUsingBlockchainContext = true
      case MinerPubkey => r.wasUsingBlockchainContext = true
      case _ =>
    }

    obj
  }
}
