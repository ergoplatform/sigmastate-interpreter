package sigma.serialization

import sigma.ast.{Height, LastBlockUtxoRootHash, MinerPubkey, SType, Value, ValueCompanion}

case class CaseObjectSerialization[V <: Value[SType]](override val opDesc: ValueCompanion, obj: V)
  extends ValueSerializer[V] {

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
