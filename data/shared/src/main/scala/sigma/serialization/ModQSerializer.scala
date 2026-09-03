package sigma.serialization

import sigma.ast.{ModQ, SType}
import sigma.ast.Value
import SigmaByteWriter._
import sigma.ast.syntax.ValueOps

// TODO v6.0: make sure it is covered with tests (see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/327)
object ModQSerializer extends ValueSerializer[ModQ] {
  override def opDesc = ModQ

  override protected def getValueChildren(obj: ModQ): IndexedSeq[Value[SType]] =
    IndexedSeq(obj.input)

  override protected def rebuildValueNode(
      obj: ModQ,
      children: IndexedSeq[Value[SType]]): Value[SType] = ModQ(children(0).asBigInt)

  def serialize(obj: ModQ, w: SigmaByteWriter): Unit = {
    w.putValue(obj.input, "this")
  }

  def parse(r: SigmaByteReader): Value[SType] = {
    val p = r.getValue().asBigInt
    ModQ(p)
  }
}
