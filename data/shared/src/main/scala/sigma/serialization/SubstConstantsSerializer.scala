package sigma.serialization

import sigma.ast.SCollection.{SByteArray, SIntArray}
import sigma.ast.{SubstConstants, Value}
import sigma.ast.syntax._
import SigmaByteWriter._
import sigma.ast.{SCollection, SType}
import sigma.serialization.CoreByteWriter.DataInfo

object SubstConstantsSerializer extends ValueSerializer[SubstConstants[SType]] {
  import sigma.ast.Operations.SubstConstantsInfo._
  override def opDesc = SubstConstants
  val scriptBytesInfo: DataInfo[SValue] = scriptBytesArg
  val positionsInfo: DataInfo[SValue] = positionsArg
  val newValuesInfo: DataInfo[SValue] = newValuesArg

  override protected def getValueChildren(
      obj: SubstConstants[SType]): IndexedSeq[Value[SType]] =
    IndexedSeq(obj.scriptBytes, obj.positions, obj.newValues)

  override protected def rebuildValueNode(
      obj: SubstConstants[SType],
      children: IndexedSeq[Value[SType]]): Value[SType] =
    SubstConstants(
      children(0).asValue[SByteArray],
      children(1).asValue[SIntArray],
      children(2).asValue[SCollection[SType]])

  def serialize(obj: SubstConstants[SType], w: SigmaByteWriter): Unit = {
    w.putValue(obj.scriptBytes, scriptBytesInfo)
    w.putValue(obj.positions, positionsInfo)
    w.putValue(obj.newValues, newValuesInfo)
  }

  def parse(r: SigmaByteReader): Value[SType] = {
    val scriptBytes = r.getValue().asValue[SByteArray]
    val positions = r.getValue().asValue[SIntArray]
    val newVals = r.getValue().asValue[SCollection[SType]]
    SubstConstants(scriptBytes, positions, newVals)
  }
}
