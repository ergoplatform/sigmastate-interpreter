package sigma.serialization.transformers

import sigma.ast.syntax.SValue
import sigma.ast.{Fold, Value}
import sigma.ast.syntax._
import sigma.serialization.{SigmaByteReader, SigmaByteWriter, ValueSerializer}
import sigma.serialization.SigmaByteWriter._
import sigma.ast.{SCollection, SFunc, SType}
import sigma.serialization.CoreByteWriter.DataInfo

case class FoldSerializer(cons: (Value[SCollection[SType]], Value[SType], Value[SFunc]) => Value[SType])
  extends ValueSerializer[Fold[SType, SType]] {
  override def opDesc = Fold
  import sigma.ast.Operations.FoldInfo._
  val thisInfo: DataInfo[SValue] = thisArg
  val zeroInfo: DataInfo[SValue] = zeroArg
  val opInfo: DataInfo[SValue] = opArg

  override def serialize(obj: Fold[SType, SType], w: SigmaByteWriter): Unit = {
    w.putValue(obj.input, thisInfo)
      .putValue(obj.zero, zeroInfo)
      .putValue(obj.foldOp, opInfo)
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val input  = r.getValue().asCollection[SType]
    val zero   = r.getValue()
    val foldOp = r.getValue().asFunc
    cons(input, zero, foldOp)
  }
}
