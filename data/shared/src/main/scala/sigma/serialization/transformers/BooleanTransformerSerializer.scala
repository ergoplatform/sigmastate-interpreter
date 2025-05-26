package sigma.serialization.transformers

import sigma.ast.syntax.SValue
import sigma.ast.{BooleanTransformer, BooleanTransformerCompanion, Value}
import sigma.ast.syntax._
import sigma.serialization.{SigmaByteReader, SigmaByteWriter, ValueSerializer}
import sigma.serialization.SigmaByteWriter._
import sigma.ast.{SBoolean, SCollection, SFunc, SType}
import sigma.serialization.CoreByteWriter.DataInfo

case class BooleanTransformerSerializer[T <: SType]
    (opDesc: BooleanTransformerCompanion,
     f: (Value[SCollection[T]], Value[SFunc]) => Value[SBoolean.type]) extends ValueSerializer[BooleanTransformer[T]] {
  val inputInfo: DataInfo[SValue] = opDesc.argInfos(0)
  val conditionInfo: DataInfo[SValue] = opDesc.argInfos(1)

  override def serialize(obj: BooleanTransformer[T], w: SigmaByteWriter): Unit =
    w.putValue(obj.input, inputInfo)
      .putValue(obj.condition, conditionInfo)

  override def parse(r: SigmaByteReader): Value[SBoolean.type] = {
    val input = r.getValue().asCollection[T]
    val condition = r.getValue().asFunc
    f(input, condition)
  }

}
