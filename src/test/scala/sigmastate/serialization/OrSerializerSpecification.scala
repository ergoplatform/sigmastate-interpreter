package sigmastate.serialization

import sigmastate.OR
import sigmastate.Values.BooleanConstant

class OrSerializerSpecification extends TableSerializationSpecification {

  def boolConst(b: Boolean): BooleanConstant = BooleanConstant.fromBoolean(b)

  override def objects = Table(
    ("object", "bytes"),
    (OR(boolConst(true),boolConst(true)), Array[Byte](39, 20, 0, 2, 2, 12, 12)),
    (OR(boolConst(true),boolConst(false)), Array[Byte](39, 20, 0, 2, 2, 12, 13)),
    (OR(boolConst(false),boolConst(true)), Array[Byte](39, 20, 0, 2, 2, 13, 12)),
    (OR(boolConst(false),boolConst(false)), Array[Byte](39, 20, 0, 2, 2, 13, 13))
  )


  tableRoundTripTest("Or: Serializer round trip")
  tablePredefinedBytesTest("Or: deserialize from predefined bytes")
}
