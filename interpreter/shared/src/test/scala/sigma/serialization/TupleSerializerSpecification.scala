package sigma.serialization

import sigma.ast.{FalseLeaf, IntConstant, Tuple}

class TupleSerializerSpecification extends TableSerializationSpecification {

  property("Tuple: Serializer round trip ") {
    forAll(tupleGen(1, 10)) { tuple: Tuple =>
      roundTripTest(tuple)
    }
  }

  override def objects = Table(
    ("object", "bytes"),
    (Tuple(IntConstant(1), FalseLeaf),
      Array[Byte](OpCodes.TupleCode, 2, 4, 2, 1, 0))
  )

  tableRoundTripTest("Specific objects serializer round trip")
  tablePredefinedBytesTest("Specific objects deserialize from predefined bytes")

}
