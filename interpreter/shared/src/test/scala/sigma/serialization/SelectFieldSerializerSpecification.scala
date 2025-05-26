package sigma.serialization

import org.scalacheck.Gen
import sigma.ast.syntax.CollectionOps
import sigma.ast.{FalseLeaf, IntConstant, SelectField, Tuple}
import sigma.serialization.OpCodes.{SelectFieldCode, TupleCode}

class SelectFieldSerializerSpecification extends TableSerializationSpecification {

  property("SelectField: Serializer round trip ") {
    forAll(tupleGen(2, 10)) { tuple: Tuple =>
      // TODO refactor: avoid usage of extension method `length`
      val index = Gen.chooseNum(1, tuple.length - 1).sample.get
      roundTripTest(SelectField(tuple, index.toByte))
    }
  }

  override def objects = Table(
    ("object", "bytes"),
    (SelectField(Tuple(IntConstant(1), FalseLeaf), 2),
      Array[Byte](SelectFieldCode, TupleCode, 2, 4, 2, 1, 0, 2))
  )

  tableRoundTripTest("Specific objects serializer round trip")
  tablePredefinedBytesTest("Specific objects deserialize from predefined bytes")

}
