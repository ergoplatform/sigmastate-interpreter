package sigma.serialization

import sigma.ast.{DeserializationSigmaBuilder, SType, SigmaBuilder}
import sigma.ast.{Constant, IntConstant}

class ConstantStoreSpecification extends SerializationSpecification {

  implicit val builder: SigmaBuilder = DeserializationSigmaBuilder

  property("empty store should have no constants") {
    val s = new ConstantStore()
    s.getAll shouldBe empty
  }

  property("put/get constant") {
    val s = new ConstantStore()
    val c = IntConstant(1)
    val ph = s.put(c)
    ph.tpe shouldEqual c.tpe
    s.get(ph.id) shouldEqual c
    s.getAll.size shouldBe 1
    s.getAll.head shouldEqual c
  }

  property("same constant put should produce a new placeholder(index)") {
    // since the same constant in different places of the tree might have different semantics
    val s = new ConstantStore()
    val c = IntConstant(1)
    val ph1 = s.put(c)
    val ph2 = s.put(c)
    ph1 should not equal ph2
    s.getAll.size shouldBe 2
    s.get(ph1.id) shouldEqual c
    s.get(ph2.id) shouldEqual c
  }

  property("pass constants on instantiation, should be accessible by index") {
    val c1 = IntConstant(1).asInstanceOf[Constant[SType]]
    val c2 = IntConstant(2).asInstanceOf[Constant[SType]]
    val constants = IndexedSeq(c1, c2)
    val s = new ConstantStore(constants)
    s.getAll.size shouldBe 2
    s.get(0) shouldEqual c1
    s.get(1) shouldEqual c2
  }

}
