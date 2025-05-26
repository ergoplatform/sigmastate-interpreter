package sigmastate

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import sigma.compiler.ir.{IRContext, TypeDescs}
import sigma.reflection.ReflectionData.registerClassEntry
import sigma.reflection.SRConstructor

object ObjA {
  class A
  object ObjB
}

class InterpreterReflectionGeneratorTests extends AnyPropSpec with Matchers {
  import ReflectionGenerator._
  class ObjB {
    class B
  }

  def test(name: String, exp: String) = {
    val normalized = normalizeName(name)
    normalized shouldBe exp
  }

  property("sigmastate.ReflectionGenerator") {
    test("", "")
    test("A", "A")
    test("A$", "A$")
    test("A$B", "A#B")
    test("A$B$", "A#B$")
    test("scala.A$B$", "scala.A#B$")
    test("sigmastate.ObjA$A", "sigmastate.ObjA.A");
    val _ = (classOf[ObjA.A], classOf[InterpreterReflectionGeneratorTests#ObjB#B])
    test("sigmastate.ReflectionGeneratorTests$ObjB$B", "sigmastate.ReflectionGeneratorTests#ObjB#B");
    Class.forName("sigmastate.ObjA$") shouldNot be (null)
    Class.forName("sigmastate.ObjA$ObjB$") shouldNot be (null)
  }

  property("inner class") {
    val ctx = null.asInstanceOf[IRContext] // ok! type level only
    val clazz = classOf[ctx.Coll.CollElem[_, _]]
    registerClassEntry(clazz,
      constructors = Array(
        new SRConstructor[Any](Array(clazz.getDeclaringClass, classOf[TypeDescs#Elem[_]])) {
          override def newInstance(args: AnyRef*): Any = {
            val cake = args(0).asInstanceOf[ctx.Coll.type]
            new cake.CollElem()(args(1).asInstanceOf[ctx.Elem[_]])
          }
        }
      )
    )
  }
}
