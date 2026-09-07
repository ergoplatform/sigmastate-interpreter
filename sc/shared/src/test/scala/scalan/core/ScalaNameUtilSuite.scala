package scalan.core

import sigma.BaseTests
import sigma.reflection.ReflectionData.registerClassEntry
import sigma.reflection.{RClass, mkMethod}

private[core] final class ScalaNameUtilFixture {
  def +\() = ???
}

class ScalaNameUtilSuite extends BaseTests {
  import ScalaNameUtil._
  registerClassEntry(classOf[ScalaNameUtilFixture],
    methods = Map(
      mkMethod(classOf[ScalaNameUtilFixture], """+\""", Array[Class[_]]()) { (obj, args) =>
        obj.asInstanceOf[ScalaNameUtilFixture].+\()
      }))

  test("Operator names should be decoded correctly") {
    cleanScalaName("$plus$bslash$up") shouldEqual("""+\^""")
  }

  test("Method names obtained by reflection should be decoded") {
    val methodNames = RClass(classOf[ScalaNameUtilFixture]).getDeclaredMethods().map {
      m => cleanScalaName(m.getName)
    }.toList

    methodNames should equal(List("""+\"""))
  }

  test("extract package and name") {
    val name = "com.my.Class"
    PackageAndName.unapply(name) should equal(Some((List("com", "my"), "Class")))
  }
}
