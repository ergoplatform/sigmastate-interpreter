package scalan.core

import sigma.BaseTests
import sigma.reflection.ReflectionData.registerClassEntry
import sigma.reflection.{RClass, mkMethod}

class ScalaNameUtilSuite extends BaseTests {
  def +\() = ???

  import ScalaNameUtil._
  registerClassEntry(classOf[ScalaNameUtilSuite],
    methods = Map(
      mkMethod(classOf[ScalaNameUtilSuite], """+\""", Array[Class[_]]()) { (obj, args) =>
        obj.asInstanceOf[ScalaNameUtilSuite].+\()
      }))

  test("Operator names should be decoded correctly") {
    cleanScalaName("$plus$bslash$up") shouldEqual("""+\^""")
  }

  test("Method names obtained by reflection should be decoded") {
    val methodNames = RClass(classOf[ScalaNameUtilSuite]).getDeclaredMethods.map {
      m => cleanScalaName(m.getName)
    }.toList.filterNot(n => n.startsWith("$"))

    methodNames should equal(List("""+\"""))
  }

  test("extract package and name") {
    val name = "com.my.Class"
    PackageAndName.unapply(name) should equal(Some((List("com", "my"), "Class")))
  }
}
