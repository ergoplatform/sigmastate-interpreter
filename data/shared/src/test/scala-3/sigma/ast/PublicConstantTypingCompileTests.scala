package sigma.ast

import scala.compiletime.testing.typeCheckErrors
import sigma.BaseTests

class PublicConstantTypingCompileTests extends BaseTests {
  test("the public Int constant factory requires an Int value") {
    val valid = typeCheckErrors("""
      import sigma.ast.{Constant, SInt}
      val constant = Constant[SInt.type](42, SInt)
      val value: Int = constant.value
    """)
    val wrongValue = typeCheckErrors("""
      import sigma.ast.{Constant, SInt}
      val constant = Constant[SInt.type]("forty-two", SInt)
    """)

    assert(valid.isEmpty, valid.mkString("\n"))
    assert(wrongValue.nonEmpty)
  }

  test("nested constant factories preserve the collection's optional element type") {
    val valid = typeCheckErrors("""
      import sigma._
      import sigma.ast._
      val ints: Coll[Option[Int]] = Colls.fromItems[Option[Int]](Some(7), None)
      val intType = SCollection[SOption[SInt.type]](SOption[SInt.type](SInt))
      val intConstant = Constant[SCollection[SOption[SInt.type]]](ints, intType)
      val intValue: Coll[Option[Int]] = intConstant.value

      val strings: Coll[Option[String]] = Colls.fromItems[Option[String]](Some("seven"), None)
      val stringType = SCollection[SOption[SString.type]](SOption[SString.type](SString))
      val stringConstant = Constant[SCollection[SOption[SString.type]]](strings, stringType)
      val stringValue: Coll[Option[String]] = stringConstant.value
    """)
    val wrongElement = typeCheckErrors("""
      import sigma._
      import sigma.ast._
      val strings: Coll[Option[String]] = Colls.fromItems[Option[String]](Some("seven"), None)
      val intType = SCollection[SOption[SInt.type]](SOption[SInt.type](SInt))
      val constant = Constant[SCollection[SOption[SInt.type]]](strings, intType)
    """)

    assert(valid.isEmpty, valid.mkString("\n"))
    assert(wrongElement.nonEmpty)
  }
}
