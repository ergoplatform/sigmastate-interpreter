package sigma.ast

import sigma._

class PublicConstantTypingTests extends BaseTests {
  test("an Int constant retains its value type and supports covariance") {
    val constant: Constant[SInt.type] = Constant[SInt.type](42, SInt)
    val value: Int = constant.value
    val widened: Constant[SType] = constant
    val numeric: Constant[SNumericType] = constant

    value shouldBe 42
    assert(widened eq constant)
    widened.tpe shouldBe SInt
    widened.value shouldBe 42
    numeric.value shouldBe 42
  }

  test("nested collection and option constants retain their element types") {
    val items: Coll[Option[Int]] = Colls.fromItems[Option[Int]](Some(7), None)
    val elementType: SOption[SInt.type] = SOption[SInt.type](SInt)
    val tpe: SCollection[SOption[SInt.type]] = SCollection[SOption[SInt.type]](elementType)
    val projected: SCollection[SOption[SInt.type]]#WrappedType = items

    val constant = Constant[SCollection[SOption[SInt.type]]](projected, tpe)
    val value: Coll[Option[Int]] = constant.value
    value.length shouldBe 2
    value(0) shouldBe Some(7)
    value(1) shouldBe None

    val collectionConstant = CollectionConstant[SOption[SInt.type]](items, elementType)
    val collectionValue: Coll[Option[Int]] = collectionConstant.value
    collectionValue shouldBe items
  }
}
