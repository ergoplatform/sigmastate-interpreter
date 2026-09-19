package sigma.compiler.ir

/** Type descriptors of the ErgoScript DSL types in the graph IR. The IR type of a DSL value is
  * its runtime `sigma.*` type; these descriptors relate it to `SType` through `stypeToElem` and
  * `elemToSType` in [[GraphBuilding]]. One object per type keeps the `import Header._` style
  * import sites working. Populated entity by entity as the staged wrappers are removed.
  */
trait Elems extends Entities { self: IRContext =>

  object SigmaProp {
    class SigmaPropElem extends EntityElem[sigma.SigmaProp]
    implicit lazy val sigmaPropElement: Elem[sigma.SigmaProp] = new SigmaPropElem
  }

  object BigInt {
    class BigIntElem extends EntityElem[sigma.BigInt]
    implicit lazy val bigIntElement: Elem[sigma.BigInt] = new BigIntElem
  }

  object UnsignedBigInt {
    class UnsignedBigIntElem extends EntityElem[sigma.UnsignedBigInt]
    implicit lazy val unsignedBigIntElement: Elem[sigma.UnsignedBigInt] = new UnsignedBigIntElem
  }

  object GroupElement {
    class GroupElementElem extends EntityElem[sigma.GroupElement]
    implicit lazy val groupElementElement: Elem[sigma.GroupElement] = new GroupElementElem
  }

  object WOption {
    /** Descriptor of `Option[A]`; the class name keeps `Elem.name` as `WOption[A]`. */
    class WOptionElem[A](val eItem: Elem[A]) extends EntityElem[Option[A]] {
      override def getName(f: TypeDesc => String) = s"$entityName[${f(eItem)}]"
      override def buildTypeArgs = TypeArgs("A" -> (eItem -> scalan.core.Invariant))
      override def canEqual(other: Any) = other.isInstanceOf[WOptionElem[_]]
      override def equals(other: Any) = (this eq other.asInstanceOf[AnyRef]) || (other match {
        case other: WOptionElem[_] => other.eItem == eItem
        case _ => false
      })
      override def hashCode = eItem.hashCode * 41 + 7
    }
    implicit final def wOptionElement[A](implicit eA: Elem[A]): Elem[Option[A]] =
      cachedElem(classOf[WOptionElem[_]], eA)(new WOptionElem[A](eA))
  }

  object Context {
    class ContextElem extends EntityElem[sigma.Context]
    implicit lazy val contextElement: Elem[sigma.Context] = new ContextElem
  }

  object Box {
    class BoxElem extends EntityElem[sigma.Box]
    implicit lazy val boxElement: Elem[sigma.Box] = new BoxElem
  }

  object AvlTree {
    class AvlTreeElem extends EntityElem[sigma.AvlTree]
    implicit lazy val avlTreeElement: Elem[sigma.AvlTree] = new AvlTreeElem
  }

  object Header {
    class HeaderElem extends EntityElem[sigma.Header]
    implicit lazy val headerElement: Elem[sigma.Header] = new HeaderElem
  }

  object PreHeader {
    class PreHeaderElem extends EntityElem[sigma.PreHeader]
    implicit lazy val preHeaderElement: Elem[sigma.PreHeader] = new PreHeaderElem
  }
}
