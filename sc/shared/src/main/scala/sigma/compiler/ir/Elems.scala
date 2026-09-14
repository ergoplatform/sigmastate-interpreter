package sigma.compiler.ir

/** Type descriptors of the ErgoScript DSL types in the graph IR. The IR type of a DSL value is
  * its runtime `sigma.*` type; these descriptors relate it to `SType` through `stypeToElem` and
  * `elemToSType` in [[GraphBuilding]]. One object per type keeps the `import Header._` style
  * import sites working. Populated entity by entity as the staged wrappers are removed.
  */
trait Elems extends Entities { self: IRContext =>

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
