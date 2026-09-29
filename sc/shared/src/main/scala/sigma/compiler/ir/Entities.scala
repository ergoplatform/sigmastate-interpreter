package sigma.compiler.ir

/** A slice in the Scalan cake with base classes for various descriptors. */
trait Entities extends TypeDescs { self: IRContext =>

  /** Base class for the descriptors of the DSL types, see [[Elems]]. */
  abstract class EntityElem[A] extends Elem[A] with scala.Equals {
    /** Name of the entity type without `Elem` suffix. */
    def entityName: String = {
      val n = sigma.reflection.Platform.safeSimpleName(this.getClass).stripSuffix("Elem")
      n
    }
    def canEqual(other: Any) = other.isInstanceOf[EntityElem[_]]

    override def equals(other: Any) = (this.eq(other.asInstanceOf[AnyRef])) || (other match {
      case other: EntityElem[_] =>
          other.canEqual(this) &&
            this.getClass == other.getClass &&
            this.typeArgsDescs == other.typeArgsDescs
      case _ => false
    })

    override def hashCode = getClass.hashCode() * 31 + typeArgsDescs.hashCode()
  }

  /** Base class for descriptors with one type parameter and a container (only `ThunkElem` today). */
  abstract class EntityElem1[A, To, C[_]](val eItem: Elem[A], val cont: Cont[C])
    extends EntityElem[To] {
    override def getName(f: TypeDesc => String) = {
      s"$entityName[${f(eItem)}]"
    }
    override def canEqual(other: Any) = other match {
      case _: EntityElem1[_, _, _] => true
      case _ => false
    }
    override def equals(other: Any) = (this eq other.asInstanceOf[AnyRef]) || (other match {
      case other: EntityElem1[_,_,_] =>
        other.canEqual(this) && cont == other.cont && eItem == other.eItem
      case _ => false
    })
    override def hashCode = eItem.hashCode * 41 + cont.hashCode
  }
}
