package sigma.compiler.ir

/** A slice in the Scalan cake with base classes for various descriptors. */
trait Entities extends TypeDescs { self: IRContext =>

  /** Base class for all descriptors of staged traits.
    * See derived classes in `impl` packages.
    */
  abstract class EntityElem[A] extends Elem[A] with scala.Equals {
    /** Optional parent type in inheritance hierarchy */
    def parent: Option[Elem[_]] = None
    /** Name of the entity type without `Elem` suffix. */
    def entityName: String = {
      val n = sigma.reflection.Platform.safeSimpleName(this.getClass).stripSuffix("Elem")
      n
    }
    def convert(x: Ref[Def[_]]): Ref[A] = !!!("should not be called")
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

  /** Base class for all descriptors of staged traits with one type parameter. */
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

  /** Base class for all descriptors of staged classes. */
  trait ConcreteElem[TData, TClass] extends EntityElem[TClass]

  /** Base class for all descriptors of staged classes with one type parameter.
    * Note, it doesn't inherit from ConcreteElem*/
  trait ConcreteElem1[A, TData, TClass, C[_]]
    extends EntityElem1[A, TClass, C] { eClass =>
  }

  /** Base class for all descriptors of staged companions */
  abstract class CompanionElem[T] extends Elem[T] { _: scala.Equals =>
    override def buildTypeArgs = EmptyTypeArgs
  }
}
