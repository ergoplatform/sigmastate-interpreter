package sigma.compiler.ir.wrappers.sigma

import sigma.compiler.ir.IRContext
import scala.language.{existentials, implicitConversions}
import sigma.data.RType
import sigma.compiler.ir.wrappers.RTypeWrapSpec

import scala.collection.compat.immutable.ArraySeq

package impl {
  import sigma.compiler.ir.meta.ModuleInfo
  import sigma.compiler.ir.wrappers.sigma.WRTypes
  import sigma.compiler.ir.{Base, GraphIRReflection, IRContext}
  import sigma.reflection.{RClass, RMethod}

/** Implementation of IR nodes related to [[RType]]. */
  // Abs -----------------------------------
trait WRTypesDefs extends Base with WRTypes {
  self: IRContext =>

  registerModule(WRTypesModule)

/** IR implementation for RType methods.
  * Prefix `W` means that this is implementation of IR wrapper over RType.
  * @see [[EntityObject]], [[WRType]]
  */
class WRTypeCls extends EntityObject("WRType") {
  // entityConst: single const for each entity
  import Liftables._

  case class WRTypeConst[SA, A](
        constValue: RType[SA],
        lA: Liftable[SA, A]
      ) extends LiftedConst[RType[SA], WRType[A]] with WRType[A]
        with Def[WRType[A]] with WRTypeConstMethods[A] {
    implicit final def eA: Elem[A] = lA.eW

    val liftable: Liftable[RType[SA], WRType[A]] = liftableRType(lA)
    val resultType: Elem[WRType[A]] = liftable.eW
  }

  trait WRTypeConstMethods[A] extends WRType[A]  { thisConst: Def[_] =>
    implicit def eA: Elem[A]
    private val WRTypeClass = RClass(classOf[WRType[A]])

    override def name: Ref[String] = {
      asRep[String](mkMethodCall(self,
        WRTypeClass.getMethod("name"),
        ArraySeq.empty,
        true, false, element[String]))
    }
  }

  case class LiftableRType[SA, A](lA: Liftable[SA, A])
    extends Liftable[RType[SA], WRType[A]] {
    lazy val eW: Elem[WRType[A]] = wRTypeElement(lA.eW)
    lazy val sourceType: RType[RType[SA]] = {
            implicit val tagSA = lA.sourceType.asInstanceOf[RType[SA]]
      RType[RType[SA]]
    }
    def lift(x: RType[SA]): Ref[WRType[A]] = WRTypeConst(x, lA)
  }
  implicit final def liftableRType[SA, A](implicit lA: Liftable[SA,A]): Liftable[RType[SA], WRType[A]] =
    LiftableRType(lA)

  private val _RTypeWrapSpec = new RTypeWrapSpec

  private val WRTypeClass = RClass(classOf[WRType[_]])

  // entityAdapter for WRType trait
  case class WRTypeAdapter[A](source: Ref[WRType[A]])
      extends Node with WRType[A]
      with Def[WRType[A]] {
    implicit lazy val eA: Elem[A] = source.elem.typeArgs("A")._1.asInstanceOf[Elem[A]]

    val resultType: Elem[WRType[A]] = element[WRType[A]]
    override def transform(t: Transformer) = WRTypeAdapter[A](t(source))

    def name: Ref[String] = {
      asRep[String](mkMethodCall(source,
        WRTypeClass.getMethod("name"),
        ArraySeq.empty,
        true, true, element[String]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit final def unrefWRType[A](p: Ref[WRType[A]]): WRType[A] = {
    if (p.node.isInstanceOf[WRType[A]@unchecked]) p.node.asInstanceOf[WRType[A]]
    else
      WRTypeAdapter(p)
  }

  // familyElem
  class WRTypeElem[A, To <: WRType[A]](implicit _eA: Elem[A])
    extends EntityElem[To] {
    def eA = _eA

    override val liftable: Liftables.Liftable[_, To] = asLiftable[RType[_], To](liftableRType(_eA.liftable))

    override protected def collectMethods: Map[RMethod, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredWrapperMethods(_RTypeWrapSpec, RClass(classOf[WRType[A]]), Set("name"))
    }

    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.core.Invariant))
  }

  implicit final def wRTypeElement[A](implicit eA: Elem[A]): Elem[WRType[A]] =
    cachedElemByClass(eA)(RClass(classOf[WRTypeElem[A, WRType[A]]]))

} // of object WRType
object WRType extends WRTypeCls
registerEntityObject("WRType", WRType)

}

object WRTypesModule extends ModuleInfo("wrappers.scalan", "WRTypes") {
  val reflection = GraphIRReflection
}
}

trait WRTypesModule extends sigma.compiler.ir.wrappers.sigma.impl.WRTypesDefs {self: IRContext =>}
