package sigma.compiler.ir.wrappers.scala

import scala.language.{existentials, implicitConversions}
import sigma.compiler.ir.IRContext
import sigma.compiler.ir.wrappers.OptionWrapSpec

import scala.collection.compat.immutable.ArraySeq

package impl {
  import sigma.compiler.ir.meta.ModuleInfo
  import sigma.compiler.ir.{Base, GraphIRReflection, IRContext}
  import sigma.data.{Nullable, RType}
  import sigma.reflection.{RClass, RMethod}

  // Abs -----------------------------------
/** IR representation of Option type and methods. */
trait WOptionsDefs extends Base with WOptions {
  self: IRContext =>

class WOptionCls extends EntityObject("WOption") {
  // entityConst: single const for each entity
  import Liftables._

  case class WOptionConst[SA, A](
        constValue: Option[SA],
        lA: Liftable[SA, A]
      ) extends LiftedConst[Option[SA], WOption[A]] with WOption[A]
        with Def[WOption[A]] with WOptionConstMethods[A] {
    implicit final def eA: Elem[A] = lA.eW

    val liftable: Liftable[Option[SA], WOption[A]] = liftableOption(lA)
    val resultType: Elem[WOption[A]] = liftable.eW
  }

  trait WOptionConstMethods[A] extends WOption[A]  { thisConst: Def[_] =>
    implicit def eA: Elem[A]
    private val WOptionClass = RClass(classOf[WOption[A]])

    override def isDefined: Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        WOptionClass.getMethod("isDefined"),
        ArraySeq.empty,
        true, false, element[Boolean]))
    }

    override def filter(p: Ref[A => Boolean]): Ref[WOption[A]] = {
      asRep[WOption[A]](mkMethodCall(self,
        WOptionClass.getMethod("filter", classOf[Sym]),
        Array[AnyRef](p),
        true, false, element[WOption[A]]))
    }

    override def map[B](f: Ref[A => B]): Ref[WOption[B]] = {
      implicit val eB = f.elem.eRange
      asRep[WOption[B]](mkMethodCall(self,
        WOptionClass.getMethod("map", classOf[Sym]),
        Array[AnyRef](f),
        true, false, element[WOption[B]]))
    }

    override def getOrElse[B](default: Ref[Thunk[B]]): Ref[B] = {
      implicit val eB = default.elem.eItem
      asRep[B](mkMethodCall(self,
        WOptionClass.getMethod("getOrElse", classOf[Sym]),
        Array[AnyRef](default),
        true, false, element[B]))
    }

    override def get: Ref[A] = {
      asRep[A](mkMethodCall(self,
        WOptionClass.getMethod("get"),
        ArraySeq.empty,
        true, false, element[A]))
    }
  }

  case class LiftableOption[SA, A](lA: Liftable[SA, A])
    extends Liftable[Option[SA], WOption[A]] {
    lazy val eW: Elem[WOption[A]] = wOptionElement(lA.eW)
    lazy val sourceType: RType[Option[SA]] = {
      implicit val tagSA = lA.sourceType.asInstanceOf[RType[SA]]
      RType[Option[SA]]
    }
    def lift(x: Option[SA]): Ref[WOption[A]] = WOptionConst(x, lA)
  }
  implicit final def liftableOption[SA, A](implicit lA: Liftable[SA,A]): Liftable[Option[SA], WOption[A]] =
    LiftableOption(lA)

  private val _OptionWrapSpec = new OptionWrapSpec

  private val WOptionClass = RClass(classOf[WOption[_]])

  // entityAdapter for WOption trait
  case class WOptionAdapter[A](source: Ref[WOption[A]])
      extends Node with WOption[A]
      with Def[WOption[A]] {
    implicit lazy val eA: Elem[A] = source.elem.typeArgs("A")._1.asInstanceOf[Elem[A]]

    val resultType: Elem[WOption[A]] = element[WOption[A]]
    override def transform(t: Transformer) = WOptionAdapter[A](t(source))

    def isDefined: Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        WOptionClass.getMethod("isDefined"),
        ArraySeq.empty,
        true, true, element[Boolean]))
    }

    def filter(p: Ref[A => Boolean]): Ref[WOption[A]] = {
      asRep[WOption[A]](mkMethodCall(source,
        WOptionClass.getMethod("filter", classOf[Sym]),
        Array[AnyRef](p),
        true, true, element[WOption[A]]))
    }

    def map[B](f: Ref[A => B]): Ref[WOption[B]] = {
      implicit val eB = f.elem.eRange
      asRep[WOption[B]](mkMethodCall(source,
        WOptionClass.getMethod("map", classOf[Sym]),
        Array[AnyRef](f),
        true, true, element[WOption[B]]))
    }

    def getOrElse[B](default: Ref[Thunk[B]]): Ref[B] = {
      implicit val eB = default.elem.eItem
      asRep[B](mkMethodCall(source,
        WOptionClass.getMethod("getOrElse", classOf[Sym]),
        Array[AnyRef](default),
        true, true, element[B]))
    }

    def get: Ref[A] = {
      asRep[A](mkMethodCall(source,
        WOptionClass.getMethod("get"),
        ArraySeq.empty,
        true, true, element[A]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit final def unrefWOption[A](p: Ref[WOption[A]]): WOption[A] = {
    if (p.node.isInstanceOf[WOption[A]@unchecked]) p.node.asInstanceOf[WOption[A]]
    else
      WOptionAdapter(p)
  }

  implicit final def castWOptionElement[A](elem: Elem[WOption[A]]): WOptionElem[A, WOption[A]] =
    elem.asInstanceOf[WOptionElem[A, WOption[A]]]

  implicit lazy val containerWOption: Functor[WOption] = new Functor[WOption] {
    def lift[A](implicit evA: Elem[A]) = element[WOption[A]]
    def unlift[A](implicit eFT: Elem[WOption[A]]) =
      castWOptionElement(eFT).eA
    def unapply[T](e: Elem[_]) = e match {
      case e: WOptionElem[_,_] => Some(asElem[WOption[T]](e))
      case _ => None
    }
    def map[A,B](xs: Ref[WOption[A]])(f: Ref[A] => Ref[B]) = { implicit val eA = unlift(xs.elem); xs.map(fun(f))}
  }

  // familyElem
  class WOptionElem[A, To <: WOption[A]](implicit _eA: Elem[A])
    extends EntityElem1[A, To, WOption](_eA, container[WOption]) {
    def eA = _eA

    override val liftable: Liftables.Liftable[_, To] = asLiftable[Option[_], To](liftableOption(_eA.liftable))

    override protected def collectMethods: Map[RMethod, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredWrapperMethods(_OptionWrapSpec, RClass(classOf[WOption[A]]), Set(
        "isDefined", "filter", "map", "getOrElse", "get"
        ))
    }

    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.core.Invariant))
  }

  implicit final def wOptionElement[A](implicit eA: Elem[A]): Elem[WOption[A]] =
    cachedElemByClass(eA)(RClass(classOf[WOptionElem[A, WOption[A]]]))

  object WOptionMethods {
    object isDefined {
      def unapply(d: Def[_]): Nullable[Ref[WOption[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "isDefined" && receiver.elem.isInstanceOf[WOptionElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[WOption[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[WOption[A]] forSome {type A}] = unapply(exp.node)
    }

    object filter {
      def unapply(d: Def[_]): Nullable[(Ref[WOption[A]], Ref[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "filter" && receiver.elem.isInstanceOf[WOptionElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[WOption[A]], Ref[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[WOption[A]], Ref[A => Boolean]) forSome {type A}] = unapply(exp.node)
    }

    object map {
      def unapply(d: Def[_]): Nullable[(Ref[WOption[A]], Ref[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "map" && receiver.elem.isInstanceOf[WOptionElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[WOption[A]], Ref[A => B]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[WOption[A]], Ref[A => B]) forSome {type A; type B}] = unapply(exp.node)
    }

    object getOrElse {
      def unapply(d: Def[_]): Nullable[(Ref[WOption[A]], Ref[Thunk[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "getOrElse" && receiver.elem.isInstanceOf[WOptionElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[WOption[A]], Ref[Thunk[B]]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[WOption[A]], Ref[Thunk[B]]) forSome {type A; type B}] = unapply(exp.node)
    }

    object get {
      def unapply(d: Def[_]): Nullable[Ref[WOption[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "get" && receiver.elem.isInstanceOf[WOptionElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[WOption[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[WOption[A]] forSome {type A}] = unapply(exp.node)
    }
  }
} // of object WOption

  registerModule(WOptionsModule)

  object WOption extends WOptionCls

  registerEntityObject("WOption", WOption)
}

object WOptionsModule extends ModuleInfo("sigma.compiler.ir.wrappers.scala", "WOptions") {
  val reflection = GraphIRReflection
}
}

trait WOptionsModule extends sigma.compiler.ir.wrappers.scala.impl.WOptionsDefs {self: IRContext =>}
