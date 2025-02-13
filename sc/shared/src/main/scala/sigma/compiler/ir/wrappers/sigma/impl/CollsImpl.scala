package sigma.compiler.ir.wrappers.sigma

import scala.language.{existentials, implicitConversions}
import sigma.compiler.ir.IRContext
import sigma.compiler.ir.wrappers.sigma.impl.CollsDefs

import scala.collection.compat.immutable.ArraySeq

package impl {
  import sigma.compiler.ir.meta.ModuleInfo
  import sigma.compiler.ir.{Base, GraphIRReflection, IRContext}
  import sigma.data.{Nullable, RType}
  import sigma.reflection.{RClass, RMethod}

// Abs -----------------------------------
/** Implementation part of IR represenation related to Coll type and methods. */
trait CollsDefs extends Base with Colls {
  self: IRContext =>

  registerModule(CollsModule)

import Coll._
import CollBuilder._
import WOption._

class CollCls extends EntityObject("Coll") {
  // entityConst: single const for each entity
  import Liftables._
  type SColl[A] = sigma.Coll[A]
  case class CollConst[SA, A](
        constValue: SColl[SA],
        lA: Liftable[SA, A]
      ) extends LiftedConst[SColl[SA], Coll[A]] with Coll[A]
        with Def[Coll[A]] with CollConstMethods[A] {
    implicit final def eA: Elem[A] = lA.eW

    val liftable: Liftable[SColl[SA], Coll[A]] = liftableColl(lA)
    val resultType: Elem[Coll[A]] = liftable.eW
  }

  trait CollConstMethods[A] extends Coll[A]  { thisConst: Def[_] =>
    implicit def eA: Elem[A]
    private val CollClass = RClass(classOf[Coll[A]])

    override def length: Ref[Int] = {
      asRep[Int](mkMethodCall(self,
        CollClass.getMethod("length"),
        ArraySeq.empty,
        true, false, element[Int]))
    }

    override def apply(i: Ref[Int]): Ref[A] = {
      asRep[A](mkMethodCall(self,
        CollClass.getMethod("apply", classOf[Sym]),
        Array[AnyRef](i),
        true, false, element[A]))
    }

    override def getOrElse(index: Ref[Int], default: Ref[A]): Ref[A] = {
      asRep[A](mkMethodCall(self,
        CollClass.getMethod("getOrElse", classOf[Sym], classOf[Sym]),
        Array[AnyRef](index, default),
        true, false, element[A]))
    }

    override def get(index: Ref[Int]): Ref[WOption[A]] = {
      asRep[WOption[A]](mkMethodCall(self,
        CollClass.getMethod("get", classOf[Sym]),
        Array[AnyRef](index),
        true, false, element[WOption[A]]))
    }


    override def map[B](f: Ref[A => B]): Ref[Coll[B]] = {
      implicit val eB = f.elem.eRange
      asRep[Coll[B]](mkMethodCall(self,
        CollClass.getMethod("map", classOf[Sym]),
        Array[AnyRef](f),
        true, false, element[Coll[B]]))
    }

    override def zip[B](ys: Ref[Coll[B]]): Ref[Coll[(A, B)]] = {
      implicit val eB = ys.eA
      asRep[Coll[(A, B)]](mkMethodCall(self,
        CollClass.getMethod("zip", classOf[Sym]),
        Array[AnyRef](ys),
        true, false, element[Coll[(A, B)]]))
    }

    override def exists(p: Ref[A => Boolean]): Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        CollClass.getMethod("exists", classOf[Sym]),
        Array[AnyRef](p),
        true, false, element[Boolean]))
    }

    override def forall(p: Ref[A => Boolean]): Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        CollClass.getMethod("forall", classOf[Sym]),
        Array[AnyRef](p),
        true, false, element[Boolean]))
    }

    override def filter(p: Ref[A => Boolean]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        CollClass.getMethod("filter", classOf[Sym]),
        Array[AnyRef](p),
        true, false, element[Coll[A]]))
    }

    override def foldLeft[B](zero: Ref[B], op: Ref[((B, A)) => B]): Ref[B] = {
      implicit val eB = zero.elem
      asRep[B](mkMethodCall(self,
        CollClass.getMethod("foldLeft", classOf[Sym], classOf[Sym]),
        Array[AnyRef](zero, op),
        true, false, element[B]))
    }

    override def indices: Ref[Coll[Int]] = {
      asRep[Coll[Int]](mkMethodCall(self,
        CollClass.getMethod("indices"),
        ArraySeq.empty,
        true, false, element[Coll[Int]]))
    }

    override def flatMap[B](f: Ref[A => Coll[B]]): Ref[Coll[B]] = {
      implicit val eB = f.elem.eRange.typeArgs("A")._1.asInstanceOf[Elem[B]]
      asRep[Coll[B]](mkMethodCall(self,
        CollClass.getMethod("flatMap", classOf[Sym]),
        Array[AnyRef](f),
        true, false, element[Coll[B]]))
    }

    override def indexOf(elem: Ref[A], from: Ref[Int]): Ref[Int] = {
      asRep[Int](mkMethodCall(self,
        CollClass.getMethod("indexOf", classOf[Sym], classOf[Sym]),
        Array[AnyRef](elem, from),
        true, false, element[Int]))
    }

    override def patch(from: Ref[Int], patch: Ref[Coll[A]], replaced: Ref[Int]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        CollClass.getMethod("patch", classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](from, patch, replaced),
        true, false, element[Coll[A]]))
    }

    override def updated(index: Ref[Int], elem: Ref[A]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        CollClass.getMethod("updated", classOf[Sym], classOf[Sym]),
        Array[AnyRef](index, elem),
        true, false, element[Coll[A]]))
    }

    override def updateMany(indexes: Ref[Coll[Int]], values: Ref[Coll[A]]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        CollClass.getMethod("updateMany", classOf[Sym], classOf[Sym]),
        Array[AnyRef](indexes, values),
        true, false, element[Coll[A]]))
    }

    override def slice(from: Ref[Int], until: Ref[Int]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        CollClass.getMethod("slice", classOf[Sym], classOf[Sym]),
        Array[AnyRef](from, until),
        true, false, element[Coll[A]]))
    }

    override def append(other: Ref[Coll[A]]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        CollClass.getMethod("append", classOf[Sym]),
        Array[AnyRef](other),
        true, false, element[Coll[A]]))
    }

    override def reverse: Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        CollClass.getMethod("reverse"),
        Array[AnyRef](),
        true, false, element[Coll[A]]))
    }

    override def distinct: Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        CollClass.getMethod("distinct"),
        Array[AnyRef](),
        true, false, element[Coll[A]]))
    }

    def startsWith(ys: Ref[Coll[A]]): Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        CollClass.getMethod("startsWith", classOf[Sym]),
        Array[AnyRef](ys),
        true, false, element[Boolean]))
    }

    def endsWith(ys: Ref[Coll[A]]): Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        CollClass.getMethod("endsWith", classOf[Sym]),
        Array[AnyRef](ys),
        true, false, element[Boolean]))
    }
  }

  case class LiftableColl[SA, A](lA: Liftable[SA, A])
    extends Liftable[SColl[SA], Coll[A]] {
    lazy val eW: Elem[Coll[A]] = collElement(lA.eW)
    lazy val sourceType: RType[SColl[SA]] = {
      implicit val tagSA: RType[SA] = lA.sourceType
      RType[SColl[SA]]
    }
    def lift(x: SColl[SA]): Ref[Coll[A]] = CollConst(x, lA)
  }
  implicit final def liftableColl[SA, A](implicit lA: Liftable[SA,A]): Liftable[SColl[SA], Coll[A]] =
    LiftableColl(lA)

  private val CollClass = RClass(classOf[Coll[_]])

  // entityAdapter for Coll trait
  case class CollAdapter[A](source: Ref[Coll[A]])
      extends Node with Coll[A]
      with Def[Coll[A]] {
    implicit lazy val eA: Elem[A] = source.elem.typeArgs("A")._1.asInstanceOf[Elem[A]]

    val resultType: Elem[Coll[A]] = element[Coll[A]]
    override def transform(t: Transformer) = CollAdapter[A](t(source))

    def length: Ref[Int] = {
      asRep[Int](mkMethodCall(source,
        CollClass.getMethod("length"),
        ArraySeq.empty,
        true, true, element[Int]))
    }

    def apply(i: Ref[Int]): Ref[A] = {
      asRep[A](mkMethodCall(source,
        CollClass.getMethod("apply", classOf[Sym]),
        Array[AnyRef](i),
        true, true, element[A]))
    }

    def getOrElse(index: Ref[Int], default: Ref[A]): Ref[A] = {
      asRep[A](mkMethodCall(source,
        CollClass.getMethod("getOrElse", classOf[Sym], classOf[Sym]),
        Array[AnyRef](index, default),
        true, true, element[A]))
    }

    def get(index: Ref[Int]): Ref[WOption[A]] = {
      asRep[WOption[A]](mkMethodCall(source,
        CollClass.getMethod("get", classOf[Sym]),
        Array[AnyRef](index),
        true, true, element[WOption[A]]))
    }

    def map[B](f: Ref[A => B]): Ref[Coll[B]] = {
      implicit val eB = f.elem.eRange
      asRep[Coll[B]](mkMethodCall(source,
        CollClass.getMethod("map", classOf[Sym]),
        Array[AnyRef](f),
        true, true, element[Coll[B]]))
    }

    def zip[B](ys: Ref[Coll[B]]): Ref[Coll[(A, B)]] = {
      implicit val eB = ys.eA
      asRep[Coll[(A, B)]](mkMethodCall(source,
        CollClass.getMethod("zip", classOf[Sym]),
        Array[AnyRef](ys),
        true, true, element[Coll[(A, B)]]))
    }

    def exists(p: Ref[A => Boolean]): Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        CollClass.getMethod("exists", classOf[Sym]),
        Array[AnyRef](p),
        true, true, element[Boolean]))
    }

    def forall(p: Ref[A => Boolean]): Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        CollClass.getMethod("forall", classOf[Sym]),
        Array[AnyRef](p),
        true, true, element[Boolean]))
    }

    def filter(p: Ref[A => Boolean]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        CollClass.getMethod("filter", classOf[Sym]),
        Array[AnyRef](p),
        true, true, element[Coll[A]]))
    }

    def foldLeft[B](zero: Ref[B], op: Ref[((B, A)) => B]): Ref[B] = {
      implicit val eB = zero.elem
      asRep[B](mkMethodCall(source,
        CollClass.getMethod("foldLeft", classOf[Sym], classOf[Sym]),
        Array[AnyRef](zero, op),
        true, true, element[B]))
    }

    def indices: Ref[Coll[Int]] = {
      asRep[Coll[Int]](mkMethodCall(source,
        CollClass.getMethod("indices"),
        ArraySeq.empty,
        true, true, element[Coll[Int]]))
    }

    def flatMap[B](f: Ref[A => Coll[B]]): Ref[Coll[B]] = {
      implicit val eB = f.elem.eRange.typeArgs("A")._1.asInstanceOf[Elem[B]]
      asRep[Coll[B]](mkMethodCall(source,
        CollClass.getMethod("flatMap", classOf[Sym]),
        Array[AnyRef](f),
        true, true, element[Coll[B]]))
    }

    override def indexOf(elem: Ref[A], from: Ref[Int]): Ref[Int] = {
      asRep[Int](mkMethodCall(source,
        CollClass.getMethod("indexOf", classOf[Sym], classOf[Sym]),
        Array[AnyRef](elem, from),
        true, true, element[Int]))
    }

    def patch(from: Ref[Int], patch: Ref[Coll[A]], replaced: Ref[Int]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        CollClass.getMethod("patch", classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](from, patch, replaced),
        true, true, element[Coll[A]]))
    }

    def updated(index: Ref[Int], elem: Ref[A]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        CollClass.getMethod("updated", classOf[Sym], classOf[Sym]),
        Array[AnyRef](index, elem),
        true, true, element[Coll[A]]))
    }

    def updateMany(indexes: Ref[Coll[Int]], values: Ref[Coll[A]]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        CollClass.getMethod("updateMany", classOf[Sym], classOf[Sym]),
        Array[AnyRef](indexes, values),
        true, true, element[Coll[A]]))
    }

    def slice(from: Ref[Int], until: Ref[Int]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        CollClass.getMethod("slice", classOf[Sym], classOf[Sym]),
        Array[AnyRef](from, until),
        true, true, element[Coll[A]]))
    }

    def append(other: Ref[Coll[A]]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        CollClass.getMethod("append", classOf[Sym]),
        Array[AnyRef](other),
        true, true, element[Coll[A]]))
    }

    def reverse: Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        CollClass.getMethod("reverse"),
        Array[AnyRef](),
        true, true, element[Coll[A]]))
    }

    def distinct: Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        CollClass.getMethod("distinct"),
        Array[AnyRef](),
        true, true, element[Coll[A]]))
    }

    def startsWith(ys: Ref[Coll[A]]): Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        CollClass.getMethod("startsWith", classOf[Sym]),
        Array[AnyRef](ys),
        true, true, element[Boolean]))
    }

    def endsWith(ys: Ref[Coll[A]]): Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        CollClass.getMethod("endsWith", classOf[Sym]),
        Array[AnyRef](ys),
        true, true, element[Boolean]))
    }
  }

  // entityUnref: single unref method for each type family
  val createCollAdapter: Ref[Coll[Any]] => Coll[Any] = x => CollAdapter(x)

  implicit final def unrefColl[A](p: Ref[Coll[A]]): Coll[A] = {
    val sym = p.asInstanceOf[SingleRef[Coll[A]]]
    sym.getAdapter(
      p.node.isInstanceOf[Coll[A]@unchecked],
      createCollAdapter.asInstanceOf[Ref[Coll[A]] => Coll[A]])
  }

  implicit final def castCollElement[A](elem: Elem[Coll[A]]): CollElem[A, Coll[A]] =
    elem.asInstanceOf[CollElem[A, Coll[A]]]

  implicit lazy val containerColl: Functor[Coll] = new Functor[Coll] {
    def lift[A](implicit evA: Elem[A]) = element[Coll[A]]
    def unlift[A](implicit eFT: Elem[Coll[A]]) =
      castCollElement(eFT).eA
    def unapply[T](e: Elem[_]) = e match {
      case e: CollElem[_,_] => Some(asElem[Coll[T]](e))
      case _ => None
    }
    def map[A,B](xs: Ref[Coll[A]])(f: Ref[A] => Ref[B]) = { implicit val eA = unlift(xs.elem); xs.map(fun(f))}
  }

  // familyElem
  class CollElem[A, To <: Coll[A]](implicit _eA: Elem[A])
    extends EntityElem1[A, To, Coll](_eA, container[Coll]) {
    def eA = _eA

    override val liftable: Liftables.Liftable[_, To] = asLiftable[SColl[_], To](liftableColl(_eA.liftable))

    override protected def collectMethods: Map[RMethod, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(RClass(classOf[Coll[A]]), RClass(classOf[SColl[_]]), Set(
        "length", "apply", "getOrElse", "map", "zip", "exists", "forall", "filter", "foldLeft", "indices", "flatMap", "indexOf", "patch", "updated", "updateMany", "slice", "append"
        ))
    }

    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.core.Invariant))
  }

  implicit final def collElement[A](implicit eA: Elem[A]): Elem[Coll[A]] =
    cachedElemByClass(eA)(RClass(classOf[CollElem[A, Coll[A]]]))

  object CollMethods {
    object length {
      def unapply(d: Def[_]): Nullable[Ref[Coll[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "length" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[Coll[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[Coll[A]] forSome {type A}] = unapply(exp.node)
    }

    object apply {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "apply" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[Int]) forSome {type A}] = unapply(exp.node)
    }

    object getOrElse {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[Int], Ref[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "getOrElse" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[Int], Ref[A]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[Int], Ref[A]) forSome {type A}] = unapply(exp.node)
    }

    object map {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "map" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[A => B]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[A => B]) forSome {type A; type B}] = unapply(exp.node)
    }

    object zip {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[Coll[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "zip" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[Coll[B]]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[Coll[B]]) forSome {type A; type B}] = unapply(exp.node)
    }

    object exists {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "exists" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[A => Boolean]) forSome {type A}] = unapply(exp.node)
    }

    object forall {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "forall" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[A => Boolean]) forSome {type A}] = unapply(exp.node)
    }

    object filter {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "filter" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[A => Boolean]) forSome {type A}] = unapply(exp.node)
    }

    object foldLeft {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[B], Ref[((B, A)) => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "foldLeft" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[B], Ref[((B, A)) => B]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[B], Ref[((B, A)) => B]) forSome {type A; type B}] = unapply(exp.node)
    }

    object indices {
      def unapply(d: Def[_]): Nullable[Ref[Coll[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "indices" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[Coll[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[Coll[A]] forSome {type A}] = unapply(exp.node)
    }

    object flatMap {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[A => Coll[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "flatMap" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[A => Coll[B]]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[A => Coll[B]]) forSome {type A; type B}] = unapply(exp.node)
    }

    object slice {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[Int], Ref[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "slice" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[Int], Ref[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[Int], Ref[Int]) forSome {type A}] = unapply(exp.node)
    }

    object append {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[Coll[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "append" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[Coll[A]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[Coll[A]]) forSome {type A}] = unapply(exp.node)
    }

  }

} // of object Coll
object Coll extends CollCls
  registerEntityObject("Coll", Coll)

object CollBuilder extends EntityObject("CollBuilder") {
  // entityConst: single const for each entity
  import Liftables._
  type SCollBuilder = sigma.CollBuilder
  case class CollBuilderConst(
        constValue: SCollBuilder
      ) extends LiftedConst[SCollBuilder, CollBuilder] with CollBuilder
        with Def[CollBuilder] with CollBuilderConstMethods {
    val liftable: Liftable[SCollBuilder, CollBuilder] = LiftableCollBuilder
    val resultType: Elem[CollBuilder] = liftable.eW
  }

  trait CollBuilderConstMethods extends CollBuilder  { thisConst: Def[_] =>

    private val CollBuilderClass = RClass(classOf[CollBuilder])

    override def fromItems[T](items: Ref[T]*)(implicit cT: Elem[T]): Ref[Coll[T]] = {
      asRep[Coll[T]](mkMethodCall(self,
        CollBuilderClass.getMethod("fromItems", classOf[Seq[_]], classOf[Elem[_]]),
        Array[AnyRef](items, cT),
        true, false, element[Coll[T]]))
    }

    override def xor(left: Ref[Coll[Byte]], right: Ref[Coll[Byte]]): Ref[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        CollBuilderClass.getMethod("xor", classOf[Sym], classOf[Sym]),
        Array[AnyRef](left, right),
        true, false, element[Coll[Byte]]))
    }

    override def replicate[T](n: Ref[Int], v: Ref[T]): Ref[Coll[T]] = {
      implicit val eT = v.elem
      asRep[Coll[T]](mkMethodCall(self,
        CollBuilderClass.getMethod("replicate", classOf[Sym], classOf[Sym]),
        Array[AnyRef](n, v),
        true, false, element[Coll[T]]))
    }
  }

  implicit object LiftableCollBuilder
    extends Liftable[SCollBuilder, CollBuilder] {
    lazy val eW: Elem[CollBuilder] = collBuilderElement
    lazy val sourceType: RType[SCollBuilder] = {
      RType[SCollBuilder]
    }
    def lift(x: SCollBuilder): Ref[CollBuilder] = CollBuilderConst(x)
  }

  private val CollBuilderClass = RClass(classOf[CollBuilder])

  // entityAdapter for CollBuilder trait
  case class CollBuilderAdapter(source: Ref[CollBuilder])
      extends Node with CollBuilder
      with Def[CollBuilder] {
    val resultType: Elem[CollBuilder] = element[CollBuilder]
    override def transform(t: Transformer) = CollBuilderAdapter(t(source))

    def fromItems[T](items: Ref[T]*)(implicit cT: Elem[T]): Ref[Coll[T]] = {
      asRep[Coll[T]](mkMethodCall(source,
        CollBuilderClass.getMethod("fromItems", classOf[Seq[_]], classOf[Elem[_]]),
        Array[AnyRef](items, cT),
        true, true, element[Coll[T]]))
    }

    def xor(left: Ref[Coll[Byte]], right: Ref[Coll[Byte]]): Ref[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        CollBuilderClass.getMethod("xor", classOf[Sym], classOf[Sym]),
        Array[AnyRef](left, right),
        true, true, element[Coll[Byte]]))
    }

    def replicate[T](n: Ref[Int], v: Ref[T]): Ref[Coll[T]] = {
      implicit val eT = v.elem
      asRep[Coll[T]](mkMethodCall(source,
        CollBuilderClass.getMethod("replicate", classOf[Sym], classOf[Sym]),
        Array[AnyRef](n, v),
        true, true, element[Coll[T]]))
    }
  }

  // entityUnref: single unref method for each type family
  val createCollBuilderAdapter: Ref[CollBuilder] => CollBuilder = x => CollBuilderAdapter(x)

  implicit final def unrefCollBuilder(p: Ref[CollBuilder]): CollBuilder =
    p.asInstanceOf[SingleRef[CollBuilder]].getAdapter(
      p.node.isInstanceOf[CollBuilder],
      createCollBuilderAdapter.asInstanceOf[Ref[CollBuilder] => CollBuilder])

  // familyElem
  class CollBuilderElem[To <: CollBuilder]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = asLiftable[SCollBuilder, To](LiftableCollBuilder)

    override protected def collectMethods: Map[RMethod, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(RClass(classOf[CollBuilder]), RClass(classOf[SCollBuilder]), Set(
        "fromItems", "xor", "replicate"
        ))
    }
  }

  implicit lazy val collBuilderElement: Elem[CollBuilder] =
    new CollBuilderElem[CollBuilder]

  object CollBuilderMethods {
    object fromItems {
      def unapply(d: Def[_]): Nullable[(Ref[CollBuilder], Seq[Ref[T]], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "fromItems" && receiver.elem.isInstanceOf[CollBuilderElem[_]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[CollBuilder], Seq[Ref[T]], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[CollBuilder], Seq[Ref[T]], Elem[T]) forSome {type T}] = unapply(exp.node)
    }

    object xor {
      def unapply(d: Def[_]): Nullable[(Ref[CollBuilder], Ref[Coll[Byte]], Ref[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "xor" && receiver.elem.isInstanceOf[CollBuilderElem[_]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[CollBuilder], Ref[Coll[Byte]], Ref[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[CollBuilder], Ref[Coll[Byte]], Ref[Coll[Byte]])] = unapply(exp.node)
    }

    object replicate {
      def unapply(d: Def[_]): Nullable[(Ref[CollBuilder], Ref[Int], Ref[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "replicate" && receiver.elem.isInstanceOf[CollBuilderElem[_]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[CollBuilder], Ref[Int], Ref[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[CollBuilder], Ref[Int], Ref[T]) forSome {type T}] = unapply(exp.node)
    }
  }

} // of object CollBuilder
  registerEntityObject("CollBuilder", CollBuilder)

  override def resetContext(): Unit = {
    super.resetContext()
  }

}

object CollsModule extends ModuleInfo("sigma", "Colls") {
  val reflection = GraphIRReflection
}
}

trait CollsModule extends CollsDefs {self: IRContext =>}
