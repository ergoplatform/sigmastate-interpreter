package sigma.compiler.ir

import scalan.core.{Contravariant, Covariant, Variance}
import sigma.data.{AVHashMap, Lazy, Nullable, RType}

import scala.annotation.implicitNotFound
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.language.implicitConversions

/** Defines [[Elem]] descriptor of types in IRContext together with related utilities.
  * @see TypeDesc
  */
abstract class TypeDescs extends Base { self: IRContext =>

  /** Helper type case method. */
  @inline final def asElem[T](d: TypeDesc): Elem[T] = d.asInstanceOf[Elem[T]]

  /** Type descriptor which is computed lazily on demand. */
  type LElem[A] = Lazy[Elem[A]]

  /** Immutable data environment used to assign data values to graph nodes. */
  type DataEnv = Map[Sym, AnyRef]

  /** State monad for symbols computed in a data environment.
    * `DataEnv` is used as the state of the state monad.
    */
  case class EnvRep[A](run: DataEnv => (DataEnv, Ref[A])) {
    def flatMap[B](f: Ref[A] => EnvRep[B]): EnvRep[B] = EnvRep { env =>
      val (env1, x) = run(env)
      val res = f(x).run(env1)
      res
    }
    def map[B](f: Ref[A] => Ref[B]): EnvRep[B] = EnvRep { env =>
      val (env1, x) = run(env)
      val y = f(x)
      (env1, y)
    }
  }
  object EnvRep {
    def add[T](entry: (Ref[T], AnyRef)): EnvRep[T] =
      EnvRep { env => val (sym, value) = entry; (env + (sym -> value), sym) }

    def lifted[ST, T](x: ST)(implicit lT: Liftables.Liftable[ST, T]): EnvRep[T] = EnvRep { env =>
      val xSym = lT.lift(x)
      val resEnv = env + ((xSym, x.asInstanceOf[AnyRef]))
      (resEnv, xSym)
    }
  }

  abstract class TypeDesc extends Serializable {
    def getName(f: TypeDesc => String): String
    lazy val name: String = getName(_.name)

    // <> to delimit because: [] is used inside name; {} looks bad with structs.
    override def toString = s"${sigma.reflection.Platform.safeSimpleName(getClass)}<$name>"
  }

  /** Type descriptor of staged types, which correspond to source (unstaged) RTypes
    * defined outside of IR cake.
    * @tparam A the type represented by this descriptor
    */
  @implicitNotFound(msg = "No Elem available for ${A}.")
  abstract class Elem[A] extends TypeDesc { _: scala.Equals =>
    import Liftables._
    def buildTypeArgs: ListMap[String, (TypeDesc, Variance)] = EmptyTypeArgs
    lazy val typeArgs: ListMap[String, (TypeDesc, Variance)] = buildTypeArgs
    lazy val typeArgsDescs: Seq[TypeDesc] = {
      val b = mutable.ArrayBuilder.make[TypeDesc]
      for (v <- typeArgs.valuesIterator) {
        b += v._1
      }
      b.result()
    }

    override def getName(f: TypeDesc => String) = {
      val className = this match {
        case be: BaseElemLiftable[_] =>
          be.sourceType.name
        case e =>
          val cl = e.getClass
          val name = sigma.reflection.Platform.safeSimpleName(cl).stripSuffix("Elem")
          name
      }
      if (typeArgs.isEmpty)
        className
      else {
        val typeArgString = typeArgsDescs.map(f).mkString(", ")
        s"$className[$typeArgString]"
      }
    }

    def liftable: Liftable[_, A] =
      !!!(s"Cannot get Liftable instance for $this")

    final lazy val sourceType: RType[_] = liftable.sourceType

    def <:<(e: Elem[_]) = e.getClass.isAssignableFrom(this.getClass)
  }

  object Elem {
    /** Map source type desciptor to stated type descriptor using liftable instance. */
    implicit def rtypeToElem[SA, A](tSA: RType[SA])(implicit lA: Liftables.Liftable[SA,A]): Elem[A] = lA.eW

    final def unapply[T, E <: Elem[T]](s: Ref[T]): Nullable[E] = Nullable(s.elem.asInstanceOf[E])
  }

  /** Instances of parametrised Elem classes, one per (class, args), so equal descriptors are
    * also the same object.
    */
  private val elemInstances = AVHashMap[(Class[_], Seq[AnyRef]), Elem[_]](100)

  final def cachedElem[E <: Elem[_]](clazz: Class[_], args: AnyRef*)(construct: => E): E = {
    val key = (clazz, args)
    elemInstances.get(key) match {
      case Nullable(e) => e.asInstanceOf[E]
      case _ =>
        val e = construct
        elemInstances.put(key, e)
        e
    }
  }


  final def element[A](implicit ea: Elem[A]): Elem[A] = ea

  abstract class BaseElem[A](defaultValue: A) extends Elem[A] with Serializable with scala.Equals

  /** Type descriptor for primitive types.
    * There is implicit `val` declaration for each primitive type. */
  class BaseElemLiftable[A](defaultValue: A, val tA: RType[A]) extends BaseElem[A](defaultValue) {
    override def buildTypeArgs = EmptyTypeArgs
    override val liftable = new Liftables.BaseLiftable[A]()(this, tA)
    override def canEqual(other: Any) = other.isInstanceOf[BaseElemLiftable[_]]
    override def equals(other: Any) = (this eq other.asInstanceOf[AnyRef]) || (other match {
      case other: BaseElemLiftable[_] => tA == other.tA
      case _ => false
    })
    override val hashCode = tA.hashCode
  }

  /** Type descriptor for `(A, B)` type where descriptors for `A` and `B` are given as arguments. */
  case class PairElem[A, B](eFst: Elem[A], eSnd: Elem[B]) extends Elem[(A, B)] {
    assert(eFst != null && eSnd != null)
    override def getName(f: TypeDesc => String) = s"(${f(eFst)}, ${f(eSnd)})"
    override def buildTypeArgs = ListMap("A" -> (eFst -> Covariant), "B" -> (eSnd -> Covariant))
    override def liftable: Liftables.Liftable[_, (A, B)] =
      Liftables.asLiftable[(_,_), (A,B)](Liftables.PairIsLiftable(eFst.liftable, eSnd.liftable))
  }

  /** Type descriptor for `A | B` type where descriptors for `A` and `B` are given as arguments. */
  case class SumElem[A, B](eLeft: Elem[A], eRight: Elem[B]) extends Elem[A | B] {
    override def getName(f: TypeDesc => String) = s"(${f(eLeft)} | ${f(eRight)})"
    override def buildTypeArgs = ListMap("A" -> (eLeft -> Covariant), "B" -> (eRight -> Covariant))
  }

  /** Type descriptor for `A => B` type where descriptors for `A` and `B` are given as arguments. */
  case class FuncElem[A, B](eDom: Elem[A], eRange: Elem[B]) extends Elem[A => B] {
    import Liftables._
    override def getName(f: TypeDesc => String) = s"${f(eDom)} => ${f(eRange)}"
    override def buildTypeArgs = ListMap("A" -> (eDom -> Contravariant), "B" -> (eRange -> Covariant))
    override def liftable: Liftable[_, A => B] =
      asLiftable[_ => _, A => B](FuncIsLiftable(eDom.liftable, eRange.liftable))
  }

  /** Type descriptor for `Any`, cannot be used implicitly. */
  val AnyElement: Elem[Any] = new BaseElemLiftable[Any](null, sigma.AnyType)

  /** Predefined Lazy value saved here to be used in hotspot code. */
  val LazyAnyElement = Lazy(AnyElement)

  implicit val BooleanElement: Elem[Boolean] = new BaseElemLiftable(false, sigma.BooleanType)
  implicit val ByteElement   : Elem[Byte]    = new BaseElemLiftable(0.toByte, sigma.ByteType)
  implicit val ShortElement  : Elem[Short]   = new BaseElemLiftable(0.toShort, sigma.ShortType)
  implicit val IntElement    : Elem[Int]     = new BaseElemLiftable(0, sigma.IntType)
  implicit val LongElement   : Elem[Long]    = new BaseElemLiftable(0L, sigma.LongType)
  implicit val UnitElement   : Elem[Unit]    = new BaseElemLiftable((), sigma.UnitType)
  implicit val StringElement : Elem[String]  = new BaseElemLiftable("", sigma.StringType)

  /** Implicitly defines element type for pairs. */
  implicit final def pairElement[A, B](implicit ea: Elem[A], eb: Elem[B]): Elem[(A, B)] =
    cachedElem(classOf[PairElem[_, _]], ea, eb)(new PairElem[A, B](ea, eb))

  /** Implicitly defines element type for sum types. */
  implicit final def sumElement[A, B](implicit ea: Elem[A], eb: Elem[B]): Elem[A | B] =
    cachedElem(classOf[SumElem[_, _]], ea, eb)(new SumElem[A, B](ea, eb))

  /** Implicitly defines element type for functions. */
  implicit final def funcElement[A, B](implicit ea: Elem[A], eb: Elem[B]): Elem[A => B] =
    cachedElem(classOf[FuncElem[_, _]], ea, eb)(new FuncElem[A, B](ea, eb))

  implicit final def PairElemExtensions[A, B](eAB: Elem[(A, B)]): PairElem[A, B] = eAB.asInstanceOf[PairElem[A, B]]
  implicit final def SumElemExtensions[A, B](eAB: Elem[A | B]): SumElem[A, B] = eAB.asInstanceOf[SumElem[A, B]]
  implicit final def FuncElemExtensions[A, B](eAB: Elem[A => B]): FuncElem[A, B] = eAB.asInstanceOf[FuncElem[A, B]]

  implicit final def toLazyElem[A](implicit eA: Elem[A]): LElem[A] = Lazy(eA)

  /** Since ListMap is immutable this empty map can be shared by all other maps created from it. */
  val EmptyTypeArgs: ListMap[String, (TypeDesc, Variance)] = ListMap.empty

  final def TypeArgs(descs: (String, (TypeDesc, Variance))*): ListMap[String, (TypeDesc, Variance)] = ListMap(descs: _*)

  // can be removed and replaced with assert(value.elem == elem) after #72
  def assertElem(value: Ref[_], elem: Elem[_]): Unit = assertElem(value, elem, "")
  def assertElem(value: Ref[_], elem: Elem[_], hint: => String): Unit = {
    assert(value.elem == elem,
      s"${value.varNameWithType} doesn't have type ${elem.name}" + (if (hint.isEmpty) "" else s"; $hint"))
  }
  def assertEqualElems[A](e1: Elem[A], e2: Elem[A], m: => String): Unit =
    assert(e1 == e2, s"Element $e1 != $e2: $m")

  /** Descriptor of type constructor of `* -> *` kind. Type constructor is not a type,
    * but rather a function from type to type.
    * It contains methods which abstract relationship between types `T`, `F[T]` etc.
    * @param F  high-kind type costructor which is described by this descriptor*/
  @implicitNotFound(msg = "No Cont available for ${F}.")
  abstract class Cont[F[_]] extends TypeDesc {
    /** Given a descriptor of type `T` produced descriptor of type `F[T]`. */
    def lift[T](implicit eT: Elem[T]): Elem[F[T]]

    /** Given a descriptor of type `F[T]` extracts a descriptor of type `T`. */
    def unlift[T](implicit eFT: Elem[F[T]]): Elem[T]

    /** Recogniser of type descriptors constructed by this type costructor.
      * This can be used in generic code, where F is not known, but this descriptor is available. */
    def unapply[T](e: Elem[_]): Option[Elem[F[T]]]

    /** Type string of this type constructor. */
    def getName(f: TypeDesc => String): String = {
      val eFAny = lift(AnyElement)
      val name = eFAny.getClass.getSimpleName.stripSuffix("Elem")
      "[x] => " + name + "[x]"
    }

    /** Whether the type constructor `F` is an instance of Functor type class. */
    final def isFunctor = this.isInstanceOf[Functor[F]]
  }

  final def container[F[_]: Cont] = implicitly[Cont[F]]

  implicit final def containerElem[F[_]:Cont, A:Elem]: Elem[F[A]] = container[F].lift(element[A])

  trait Functor[F[_]] extends Cont[F] {
    def map[A,B](a: Ref[F[A]])(f: Ref[A] => Ref[B]): Ref[F[B]]
  }
}
