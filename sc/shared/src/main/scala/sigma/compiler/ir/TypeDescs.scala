package sigma.compiler.ir

import debox.cfor
import scalan.core.{Contravariant, Covariant, Variance}
import sigma.data.{AVHashMap, Lazy, Nullable, RType}
import sigma.reflection.{RClass, RConstructor, RMethod}
import sigma.util.CollectionUtil
import sigma.compiler.ir.wrappers.WrapSpec

import scala.annotation.implicitNotFound
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.language.implicitConversions

/** Defines [[Elem]] descriptor of types in IRContext together with related utilities.
  * @see MethodDesc
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

  /** Abstract class for a method descriptors to assist invocation of MethodCall nodes. */
  sealed abstract class MethodDesc {
    /** The described method */
    def method: RMethod
  }

  /** Decriptor for a method of a class.
    * @param method The RMethod object representing the method.
    */
  case class RMethodDesc(method: RMethod) extends MethodDesc

  /** Descriptor for a method of a wrapper class.
    *
    * @param wrapSpec The wrapping specification of the method.
    * @param method   The RMethod object representing the method.
    */
  case class WMethodDesc(wrapSpec: WrapSpec, method: RMethod) extends MethodDesc

  // TODO optimize performance hot spot (45% of invokeUnlifted time), reduce allocation of Some
  final def getSourceValues(dataEnv: DataEnv, forWrapper: Boolean, stagedValues: AnyRef*): Seq[AnyRef] = {
    import sigma.data.OverloadHack._
    val limit = stagedValues.length
    val res = mutable.ArrayBuilder.make[AnyRef]
    res.sizeHint(limit)
    cfor(0)(_ < limit, _ + 1) { i =>
      val v = stagedValues.apply(i)
      v match {
        case s: Sym =>
          res += dataEnv(s)
        case vec: Seq[AnyRef]@unchecked =>
          res += getSourceValues(dataEnv, forWrapper, vec:_*)
        case e: Elem[_] =>
          val arg =
            if (forWrapper) e.sourceType.classTag  // WrapSpec classes use ClassTag implicit arguments
            else e.sourceType
          res += arg
        case _: Overloaded => // filter out special arguments
      }
    }
    res.result()
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
    protected def collectMethods: Map[RMethod, MethodDesc] = Map() // TODO optimize: all implementations
    protected lazy val methods: Map[RMethod, MethodDesc] = collectMethods

    // TODO optimize: benchamrk against the version below it
    //    def invokeUnlifted(mc: MethodCall, dataEnv: DataEnv): AnyRef = {
    //      val srcArgs = DBuffer.ofSize[AnyRef](mc.args.length + 10)  // with some spare space to have only single allocation
    //      val res = methods.get(mc.method) match {
    //        case Some(WMethodDesc(wrapSpec, method)) =>
    //          getSourceValues(dataEnv, true, mc.receiver, srcArgs)
    //          getSourceValues(dataEnv, true, mc.args, srcArgs)
    //          def msg = s"Cannot invoke method $method on object $wrapSpec with arguments $srcArgs"
    //          val res =
    //            try method.invoke(wrapSpec, srcArgs.toArray:_*)
    //            catch {
    //              case t: Throwable => !!!(msg, t)
    //            }
    //          res
    //        case Some(RMethodDesc(method)) =>
    //          getSourceValues(dataEnv, false, mc.receiver, srcArgs)
    //          val srcObj = srcArgs(0)
    //          srcArgs.pop()
    //          getSourceValues(dataEnv, false, mc.args, srcArgs)
    //          def msg = s"Cannot invoke method $method on object $srcObj with arguments ${srcArgs.toArray.toSeq}"
    //          val res =
    //            try method.invoke(srcObj, srcArgs.toArray:_*)
    //            catch {
    //              case t: Throwable => !!!(msg, t)
    //            }
    //          res
    //        case None =>
    //          !!!(s"Cannot perform unliftedInvoke of $mc")
    //      }
    //      // this if is required because res == null in case of Unit return type
    //      if (mc.selfType == UnitElement) ().asInstanceOf[AnyRef] else res
    //    }

    /** Invoke source type method corresponding to the given MethodCall node.
      * The instance of receiver is obtained from `dataEnv` using mc.receiver symbol.
      * The Method descriptor of the source class is taken from `this.methods` mapping.
      * @param mc   IR node representing method invocation
      * @param dataEnv  environment where each symbol of 'mc' has associated data value
      * @return  data value returned from invoked method
      */
    def invokeUnlifted(mc: MethodCall, dataEnv: DataEnv): Any = {
      val res = methods.get(mc.method) match {
        case Some(WMethodDesc(wrapSpec, method)) =>
          val srcArgs = getSourceValues(dataEnv, true, mc.receiver +: mc.args:_*)
          def msg = s"Cannot invoke method $method on object $wrapSpec with arguments $srcArgs"
          val res =
            try method.invoke(wrapSpec, srcArgs:_*)
            catch {
              case t: Throwable => !!!(msg, t)
            }
          res
        case Some(RMethodDesc(method)) =>
          val srcObj = getSourceValues(dataEnv, false, mc.receiver).head
          val srcArgs = getSourceValues(dataEnv, false, mc.args:_*)
          def msg = s"Cannot invoke method $method on object $srcObj with arguments $srcArgs"
          val res =
            try method.invoke(srcObj, srcArgs:_*)
            catch {
              case t: Throwable => !!!(msg, t)
            }
          res
        case None =>
          !!!(s"Cannot perform unliftedInvoke of $mc")
      }
      // this if is required because res == null in case of Unit return type
      if (mc.resultType == UnitElement) ().asInstanceOf[AnyRef] else res
    }

    def <:<(e: Elem[_]) = e.getClass.isAssignableFrom(this.getClass)
  }

  object Elem {
    /** Map source type desciptor to stated type descriptor using liftable instance. */
    implicit def rtypeToElem[SA, A](tSA: RType[SA])(implicit lA: Liftables.Liftable[SA,A]): Elem[A] = lA.eW

    final def unapply[T, E <: Elem[T]](s: Ref[T]): Nullable[E] = Nullable(s.elem.asInstanceOf[E])

    /** Build a mapping between methods of staged class and the corresponding methods of source class.
      * The methods are related using names.
      * The computed mapping can be used to project MethodCalls IR nodes back to the corresponding
      * methods of source classes and then making their invocation using Java Reflection (Method.invoke).
      * @param cls         staged class where `methodNames` should be looked up
      * @param srcCls      source class where `methodNames` should be looked up
      * @param methodNames list of method names to lookup in both classes
      * @return  a sequence of pairs relating for each staged method the corresponding method from
      *          source classes.
      */
    def declaredMethods(cls: RClass[_], srcCls: RClass[_], methodNames: Set[String]): Seq[(RMethod, MethodDesc)] = {
      val rmethods = cls.getDeclaredMethods.filter(m => methodNames.contains(m.getName))
      val smethods = srcCls.getDeclaredMethods.filter(m => methodNames.contains(m.getName))
      val mapping = CollectionUtil.joinSeqs(rmethods, smethods)(_.getName, _.getName)
      mapping.map { case (rm, sm) =>
        (rm, RMethodDesc(sm))
      }.toSeq
    }

    /** Build a mapping between methods of staged wrapper and the corresponding methods of wrapper spec class.
      * The methods are related using names.
      * @param wrapSpec    wrapper specification class where `methodNames` should be looked up
      * @param wcls        wrapper class where `methodNames` should be looked up
      * @param methodNames list of method names to lookup in both classes
      * @return  a sequence of pairs relating for each wrapper method the corresponding method from
      *          source classes.
      */
    def declaredWrapperMethods(wrapSpec: WrapSpec, wcls: RClass[_], methodNames: Set[String]): Seq[(RMethod, MethodDesc)] = {
      val specCls = RClass(wrapSpec.getClass)
      val wMethods = wcls.getDeclaredMethods.filter(m => methodNames.contains(m.getName))
      val specMethods = specCls.getDeclaredMethods.filter(m => methodNames.contains(m.getName))
      val mapping = CollectionUtil.joinSeqs(wMethods, specMethods)(_.getName, _.getName)
      mapping.map { case (wm, sm) =>
        (wm, WMethodDesc(wrapSpec, sm))
      }.toSeq
    }

  }

  /** Invoke source type method corresponding to the given MethodCall node.
    * This method delegated the work to the given element instance.
    * @param e    type descriptor of receiver node
    * @param mc   IR node representing method invocation
    * @param dataEnv  environment where each symbol of 'mc' has associated data value
    * @return  data value returned from invoked method
    */
  def invokeUnlifted(e: Elem[_], mc: MethodCall, dataEnv: DataEnv): Any =
    e.invokeUnlifted(mc, dataEnv)

  /** Get first (and the only) constructor of the `clazz`. */
  private[compiler] final def getConstructor(clazz: RClass[_]): RConstructor[_] = {
    val constructors = clazz.getConstructors()
    if (constructors.length != 1)
      !!!(s"Element class $clazz has ${constructors.length} constructors, 1 expected")
    else
      constructors(0)
  }

  /** Retrieve an instance of the given Elem class by either looking up in the cache
    * or creating a new one.
    * We assume that all Elem instances are uniquely defined by (clazz, args)
    * @param args  arguments of Elem class constructor
    * @param clazz Elem class
    */
  final def cachedElemByClass[E <: Elem[_]](args: AnyRef*)(implicit clazz: RClass[E]) = {
    cachedElem0(clazz, Nullable.None.asInstanceOf[Nullable[RConstructor[_]]], args).asInstanceOf[E]
  }

  /** Elements cache information for each Elem class. */
  class ElemCacheEntry(
    /** Constructor of the class to create new instances. */
    val constructor: RConstructor[_],
    /** Whether owner argument of constructor exists and of which kind. */
    val ownerType: OwnerKind,
    /** Created instances of elements, one for each unique collection of args. */
    val elements: AVHashMap[Seq[AnyRef], AnyRef]
  )
    
  protected val elemCache = AVHashMap[RClass[_], ElemCacheEntry](1000)

  private[compiler] final def cachedElem0(clazz: RClass[_], optConstructor: Nullable[RConstructor[_]], args: Seq[AnyRef]): Elem[_] = {
    val entry = elemCache.get(clazz) match {
      case Nullable(entry) => entry
      case _ =>
        val constructor = if (optConstructor.isEmpty) getConstructor(clazz) else optConstructor.get
        val ownerType = getOwnerKind(constructor)
        val entry = new ElemCacheEntry(constructor, ownerType, AVHashMap(10))
        elemCache.put(clazz, entry)
        entry
    }
    val e = entry.elements.get(args) match {
      case Nullable(e) => e
      case _ =>
        val constructorArgs = addOwnerParameter(entry.ownerType, args)
        val e = entry.constructor.newInstance(constructorArgs: _*).asInstanceOf[AnyRef]
        entry.elements.put(args, e)
        e
    }
    e.asInstanceOf[Elem[_]]
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
    cachedElemByClass[PairElem[A, B]](ea, eb)(RClass(classOf[PairElem[A, B]]))

  /** Implicitly defines element type for sum types. */
  implicit final def sumElement[A, B](implicit ea: Elem[A], eb: Elem[B]): Elem[A | B] =
    cachedElemByClass[SumElem[A, B]](ea, eb)(RClass(classOf[SumElem[A, B]]))

  /** Implicitly defines element type for functions. */
  implicit final def funcElement[A, B](implicit ea: Elem[A], eb: Elem[B]): Elem[A => B] =
    cachedElemByClass[FuncElem[A, B]](ea, eb)(RClass(classOf[FuncElem[A, B]]))

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
