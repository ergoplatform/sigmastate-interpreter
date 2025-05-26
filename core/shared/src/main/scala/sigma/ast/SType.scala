package sigma.ast

import sigma.Evaluation.stypeToRType
import sigma.ast.SCollection.SByteArray
import sigma.ast.SType.TypeCode
import sigma.data.OverloadHack.Overloaded1
import sigma.data.{CBigInt, Nullable, SigmaConstants}
import sigma.reflection.{RClass, RMethod, ReflectionData}
import sigma.util.Extensions.{IntOps, LongOps, ShortOps}
import sigma.{AvlTree, BigInt, Box, Coll, Context, Evaluation, GroupElement, Header, PreHeader, SigmaDslBuilder, SigmaProp}

import java.math.BigInteger

/** Base type for all AST nodes of ErgoTree. */
trait SigmaNode extends Product

/** Every type descriptor is a tree represented by nodes in SType hierarchy.
  * In order to extend type family:
  * - Implement concrete class derived from SType
  * - Implement serializer (see SCollectionSerializer) and register it in STypeSerializer.table
  * Each SType is serialized to array of bytes by:
  * - emitting typeCode of each node (see special case for collections below)
  * - then recursively serializing subtrees from left to right on each level
  * - for each collection of primitive type there is special type code to emit single byte instead of two bytes
  * Types code intervals
  * - (1 .. MaxPrimTypeCode)  // primitive types
  * - (CollectionTypeCode .. CollectionTypeCode + MaxPrimTypeCode) // collections of primitive types
  * - (MaxCollectionTypeCode ..)  // Other types
  * Collection of non-primitive type is serialized as (CollectionTypeCode, serialize(elementType))
  * */
sealed trait SType extends SigmaNode {
  /** The underlying Scala type of data values described by this type descriptor.
    * E.g. scala.Int for SInt descriptor.
    */
  type WrappedType

  /** Type code used in serialization of SType values.
    * @see TypeSerializer
    */
  val typeCode: SType.TypeCode

  /** Returns true if this type embeddable, i.e. a type that can be combined with type
    * constructor for optimized encoding. For each embeddable type `T`, and type
    * constructor `C`, the type `C[T]` can be represented by a single byte.
    * @see [[sigmastate.serialization.TypeSerializer]]
    */
  def isEmbeddable: Boolean = false

  /** Elvis operator for types. See https://en.wikipedia.org/wiki/Elvis_operator*/
  def ?:(whenNoType: => SType): SType = if (this == NoType) whenNoType else this


  /** Returns parsable type term string of the type described by this type descriptor.
    * For every type it should be inverse to SigmaTyper.parseType.
    * This is default fallback implementation, should be overriden if it
    * is not correct for a particular type. */
  def toTermString: String = {
    val t = Evaluation.stypeToRType(this)
    t.name
  }
}

object SType {
  /** Representation of type codes used in serialization. */
  type TypeCode = Byte

  /** Named type variables and parameters used in generic types and method signatures.
    * Generic type terms like `(Coll[IV],(IV) => Boolean) => Boolean` are used to represent
    * method types of `Coll`` and `Option`` types. Each such type is an instance of [[SFunc]].
    * To represent variables (such as `IV` in the example above) [[STypeVar]] instances
    * are used.
    *
    * Generic types are not supported by ErgoTree serialization format and STypeVars are
    * used internally and never serialized (there is no serializer for STypeVar).
    * Thus the usage of type variables is limited.
    *
    * All necessary type variables can be declared in advance and reused across all code
    * base. This allows to avoid allocation of many duplicates and also improve
    * performance of SType values.
    */
  val tT = STypeVar("T")
  val tR = STypeVar("R")
  val tK = STypeVar("K")
  val tL = STypeVar("L")
  val tO = STypeVar("O")
  val tD = STypeVar("D")
  val tV = STypeVar("V")
  val tIV = STypeVar("IV")
  val tOV = STypeVar("OV")

  val paramT = STypeParam(tT)
  val paramR = STypeParam(tR)
  val paramIV = STypeParam(tIV)
  val paramOV = STypeParam(tOV)
  val paramIVSeq: Seq[STypeParam] = Array(paramIV)

  val IndexedSeqOfT2: IndexedSeq[SType] = Array(SType.tT, SType.tT)

  /** Immutable empty array, can be used to avoid repeated allocations. */
  val EmptyArray = Array.empty[SType]

  /** Immutable empty IndexedSeq, can be used to avoid repeated allocations. */
  val EmptySeq: IndexedSeq[SType] = EmptyArray

  /** All pre-defined types should be listed here. Note, NoType is not listed.
    * Should be in sync with sigmastate.lang.Types.predefTypes. */
  val allPredefTypes: Seq[SType] = Array[SType](
    SBoolean, SByte, SShort, SInt, SLong, SBigInt, SContext,
    SGlobal, SHeader, SPreHeader, SAvlTree, SGroupElement, SSigmaProp, SString, SBox,
    SUnit, SAny)

  /** A mapping of object types supporting MethodCall operations. For each serialized
    * typeId this map contains a companion object which can be used to access the list of
    * corresponding methods.
    *
    * NOTE: in the current implementation only monomorphic methods are supported (without
    * type parameters)
    *
    * NOTE2: in v3.x SNumericType.typeId is silently shadowed by SGlobal.typeId as part of
    * `toMap` operation. As a result, the methods collected into SByte.methods cannot be
    * resolved (using SMethod.fromIds()) for all numeric types (SByte, SShort, SInt,
    * SLong, SBigInt). See the corresponding regression `property("MethodCall on numerics")`.
    * However, this "shadowing" is not a problem since all casting methods are implemented
    * via Downcast, Upcast opcodes and the remaining `toBytes`, `toBits` methods are not
    * implemented at all.
    * In order to allow MethodCalls on numeric types in future versions the SNumericType.typeId
    * should be changed and SGlobal.typeId should be preserved. The regression tests in
    * `property("MethodCall Codes")` should pass.
    */
  // TODO v6.0: should contain all numeric types (including also SNumericType)
  //  to support method calls like 10.toByte which encoded as MethodCall with typeId = 4, methodId = 1
  //  see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/667
  lazy val types: Map[Byte, STypeCompanion] = Seq(
    SBoolean, SNumericType, SString, STuple, SGroupElement, SSigmaProp, SContext, SGlobal, SHeader, SPreHeader,
    SAvlTree, SBox, SOption, SCollection, SBigInt
  ).map { t => (t.typeId, t) }.toMap

  /** Checks that the type of the value corresponds to the descriptor `tpe`.
    * If the value has complex structure only root type constructor is checked.
    * NOTE, this method is used in ErgoTree evaluation to systematically check that each
    * tree node evaluates to a value of the expected type.
    * Shallow runtime checks are enough if:
    * 1) ErgoTree is well-typed, i.e. each sub-expression has correct types (agree with
    *    the argument type).
    * 2) `isValueOfType == true` for each tree leaf
    * 3) `isValueOfType == true` for each sub-expression
    *
    * @param x value to check type
    * @param tpe   type descriptor to check value against
    * @return true if the given `value` is of type tpe`
    */
  def isValueOfType[T <: SType](x: Any, tpe: T): Boolean = tpe match {
    case SBoolean => x.isInstanceOf[Boolean]
    case SByte => x.isInstanceOf[Byte]
    case SShort => x.isInstanceOf[Short]
    case SInt => x.isInstanceOf[Int]
    case SLong => x.isInstanceOf[Long]
    case SBigInt => x.isInstanceOf[BigInt]
    case SGroupElement => x.isInstanceOf[GroupElement]
    case SSigmaProp => x.isInstanceOf[SigmaProp]
    case SBox => x.isInstanceOf[Box]
    case _: SCollectionType[_] => x.isInstanceOf[Coll[_]]
    case _: SOption[_] => x.isInstanceOf[Option[_]]
    case t: STuple =>
      if (t.items.length == 2) x.isInstanceOf[Tuple2[_,_]]
      else sys.error(s"Unsupported tuple type $t")
    case tF: SFunc =>
      if (tF.tDom.length == 1) x.isInstanceOf[Function1[_,_]]
      else sys.error(s"Unsupported function type $tF")
    case SContext => x.isInstanceOf[Context]
    case SAvlTree => x.isInstanceOf[AvlTree]
    case SGlobal => x.isInstanceOf[SigmaDslBuilder]
    case SHeader => x.isInstanceOf[Header]
    case SPreHeader => x.isInstanceOf[PreHeader]
    case SUnit => x.isInstanceOf[Unit]
    case _ => sys.error(s"Unknown type $tpe")
  }


  implicit class AnyOps(val x: Any) extends AnyVal {
    /** Helper method to simplify type casts. */
    def asWrappedType: SType#WrappedType = x.asInstanceOf[SType#WrappedType]
  }
}

/** Basic interface for all type companions.
  * This is necessary to make distinction between concrete type descriptor of a type like Coll[Int]
  * and generic descriptor of Coll[T] type constructor.
  * Some simple types like Int, GroupElement inherit from both SType and STypeCompanion.
  * @see SInt, SGroupElement, SType
  */
trait STypeCompanion {
  /** Force initialization of reflection. */
  val reflection = ReflectionData

  /** Type identifier to use in method serialization */
  def typeId: Byte

  /** If this is SType instance then returns the name of the corresponding RType.
    * Otherwise returns the name of type companion object (e.g. SCollection).
    */
  def typeName: String = {
    this match {
      case t: SType =>
        val rtype = stypeToRType(t)
        rtype.name
      case _ => this.getClass.getSimpleName.replace("$", "")
    }
  }

  /** Class which represents values of this type. When method call is executed, the corresponding method
    * of this class is invoked via [[RMethod]].invoke(). */
  def reprClass: RClass[_]

  /** Represents class of `this`. */
  lazy val thisRClass: RClass[_] = RClass(this.getClass)
}


/** Special type to represent untyped values.
  * Interpreter raises an error when encounter a Value with this type.
  * All Value nodes with this type should be elimitanted during typing.
  * If no specific type can be assigned statically during typing,
  * then either error should be raised or type SAny should be assigned
  * which is interpreted as dynamic typing. */
case object NoType extends SType {
  type WrappedType = Nothing
  override val typeCode = 0: Byte
}

/** Type variable which is used in generic method/func signatures.
  * Used by ErgoScript compiler IR and eliminated during compilation.
  * It is not used in ErgoTree.
  */
case class STypeVar(name: String) extends SType {
  require(name.length <= 255, "name is too long")
  override type WrappedType = Any
  override val typeCode = STypeVar.TypeCode
  override def toString = name
  override def toTermString: String = name
}
object STypeVar {
  val TypeCode: TypeCode = 103: Byte
  implicit def liftString(n: String): STypeVar = STypeVar(n)

  /** Immutable empty array, can be used to avoid repeated allocations. */
  val EmptyArray = Array.empty[STypeVar]

  /** Immutable empty IndexedSeq, can be used to avoid repeated allocations. */
  val EmptySeq: IndexedSeq[STypeVar] = EmptyArray
}

/** Base trait for all pre-defined types which are not necessary primitive (e.g. Box, AvlTree).
  */
trait SPredefType extends SType {
}

/** Base type for SBoolean and SSigmaProp. */
trait SLogical extends SType {
}

/** Base trait implemented by all generic types (those which has type parameters,
  * e.g. Coll[T], Option[T], etc.)*/
trait SGenericType {
  /** Type parameters of this generic type. */
  def typeParams: Seq[STypeParam]
}

/** Base trait for all embeddable types.
  */
trait SEmbeddable extends SType {
  override def isEmbeddable: Boolean = true
  /** Type code of embeddable type can be combined with code of type constructor.
    * Resulting code can be serialized. This simple convention allows to save space for most frequently used types.
    * See TypeSerializer */
  @inline final def embedIn(typeConstrId: Byte): Byte = (typeConstrId + this.typeCode).toByte
}

/** Base trait for all primitive types (aka atoms) which don't have internal type items.
  * All primitive types can occupy a reserved interval of codes from 1 to MaxPrimTypeCode. */
trait SPrimType extends SType with SPredefType {
}

/** Primitive type recognizer to pattern match on TypeCode */
object SPrimType {
  def unapply(t: SType): Option[SType] = SType.allPredefTypes.find(_ == t)

  /** Type code of the last valid prim type so that (1 to LastPrimTypeCode) is a range of valid codes. */
  final val LastPrimTypeCode: Byte = 8: Byte

  /** Upper limit of the interval of valid type codes for primitive types */
  final val MaxPrimTypeCode: Byte = 11: Byte

  /** Max possible number of primitive types. */
  final val PrimRange: Byte = (MaxPrimTypeCode + 1).toByte
}

/** Base trait for all types which have methods (and properties) */
trait SProduct extends SType {
}

/** Monomorphic type descriptor i.e. a type without generic parameters.
  * @see `SGenericType`
  */
trait SMonoType extends SType with STypeCompanion {
}

/** Marker trait for all numeric types. */
trait SNumericType extends SProduct with STypeCompanion {
  /** Upcasts the given value of a smaller type to this larger type.
    * Corresponds to section 5.1.2 Widening Primitive Conversion of Java Language Spec.
    * @param n numeric value to be converted
    * @return a value of WrappedType of this type descriptor's instance.
    * @throw exception if `n` has actual type which is larger than this type.
    */
  def upcast(n: AnyVal): WrappedType

  /** Downcasts the given value of a larger type to this smaller type.
    * Corresponds to section 5.1.3 Narrowing Primitive Conversion of Java Language Spec.
    * @param n numeric value to be converted
    * @return a value of WrappedType of this type descriptor's instance.
    * @throw exception if the actual value of `i` cannot fit into this type.
    */
  def downcast(n: AnyVal): WrappedType

  /** Returns a type which is larger. */
  @inline def max(that: SNumericType): SNumericType =
    if (this.numericTypeIndex > that.numericTypeIndex) this else that

  /** Returns true if this numeric type is larger than that. */
  @inline final def >(that: SNumericType): Boolean = this.numericTypeIndex > that.numericTypeIndex

  /** Numeric types are ordered by the number of bytes to store the numeric values.
    * @return index in the array of all numeric types. */
  def numericTypeIndex: Int

  override def toString: String = this.getClass.getSimpleName
}

object SNumericType extends STypeCompanion {
  /** Array of all numeric types ordered by number of bytes in the representation. */
  final val allNumericTypes = Array(SByte, SShort, SInt, SLong, SBigInt)

  // TODO v6.0: this typeId is now shadowed by SGlobal.typeId
  //  see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/667
  override def typeId: TypeCode = 106: Byte

  /** Since this object is not used in SMethod instances. */
  override def reprClass: RClass[_] = sys.error(s"Shouldn't be called.")
}

/** Descriptor of ErgoTree type `Boolean` holding `true` or `false` values. */
case object SBoolean extends SPrimType with SEmbeddable with SLogical with SProduct with SMonoType {
  override type WrappedType = Boolean
  override val typeCode: TypeCode = 1: Byte
  override val reprClass: RClass[_] = RClass(classOf[Boolean])
  override def typeId = typeCode
  implicit def typeBoolean: SBoolean.type = this
}

/** Descriptor of ErgoTree type `Byte` - 8-bit signed integer. */
case object SByte extends SPrimType with SEmbeddable with SNumericType with SMonoType {
  override type WrappedType = Byte
  override val typeCode: TypeCode = 2: Byte
  override val reprClass: RClass[_] = RClass(classOf[Byte])
  implicit def typeByte: SByte.type = this
  override def typeId = typeCode
  override def numericTypeIndex: Int = 0
  override def upcast(v: AnyVal): Byte = v match {
    case b: Byte => b
    case _ => sys.error(s"Cannot upcast value $v to the type $this")
  }
  override def downcast(v: AnyVal): Byte = v match {
    case b: Byte => b
    case s: Short => s.toByteExact
    case i: Int => i.toByteExact
    case l: Long => l.toByteExact
    case _ => sys.error(s"Cannot downcast value $v to the type $this")
  }
}

/** Descriptor of ErgoTree type `Short` - 16-bit signed integer. */
case object SShort extends SPrimType with SEmbeddable with SNumericType with SMonoType {
  override type WrappedType = Short
  override val typeCode: TypeCode = 3: Byte
  override val reprClass: RClass[_] = RClass(classOf[Short])
  implicit val typeShort: SShort.type = this
  override def typeId = typeCode
  override def numericTypeIndex: Int = 1
  override def upcast(v: AnyVal): Short = v match {
    case x: Byte => x.toShort
    case x: Short => x
    case _ => sys.error(s"Cannot upcast value $v to the type $this")
  }
  override def downcast(v: AnyVal): Short = v match {
    case s: Short => s
    case i: Int => i.toShortExact
    case l: Long => l.toShortExact
    case _ => sys.error(s"Cannot downcast value $v to the type $this")
  }
}

/** Descriptor of ErgoTree type `Int` - 32-bit signed integer. */
case object SInt extends SPrimType with SEmbeddable with SNumericType with SMonoType {
  override type WrappedType = Int
  override val typeCode: TypeCode = 4: Byte
  override val reprClass: RClass[_] = RClass(classOf[Int])
  override def typeId = typeCode
  override def numericTypeIndex: Int = 2
  implicit def typeInt: SInt.type = this
  override def upcast(v: AnyVal): Int = v match {
    case x: Byte => x.toInt
    case x: Short => x.toInt
    case x: Int => x
    case _ => sys.error(s"Cannot upcast value $v to the type $this")
  }
  override def downcast(v: AnyVal): Int = v match {
    case b: Byte => b.toInt
    case s: Short => s.toInt
    case i: Int => i
    case l: Long => l.toIntExact
    case _ => sys.error(s"Cannot downcast value $v to the type $this")
  }
}

/** Descriptor of ErgoTree type `Long` - 64-bit signed integer. */
case object SLong extends SPrimType with SEmbeddable with SNumericType with SMonoType {
  override type WrappedType = Long
  override val typeCode: TypeCode = 5: Byte
  override val reprClass: RClass[_] = RClass(classOf[Long])
  override def typeId = typeCode
  override def numericTypeIndex: Int = 3
  implicit def typeLong: SLong.type = this

  override def upcast(v: AnyVal): Long = v match {
    case x: Byte => x.toLong
    case x: Short => x.toLong
    case x: Int => x.toLong
    case x: Long => x
    case _ => sys.error(s"Cannot upcast value $v to the type $this")
  }
  override def downcast(v: AnyVal): Long = v match {
    case b: Byte => b.toLong
    case s: Short => s.toLong
    case i: Int => i.toLong
    case l: Long => l
    case _ => sys.error(s"Cannot downcast value $v to the type $this")
  }
}

/** Type of 256 bit integet values. Implemented using [[java.math.BigInteger]]. */
case object SBigInt extends SPrimType with SEmbeddable with SNumericType with SMonoType {
  override type WrappedType = BigInt
  override val typeCode: TypeCode = 6: Byte
  override val reprClass: RClass[_] = RClass(classOf[BigInt])
  override def typeId = typeCode
  implicit def typeBigInt: SBigInt.type = this

  /** Type of Relation binary op like GE, LE, etc. */
  val RelationOpType = SFunc(Array(SBigInt, SBigInt), SBoolean)

  /** The maximum size of BigInteger value in byte array representation. */
  val MaxSizeInBytes: Long = SigmaConstants.MaxBigIntSizeInBytes.value

  override def numericTypeIndex: Int = 4

  override def upcast(v: AnyVal): BigInt = {
    val bi = v match {
      case x: Byte => BigInteger.valueOf(x.toLong)
      case x: Short => BigInteger.valueOf(x.toLong)
      case x: Int => BigInteger.valueOf(x.toLong)
      case x: Long => BigInteger.valueOf(x)
      case _ => sys.error(s"Cannot upcast value $v to the type $this")
    }
    CBigInt(bi)
  }
  override def downcast(v: AnyVal): BigInt = {
    val bi = v match {
      case x: Byte => BigInteger.valueOf(x.toLong)
      case x: Short => BigInteger.valueOf(x.toLong)
      case x: Int => BigInteger.valueOf(x.toLong)
      case x: Long => BigInteger.valueOf(x)
      case _ => sys.error(s"Cannot downcast value $v to the type $this")
    }
    CBigInt(bi)
  }
}

/** Descriptor of type `String` which is not used in ErgoTree, but used in ErgoScript.
  * NOTE: this descriptor both type and type companion */
case object SString extends SProduct with SMonoType {
  override type WrappedType = String
  override val typeCode: TypeCode = 102: Byte
  override def typeId = typeCode
  override def reprClass: RClass[_] = RClass(classOf[String])
}

/** Descriptor of ErgoTree type `GroupElement`.
  * NOTE: this descriptor both type and type companion */
case object SGroupElement extends SProduct with SPrimType with SEmbeddable with SMonoType {
  override type WrappedType = GroupElement
  override val typeCode: TypeCode = 7: Byte
  override val reprClass: RClass[_] = RClass(classOf[GroupElement])
  override def typeId = typeCode
  implicit def typeGroupElement: SGroupElement.type = this
}

/** Descriptor of ErgoTree type `SigmaProp` which represent sigma-protocol propositions. */
case object SSigmaProp extends SProduct with SPrimType with SEmbeddable with SLogical with SMonoType {
  import SType._
  override type WrappedType = SigmaProp
  override val typeCode: TypeCode = 8: Byte
  override val reprClass: RClass[_] = RClass(classOf[SigmaProp])
  override def typeId = typeCode
  implicit def typeSigmaProp: SSigmaProp.type = this

  /** The maximum size of SigmaProp value in serialized byte array representation. */
  val MaxSizeInBytes: Long = SigmaConstants.MaxSigmaPropSizeInBytes.value
}

/** Any other type is implicitly subtype of this type. */
case object SAny extends SPrimType with SMonoType {
  override type WrappedType = Any
  override val typeCode: TypeCode = 97: Byte

  /** Type identifier to use in method serialization */
  override def typeId: TypeCode = typeCode

  /** Class which represents values of this type. When method call is executed, the corresponding method
    * of this class is invoked via [[RMethod]].invoke(). */
  override def reprClass: RClass[_] = RClass(classOf[Any])
}

/** The type with single inhabitant value `()` */
case object SUnit extends SPrimType with SMonoType {
  override type WrappedType = Unit
  override val typeCode: TypeCode = 98: Byte
  /** Type identifier to use in method serialization */
  override def typeId: TypeCode = typeCode

  /** Class which represents values of this type. When method call is executed, the corresponding method
    * of this class is invoked via [[RMethod]].invoke(). */
  override def reprClass: RClass[_] = RClass(classOf[Unit])
}

/** Helper constuctor/extractor for tuples of two types. */
object SPair {
  def apply(l: SType, r: SType) = STuple(Array(l, r))
  def unapply(t: STuple): Nullable[(SType, SType)] = t match {
    case STuple(IndexedSeq(l, r)) => Nullable((l, r))
    case _ => Nullable.None
  }
}

/** Type descriptor of lambda types. */
case class SFunc(tDom: IndexedSeq[SType],  tRange: SType, tpeParams: Seq[STypeParam] = Nil)
    extends SType with SGenericType
{
  override type WrappedType = Any => tRange.WrappedType
  override val typeCode = SFunc.FuncTypeCode
  override def toString = {
    val args = if (tpeParams.isEmpty) "" else tpeParams.mkString("[", ",", "]")
    s"$args(${tDom.mkString(",")}) => $tRange"
  }
  override def toTermString = {
    val args = if (tpeParams.isEmpty) "" else tpeParams.mkString("[", ",", "]")
    s"$args(${tDom.map(_.toTermString).mkString(",")}) => ${tRange.toTermString}"
  }
  override val typeParams: Seq[STypeParam] = tpeParams

  /** Generalize this type and return a new descriptor. */
  def getGenericType: SFunc = {
    val typeParams: Seq[STypeParam] = tDom.zipWithIndex
        .map { case (_, i) => STypeParam(SType.tD.name + (i + 1)) } :+ STypeParam(SType.tR.name)
    val ts = typeParams.map(_.ident)
    SFunc(ts.init.toIndexedSeq, ts.last, Nil)
  }

  /** Transform function into method type by adding the given `objType` as the first
    * argument type (aka method receiver type).
    */
  def withReceiverType(objType: SType) = this.copy(tDom = objType +: tDom)
}

object SFunc {
  final val FuncTypeCode: TypeCode = TypeCodes.FirstFuncType
  def apply(tDom: SType, tRange: SType): SFunc = SFunc(Array(tDom), tRange) // HOTSPOT:
  val identity = { x: Any => x }
}

/** Used by ErgoScript compiler IR and eliminated during compilation.
  * It is not used in ErgoTree.
  */
case class STypeApply(name: String, args: IndexedSeq[SType] = IndexedSeq()) extends SType {
  override type WrappedType = Any
  override val typeCode = STypeApply.TypeCode
}
object STypeApply {
  val TypeCode = 94: Byte
}

/** Type description of optional values. Instances of `Option`
  *  are either constructed by `Some` or by `None` constructors. */
case class SOption[ElemType <: SType](elemType: ElemType) extends SProduct with SGenericType {
  override type WrappedType = Option[ElemType#WrappedType]
  override val typeCode: TypeCode = SOption.OptionTypeCode
  override def toString = s"Option[$elemType]"
  override def toTermString: String = s"Option[${elemType.toTermString}]"
  override lazy val typeParams: Seq[STypeParam] = Array(SType.paramT)
}

object SOption extends STypeCompanion {
  /** Code of `Option[_]` type constructor. */
  val OptionTypeConstrId = 3
  /** Type code for `Option[T] for some T` type used in TypeSerializer. */
  val OptionTypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * OptionTypeConstrId).toByte
  /** Code of `Option[Coll[_]]` type constructor. */
  val OptionCollectionTypeConstrId = 4
  /** Type code for `Option[Coll[T]] for some T` type used in TypeSerializer. */
  val OptionCollectionTypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * OptionCollectionTypeConstrId).toByte

  override def typeId = OptionTypeCode

  override val reprClass: RClass[_] = RClass(classOf[Option[_]])

  type SBooleanOption      = SOption[SBoolean.type]
  type SByteOption         = SOption[SByte.type]
  type SShortOption        = SOption[SShort.type]
  type SIntOption          = SOption[SInt.type]
  type SLongOption         = SOption[SLong.type]
  type SBigIntOption       = SOption[SBigInt.type]
  type SGroupElementOption = SOption[SGroupElement.type]
  type SBoxOption          = SOption[SBox.type]
  type SAvlTreeOption      = SOption[SAvlTree.type]

  /** This descriptors are instantiated once here and then reused. */
  implicit val SByteOption = SOption(SByte)
  implicit val SByteArrayOption = SOption(SByteArray)
  implicit val SShortOption = SOption(SShort)
  implicit val SIntOption = SOption(SInt)
  implicit val SLongOption = SOption(SLong)
  implicit val SBigIntOption = SOption(SBigInt)
  implicit val SBooleanOption = SOption(SBoolean)
  implicit val SAvlTreeOption = SOption(SAvlTree)
  implicit val SGroupElementOption = SOption(SGroupElement)
  implicit val SSigmaPropOption = SOption(SSigmaProp)
  implicit val SBoxOption = SOption(SBox)

  def apply[T <: SType](implicit elemType: T, ov: Overloaded1): SOption[T] = SOption(elemType)
}


/** Base class for descriptors of `Coll[T]` ErgoTree type for some elemType T. */
trait SCollection[T <: SType] extends SProduct with SGenericType {
  def elemType: T
  override type WrappedType = Coll[T#WrappedType]
}

/** Descriptor of `Coll[T]` ErgoTree type for some elemType T. */
case class SCollectionType[T <: SType](elemType: T) extends SCollection[T] {
  override val typeCode: TypeCode = SCollectionType.CollectionTypeCode
  override def typeParams: Seq[STypeParam] = SCollectionType.typeParams
  override def toString = s"Coll[$elemType]"
  override def toTermString = s"Coll[${elemType.toTermString}]"
}

object SCollectionType {
  /** Code of `Coll[_]` type constructor. */
  val CollectionTypeConstrId = 1

  /** Type code for `Coll[T] for some T` type used in TypeSerializer. */
  val CollectionTypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * CollectionTypeConstrId).toByte

  /** Code of `Coll[Coll[_]]` type constructor. */
  val NestedCollectionTypeConstrId = 2

  /** Type code for `Coll[Coll[T]] for some T` type used in TypeSerializer. */
  val NestedCollectionTypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * NestedCollectionTypeConstrId).toByte

  /** Array of generic type parameters reused in all SCollectionType instances. */
  val typeParams: Seq[STypeParam] = Array(SType.paramIV)
}

object SCollection extends STypeCompanion {
  override val reprClass: RClass[_] = RClass(classOf[Coll[_]])
  override def typeId = SCollectionType.CollectionTypeCode

  /** Costructs a collection type with the given type of elements. */
  implicit def typeCollection[V <: SType](implicit tV: V): SCollection[V] = SCollection[V](tV)

  /** Helper descriptors reused across different method descriptors. */
  def tIV = SType.tIV
  def tOV = SType.tOV

  /** This descriptors are instantiated once here and then reused. */
  val ThisType = SCollection(tIV)
  val tOVColl = SCollection(tOV)
  val tPredicate = SFunc(tIV, SBoolean)

  /** Helper constructors. */
  def apply[T <: SType](elemType: T): SCollection[T] = SCollectionType(elemType)
  def apply[T <: SType](implicit elemType: T, ov: Overloaded1): SCollection[T] = SCollectionType(elemType)

  type SBooleanArray      = SCollection[SBoolean.type]
  type SByteArray         = SCollection[SByte.type]
  type SShortArray        = SCollection[SShort.type]
  type SIntArray          = SCollection[SInt.type]
  type SLongArray         = SCollection[SLong.type]
  type SBigIntArray       = SCollection[SBigInt.type]
  type SGroupElementArray = SCollection[SGroupElement.type]
  type SBoxArray          = SCollection[SBox.type]
  type SAvlTreeArray      = SCollection[SAvlTree.type]

  /** This descriptors are instantiated once here and then reused. */
  val SBooleanArray      = SCollection(SBoolean)
  val SByteArray         = SCollection(SByte)
  val SByteArray2        = SCollection(SCollection(SByte))
  val SShortArray        = SCollection(SShort)
  val SIntArray          = SCollection(SInt)
  val SLongArray         = SCollection(SLong)
  val SBigIntArray       = SCollection(SBigInt)
  val SGroupElementArray = SCollection(SGroupElement)
  val SSigmaPropArray    = SCollection(SSigmaProp)
  val SBoxArray          = SCollection(SBox)
  val SAvlTreeArray      = SCollection(SAvlTree)
  val SHeaderArray       = SCollection(SHeader)
}

/** Type descriptor of tuple type. */
case class STuple(items: IndexedSeq[SType]) extends SCollection[SAny.type] {
  override val typeCode = STuple.TupleTypeCode

  override def elemType: SAny.type = SAny

  override val typeParams = Nil

  override def toTermString = s"(${items.map(_.toTermString).mkString(",")})"
  override def toString = s"(${items.mkString(",")})"
}

object STuple extends STypeCompanion {
  /** Code of `(_, T) for some embeddable T` type constructor. */
  val Pair1TypeConstrId = 5
  /** Type code for `(E, T) for some embeddable T` type used in TypeSerializer. */
  val Pair1TypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * Pair1TypeConstrId).toByte

  /** Code of `(T, _) for some embeddable T` type constructor. */
  val Pair2TypeConstrId = 6
  /** Type code for `(T, E) for some embeddable T` type used in TypeSerializer. */
  val Pair2TypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * Pair2TypeConstrId).toByte
  val TripleTypeCode: TypeCode = Pair2TypeCode

  /** Type constructor code of symmetric pair `(T, T)` for some embeddable T. */
  val PairSymmetricTypeConstrId = 7
  /** Type code of symmetric pair `(T, T)` for some embeddable T. */
  val PairSymmetricTypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * PairSymmetricTypeConstrId).toByte
  val QuadrupleTypeCode: TypeCode = PairSymmetricTypeCode

  /** Type code of generic tuple type. */
  val TupleTypeCode = ((SPrimType.MaxPrimTypeCode + 1) * 8).toByte

  override def typeId = TupleTypeCode

  override val reprClass: RClass[_] = RClass(classOf[Product2[_,_]])

  /** Helper factory method. */
  def apply(items: SType*): STuple = STuple(items.toArray)

}

/** Type descriptor of `Box` type of ErgoTree. */
case object SBox extends SProduct with SPredefType with SMonoType {
  override type WrappedType = Box
  override val typeCode: TypeCode = 99: Byte
  override val reprClass: RClass[_] = RClass(classOf[Box])
  override def typeId = typeCode
  implicit def typeBox: SBox.type = this
}

/** Type descriptor of `AvlTree` type of ErgoTree. */
case object SAvlTree extends SProduct with SPredefType with SMonoType {
  override type WrappedType = AvlTree
  override val typeCode: TypeCode = 100: Byte
  override val reprClass: RClass[_] = RClass(classOf[AvlTree])
  override def typeId = typeCode
  implicit def typeAvlTree: SAvlTree.type = this

  import SOption._
  lazy val TCollOptionCollByte = SCollection(SByteArrayOption)
  lazy val CollKeyValue = SCollection(STuple(SByteArray, SByteArray))
}

/** Type descriptor of `Context` type of ErgoTree. */
case object SContext extends SProduct with SPredefType with SMonoType {
  override type WrappedType = Context
  override val typeCode: TypeCode = 101: Byte
  override def reprClass: RClass[_] = RClass(classOf[Context])
  override def typeId = typeCode
}

/** Type descriptor of `Header` type of ErgoTree. */
case object SHeader extends SProduct with SPredefType with SMonoType {
  override type WrappedType = Header
  override val typeCode: TypeCode = 104: Byte
  override val reprClass: RClass[_] = RClass(classOf[Header])
  override def typeId = typeCode
}

/** Type descriptor of `PreHeader` type of ErgoTree. */
case object SPreHeader extends SProduct with SPredefType with SMonoType {
  override type WrappedType = PreHeader
  override val typeCode: TypeCode = 105: Byte
  override val reprClass: RClass[_] = RClass(classOf[PreHeader])
  override def typeId = typeCode
}

/** This type is introduced to unify handling of global and non-global (i.e. methods) operations.
  * It unifies implementation of global operation with implementation of methods and avoids code
  * duplication (following DRY principle https://en.wikipedia.org/wiki/Don%27t_repeat_yourself).
  * The WrappedType is `sigma.SigmaDslBuilder`, which is an interface implemented by
  * the singleton sigmastate.eval.CostingSigmaDslBuilder
  *
  * The Constant(...) tree node of this type are not allowed, as well as using it in register and
  * context variables (aka ContextExtension)
  *
  * When new methods are added to this type via a soft-fork, they will be serialized as part
  * of ErgoTree using MethodCallSerializer, where SGlobal.typeCode will be used.
  *
  * @see sigmastate.lang.SigmaPredef
  * */
case object SGlobal extends SProduct with SPredefType with SMonoType {
  override type WrappedType = SigmaDslBuilder
  override val typeCode: TypeCode = 106: Byte
  override val reprClass: RClass[_] = RClass(classOf[SigmaDslBuilder])
  override def typeId = typeCode
}

