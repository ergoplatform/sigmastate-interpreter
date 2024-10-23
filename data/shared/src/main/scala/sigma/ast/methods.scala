package sigma.ast

import org.ergoplatform._
import org.ergoplatform.validation._
import sigma.Evaluation.stypeToRType
import sigma._
import sigma.ast.SCollection.{SBooleanArray, SBoxArray, SByteArray, SByteArray2, SHeaderArray}
import sigma.ast.SMethod.{MethodCallIrBuilder, MethodCostFunc, javaMethodOf}
import sigma.ast.SType.{TypeCode, paramT, tT}
import sigma.ast.syntax.{SValue, ValueOps}
import sigma.data.ExactIntegral.{ByteIsExactIntegral, IntIsExactIntegral, LongIsExactIntegral, ShortIsExactIntegral}
import sigma.data.NumericOps.BigIntIsExactIntegral
import sigma.data.OverloadHack.Overloaded1
import sigma.data.{DataValueComparer, KeyValueColl, Nullable, RType, SigmaConstants}
import sigma.eval.{CostDetails, ErgoTreeEvaluator, TracedCost}
import sigma.reflection.RClass
import sigma.serialization.CoreByteWriter.ArgInfo
import sigma.serialization.{DataSerializer, SigmaByteWriter, SigmaSerializer}
import sigma.utils.SparseArrayContainer

import scala.annotation.unused

/** Base type for all companions of AST nodes of sigma lang. */
trait SigmaNodeCompanion

/** Defines recognizer method which allows the derived object to be used in patterns
  * to recognize method descriptors by method name.
  * @see SCollecton
  */
trait MethodByNameUnapply extends MethodsContainer {
  def unapply(methodName: String): Option[SMethod] = methods.find(_.name == methodName)
}

/** Base trait for all method containers (which store methods and properties) */
sealed trait MethodsContainer {
  /** Type for which this container defines methods. */
  def ownerType: STypeCompanion

  override def toString: String =
    getClass.getSimpleName.stripSuffix("$") // e.g. SInt, SCollection, etc

  /** Represents class of `this`. */
  lazy val thisRClass: RClass[_] = RClass(this.getClass)
  def typeId: Byte = ownerType.typeId
  def typeName: String = ownerType.typeName

  /** Returns -1 if `method` is not found. */
  def methodIndex(name: String): Int = methods.indexWhere(_.name == name)

  /** Returns true if this type has a method with the given name. */
  def hasMethod(name: String): Boolean = methodIndex(name) != -1

  /** This method should be overriden in derived classes to add new methods in addition to inherited.
    * Typical override: `super.getMethods() ++ Seq(m1, m2, m3)`
    */
  protected def getMethods(): Seq[SMethod] = Nil

  /** Returns all the methods of this type. */
  def methods: Seq[SMethod] = {                        //todo: consider versioned caching
    val ms = getMethods().toArray
    assert(ms.map(_.name).distinct.length == ms.length, s"Duplicate method names in $this")
    ms.groupBy(_.objType).foreach { case (comp, ms) =>
      assert(ms.map(_.methodId).distinct.length == ms.length, s"Duplicate method ids in $comp: $ms")
    }
    ms
  }
  private def _methodsMap: Map[Byte, Map[Byte, SMethod]] = methods //todo: consider versioned caching
      .groupBy(_.objType.typeId)
      .map { case (typeId, ms) => (typeId -> ms.map(m => m.methodId -> m).toMap) }

  /** Lookup method by its id in this type. */
  @inline def getMethodById(methodId: Byte): Option[SMethod] =
    _methodsMap.get(typeId) match {
      case Some(ms) => ms.get(methodId)
      case None => None
    }

  /** Lookup method in this type by method's id or throw ValidationException.
    * This method can be used in trySoftForkable section to either obtain valid method
    * or catch ValidatioinException which can be checked for soft-fork condition.
    * It delegate to getMethodById to lookup method.
    *
    * @see getMethodById
    */
  def methodById(methodId: Byte): SMethod = {
    if (VersionContext.current.isV6SoftForkActivated) {
      ValidationRules.CheckAndGetMethodV6(this, methodId)
    } else {
      ValidationRules.CheckAndGetMethod(this, methodId)
    }
  }

  /** Finds a method descriptor [[SMethod]] for the given name. */
  def method(methodName: String): Option[SMethod] = methods.find(_.name == methodName)

  /** Looks up the method descriptor by the method name. */
  def getMethodByName(name: String): SMethod = methods.find(_.name == name).get

}
object MethodsContainer {
  private val containers = new SparseArrayContainer[MethodsContainer](Array(
    SByteMethods,
    SShortMethods,
    SIntMethods,
    SLongMethods,
    SBigIntMethods,
    SBooleanMethods,
    SStringMethods,
    SGroupElementMethods,
    SSigmaPropMethods,
    SBoxMethods,
    SAvlTreeMethods,
    SHeaderMethods,
    SPreHeaderMethods,
    SGlobalMethods,
    SContextMethods,
    SCollectionMethods,
    SOptionMethods,
    STupleMethods,
    SUnitMethods,
    SAnyMethods
  ).map(m => (m.typeId, m)))

  def contains(typeId: TypeCode): Boolean = containers.contains(typeId)

  def apply(typeId: TypeCode): MethodsContainer = containers(typeId)

  /** Finds the method of the give type.
    *
    * @param tpe        type of the object for which the method is looked up
    * @param methodName name of the method
    * @return method descriptor or None if not found
    */
  def getMethod(tpe: SType, methodName: String): Option[SMethod] = tpe match {
    case tup: STuple =>
      STupleMethods.getTupleMethod(tup, methodName)
    case _ =>
      containers.get(tpe.typeCode).flatMap(_.method(methodName))
  }
}

trait MonoTypeMethods extends MethodsContainer {
  def ownerType: SMonoType
  /** Helper method to create method descriptors for properties (i.e. methods without args). */
  protected def propertyCall(
      name: String,
      tpeRes: SType,
      id: Byte,
      costKind: CostKind): SMethod =
    SMethod(this, name, SFunc(this.ownerType, tpeRes), id, costKind)
        .withIRInfo(MethodCallIrBuilder)
        .withInfo(PropertyCall, "")

  /** Helper method to create method descriptors for properties (i.e. methods without args). */
  protected def property(
      name: String,
      tpeRes: SType,
      id: Byte,
      valueCompanion: ValueCompanion): SMethod =
    SMethod(this, name, SFunc(this.ownerType, tpeRes), id, valueCompanion.costKind)
        .withIRInfo(MethodCallIrBuilder)
        .withInfo(valueCompanion, "")
}

trait SNumericTypeMethods extends MonoTypeMethods {
  import SNumericTypeMethods.tNum

  private val subst = Map(tNum -> this.ownerType)

  val v5Methods = {
    SNumericTypeMethods.v5Methods.map { m =>
      m.copy(stype = applySubst(m.stype, subst).asFunc)
    }
  }

  val v6Methods = {
    SNumericTypeMethods.v6Methods.map { m =>
      m.copy(
        objType = this, // associate the method with the concrete numeric type
        stype = applySubst(m.stype, subst).asFunc
      )}
  }

  protected override def getMethods(): Seq[SMethod] = {
    if (VersionContext.current.isV6SoftForkActivated) {
      super.getMethods() ++ v6Methods
    } else {
      super.getMethods() ++ v5Methods
    }
  }
}

object SNumericTypeMethods extends MethodsContainer {

  /** Type for which this container defines methods. */
  override def ownerType: STypeCompanion = SNumericType

  /** Type variable used in generic signatures of method descriptors. */
  val tNum = STypeVar("TNum")

  /** Cost function which is assigned for numeric cast MethodCall nodes in ErgoTree.
    * It is called as part of MethodCall.eval method. */
  val costOfNumericCast: MethodCostFunc = new MethodCostFunc {
    override def apply(
        E: ErgoTreeEvaluator,
        mc: MethodCall,
        obj: Any,
        args: Array[Any]): CostDetails = {
      val targetTpe = mc.method.stype.tRange
      val cast      = getNumericCast(mc.obj.tpe, mc.method.name, targetTpe).get
      val costKind  = if (cast == Downcast) Downcast.costKind else Upcast.costKind
      TracedCost(Array(TypeBasedCostItem(MethodDesc(mc.method), costKind, targetTpe)))
    }
  }

  /** The following SMethod instances are descriptors of methods available on all numeric
    * types.
    *
    * @see `val methods` below
    * */
  val ToByteMethod  : SMethod = SMethod(this, "toByte", SFunc(tNum, SByte), 1, null)
      .withCost(costOfNumericCast)
      .withInfo(PropertyCall, "Converts this numeric value to \\lst{Byte}, throwing exception if overflow.")

  val ToShortMethod : SMethod = SMethod(this, "toShort", SFunc(tNum, SShort), 2, null)
      .withCost(costOfNumericCast)
      .withInfo(PropertyCall, "Converts this numeric value to \\lst{Short}, throwing exception if overflow.")

  val ToIntMethod   : SMethod = SMethod(this, "toInt", SFunc(tNum, SInt), 3, null)
      .withCost(costOfNumericCast)
      .withInfo(PropertyCall, "Converts this numeric value to \\lst{Int}, throwing exception if overflow.")

  val ToLongMethod  : SMethod = SMethod(this, "toLong", SFunc(tNum, SLong), 4, null)
      .withCost(costOfNumericCast)
      .withInfo(PropertyCall, "Converts this numeric value to \\lst{Long}, throwing exception if overflow.")

  val ToBigIntMethod: SMethod = SMethod(this, "toBigInt", SFunc(tNum, SBigInt), 5, null)
      .withCost(costOfNumericCast)
      .withInfo(PropertyCall, "Converts this numeric value to \\lst{BigInt}")

  /** Cost of: 1) creating Byte collection from a numeric value */
  val ToBytes_CostKind = FixedCost(JitCost(5))

  val ToBytesMethod: SMethod = SMethod(
    this, "toBytes", SFunc(tNum, SByteArray), 6, ToBytes_CostKind)
      .withIRInfo(MethodCallIrBuilder)
      .withUserDefinedInvoke({ (m: SMethod, obj: Any, _: Array[Any]) =>
        m.objType match {
          case SByteMethods => ByteIsExactIntegral.toBigEndianBytes(obj.asInstanceOf[Byte])
          case SShortMethods => ShortIsExactIntegral.toBigEndianBytes(obj.asInstanceOf[Short])
          case SIntMethods => IntIsExactIntegral.toBigEndianBytes(obj.asInstanceOf[Int])
          case SLongMethods => LongIsExactIntegral.toBigEndianBytes(obj.asInstanceOf[Long])
          case SBigIntMethods => obj.asInstanceOf[BigInt].toBytes
        }
      })
      .withInfo(PropertyCall,
        """ Returns a big-endian representation of this numeric value in a collection of bytes.
         | For example, the \lst{Int} value \lst{0x12131415} would yield the
         | collection of bytes \lst{[0x12, 0x13, 0x14, 0x15]}.
          """.stripMargin)

  /** Cost of: 1) creating Boolean collection (one bool for each bit) from a numeric
    * value. */
  val ToBits_CostKind = FixedCost(JitCost(5))

  val ToBitsMethod: SMethod = SMethod(
    this, "toBits", SFunc(tNum, SBooleanArray), 7, ToBits_CostKind)
      .withIRInfo(MethodCallIrBuilder)
      .withUserDefinedInvoke({ (m: SMethod, obj: Any, _: Array[Any]) =>
        m.objType match {
          case SByteMethods => ByteIsExactIntegral.toBits(obj.asInstanceOf[Byte])
          case SShortMethods => ShortIsExactIntegral.toBits(obj.asInstanceOf[Short])
          case SIntMethods => IntIsExactIntegral.toBits(obj.asInstanceOf[Int])
          case SLongMethods => LongIsExactIntegral.toBits(obj.asInstanceOf[Long])
          case SBigIntMethods => BigIntIsExactIntegral.toBits(obj.asInstanceOf[BigInt])
        }
      })
      .withInfo(PropertyCall,
        """ Returns a big-endian representation of this numeric in a collection of Booleans.
         |  Each boolean corresponds to one bit.
          """.stripMargin)

  /** Cost of inverting bits of a number. */
  val BitwiseOp_CostKind = FixedCost(JitCost(5))

  val BitwiseInverseMethod: SMethod = SMethod(
    this, "bitwiseInverse", SFunc(tNum, tNum), 8, BitwiseOp_CostKind)
    .withIRInfo(MethodCallIrBuilder)
    .withUserDefinedInvoke({ (m: SMethod, obj: Any, _: Array[Any]) =>
      m.objType match {
        case SByteMethods => ByteIsExactIntegral.bitwiseInverse(obj.asInstanceOf[Byte])
        case SShortMethods => ShortIsExactIntegral.bitwiseInverse(obj.asInstanceOf[Short])
        case SIntMethods => IntIsExactIntegral.bitwiseInverse(obj.asInstanceOf[Int])
        case SLongMethods => LongIsExactIntegral.bitwiseInverse(obj.asInstanceOf[Long])
        case SBigIntMethods => BigIntIsExactIntegral.bitwiseInverse(obj.asInstanceOf[BigInt])
      }
    })
    .withInfo(PropertyCall, desc = "Returns bitwise inverse of this numeric. ")

  val BitwiseOrMethod: SMethod = SMethod(
    this, "bitwiseOr", SFunc(Array(tNum, tNum), tNum), 9, BitwiseOp_CostKind)
    .withIRInfo(MethodCallIrBuilder)
    .withUserDefinedInvoke({ (m: SMethod, obj: Any, other: Array[Any]) =>
      m.objType match {
        case SByteMethods => ByteIsExactIntegral.bitwiseOr(obj.asInstanceOf[Byte], other.head.asInstanceOf[Byte])
        case SShortMethods => ShortIsExactIntegral.bitwiseOr(obj.asInstanceOf[Short], other.head.asInstanceOf[Short])
        case SIntMethods => IntIsExactIntegral.bitwiseOr(obj.asInstanceOf[Int], other.head.asInstanceOf[Int])
        case SLongMethods => LongIsExactIntegral.bitwiseOr(obj.asInstanceOf[Long], other.head.asInstanceOf[Long])
        case SBigIntMethods => BigIntIsExactIntegral.bitwiseOr(obj.asInstanceOf[BigInt], other.head.asInstanceOf[BigInt])
      }
    })
    .withInfo(MethodCall,
      """ Returns bitwise or of this numeric and provided one. """.stripMargin,
      ArgInfo("that", "A numeric value to calculate or with."))

  val BitwiseAndMethod: SMethod = SMethod(
    this, "bitwiseAnd", SFunc(Array(tNum, tNum), tNum), 10, BitwiseOp_CostKind)
    .withIRInfo(MethodCallIrBuilder)
    .withUserDefinedInvoke({ (m: SMethod, obj: Any, other: Array[Any]) =>
      m.objType match {
        case SByteMethods => ByteIsExactIntegral.bitwiseAnd(obj.asInstanceOf[Byte], other.head.asInstanceOf[Byte])
        case SShortMethods => ShortIsExactIntegral.bitwiseAnd(obj.asInstanceOf[Short], other.head.asInstanceOf[Short])
        case SIntMethods => IntIsExactIntegral.bitwiseAnd(obj.asInstanceOf[Int], other.head.asInstanceOf[Int])
        case SLongMethods => LongIsExactIntegral.bitwiseAnd(obj.asInstanceOf[Long], other.head.asInstanceOf[Long])
        case SBigIntMethods => BigIntIsExactIntegral.bitwiseAnd(obj.asInstanceOf[BigInt], other.head.asInstanceOf[BigInt])
      }
    })
    .withInfo(MethodCall,
      """ Returns bitwise and of this numeric and provided one. """.stripMargin,
      ArgInfo("that", "A numeric value to calculate and with."))

  val BitwiseXorMethod: SMethod = SMethod(
    this, "bitwiseXor", SFunc(Array(tNum, tNum), tNum), 11, BitwiseOp_CostKind)
    .withIRInfo(MethodCallIrBuilder)
    .withUserDefinedInvoke({ (m: SMethod, obj: Any, other: Array[Any]) =>
      m.objType match {
        case SByteMethods => ByteIsExactIntegral.bitwiseXor(obj.asInstanceOf[Byte], other.head.asInstanceOf[Byte])
        case SShortMethods => ShortIsExactIntegral.bitwiseXor(obj.asInstanceOf[Short], other.head.asInstanceOf[Short])
        case SIntMethods => IntIsExactIntegral.bitwiseXor(obj.asInstanceOf[Int], other.head.asInstanceOf[Int])
        case SLongMethods => LongIsExactIntegral.bitwiseXor(obj.asInstanceOf[Long], other.head.asInstanceOf[Long])
        case SBigIntMethods => BigIntIsExactIntegral.bitwiseXor(obj.asInstanceOf[BigInt], other.head.asInstanceOf[BigInt])
      }
    })
    .withInfo(MethodCall,
      """ Returns bitwise xor of this numeric and provided one. """.stripMargin,
      ArgInfo("that", "A numeric value to calculate xor with."))

  val ShiftLeftMethod: SMethod = SMethod(
    this, "shiftLeft", SFunc(Array(tNum, SInt), tNum), 12, BitwiseOp_CostKind)
    .withIRInfo(MethodCallIrBuilder)
    .withUserDefinedInvoke({ (m: SMethod, obj: Any, other: Array[Any]) =>
      m.objType match {
        case SByteMethods => ByteIsExactIntegral.shiftLeft(obj.asInstanceOf[Byte], other.head.asInstanceOf[Int])
        case SShortMethods => ShortIsExactIntegral.shiftLeft(obj.asInstanceOf[Short], other.head.asInstanceOf[Int])
        case SIntMethods => IntIsExactIntegral.shiftLeft(obj.asInstanceOf[Int], other.head.asInstanceOf[Int])
        case SLongMethods => LongIsExactIntegral.shiftLeft(obj.asInstanceOf[Long], other.head.asInstanceOf[Int])
        case SBigIntMethods => BigIntIsExactIntegral.shiftLeft(obj.asInstanceOf[BigInt], other.head.asInstanceOf[Int])
      }
    })
    .withInfo(MethodCall,
      """ Returns a big-endian representation of this numeric in a collection of Booleans.
        |  Each boolean corresponds to one bit.
          """.stripMargin,
      ArgInfo("bits", "Number of bit to shift to the left. Note, that bits value must be non-negative and less than " +
                      "the size of the number in bits (e.g. 64 for Long, 256 for BigInt)"))

  val ShiftRightMethod: SMethod = SMethod(
    this, "shiftRight", SFunc(Array(tNum, SInt), tNum), 13, BitwiseOp_CostKind)
    .withIRInfo(MethodCallIrBuilder)
    .withUserDefinedInvoke({ (m: SMethod, obj: Any, other: Array[Any]) =>
      m.objType match {
        case SByteMethods => ByteIsExactIntegral.shiftRight(obj.asInstanceOf[Byte], other.head.asInstanceOf[Int])
        case SShortMethods => ShortIsExactIntegral.shiftRight(obj.asInstanceOf[Short], other.head.asInstanceOf[Int])
        case SIntMethods => IntIsExactIntegral.shiftRight(obj.asInstanceOf[Int], other.head.asInstanceOf[Int])
        case SLongMethods => LongIsExactIntegral.shiftRight(obj.asInstanceOf[Long], other.head.asInstanceOf[Int])
        case SBigIntMethods => BigIntIsExactIntegral.shiftRight(obj.asInstanceOf[BigInt], other.head.asInstanceOf[Int])
      }
    })
    .withInfo(MethodCall,
      """ Returns a big-endian representation of this numeric in a collection of Booleans.
        |  Each boolean corresponds to one bit.
          """.stripMargin,
      ArgInfo("bits", "Number of bit to shift to the right. Note, that bits value must be non-negative and less than " +
        "the size of the number in bits (e.g. 64 for Long, 256 for BigInt)"))

  lazy val v5Methods = Array(
    ToByteMethod, // see Downcast
    ToShortMethod, // see Downcast
    ToIntMethod, // see Downcast
    ToLongMethod, // see Downcast
    ToBigIntMethod, // see Downcast
    ToBytesMethod,
    ToBitsMethod
  )

  lazy val v6Methods = v5Methods ++ Array(
    BitwiseInverseMethod,
    BitwiseOrMethod,
    BitwiseAndMethod,
    BitwiseXorMethod,
    ShiftLeftMethod,
    ShiftRightMethod
  )

  protected override def getMethods(): Seq[SMethod] = {
    throw new Exception("SNumericTypeMethods.getMethods shouldn't ever be called")
  }

  /** Collection of names of numeric casting methods (like `toByte`, `toInt`, etc). */
  // todo: add unsigned big int
  val castMethods: Array[String] =
    Array(ToByteMethod, ToShortMethod, ToIntMethod, ToLongMethod, ToBigIntMethod)
        .map(_.name)

  /** Checks the given name is numeric type cast method (like toByte, toInt, etc.). */
  def isCastMethod(name: String): Boolean = castMethods.contains(name)

  /** Convert the given method to a cast operation from fromTpe to resTpe. */
  def getNumericCast(
      fromTpe: SType,
      methodName: String,
      resTpe: SType): Option[NumericCastCompanion] = (fromTpe, resTpe) match {
    case (from: SNumericType, to: SNumericType) if isCastMethod(methodName) =>
      val op = if (to > from) Upcast else Downcast
      Some(op)
    case _ => None // the method in not numeric type cast
  }

}

/** Methods of ErgoTree type `Boolean`. */
case object SBooleanMethods extends MonoTypeMethods {
  /** Type for which this container defines methods. */
  override def ownerType: SMonoType = SBoolean

  val ToByte = "toByte"
  protected override def getMethods() = super.getMethods()
  /* TODO soft-fork: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
  ++ Seq(
    SMethod(this, ToByte, SFunc(this, SByte), 1)
      .withInfo(PropertyCall, "Convert true to 1 and false to 0"),
  )
  */
}

/** Methods of ErgoTree type `Byte`. */
case object SByteMethods extends SNumericTypeMethods {
  /** Type for which this container defines methods. */
  override def ownerType: SMonoType = SByte
}

/** Methods of ErgoTree type `Short`. */
case object SShortMethods extends SNumericTypeMethods {
  /** Type for which this container defines methods. */
  override def ownerType: SMonoType = ast.SShort
}

/** Descriptor of ErgoTree type `Int`. */
case object SIntMethods extends SNumericTypeMethods {
  /** Type for which this container defines methods. */
  override def ownerType: SMonoType = SInt
}

/** Descriptor of ErgoTree type `Long`. */
case object SLongMethods extends SNumericTypeMethods {
  /** Type for which this container defines methods. */
  override def ownerType: SMonoType = SLong
}

/** Methods of BigInt type. Implemented using [[java.math.BigInteger]]. */
case object SBigIntMethods extends SNumericTypeMethods {
  /** Type for which this container defines methods. */
  override def ownerType: SMonoType = SBigInt

  protected override def getMethods(): Seq[SMethod]  = {
    if (VersionContext.current.isV6SoftForkActivated) {
      super.getMethods()
      //    ModQMethod,
      //    PlusModQMethod,
      //    MinusModQMethod,
      // TODO soft-fork: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
      // MultModQMethod,
    } else {
      super.getMethods()
    }
  }

}

/** Methods of type `String`. */
case object SStringMethods extends MonoTypeMethods {
  /** Type for which this container defines methods. */
  override def ownerType: SMonoType = SString
}

/** Methods of type `GroupElement`. */
case object SGroupElementMethods extends MonoTypeMethods {
  /** Type for which this container defines methods. */
  override def ownerType: SMonoType = SGroupElement

  /** Cost of: 1) serializing EcPointType to bytes 2) packing them in Coll. */
  val GetEncodedCostKind = FixedCost(JitCost(250))

  /** The following SMethod instances are descriptors of methods defined in `GroupElement` type. */
  lazy val GetEncodedMethod: SMethod = SMethod(
    this, "getEncoded", SFunc(Array(this.ownerType), SByteArray), 2, GetEncodedCostKind)
    .withIRInfo(MethodCallIrBuilder)
    .withInfo(PropertyCall, "Get an encoding of the point value.")

  lazy val ExponentiateMethod: SMethod = SMethod(
    this, "exp", SFunc(Array(this.ownerType, SBigInt), this.ownerType), 3, Exponentiate.costKind)
    .withIRInfo({ case (builder, obj, _, Seq(arg), _) =>
      builder.mkExponentiate(obj.asGroupElement, arg.asBigInt)
    })
    .withInfo(Exponentiate,
      "Exponentiate this \\lst{GroupElement} to the given number. Returns this to the power of k",
      ArgInfo("k", "The power"))

  lazy val MultiplyMethod: SMethod = SMethod(
    this, "multiply", SFunc(Array(this.ownerType, SGroupElement), this.ownerType), 4, MultiplyGroup.costKind)
    .withIRInfo({ case (builder, obj, _, Seq(arg), _) =>
      builder.mkMultiplyGroup(obj.asGroupElement, arg.asGroupElement)
    })
    .withInfo(MultiplyGroup, "Group operation.", ArgInfo("other", "other element of the group"))

  /** Cost of: 1) calling EcPoint.negate 2) wrapping in GroupElement. */
  val Negate_CostKind = FixedCost(JitCost(45))

  lazy val NegateMethod: SMethod = SMethod(
    this, "negate", SFunc(this.ownerType, this.ownerType), 5, Negate_CostKind)
    .withIRInfo(MethodCallIrBuilder)
    .withInfo(PropertyCall, "Inverse element of the group.")

  protected override def getMethods(): Seq[SMethod] = super.getMethods() ++ Seq(
    /* TODO soft-fork: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
    SMethod(this, "isIdentity", SFunc(this, SBoolean),   1)
        .withInfo(PropertyCall, "Checks if this value is identity element of the eliptic curve group."),
    */
    GetEncodedMethod,
    ExponentiateMethod,
    MultiplyMethod,
    NegateMethod
  )
}

/** Methods of type `SigmaProp` which represent sigma-protocol propositions. */
case object SSigmaPropMethods extends MonoTypeMethods {
  /** Type for which this container defines methods. */
  override def ownerType: SMonoType = SSigmaProp

  /** The maximum size of SigmaProp value in serialized byte array representation. */
  val MaxSizeInBytes: Long = SigmaConstants.MaxSigmaPropSizeInBytes.value

  val PropBytes = "propBytes"
  val IsProven = "isProven"
  lazy val PropBytesMethod = SMethod(
    this, PropBytes, SFunc(this.ownerType, SByteArray), 1, SigmaPropBytes.costKind)
    .withInfo(SigmaPropBytes, "Serialized bytes of this sigma proposition taken as ErgoTree.")

  lazy val IsProvenMethod = SMethod(this, IsProven, SFunc(this.ownerType, SBoolean), 2, null)
      .withInfo(// available only at frontend of ErgoScript
        "Verify that sigma proposition is proven.")

  protected override def getMethods() = super.getMethods() ++ Seq(
    PropBytesMethod, IsProvenMethod
  )
}

/** Any other type is implicitly subtype of this type. */
case object SAnyMethods extends MonoTypeMethods {
  override def ownerType: SMonoType = SAny
}

/** The type with single inhabitant value `()` */
case object SUnitMethods extends MonoTypeMethods {
  /** Type for which this container defines methods. */
  override def ownerType = SUnit
}

object SOptionMethods extends MethodsContainer {
  /** Type for which this container defines methods. */
  override def ownerType: STypeCompanion = SOption

  /** Code of `Option[_]` type constructor. */
  val OptionTypeConstrId = 3
  /** Type code for `Option[T] for some T` type used in TypeSerializer. */
  val OptionTypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * OptionTypeConstrId).toByte
  /** Code of `Option[Coll[_]]` type constructor. */
  val OptionCollectionTypeConstrId = 4
  /** Type code for `Option[Coll[T]] for some T` type used in TypeSerializer. */
  val OptionCollectionTypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * OptionCollectionTypeConstrId).toByte

  val IsDefined = "isDefined"
  val Get = "get"
  val GetOrElse = "getOrElse"

  import SType.{paramR, paramT, tR, tT}

  /** Type descriptor of `this` argument used in the methods below. */
  val ThisType = SOption(tT)

  /** The following SMethod instances are descriptors of methods defined in `Option` type. */
  val IsDefinedMethod = SMethod(
    this, IsDefined, SFunc(ThisType, SBoolean), 2, OptionIsDefined.costKind)
      .withIRInfo({
        case (builder, obj, _, args, _) if args.isEmpty => builder.mkOptionIsDefined(obj.asValue[SOption[SType]])
      })
      .withInfo(OptionIsDefined,
        "Returns \\lst{true} if the option is an instance of \\lst{Some}, \\lst{false} otherwise.")

  val GetMethod = SMethod(this, Get, SFunc(ThisType, tT), 3, OptionGet.costKind)
      .withIRInfo({
        case (builder, obj, _, args, _) if args.isEmpty => builder.mkOptionGet(obj.asValue[SOption[SType]])
      })
      .withInfo(OptionGet,
      """Returns the option's value. The option must be nonempty. Throws exception if the option is empty.""")

  lazy val GetOrElseMethod = SMethod(
    this, GetOrElse, SFunc(Array(ThisType, tT), tT, Array[STypeParam](tT)), 4, OptionGetOrElse.costKind)
      .withIRInfo(irBuilder = {
        case (builder, obj, _, Seq(d), _) => builder.mkOptionGetOrElse(obj.asValue[SOption[SType]], d)
      })
      .withInfo(OptionGetOrElse,
        """Returns the option's value if the option is nonempty, otherwise
         |return the result of evaluating \lst{default}.
        """.stripMargin, ArgInfo("default", "the default value"))

// TODO soft-fork: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
//  val FoldMethod = SMethod(
//    this, Fold, SFunc(Array(ThisType, tR, SFunc(tT, tR)), tR, Array[STypeParam](tT, tR)), 5, FixedCost(JitCost(1)))
//      .withInfo(MethodCall,
//        """Returns the result of applying \lst{f} to this option's
//         |  value if the option is nonempty.  Otherwise, evaluates
//         |  expression \lst{ifEmpty}.
//         |  This is equivalent to \lst{option map f getOrElse ifEmpty}.
//        """.stripMargin,
//        ArgInfo("ifEmpty", "the expression to evaluate if empty"),
//        ArgInfo("f", "the function to apply if nonempty"))

  val MapMethod = SMethod(this, "map",
    SFunc(Array(ThisType, SFunc(tT, tR)), SOption(tR), Array(paramT, paramR)), 7, FixedCost(JitCost(20)))
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(MethodCall,
        """Returns a \lst{Some} containing the result of applying \lst{f} to this option's
         |   value if this option is nonempty.
         |   Otherwise return \lst{None}.
        """.stripMargin, ArgInfo("f", "the function to apply"))

  val FilterMethod = SMethod(this, "filter",
    SFunc(Array(ThisType, SFunc(tT, SBoolean)), ThisType, Array(paramT)), 8, FixedCost(JitCost(20)))
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(MethodCall,
        """Returns this option if it is nonempty and applying the predicate \lst{p} to
         |  this option's value returns true. Otherwise, return \lst{None}.
        """.stripMargin, ArgInfo("p", "the predicate used for testing"))

  override protected def getMethods(): Seq[SMethod] = super.getMethods() ++
      Seq(
        IsDefinedMethod,
        GetMethod,
        GetOrElseMethod,
        /* TODO soft-fork: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
        FoldMethod,
        */
        MapMethod,
        FilterMethod
      )

  /** Creates a descriptor of `Option[T]` type for the given element type `T`. */
  def apply[T <: SType](implicit elemType: T, @unused ov: Overloaded1): SOption[T] = SOption(elemType)
}

object SCollectionMethods extends MethodsContainer with MethodByNameUnapply {
  import SType.{paramIV, paramIVSeq, paramOV}

  /** Type for which this container defines methods. */
  override def ownerType: STypeCompanion = SCollection

  /** Helper descriptors reused across different method descriptors. */
  def tIV = SType.tIV
  def tOV = SType.tOV

  /** This descriptors are instantiated once here and then reused. */
  val ThisType = SCollection(tIV)
  val tOVColl = SCollection(tOV)
  val tPredicate = SFunc(tIV, SBoolean)

  /** The following SMethod instances are descriptors of methods defined in `Coll` type. */
  val SizeMethod = SMethod(this, "size", SFunc(ThisType, SInt), 1, SizeOf.costKind)
      .withInfo(SizeOf, "The size of the collection in elements.")

  val GetOrElseMethod = SMethod(
    this, "getOrElse", SFunc(Array(ThisType, SInt, tIV), tIV, paramIVSeq), 2, DynamicCost)
      .withIRInfo({ case (builder, obj, _, Seq(index, defaultValue), _) =>
        val index1 = index.asValue[SInt.type]
        val defaultValue1 = defaultValue.asValue[SType]
        builder.mkByIndex(obj.asValue[SCollection[SType]], index1, Some(defaultValue1))
      })
      .withInfo(ByIndex, "Return the element of collection if \\lst{index} is in range \\lst{0 .. size-1}",
        ArgInfo("index", "index of the element of this collection"),
        ArgInfo("default", "value to return when \\lst{index} is out of range"))

  /** Implements evaluation of Coll.getOrElse method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def getOrElse_eval[A](mc: MethodCall, xs: Coll[A], i: Int, default: A)(implicit E: ErgoTreeEvaluator): A = {
    E.addCost(ByIndex.costKind, mc.method.opDesc)
    // the following lines should be semantically the same as in ByIndex.eval
    Value.checkType(mc.args.last.tpe, default)
    xs.getOrElse(i, default)
  }

  val MapMethod = SMethod(this, "map",
    SFunc(Array(ThisType, SFunc(tIV, tOV)), tOVColl, Array(paramIV, paramOV)), 3, MapCollection.costKind)
      .withIRInfo({
        case (builder, obj, _, Seq(mapper), _) => builder.mkMapCollection(obj.asValue[SCollection[SType]], mapper.asFunc)
      })
      .withInfo(MapCollection,
        """ Builds a new collection by applying a function to all elements of this collection.
         | Returns a new collection of type \lst{Coll[B]} resulting from applying the given function
         | \lst{f} to each element of this collection and collecting the results.
        """.stripMargin,
        ArgInfo("f", "the function to apply to each element"))

  /** Implements evaluation of Coll.map method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def map_eval[A,B](mc: MethodCall, xs: Coll[A], f: A => B)(implicit E: ErgoTreeEvaluator): Coll[B] = {
    val tpeB = mc.tpe.asInstanceOf[SCollection[SType]].elemType
    val tB = Evaluation.stypeToRType(tpeB).asInstanceOf[RType[B]]
    E.addSeqCostNoOp(MapCollection.costKind, xs.length, mc.method.opDesc)
    xs.map(f)(tB)
  }

  val ExistsMethod = SMethod(this, "exists",
    SFunc(Array(ThisType, tPredicate), SBoolean, paramIVSeq), 4, Exists.costKind)
      .withIRInfo({
        case (builder, obj, _, Seq(c), _) => builder.mkExists(obj.asValue[SCollection[SType]], c.asFunc)
      })
      .withInfo(Exists,
        """Tests whether a predicate holds for at least one element of this collection.
         |Returns \lst{true} if the given predicate \lst{p} is satisfied by at least one element of this collection, otherwise \lst{false}
        """.stripMargin,
        ArgInfo("p", "the predicate used to test elements"))

  val FoldMethod = SMethod(
    this, "fold",
    SFunc(Array(ThisType, tOV, SFunc(Array(tOV, tIV), tOV)), tOV, Array(paramIV, paramOV)),
    5, Fold.costKind)
      .withIRInfo({
        case (builder, obj, _, Seq(z, op), _) => builder.mkFold(obj.asValue[SCollection[SType]], z, op.asFunc)
      })
      .withInfo(Fold, "Applies a binary operator to a start value and all elements of this collection, going left to right.",
        ArgInfo("zero", "a starting value"),
        ArgInfo("op", "the binary operator"))

  val ForallMethod = SMethod(this, "forall",
    SFunc(Array(ThisType, tPredicate), SBoolean, paramIVSeq), 6, ForAll.costKind)
      .withIRInfo({
        case (builder, obj, _, Seq(c), _) => builder.mkForAll(obj.asValue[SCollection[SType]], c.asFunc)
      })
      .withInfo(ForAll,
        """Tests whether a predicate holds for all elements of this collection.
         |Returns \lst{true} if this collection is empty or the given predicate \lst{p}
         |holds for all elements of this collection, otherwise \lst{false}.
        """.stripMargin,
        ArgInfo("p", "the predicate used to test elements"))

  val SliceMethod = SMethod(this, "slice",
    SFunc(Array(ThisType, SInt, SInt), ThisType, paramIVSeq), 7, Slice.costKind)
      .withIRInfo({
        case (builder, obj, _, Seq(from, until), _) =>
          builder.mkSlice(obj.asCollection[SType], from.asIntValue, until.asIntValue)
      })
      .withInfo(Slice,
        """Selects an interval of elements.  The returned collection is made up
         |  of all elements \lst{x} which satisfy the invariant:
         |  \lst{
         |     from <= indexOf(x) < until
         |  }
        """.stripMargin,
        ArgInfo("from", "the lowest index to include from this collection"),
        ArgInfo("until", "the lowest index to EXCLUDE from this collection"))

  val FilterMethod = SMethod(this, "filter",
    SFunc(Array(ThisType, tPredicate), ThisType, paramIVSeq), 8, Filter.costKind)
      .withIRInfo({
        case (builder, obj, _, Seq(l), _) => builder.mkFilter(obj.asValue[SCollection[SType]], l.asFunc)
      })
      .withInfo(Filter,
        """Selects all elements of this collection which satisfy a predicate.
         | Returns  a new collection consisting of all elements of this collection that satisfy the given
         | predicate \lst{p}. The order of the elements is preserved.
        """.stripMargin,
        ArgInfo("p", "the predicate used to test elements."))

  val AppendMethod = SMethod(this, "append",
    SFunc(Array(ThisType, ThisType), ThisType, paramIVSeq), 9, Append.costKind)
      .withIRInfo({
        case (builder, obj, _, Seq(xs), _) =>
          builder.mkAppend(obj.asCollection[SType], xs.asCollection[SType])
      })
      .withInfo(Append, "Puts the elements of other collection after the elements of this collection (concatenation of 2 collections)",
        ArgInfo("other", "the collection to append at the end of this"))

  val ApplyMethod = SMethod(this, "apply",
    SFunc(Array(ThisType, SInt), tIV, Array[STypeParam](tIV)), 10, ByIndex.costKind)
      .withInfo(ByIndex,
        """The element at given index.
         | Indices start at \lst{0}; \lst{xs.apply(0)} is the first element of collection \lst{xs}.
         | Note the indexing syntax \lst{xs(i)} is a shorthand for \lst{xs.apply(i)}.
         | Returns the element at the given index.
         | Throws an exception if \lst{i < 0} or \lst{length <= i}
        """.stripMargin, ArgInfo("i", "the index"))

  /** Cost of creating a collection of indices */
  val IndicesMethod_CostKind = PerItemCost(
    baseCost = JitCost(20), perChunkCost = JitCost(2), chunkSize = 16)

  val IndicesMethod = SMethod(
    this, "indices", SFunc(ThisType, SCollection(SInt)), 14, IndicesMethod_CostKind)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(PropertyCall,
        """Produces the range of all indices of this collection as a new collection
         | containing [0 .. length-1] values.
        """.stripMargin)

  /** Implements evaluation of Coll.indices method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def indices_eval[A, B](mc: MethodCall, xs: Coll[A])
                        (implicit E: ErgoTreeEvaluator): Coll[Int] = {
    val m = mc.method
    E.addSeqCost(m.costKind.asInstanceOf[PerItemCost], xs.length, m.opDesc) { () =>
      xs.indices
    }
  }
  /** BaseCost:
    * 1) base cost of Coll.flatMap
    * PerChunkCost:
    * 1) cost of Coll.flatMap (per item)
    * 2) new collection is allocated for each item
    * 3) each collection is then appended to the resulting collection */
  val FlatMapMethod_CostKind = PerItemCost(
    baseCost = JitCost(60), perChunkCost = JitCost(10), chunkSize = 8)

  val FlatMapMethod = SMethod(this, "flatMap",
    SFunc(Array(ThisType, SFunc(tIV, tOVColl)), tOVColl, Array(paramIV, paramOV)),
    15, FlatMapMethod_CostKind)
      .withIRInfo(
        MethodCallIrBuilder,
        javaMethodOf[Coll[_], Function1[_,_], RType[_]]("flatMap"),
        { mtype => Array(mtype.tRange.asCollection[SType].elemType) })
      .withInfo(MethodCall,
        """ Builds a new collection by applying a function to all elements of this collection
         | and using the elements of the resulting collections.
         | Function \lst{f} is constrained to be of the form \lst{x => x.someProperty}, otherwise
         | it is illegal.
         | Returns a new collection of type \lst{Coll[B]} resulting from applying the given collection-valued function
         | \lst{f} to each element of this collection and concatenating the results.
        """.stripMargin, ArgInfo("f", "the function to apply to each element."))

  /** We assume all flatMap body patterns have similar executon cost. */
  final val CheckFlatmapBody_Info = OperationCostInfo(
    PerItemCost(baseCost = JitCost(20), perChunkCost = JitCost(20), chunkSize = 1),
    NamedDesc("CheckFlatmapBody"))


  /** This patterns recognize all expressions, which are allowed as lambda body
    * of flatMap. Other bodies are rejected with throwing exception.
    */
  val flatMap_BodyPatterns = Array[PartialFunction[SValue, Int]](
    { case MethodCall(ValUse(id, _), _, args, _) if args.isEmpty => id },
    { case ExtractScriptBytes(ValUse(id, _)) => id },
    { case ExtractId(ValUse(id, _)) => id },
    { case SigmaPropBytes(ValUse(id, _)) => id },
    { case ExtractBytes(ValUse(id, _)) => id },
    { case ExtractBytesWithNoRef(ValUse(id, _)) => id }
  )

  /** Check the given expression is valid body of flatMap argument lambda.
    * @param varId id of lambda variable (see [[FuncValue]].args)
    * @param expr expression with is expected to use varId in ValUse node.
    * @return true if the body is allowed
    */
  def isValidPropertyAccess(varId: Int, expr: SValue)
                           (implicit E: ErgoTreeEvaluator): Boolean = {
    var found = false
    // NOTE: the cost depends on the position of the pattern since
    // we are checking until the first matching pattern found.
    E.addSeqCost(CheckFlatmapBody_Info) { () =>
      // the loop is bounded because flatMap_BodyPatterns is fixed
      var i = 0
      val nPatterns = flatMap_BodyPatterns.length
      while (i < nPatterns && !found) {
        val p = flatMap_BodyPatterns(i)
        found = p.lift(expr) match {
          case Some(id) => id == varId  // `id` in the pattern is equal to lambda `varId`
          case None => false
        }
        i += 1
      }
      i // how many patterns checked
    }
    found
  }

  /** Operation descriptor for matching `flatMap` method calls with valid lambdas. */
  final val MatchSingleArgMethodCall_Info = OperationCostInfo(
    FixedCost(JitCost(30)), NamedDesc("MatchSingleArgMethodCall"))

  /** Recognizer of `flatMap` method calls with valid lambdas. */
  object IsSingleArgMethodCall {
    def unapply(mc:MethodCall)
               (implicit E: ErgoTreeEvaluator): Nullable[(Int, SValue)] = {
      var res: Nullable[(Int, SValue)] = Nullable.None
      E.addFixedCost(MatchSingleArgMethodCall_Info) {
        res = mc match {
          case MethodCall(_, _, Seq(FuncValue(args, body)), _) if args.length == 1 =>
            val id = args(0)._1
            Nullable((id, body))
          case _ =>
            Nullable.None
        }
      }
      res
    }
  }

  /** Checks that the given [[MethodCall]] operation is valid flatMap. */
  def checkValidFlatmap(mc: MethodCall)(implicit E: ErgoTreeEvaluator) = {
    mc match {
      case IsSingleArgMethodCall(varId, lambdaBody)
            if isValidPropertyAccess(varId, lambdaBody) =>
        // ok, do nothing
      case _ =>
        throwInvalidFlatmap(mc)
    }
  }

  def throwInvalidFlatmap(mc: MethodCall) = {
    sys.error(
      s"Unsupported lambda in flatMap: allowed usage `xs.flatMap(x => x.property)`: $mc")
  }

  /** Implements evaluation of Coll.flatMap method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def flatMap_eval[A, B](mc: MethodCall, xs: Coll[A], f: A => Coll[B])
                        (implicit E: ErgoTreeEvaluator): Coll[B] = {
    val m = mc.method
    var res: Coll[B] = null
    E.addSeqCost(m.costKind.asInstanceOf[PerItemCost], m.opDesc) { () =>
      val tpeB = mc.tpe.asInstanceOf[SCollection[SType]].elemType
      val tB = Evaluation.stypeToRType(tpeB).asInstanceOf[RType[B]]
      res = xs.flatMap(f)(tB)
      res.length
    }
    res
  }

  val PatchMethod = SMethod(this, "patch",
    SFunc(Array(ThisType, SInt, ThisType, SInt), ThisType, paramIVSeq),
      19, PerItemCost(baseCost = JitCost(30), perChunkCost = JitCost(2), chunkSize = 10))
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(MethodCall,
        "Produces a new Coll where a slice of elements in this Coll is replaced by another Coll.")

  /** Implements evaluation of Coll.patch method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def patch_eval[A](mc: MethodCall, xs: Coll[A], from: Int, patch: Coll[A], replaced: Int)
                   (implicit E: ErgoTreeEvaluator): Coll[A] = {
    val m = mc.method
    val nItems = xs.length + patch.length
    E.addSeqCost(m.costKind.asInstanceOf[PerItemCost], nItems, m.opDesc) { () =>
      xs.patch(from, patch, replaced)
    }
  }

  val UpdatedMethod = SMethod(this, "updated",
    SFunc(Array(ThisType, SInt, tIV), ThisType, paramIVSeq),
    20, PerItemCost(baseCost = JitCost(20), perChunkCost = JitCost(1), chunkSize = 10))
      .withIRInfo(MethodCallIrBuilder, javaMethodOf[Coll[_], Int, Any]("updated"))
      .withInfo(MethodCall,
        "A copy of this Coll with one single replaced element.")

  /** Implements evaluation of Coll.updated method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def updated_eval[A](mc: MethodCall, coll: Coll[A], index: Int, elem: A)
                     (implicit E: ErgoTreeEvaluator): Coll[A] = {
    val m = mc.method
    val costKind = m.costKind.asInstanceOf[PerItemCost]
    E.addSeqCost(costKind, coll.length, m.opDesc) { () =>
      coll.updated(index, elem)
    }
  }

  val UpdateManyMethod = SMethod(this, "updateMany",
    SFunc(Array(ThisType, SCollection(SInt), ThisType), ThisType, paramIVSeq),
    21, PerItemCost(baseCost = JitCost(20), perChunkCost = JitCost(2), chunkSize = 10))
      .withIRInfo(MethodCallIrBuilder).withInfo(MethodCall, "")

  /** Implements evaluation of Coll.updateMany method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def updateMany_eval[A](mc: MethodCall, coll: Coll[A], indexes: Coll[Int], values: Coll[A])
                        (implicit E: ErgoTreeEvaluator): Coll[A] = {
    val costKind = mc.method.costKind.asInstanceOf[PerItemCost]
    E.addSeqCost(costKind, coll.length, mc.method.opDesc) { () =>
      coll.updateMany(indexes, values)
    }
  }

  val IndexOfMethod = SMethod(this, "indexOf",
    SFunc(Array(ThisType, tIV, SInt), SInt, paramIVSeq),
      26, PerItemCost(baseCost = JitCost(20), perChunkCost = JitCost(10), chunkSize = 2))
      .withIRInfo(MethodCallIrBuilder, javaMethodOf[Coll[_], Any, Int]("indexOf"))
      .withInfo(MethodCall, "Returns index of a collection element, or -1 if not found")

  /** Implements evaluation of Coll.indexOf method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def indexOf_eval[A](mc: MethodCall, xs: Coll[A], elem: A, from: Int)
                     (implicit E: ErgoTreeEvaluator): Int = {
    val costKind = mc.method.costKind.asInstanceOf[PerItemCost]
    var res: Int = -1
    E.addSeqCost(costKind, mc.method.opDesc) { () =>
      // this loop is bounded because MaxArrayLength limit is enforced
      val len = xs.length
      val start = math.max(from, 0)
      var i = start
      var different = true
      while (i < len && different) {
        different = !DataValueComparer.equalDataValues(xs(i), elem)
        i += 1
      }
      if (!different)
        res = i - 1
      i - start  // return number of performed iterations
    }
    res
  }

  /** Cost descriptor of Coll.zip operation. */
  val Zip_CostKind = PerItemCost(
    baseCost = JitCost(10), perChunkCost = JitCost(1), chunkSize = 10)

  val ZipMethod = SMethod(this, "zip",
    SFunc(Array(ThisType, tOVColl), SCollection(STuple(tIV, tOV)), Array[STypeParam](tIV, tOV)), 29, Zip_CostKind)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(MethodCall, "")

  /** Implements evaluation of Coll.zip method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def zip_eval[A, B](mc: MethodCall, xs: Coll[A], ys: Coll[B])
                    (implicit E: ErgoTreeEvaluator): Coll[(A,B)] = {
    val m = mc.method
    E.addSeqCost(m.costKind.asInstanceOf[PerItemCost], xs.length, m.opDesc) { () =>
      xs.zip(ys)
    }
  }

  // ======== 6.0 methods below ===========

  private val reverseCostKind = Append.costKind

  val ReverseMethod = SMethod(this, "reverse",
    SFunc(Array(ThisType), ThisType, paramIVSeq), 30, reverseCostKind)
    .withIRInfo(MethodCallIrBuilder)
    .withInfo(MethodCall, "")

  /** Implements evaluation of Coll.reverse method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def reverse_eval[A](mc: MethodCall, xs: Coll[A])
                    (implicit E: ErgoTreeEvaluator): Coll[A] = {
    val m = mc.method
    E.addSeqCost(m.costKind.asInstanceOf[PerItemCost], xs.length, m.opDesc) { () =>
      xs.reverse
    }
  }

  private val distinctCostKind = PerItemCost(baseCost = JitCost(60), perChunkCost = JitCost(5), chunkSize = 100)

  val DistinctMethod = SMethod(this, "distinct",
    SFunc(Array(ThisType), ThisType, paramIVSeq), 31, distinctCostKind)
    .withIRInfo(MethodCallIrBuilder)
    .withInfo(MethodCall, "Returns inversed collection.")

  /** Implements evaluation of Coll.reverse method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def distinct_eval[A](mc: MethodCall, xs: Coll[A])
                     (implicit E: ErgoTreeEvaluator): Coll[A] = {
    val m = mc.method
    E.addSeqCost(m.costKind.asInstanceOf[PerItemCost], xs.length, m.opDesc) { () =>
      xs.distinct
    }
  }

  private val startsWithCostKind = Zip_CostKind

  val StartsWithMethod = SMethod(this, "startsWith",
    SFunc(Array(ThisType, ThisType), SBoolean, paramIVSeq), 32, startsWithCostKind)
    .withIRInfo(MethodCallIrBuilder)
    .withInfo(MethodCall, "Returns true if this collection starts with given one, false otherwise.",
      ArgInfo("prefix", "Collection to be checked for being a prefix of this collection."))

  /** Implements evaluation of Coll.zip method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def startsWith_eval[A](mc: MethodCall, xs: Coll[A], ys: Coll[A])
                    (implicit E: ErgoTreeEvaluator): Boolean = {
    val m = mc.method
    E.addSeqCost(m.costKind.asInstanceOf[PerItemCost], xs.length, m.opDesc) { () =>
      xs.startsWith(ys)
    }
  }

  private val endsWithCostKind = Zip_CostKind

  val EndsWithMethod = SMethod(this, "endsWith",
    SFunc(Array(ThisType, ThisType), SBoolean, paramIVSeq), 33, endsWithCostKind)
    .withIRInfo(MethodCallIrBuilder)
    .withInfo(MethodCall, "Returns true if this collection ends with given one, false otherwise.",
      ArgInfo("suffix", "Collection to be checked for being a suffix of this collection."))

  /** Implements evaluation of Coll.zip method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def endsWith_eval[A](mc: MethodCall, xs: Coll[A], ys: Coll[A])
                        (implicit E: ErgoTreeEvaluator): Boolean = {
    val m = mc.method
    E.addSeqCost(m.costKind.asInstanceOf[PerItemCost], xs.length, m.opDesc) { () =>
      xs.endsWith(ys)
    }
  }

  val GetMethod = SMethod(this, "get",
    SFunc(Array(ThisType, SInt), SOption(tIV), Array[STypeParam](tIV)), 34, ByIndex.costKind)
    .withIRInfo(MethodCallIrBuilder)
    .withInfo(MethodCall,
              "Returns Some(element) if there is an element at given index, None otherwise.",
              ArgInfo("index", "Index of an element (starting from 0).")
            )

  private val v5Methods = super.getMethods() ++ Seq(
    SizeMethod,
    GetOrElseMethod,
    MapMethod,
    ExistsMethod,
    FoldMethod,
    ForallMethod,
    SliceMethod,
    FilterMethod,
    AppendMethod,
    ApplyMethod,
    IndicesMethod,
    FlatMapMethod,
    PatchMethod,
    UpdatedMethod,
    UpdateManyMethod,
    IndexOfMethod,
    ZipMethod
  )

  private val v6Methods = v5Methods ++ Seq(
    ReverseMethod,
    DistinctMethod,
    StartsWithMethod,
    EndsWithMethod,
    GetMethod
  )

  /** This method should be overriden in derived classes to add new methods in addition to inherited.
    * Typical override: `super.getMethods() ++ Seq(m1, m2, m3)`
    */
  override protected def getMethods(): Seq[SMethod] = {
    if (VersionContext.current.isV6SoftForkActivated) {
      v6Methods
    } else {
      v5Methods
    }
  }

}

object STupleMethods extends MethodsContainer {
  /** Type for which this container defines methods. */
  override def ownerType: STypeCompanion = STuple

  private val MaxTupleLength: Int = SigmaConstants.MaxTupleLength.value

  private val componentNames = Array.tabulate(MaxTupleLength) { i => s"_${i + 1}" }

  /** A list of Coll methods inherited from Coll type and available as method of tuple. */
  lazy val colMethods: Seq[SMethod] = {
    val subst = Map(SType.tIV -> SAny)
    // TODO: implement other methods
    val activeMethods = Set(1.toByte /*Coll.size*/, 10.toByte /*Coll.apply*/)
    SCollectionMethods.methods.filter(m => activeMethods.contains(m.methodId)).map { m =>
      m.copy(stype = applySubst(m.stype, subst).asFunc)
    }
  }

  def getTupleMethod(tup: STuple, name: String): Option[SMethod] = {
    colMethods.find(_.name == name).orElse {
      val iComponent = componentNames.lastIndexOf(name, end = tup.items.length - 1)
      if (iComponent == -1) None
      else
        Some(SMethod(
          STupleMethods, name, SFunc(tup, tup.items(iComponent)),
          (iComponent + 1).toByte, SelectField.costKind))
    }
  }

  override protected def getMethods(): Seq[SMethod] = super.getMethods()
}

/** Type descriptor of `Box` type of ErgoTree. */
case object SBoxMethods extends MonoTypeMethods {
  import ErgoBox._
  import SType.{paramT, tT}

  override def ownerType: SMonoType = SBox

  /** Defined once here and then reused in SMethod descriptors. */
  lazy val GetRegFuncType = SFunc(Array(SBox), SOption(tT), Array(paramT))

  /** Creates a descriptor for the given register method. (i.e. R1, R2, etc) */
  def registers(idOfs: Int): Seq[SMethod] = {
    allRegisters.map { i =>
      i match {
        case r: MandatoryRegisterId =>
          SMethod(this, s"R${i.asIndex}",
            GetRegFuncType, (idOfs + i.asIndex + 1).toByte, ExtractRegisterAs.costKind)
              .withInfo(ExtractRegisterAs, r.purpose)
        case _ =>
          SMethod(this, s"R${i.asIndex}",
            GetRegFuncType, (idOfs + i.asIndex + 1).toByte, ExtractRegisterAs.costKind)
              .withInfo(ExtractRegisterAs, "Non-mandatory register")
      }
    }
  }

  val PropositionBytes = "propositionBytes"
  val Value = "value"
  val Id = "id"
  val Bytes = "bytes"
  val BytesWithoutRef = "bytesWithoutRef"
  val CreationInfo = "creationInfo"
  val GetReg = "getReg"

  // should be lazy, otherwise lead to initialization error
  lazy val ValueMethod = SMethod(
    this, Value, SFunc(SBox, SLong), 1, ExtractAmount.costKind)
      .withInfo(ExtractAmount,
        "Mandatory: Monetary value, in Ergo tokens (NanoErg unit of measure)")

  lazy val PropositionBytesMethod = SMethod(
    this, PropositionBytes, SFunc(SBox, SByteArray), 2, ExtractScriptBytes.costKind)
      .withInfo(ExtractScriptBytes,
        "Serialized bytes of guarding script, which should be evaluated to true in order to\n" +
        " open this box. (aka spend it in a transaction)")

  lazy val BytesMethod = SMethod(
    this, Bytes, SFunc(SBox, SByteArray), 3, ExtractBytes.costKind)
      .withInfo(ExtractBytes, "Serialized bytes of this box's content, including proposition bytes.")

  lazy val BytesWithoutRefMethod = SMethod(
    this, BytesWithoutRef, SFunc(SBox, SByteArray), 4, ExtractBytesWithNoRef.costKind)
      .withInfo(ExtractBytesWithNoRef,
        "Serialized bytes of this box's content, excluding transactionId and index of output.")

  lazy val IdMethod = SMethod(this, Id, SFunc(SBox, SByteArray), 5, ExtractId.costKind)
      .withInfo(ExtractId,
        "Blake2b256 hash of this box's content, basically equals to \\lst{blake2b256(bytes)}")

  lazy val creationInfoMethod = SMethod(
    this, CreationInfo, ExtractCreationInfo.OpType, 6, ExtractCreationInfo.costKind)
      .withInfo(ExtractCreationInfo,
        """ If \lst{tx} is a transaction which generated this box, then \lst{creationInfo._1}
         | is a height of the tx's block. The \lst{creationInfo._2} is a serialized transaction
         | identifier followed by box index in the transaction outputs.
        """.stripMargin ) // see ExtractCreationInfo

  lazy val getRegMethodV5 = SMethod(this, "getReg",
    SFunc(Array(SBox, SInt), SOption(tT), Array(paramT)), 7, ExtractRegisterAs.costKind)
      .withInfo(ExtractRegisterAs,
        """ Extracts register by id and type.
         | Type param \lst{T} expected type of the register.
         | Returns \lst{Some(value)} if the register is defined and has given type and \lst{None} otherwise
        """.stripMargin,
        ArgInfo("regId", "zero-based identifier of the register."))

  lazy val getRegMethodV6 = SMethod(this, "getReg",
    SFunc(Array(SBox, SInt), SOption(tT), Array(paramT)), 7, ExtractRegisterAs.costKind, Seq(tT))
    .withIRInfo(MethodCallIrBuilder,
      javaMethodOf[Box, Int, RType[_]]("getReg"),
      { mtype => Array(mtype.tRange.asOption[SType].elemType) })
    .withInfo(MethodCall, """ Extracts register by id and type.
                            | Type param \lst{T} expected type of the register.
                            | Returns \lst{Some(value)} if the register is defined and has given type and \lst{None} otherwise
        """.stripMargin,
      ArgInfo("regId", "zero-based identifier of the register."))

  lazy val tokensMethod = SMethod(
    this, "tokens", SFunc(SBox, ErgoBox.STokensRegType), 8, FixedCost(JitCost(15)))
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(PropertyCall, "Secondary tokens")

  lazy val commonBoxMethods = super.getMethods() ++ Array(
    ValueMethod, // see ExtractAmount
    PropositionBytesMethod, // see ExtractScriptBytes
    BytesMethod, // see ExtractBytes
    BytesWithoutRefMethod, // see ExtractBytesWithNoRef
    IdMethod, // see ExtractId
    creationInfoMethod,
    tokensMethod
  ) ++ registers(8)

  lazy val v5Methods = commonBoxMethods ++ Array(
    getRegMethodV5
  )

  lazy val v6Methods = commonBoxMethods ++ Array(
    getRegMethodV6
  )

  // should be lazy to solve recursive initialization
  protected override def getMethods() = {
    if (VersionContext.current.isV6SoftForkActivated) {
      v6Methods
    } else {
      v5Methods
    }
  }

}

/** Type descriptor of `AvlTree` type of ErgoTree. */
case object SAvlTreeMethods extends MonoTypeMethods {
  import SOption._

  override def ownerType: SMonoType = SAvlTree

  lazy val TCollOptionCollByte = SCollection(SByteArrayOption)
  lazy val CollKeyValue = SCollection(STuple(SByteArray, SByteArray))

  lazy val digestMethod = SMethod(this, "digest", SFunc(SAvlTree, SByteArray), 1, FixedCost(JitCost(15)))
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(PropertyCall,
        """Returns digest of the state represented by this tree.
         | Authenticated tree \lst{digest} = \lst{root hash bytes} ++ \lst{tree height}
        """.stripMargin)

  /** Cost descriptor of `digest` method. */
  lazy val digest_Info = {
    val m = digestMethod
    OperationCostInfo(m.costKind.asInstanceOf[FixedCost], m.opDesc)
  }

  lazy val enabledOperationsMethod = SMethod(
    this, "enabledOperations", SFunc(SAvlTree, SByte), 2, FixedCost(JitCost(15)))
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(PropertyCall,
        """ Flags of enabled operations packed in single byte.
         | \lst{isInsertAllowed == (enabledOperations & 0x01) != 0}\newline
         | \lst{isUpdateAllowed == (enabledOperations & 0x02) != 0}\newline
         | \lst{isRemoveAllowed == (enabledOperations & 0x04) != 0}
        """.stripMargin)

  lazy val keyLengthMethod = SMethod(
    this, "keyLength", SFunc(SAvlTree, SInt), 3, FixedCost(JitCost(15)))
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(PropertyCall,
        """
         |
            """.stripMargin)

  lazy val valueLengthOptMethod = SMethod(
    this, "valueLengthOpt", SFunc(SAvlTree, SIntOption), 4, FixedCost(JitCost(15)))
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(PropertyCall,
        """
         |
        """.stripMargin)

  lazy val isInsertAllowedMethod = SMethod(
    this, "isInsertAllowed", SFunc(SAvlTree, SBoolean), 5, FixedCost(JitCost(15)))
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(PropertyCall,
        """
         |
        """.stripMargin)

  lazy val isInsertAllowed_Info = {
    val m = isInsertAllowedMethod
    OperationCostInfo(m.costKind.asInstanceOf[FixedCost], m.opDesc)
  }

  lazy val isUpdateAllowedMethod = SMethod(
    this, "isUpdateAllowed", SFunc(SAvlTree, SBoolean), 6, FixedCost(JitCost(15)))
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(PropertyCall,
        """
         |
        """.stripMargin)

  lazy val isUpdateAllowed_Info = {
    val m = isUpdateAllowedMethod
    OperationCostInfo(m.costKind.asInstanceOf[FixedCost], m.opDesc)
  }

  lazy val isRemoveAllowedMethod = SMethod(
    this, "isRemoveAllowed", SFunc(SAvlTree, SBoolean), 7, FixedCost(JitCost(15)))
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(PropertyCall,
        """
         |
        """.stripMargin)

  lazy val isRemoveAllowed_Info = {
    val m = isRemoveAllowedMethod
    OperationCostInfo(m.costKind.asInstanceOf[FixedCost], m.opDesc)
  }

  lazy val updateOperationsMethod  = SMethod(this, "updateOperations",
    SFunc(Array(SAvlTree, SByte), SAvlTree), 8, FixedCost(JitCost(45)))
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(MethodCall,
        """
         |
        """.stripMargin)

  lazy val containsMethod = SMethod(this, "contains",
    SFunc(Array(SAvlTree, SByteArray, SByteArray), SBoolean), 9, DynamicCost)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(MethodCall,
        """
         |   /** Checks if an entry with key `key` exists in this tree using proof `proof`.
         |    * Throws exception if proof is incorrect
         |
         |    * @note CAUTION! Does not support multiple keys check, use [[getMany]] instead.
         |    * Return `true` if a leaf with the key `key` exists
         |    * Return `false` if leaf with provided key does not exist.
         |    * @param key    a key of an element of this authenticated dictionary.
         |    * @param proof
         |    */
         |
        """.stripMargin)

  /** The proof may contain keys, labels and values, we don't know for sure how many,
    * but we assume the cost is O(proof.length).
    * So the following is an approximation of the proof parsing cost.
    */
  final val CreateAvlVerifier_Info = OperationCostInfo(
    PerItemCost(baseCost = JitCost(110), perChunkCost = JitCost(20), chunkSize = 64),
    NamedDesc("CreateAvlVerifier"))

  final val LookupAvlTree_Info = OperationCostInfo(
    PerItemCost(baseCost = JitCost(40), perChunkCost = JitCost(10), chunkSize = 1),
    NamedDesc("LookupAvlTree"))

  final val InsertIntoAvlTree_Info = OperationCostInfo(
    PerItemCost(baseCost = JitCost(40), perChunkCost = JitCost(10), chunkSize = 1),
    NamedDesc("InsertIntoAvlTree"))

  final val UpdateAvlTree_Info = OperationCostInfo(
    PerItemCost(baseCost = JitCost(120), perChunkCost = JitCost(20), chunkSize = 1),
    NamedDesc("UpdateAvlTree"))

  final val RemoveAvlTree_Info = OperationCostInfo(
    PerItemCost(baseCost = JitCost(100), perChunkCost = JitCost(15), chunkSize = 1),
    NamedDesc("RemoveAvlTree"))

  /** Implements evaluation of AvlTree.contains method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def contains_eval(mc: MethodCall, tree: AvlTree, key: Coll[Byte], proof: Coll[Byte])
                   (implicit E: ErgoTreeEvaluator): Boolean = {
    E.contains_eval(mc, tree, key, proof)
  }

  lazy val getMethod = SMethod(this, "get",
    SFunc(Array(SAvlTree, SByteArray, SByteArray), SByteArrayOption), 10, DynamicCost)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(MethodCall,
        """
         |  /** Perform a lookup of key `key` in this tree using proof `proof`.
         |    * Throws exception if proof is incorrect
         |    *
         |    * @note CAUTION! Does not support multiple keys check, use [[getMany]] instead.
         |    * Return Some(bytes) of leaf with key `key` if it exists
         |    * Return None if leaf with provided key does not exist.
         |    * @param key    a key of an element of this authenticated dictionary.
         |    * @param proof
         |    */
         |
        """.stripMargin)

  /** Implements evaluation of AvlTree.get method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def get_eval(mc: MethodCall, tree: AvlTree, key: Coll[Byte], proof: Coll[Byte])
              (implicit E: ErgoTreeEvaluator): Option[Coll[Byte]] = {
    E.get_eval(mc, tree, key, proof)
  }

  lazy val getManyMethod = SMethod(this, "getMany",
    SFunc(Array(SAvlTree, SByteArray2, SByteArray), TCollOptionCollByte), 11, DynamicCost)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(MethodCall,
        """
         |  /** Perform a lookup of many keys `keys` in this tree using proof `proof`.
         |    *
         |    * @note CAUTION! Keys must be ordered the same way they were in lookup before proof was generated.
         |    * For each key return Some(bytes) of leaf if it exists and None if is doesn't.
         |    * @param keys    keys of elements of this authenticated dictionary.
         |    * @param proof
         |    */
         |
        """.stripMargin)

  /** Implements evaluation of AvlTree.getMany method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def getMany_eval(mc: MethodCall, tree: AvlTree, keys: Coll[Coll[Byte]], proof: Coll[Byte])
                  (implicit E: ErgoTreeEvaluator): Coll[Option[Coll[Byte]]] = {
    E.getMany_eval(mc, tree, keys, proof)
  }

  lazy val insertMethod = SMethod(this, "insert",
    SFunc(Array(SAvlTree, CollKeyValue, SByteArray), SAvlTreeOption), 12, DynamicCost)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(MethodCall,
        """
         |  /** Perform insertions of key-value entries into this tree using proof `proof`.
         |    * Throws exception if proof is incorrect
         |    *
         |    * @note CAUTION! Pairs must be ordered the same way they were in insert ops before proof was generated.
         |    * Return Some(newTree) if successful
         |    * Return None if operations were not performed.
         |    * @param operations   collection of key-value pairs to insert in this authenticated dictionary.
         |    * @param proof
         |    */
         |
        """.stripMargin)

  /** Implements evaluation of AvlTree.insert method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def insert_eval(mc: MethodCall, tree: AvlTree, entries: KeyValueColl, proof: Coll[Byte])
                 (implicit E: ErgoTreeEvaluator): Option[AvlTree] = {
    E.insert_eval(mc, tree, entries, proof)
  }

  lazy val updateMethod = SMethod(this, "update",
    SFunc(Array(SAvlTree, CollKeyValue, SByteArray), SAvlTreeOption), 13, DynamicCost)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(MethodCall,
        """
         |  /** Perform updates of key-value entries into this tree using proof `proof`.
         |    * Throws exception if proof is incorrect
         |    *
         |    * @note CAUTION! Pairs must be ordered the same way they were in update ops before proof was generated.
         |    * Return Some(newTree) if successful
         |    * Return None if operations were not performed.
         |    * @param operations   collection of key-value pairs to update in this authenticated dictionary.
         |    * @param proof
         |    */
         |
        """.stripMargin)

  /** Implements evaluation of AvlTree.update method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def update_eval(mc: MethodCall, tree: AvlTree,
                  operations: KeyValueColl, proof: Coll[Byte])
                 (implicit E: ErgoTreeEvaluator): Option[AvlTree] = {
    E.update_eval(mc, tree, operations, proof)
  }

  lazy val removeMethod = SMethod(this, "remove",
    SFunc(Array(SAvlTree, SByteArray2, SByteArray), SAvlTreeOption), 14, DynamicCost)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(MethodCall,
        """
         |  /** Perform removal of entries into this tree using proof `proof`.
         |    * Throws exception if proof is incorrect
         |    * Return Some(newTree) if successful
         |    * Return None if operations were not performed.
         |    *
         |    * @note CAUTION! Keys must be ordered the same way they were in remove ops before proof was generated.
         |    * @param operations   collection of keys to remove from this authenticated dictionary.
         |    * @param proof
         |    */
         |
        """.stripMargin)

  /** Implements evaluation of AvlTree.remove method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def remove_eval(mc: MethodCall, tree: AvlTree,
                  operations: Coll[Coll[Byte]], proof: Coll[Byte])
                 (implicit E: ErgoTreeEvaluator): Option[AvlTree] = {
    E.remove_eval(mc, tree, operations, proof)
  }

  lazy val updateDigestMethod = SMethod(this, "updateDigest",
    SFunc(Array(SAvlTree, SByteArray), SAvlTree), 15, FixedCost(JitCost(40)))
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(MethodCall,
        """
         |
        """.stripMargin)

  lazy val updateDigest_Info = {
    val m = updateDigestMethod
    OperationCostInfo(m.costKind.asInstanceOf[FixedCost], m.opDesc)
  }

  protected override def getMethods(): Seq[SMethod] = super.getMethods() ++ Seq(
    digestMethod,
    enabledOperationsMethod,
    keyLengthMethod,
    valueLengthOptMethod,
    isInsertAllowedMethod,
    isUpdateAllowedMethod,
    isRemoveAllowedMethod,
    updateOperationsMethod,
    containsMethod,
    getMethod,
    getManyMethod,
    insertMethod,
    updateMethod,
    removeMethod,
    updateDigestMethod
  )
}

/** Type descriptor of `Context` type of ErgoTree. */
case object SContextMethods extends MonoTypeMethods {
  override def ownerType: SMonoType = SContext

  /** Arguments on context operation such as getVar, DeserializeContext etc.
    * This value can be reused where necessary to avoid allocations. */
  val ContextFuncDom: IndexedSeq[SType] = Array(SContext, SByte)

  import SType.{paramT, tT}

  lazy val dataInputsMethod = propertyCall("dataInputs", SBoxArray, 1, FixedCost(JitCost(15)))
  lazy val headersMethod    = propertyCall("headers", SHeaderArray, 2, FixedCost(JitCost(15)))
  lazy val preHeaderMethod  = propertyCall("preHeader", SPreHeader, 3, FixedCost(JitCost(15)))
  lazy val inputsMethod     = property("INPUTS", SBoxArray, 4, Inputs)
  lazy val outputsMethod    = property("OUTPUTS", SBoxArray, 5, Outputs)
  lazy val heightMethod     = property("HEIGHT", SInt, 6, Height)
  lazy val selfMethod       = property("SELF", SBox, 7, Self)
  lazy val selfBoxIndexMethod = propertyCall("selfBoxIndex", SInt, 8, FixedCost(JitCost(20)))
  lazy val lastBlockUtxoRootHashMethod = property("LastBlockUtxoRootHash", SAvlTree, 9, LastBlockUtxoRootHash)
  lazy val minerPubKeyMethod = property("minerPubKey", SByteArray, 10, MinerPubkey)
  lazy val getVarMethod = SMethod(
    this, "getVar", SFunc(ContextFuncDom, SOption(tT), Array(paramT)), 11, GetVar.costKind)
    .withInfo(GetVar, "Get context variable with given \\lst{varId} and type.",
      ArgInfo("varId", "\\lst{Byte} identifier of context variable"))

  protected override def getMethods() = super.getMethods() ++ Seq(
    dataInputsMethod, headersMethod, preHeaderMethod, inputsMethod, outputsMethod, heightMethod, selfMethod,
    selfBoxIndexMethod, lastBlockUtxoRootHashMethod, minerPubKeyMethod, getVarMethod
  )

  /** Names of methods which provide blockchain context.
   * This value can be reused where necessary to avoid allocations. */
  val BlockchainContextMethodNames: IndexedSeq[String] = Array(
    headersMethod.name, preHeaderMethod.name, heightMethod.name,
    lastBlockUtxoRootHashMethod.name, minerPubKeyMethod.name
  )
}

/** Type descriptor of `Header` type of ErgoTree. */
case object SHeaderMethods extends MonoTypeMethods {
  override def ownerType: SMonoType = SHeader

  lazy val idMethod               = propertyCall("id", SByteArray, 1, FixedCost(JitCost(10)))
  lazy val versionMethod          = propertyCall("version",  SByte,      2, FixedCost(JitCost(10)))
  lazy val parentIdMethod         = propertyCall("parentId", SByteArray, 3, FixedCost(JitCost(10)))
  lazy val ADProofsRootMethod     = propertyCall("ADProofsRoot", SByteArray, 4, FixedCost(JitCost(10)))
  lazy val stateRootMethod        = propertyCall("stateRoot", SAvlTree, 5, FixedCost(JitCost(10)))
  lazy val transactionsRootMethod = propertyCall("transactionsRoot", SByteArray, 6, FixedCost(JitCost(10)))
  lazy val timestampMethod        = propertyCall("timestamp", SLong, 7, FixedCost(JitCost(10)))
  lazy val nBitsMethod            = propertyCall("nBits", SLong, 8, FixedCost(JitCost(10)))
  lazy val heightMethod           = propertyCall("height", SInt, 9, FixedCost(JitCost(10)))
  lazy val extensionRootMethod    = propertyCall("extensionRoot", SByteArray, 10, FixedCost(JitCost(10)))
  lazy val minerPkMethod          = propertyCall("minerPk", SGroupElement, 11, FixedCost(JitCost(10)))
  lazy val powOnetimePkMethod     = propertyCall("powOnetimePk", SGroupElement, 12, FixedCost(JitCost(10)))
  lazy val powNonceMethod         = propertyCall("powNonce", SByteArray, 13, FixedCost(JitCost(10)))
  lazy val powDistanceMethod      = propertyCall("powDistance", SBigInt, 14, FixedCost(JitCost(10)))
  lazy val votesMethod            = propertyCall("votes", SByteArray, 15, FixedCost(JitCost(10)))

  // cost of checkPoW is 700 as about 2*32 hashes required, and 1 hash (id) over short data costs 10
  lazy val checkPowMethod = SMethod(
    this, "checkPow", SFunc(Array(SHeader), SBoolean), 16, FixedCost(JitCost(700)))
    .withIRInfo(MethodCallIrBuilder)
    .withInfo(MethodCall, "Validate header's proof-of-work")

  private lazy val v5Methods = super.getMethods() ++ Seq(
    idMethod, versionMethod, parentIdMethod, ADProofsRootMethod, stateRootMethod, transactionsRootMethod,
    timestampMethod, nBitsMethod, heightMethod, extensionRootMethod, minerPkMethod, powOnetimePkMethod,
    powNonceMethod, powDistanceMethod, votesMethod)

  // 6.0 : checkPow method added
  private lazy val v6Methods = v5Methods ++ Seq(checkPowMethod)

  protected override def getMethods() = {
    if (VersionContext.current.isV6SoftForkActivated) {
      v6Methods
    } else {
      v5Methods
    }
  }
}

/** Type descriptor of `PreHeader` type of ErgoTree. */
case object SPreHeaderMethods extends MonoTypeMethods {
  override def ownerType: SMonoType = SPreHeader

  lazy val versionMethod          = propertyCall("version",  SByte,      1, FixedCost(JitCost(10)))
  lazy val parentIdMethod         = propertyCall("parentId", SByteArray, 2, FixedCost(JitCost(10)))
  lazy val timestampMethod        = propertyCall("timestamp", SLong, 3, FixedCost(JitCost(10)))
  lazy val nBitsMethod            = propertyCall("nBits", SLong, 4, FixedCost(JitCost(10)))
  lazy val heightMethod           = propertyCall("height", SInt, 5, FixedCost(JitCost(10)))
  lazy val minerPkMethod          = propertyCall("minerPk", SGroupElement, 6, FixedCost(JitCost(10)))
  lazy val votesMethod            = propertyCall("votes", SByteArray, 7, FixedCost(JitCost(10)))

  protected override def getMethods() = super.getMethods() ++ Seq(
    versionMethod, parentIdMethod, timestampMethod, nBitsMethod, heightMethod, minerPkMethod, votesMethod
  )
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
case object SGlobalMethods extends MonoTypeMethods {
  override def ownerType: SMonoType = SGlobal

  lazy val groupGeneratorMethod = SMethod(
    this, "groupGenerator", SFunc(SGlobal, SGroupElement), 1, GroupGenerator.costKind)
    .withIRInfo({ case (_, _, _, _, _) => GroupGenerator })
    .withInfo(GroupGenerator, "")

  lazy val xorMethod = SMethod(
    this, "xor", SFunc(Array(SGlobal, SByteArray, SByteArray), SByteArray), 2, Xor.costKind)
    .withIRInfo({
        case (_, _, _, Seq(l, r), _) => Xor(l.asByteArray, r.asByteArray)
    })
    .withInfo(Xor, "Byte-wise XOR of two collections of bytes",
      ArgInfo("left", "left operand"), ArgInfo("right", "right operand"))

  /** Implements evaluation of Global.xor method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod, Xor.eval, Xor.xorWithCosting
    */
  def xor_eval(mc: MethodCall, G: SigmaDslBuilder, ls: Coll[Byte], rs: Coll[Byte])
              (implicit E: ErgoTreeEvaluator): Coll[Byte] = {
    Xor.xorWithCosting(ls, rs)
  }

  private val BigEndianBytesCostKind = FixedCost(JitCost(10))

  // id = 4 is reserved for deserializeTo ()
  lazy val fromBigEndianBytesMethod = SMethod(
    this, "fromBigEndianBytes", SFunc(Array(SGlobal, SByteArray), tT, Array(paramT)), 5, BigEndianBytesCostKind, Seq(tT))
    .withIRInfo(MethodCallIrBuilder,
      javaMethodOf[SigmaDslBuilder, Coll[Byte], RType[_]]("fromBigEndianBytes"),
      { mtype => Array(mtype.tRange) })
    .withInfo(MethodCall,
      "Decode a number from big endian bytes.",
      ArgInfo("first", "Bytes which are big-endian encoded number."))

  lazy val serializeMethod = SMethod(this, "serialize",
    SFunc(Array(SGlobal, tT), SByteArray, Array(paramT)), 3, DynamicCost)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(MethodCall, "Serializes the given `value` into bytes using the default serialization format.",
        ArgInfo("value", "value to be serialized"))


  /** Implements evaluation of Global.serialize method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def serialize_eval(mc: MethodCall, G: SigmaDslBuilder, value: SType#WrappedType)
      (implicit E: ErgoTreeEvaluator): Coll[Byte] = {

    E.addCost(SigmaByteWriter.StartWriterCost)

    val addFixedCostCallback = { (costInfo: OperationCostInfo[FixedCost]) =>
      E.addCost(costInfo)
    }
    val addPerItemCostCallback = { (info: OperationCostInfo[PerItemCost], nItems: Int) =>
      E.addSeqCostNoOp(info.costKind, nItems, info.opDesc)
    }
    val w = SigmaSerializer.startWriter(None,
      Some(addFixedCostCallback), Some(addPerItemCostCallback))

    DataSerializer.serialize(value, mc.args(0).tpe, w)
    Colls.fromArray(w.toBytes)
  }

  protected override def getMethods() = super.getMethods() ++ {
    if (VersionContext.current.isV6SoftForkActivated) {
      Seq(
        groupGeneratorMethod,
        xorMethod,
        serializeMethod,
        fromBigEndianBytesMethod
      )
    } else {
      Seq(
        groupGeneratorMethod,
        xorMethod
      )
    }
  }
}

