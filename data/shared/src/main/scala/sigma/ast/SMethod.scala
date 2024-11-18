package sigma.ast

import debox.cfor
import sigma.ast.SMethod.{InvokeDescBuilder, MethodCostFunc}
import sigma.ast.syntax._
import sigma.data.RType
import sigma.eval.{CostDetails, ErgoTreeEvaluator, GivenCost, TracedCost}
import sigma.reflection.{RClass, RMethod}
import sigma.serialization.CoreByteWriter.ArgInfo
import sigma.validation.ValidationRules.CheckTypeWithMethods
import sigma.{Coll, Evaluation}

import scala.collection.compat.immutable.ArraySeq
import scala.reflect.ClassTag

/** Meta information which can be attached to SMethod.
  * @param opDesc  optional operation descriptor
  * @param description  human readable description of the method
  * @param args         one item for each argument */
case class OperationInfo(opDesc: Option[ValueCompanion], description: String, args: Seq[ArgInfo]) {
  def isFrontendOnly: Boolean = opDesc.isEmpty
  def opTypeName: String = opDesc.map(_.typeName).getOrElse("(FRONTEND ONLY)")
}

object OperationInfo {
  /** Convenience factory method. */
  def apply(opDesc: ValueCompanion, description: String, args: Seq[ArgInfo]): OperationInfo =
    OperationInfo(Some(opDesc), description, args)
}

/** Meta information connecting SMethod with ErgoTree.
  * The optional builder is used by front-end ErgoScript compiler to replace method calls
  * with ErgoTree nodes. In many cases [[SMethod.MethodCallIrBuilder]] builder is used.
  * However there are specific cases where more complex builders are used, see for example
  * usage of `withIRInfo` in the declaration of [[SCollection.GetOrElseMethod]].
  * @param  irBuilder  optional method call recognizer and ErgoTree node builder.
  *                    When the partial function is defined on a tuple
  *                    (builder, obj, m, args, subst) it transforms it to a new ErgoTree
  *                    node, which is then used in the resuting ErgoTree coming out of
  *                    the ErgoScript compiler.
  * @param javaMethod  Java [[Method]] which should be used to evaluate
  *                    [[sigmastate.lang.Terms.MethodCall]] node of ErgoTree.
  * @param invokeDescsBuilder optional builder of additional type descriptors (see extraDescriptors)
  */
case class MethodIRInfo(
    irBuilder: Option[PartialFunction[(SigmaBuilder, SValue, SMethod, Seq[SValue], STypeSubst), SValue]],
    javaMethod: Option[RMethod],
    invokeDescsBuilder: Option[InvokeDescBuilder]
)

/** Represents method descriptor.
  *
  * @param objType         type or type constructor descriptor
  * @param name            method name
  * @param stype           method signature type,
  *                        where `stype.tDom`` - argument type and
  *                        `stype.tRange` - method result type.
  * @param methodId        method code, it should be unique among methods of the same objType.
  * @param costKind        cost descriptor for this method
  * @param explicitTypeArgs list of type parameters which require explicit
  *                        serialization in [[MethodCall]]s (i.e for deserialize[T], getVar[T], getReg[T])
  * @param irInfo          meta information connecting SMethod with ErgoTree (see [[MethodIRInfo]])
  * @param docInfo         optional human readable method description data
  * @param costFunc        optional specification of how the cost should be computed for the
  *                        given method call (See ErgoTreeEvaluator.calcCost method).
  * @param userDefinedInvoke optional custom method evaluation function
  */
case class SMethod(
    objType: MethodsContainer,
    name: String,
    stype: SFunc,
    methodId: Byte,
    costKind: CostKind,
    explicitTypeArgs: Seq[STypeVar],
    irInfo: MethodIRInfo,
    docInfo: Option[OperationInfo],
    costFunc: Option[MethodCostFunc],
    userDefinedInvoke: Option[SMethod.InvokeHandler]
) {

  /** Operation descriptor of this method. */
  lazy val opDesc = MethodDesc(this)

  /** Return true if this method has explicit type parameters, which need to be serialized
    * as part of [[MethodCall]].
    */
  def hasExplicitTypeArgs: Boolean = explicitTypeArgs.nonEmpty

  /** Finds and keeps the [[RMethod]] instance which corresponds to this method descriptor.
    * The lazy value is forced only if irInfo.javaMethod == None
    */
  lazy val javaMethod: RMethod = {
    irInfo.javaMethod.getOrElse {
      val paramTypes = stype.tDom.drop(1).map(t => t match {
        case _: STypeVar => classOf[AnyRef]
        case _: SFunc => classOf[_ => _]
        case _ => Evaluation.stypeToRType(t).classTag.runtimeClass
      }).toArray
      val m = objType.ownerType.reprClass.getMethod(name, paramTypes:_*)
      m
    }
  }

  /** Additional type descriptors, which are necessary to perform invocation of Method
    * associated with this instance.
    * @see MethodCall.eval
    */
  lazy val extraDescriptors: Seq[RType[_]] = {
    irInfo.invokeDescsBuilder match {
      case Some(builder) =>
        builder(stype).map(Evaluation.stypeToRType)
      case None =>
        ArraySeq.empty[RType[_]]
    }
  }

  /** Invoke this method on the given object with the arguments.
    * This is used for methods with FixedCost costKind. */
  def invokeFixed(obj: Any, args: Array[Any]): Any = {
    userDefinedInvoke match {
      case Some(h) =>
        h(this, obj, args)
      case None =>
        javaMethod.invoke(obj, args.asInstanceOf[Array[AnyRef]]:_*)
    }
  }

  // TODO optimize: avoid lookup when this SMethod is created via `specializeFor`
  /** Return generic template of this method. */
  @inline final def genericMethod: SMethod = {
    objType.getMethodById(methodId).get
  }

  /** Returns refection [[RMethod]] which must be invoked to evaluate this method.
    * The method is resolved by its name using `name + "_eval"` naming convention.
    * @see `map_eval`, `flatMap_eval` and other `*_eval` methods.
    * @hotspot don't beautify the code */
  lazy val evalMethod: RMethod = {
    val argTypes = stype.tDom
    val nArgs = argTypes.length
    val paramTypes = new Array[Class[_]](nArgs + 2)
    paramTypes(0) = classOf[MethodCall]
    cfor(0)(_ < nArgs, _ + 1) { i =>
      paramTypes(i + 1) = argTypes(i) match {
        case _: STypeVar => classOf[AnyRef]
        case _: SFunc => classOf[_ => _]
        case _: SCollectionType[_] => classOf[Coll[_]]
        case _: SOption[_] => classOf[Option[_]]
        case t =>
          Evaluation.stypeToRType(t).classTag.runtimeClass
      }
    }
    paramTypes(paramTypes.length - 1) = classOf[ErgoTreeEvaluator]

    val methodName = name + "_eval"
    val m = try {
      objType.thisRClass.getMethod(methodName, paramTypes:_*)
    }
    catch { case e: NoSuchMethodException =>
      throw new RuntimeException(s"Cannot find eval method def $methodName(${Seq(paramTypes:_*)})", e)
    }
    m
  }

  /** Create a new instance with the given user-defined invoke handler. */
  def withUserDefinedInvoke(handler: SMethod.InvokeHandler): SMethod = {
    copy(userDefinedInvoke = Some(handler))
  }

  /** Create a new instance with the given stype. */
  def withSType(newSType: SFunc): SMethod = copy(stype = newSType)

  /** Create a new instance with the given cost function. */
  def withCost(costFunc: MethodCostFunc): SMethod = copy(costFunc = Some(costFunc))

  /** Create a new instance in which the `stype` field transformed using
    * the given substitution. */
  def withConcreteTypes(subst: Map[STypeVar, SType]): SMethod =
    withSType(stype.withSubstTypes(subst).asFunc)

  /** Name of a language operation represented by this method. */
  def opName = objType.getClass.getSimpleName + "." + name

  /** Specializes this instance by creating a new [[SMethod]] instance where signature
    * is specialized with respect to the given object and args types. It is used in
    * [[sigmastate.serialization.MethodCallSerializer]] `parse` method, so it is part of
    * consensus protocol.
    *
    * @param objTpe specific type of method receiver (aka object)
    * @param args   specific types of method arguments
    * @return new instance of method descriptor with specialized signature
    * @consensus
    */
  def specializeFor(objTpe: SType, args: Seq[SType]): SMethod = {
    unifyTypeLists(stype.tDom, objTpe +: args) match {
      case Some(subst) if subst.nonEmpty =>
        withConcreteTypes(subst)
      case _ => this
    }
  }

  /** Create a new instance with the given [[OperationInfo]] parameters. */
  def withInfo(opDesc: ValueCompanion, desc: String, args: ArgInfo*): SMethod = {
    this.copy(docInfo = Some(OperationInfo(opDesc, desc, ArgInfo("this", "this instance") +: args.toSeq)))
  }

  /** Create a new instance with the given [[OperationInfo]] parameters.
    * NOTE: opDesc parameter is not defined and falls back to None.
    */
  def withInfo(desc: String, args: ArgInfo*): SMethod = {
    this.copy(docInfo = Some(OperationInfo(None, desc, ArgInfo("this", "this instance") +: args.toSeq)))
  }

  /** Create a new instance with the given IR builder (aka MethodCall rewriter) parameter. */
  def withIRInfo(
      irBuilder: PartialFunction[(SigmaBuilder, SValue, SMethod, Seq[SValue], STypeSubst), SValue],
      javaMethod: RMethod = null,
      invokeHandler: InvokeDescBuilder = null): SMethod = {
    this.copy(irInfo = MethodIRInfo(Some(irBuilder), Option(javaMethod), Option(invokeHandler)))
  }

  /** Lookup [[ArgInfo]] for the given argName or throw an exception. */
  def argInfo(argName: String): ArgInfo =
    docInfo.get.args.find(_.name == argName).get
}


object SMethod {
  /** Type of functions used to assign cost to method call nodes.
    * For a function `f: (mc, obj, args) => cost` it is called before the evaluation of
    * the `mc` node with the given `obj` as method receiver and `args` as method
    * arguments.
    */
  abstract class MethodCostFunc extends Function4[ErgoTreeEvaluator, MethodCall, Any, Array[Any], CostDetails] {
    /**
      * The function returns an estimated cost of evaluation BEFORE actual evaluation of
      * the method. For this reason [[MethodCostFunc]] is not used for higher-order
      * operations like `Coll.map`, `Coll.filter` etc.
      */
    def apply(E: ErgoTreeEvaluator, mc: MethodCall, obj: Any, args: Array[Any]): CostDetails
  }

  /** Returns a cost function which always returs the given cost. */
  def givenCost(costKind: FixedCost): MethodCostFunc = new MethodCostFunc {
    override def apply(E: ErgoTreeEvaluator,
        mc: MethodCall,
        obj: Any, args: Array[Any]): CostDetails = {
      if (E.settings.costTracingEnabled)
        TracedCost(Array(FixedCostItem(MethodDesc(mc.method), costKind)))
      else
        GivenCost(costKind.cost)
    }
  }

  /** Returns a cost function which expects `obj` to be of `Coll[T]` type and
    * uses its length to compute SeqCostItem  */
  def perItemCost(costKind: PerItemCost): MethodCostFunc = new MethodCostFunc {
    override def apply(E: ErgoTreeEvaluator,
        mc: MethodCall,
        obj: Any, args: Array[Any]): CostDetails = obj match {
      case coll: Coll[a] =>
        if (E.settings.costTracingEnabled) {
          val desc = MethodDesc(mc.method)
          TracedCost(Array(SeqCostItem(desc, costKind, coll.length)))
        }
        else
          GivenCost(costKind.cost(coll.length))
      case _ =>
        sys.error(
          s"Invalid object $obj of method call $mc: Coll type is expected")
    }
  }

  /** Some runtime methods (like Coll.map, Coll.flatMap) require additional RType descriptors.
    * The builder can extract those descriptors from the given type of the method signature.
    */
  type InvokeDescBuilder = SFunc => Seq[SType]

  /** Type of user-defined function which is called to handle method invocation.
    * Instances of this type can be attached to [[SMethod]] instances.
    * @see SNumericTypeMethods.ToBytesMethod
    */
  type InvokeHandler = (SMethod, Any, Array[Any]) => Any

  /** Return [[Method]] descriptor for the given `methodName` on the given `cT` type.
    * @param methodName the name of the method to lookup
    * @param cT the class where to search the methodName
    * @param cA1 the class of the method argument
    */
  def javaMethodOf[T, A1](methodName: String)
      (implicit cT: ClassTag[T], cA1: ClassTag[A1]): RMethod =
    RClass(cT.runtimeClass).getMethod(methodName, cA1.runtimeClass)

  /** Return [[Method]] descriptor for the given `methodName` on the given `cT` type.
    * @param methodName the name of the method to lookup
    * @param cT the class where to search the methodName
    * @param cA1 the class of the method's first argument
    * @param cA2 the class of the method's second argument
    */
  def javaMethodOf[T, A1, A2]
      (methodName: String)
          (implicit cT: ClassTag[T], cA1: ClassTag[A1], cA2: ClassTag[A2]): RMethod =
    RClass(cT.runtimeClass).getMethod(methodName, cA1.runtimeClass, cA2.runtimeClass)

  /** Return [[Method]] descriptor for the given `methodName` on the given `cT` type.
    * @param methodName the name of the method to lookup
    * @param cT the class where to search the methodName
    * @param cA1 the class of the method's first argument
    * @param cA2 the class of the method's second argument
    * @param cA3 the class of the method's third argument
    */
  def javaMethodOf[T, A1, A2, A3]
      (methodName: String)
          (implicit cT: ClassTag[T], cA1: ClassTag[A1], cA2: ClassTag[A2], cA3: ClassTag[A3]): RMethod =
    RClass(cT.runtimeClass).getMethod(methodName, cA1.runtimeClass, cA2.runtimeClass, cA3.runtimeClass)

  /** Default fallback method call recognizer which builds MethodCall ErgoTree nodes. */
  val MethodCallIrBuilder: PartialFunction[(SigmaBuilder, SValue, SMethod, Seq[SValue], STypeSubst), SValue] = {
    case (builder, obj, method, args, tparamSubst) =>
      builder.mkMethodCall(obj, method, args.toIndexedSeq, tparamSubst)
  }

  /** Convenience factory method. */
  def apply(objType: MethodsContainer, name: String, stype: SFunc,
      methodId: Byte,
      costKind: CostKind,
      explicitTypeArgs: Seq[STypeVar] = Nil
  ): SMethod = {
    SMethod(
      objType, name, stype, methodId, costKind, explicitTypeArgs,
      MethodIRInfo(None, None, None), None, None, None)
  }


  /** Looks up [[SMethod]] instance for the given type and method ids.
    *
    * @param typeId   id of a type which can contain methods
    * @param methodId id of a method of the type given by `typeId`
    * @return an instance of [[SMethod]] which may contain generic type variables in the
    *         signature (see SMethod.stype). As a result `specializeFor` is called by
    *         deserializer to obtain monomorphic method descriptor.
    * @consensus this is method is used in [[sigmastate.serialization.MethodCallSerializer]]
    *            `parse` method and hence it is part of consensus protocol
    */
  def fromIds(typeId: Byte, methodId: Byte): SMethod = {
    CheckTypeWithMethods(typeId, MethodsContainer.contains(typeId))
    val container = MethodsContainer(typeId)
    val method = container.methodById(methodId)
    method
  }
}

