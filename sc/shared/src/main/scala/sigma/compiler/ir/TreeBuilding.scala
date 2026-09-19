package sigma.compiler.ir

import sigma.ast._
import sigma.ast.syntax.{ValueOps, _}
import sigma.serialization.OpCodes._
import sigma.serialization.ConstantStore
import sigma.serialization.ValueCodes.OpCode

import scala.collection.mutable.ArrayBuffer

/** Implementation of IR-graph to ErgoTree expression translation.
  * This, in a sense, is inverse to [[GraphBuilding]], however roundtrip identity is not
  * possible, because one of the goals of Tree -> Graph -> Tree translation is to perform
  * size optimization of the resulting tree.
  *
  * The main optimizations that are achieved by Tree -> Graph -> Tree process:
  * 1) Common Subexpression Elimination which is done in GraphBuilding
  * 2) ValDef introduction minimization, which is done in TreeBuilding. The ValDef is
  * introduced only for graph nodes (i.e. subcomputations) that have more than 1 usage.
  *
  * @see buildTree method
  * */
trait TreeBuilding extends Base { IR: IRContext =>
  import Liftables._

  /** Describes assignment of valIds for symbols which become ValDefs.
    * Each ValDef in current scope have entry in this map */
  type DefEnv = Map[Sym, (Int, SType)]

  /** Recognizes arithmetic operation of graph IR and returns its ErgoTree code. */
  object IsArithOp {
    def unapply(op: EndoBinOp[_]): Option[OpCode] = op match {
      case _: NumericPlus[_]    => Some(PlusCode)
      case _: NumericMinus[_]   => Some(MinusCode)
      case _: NumericTimes[_]   => Some(MultiplyCode)
      case _: IntegralDivide[_] => Some(DivisionCode)
      case _: IntegralMod[_]    => Some(ModuloCode)
      case _: OrderingMin[_]    => Some(MinCode)
      case _: OrderingMax[_]    => Some(MaxCode)
      case _ => None
    }
  }

  /** Recognizes comparison operation of graph IR and returns the corresponding ErgoTree
    * builder function.
    */
  object IsRelationOp {
    def unapply(op: BinOp[_,_]): Option[(SValue, SValue) => Value[SBoolean.type]] = op match {
      case _: Equals[_]       => Some(builder.mkEQ[SType])
      case _: NotEquals[_]    => Some(builder.mkNEQ[SType])
      case _: OrderingGT[_]   => Some(builder.mkGT[SType])
      case _: OrderingLT[_]   => Some(builder.mkLT[SType])
      case _: OrderingGTEQ[_] => Some(builder.mkGE[SType])
      case _: OrderingLTEQ[_] => Some(builder.mkLE[SType])
      case _ => None
    }
  }

  /** Recognizes logical binary operation of graph IR and returns the corresponding
    * ErgoTree builder function.
    */
  object IsLogicalBinOp {
    def unapply(op: BinOp[_,_]): Option[(BoolValue, BoolValue) => Value[SBoolean.type]] = op match {
      case And => Some(builder.mkBinAnd)
      case Or  => Some(builder.mkBinOr)
      case BinaryXorOp => Some(builder.mkBinXor)
      case _ => None
    }
  }

  /** Recognizes unary logical operation of graph IR and returns the corresponding
    * ErgoTree builder function.
    */
  object IsLogicalUnOp {
    def unapply(op: UnOp[_,_]): Option[BoolValue => BoolValue] = op match {
      case Not => Some({ v: BoolValue => builder.mkLogicalNot(v) })
      case _ => None
    }
  }

  /** Recognizes unary numeric operation of graph IR and returns the corresponding
    * ErgoTree builder function.
    */
  object IsNumericUnOp {
    def unapply(op: UnOp[_,_]): Option[SValue => SValue] = op match {
      case NumericNegate(_) => Some({ v: SValue => builder.mkNegation(v.asNumValue) })
      case _: NumericToBigEndianBytes[_] =>
        val mkNode = { v: SValue =>
          val receiverType = v.tpe.asNumTypeOrElse(error(s"Expected numeric type, got: ${v.tpe}"))
          val m = SMethod.fromIds(receiverType.typeId, SNumericTypeMethods.ToBytesMethod.methodId)
          builder.mkMethodCall(v.asNumValue, m, IndexedSeq.empty)
        }
        Some(mkNode)
      case _: NumericToBits[_] =>
        val mkNode = { v: SValue =>
          val receiverType = v.tpe.asNumTypeOrElse(error(s"Expected numeric type, got: ${v.tpe}"))
          val m = SMethod.fromIds(receiverType.typeId, SNumericTypeMethods.ToBitsMethod.methodId)
          builder.mkMethodCall(v.asNumValue, m, IndexedSeq.empty)
        }
        Some(mkNode)
      case _: NumericBitwiseInverse[_] =>
        val mkNode = { v: SValue =>
          val receiverType = v.tpe.asNumTypeOrElse(error(s"Expected numeric type, got: ${v.tpe}"))
          val m = SMethod.fromIds(receiverType.typeId, SNumericTypeMethods.BitwiseInverseMethod.methodId)
          builder.mkMethodCall(v.asNumValue, m, IndexedSeq.empty)
        }
        Some(mkNode)
      case _ => None
    }
  }

  /** Recognizes context property in the graph IR and returns the corresponding
    * ErgoTree node.
    */
  private val HeightCall  = CallPattern(SContextMethods.heightMethod)
  private val InputsCall  = CallPattern(SContextMethods.inputsMethod)
  private val OutputsCall = CallPattern(SContextMethods.outputsMethod)
  private val SelfCall    = CallPattern(SContextMethods.selfMethod)

  object IsContextProperty {
    def unapply(d: Def[_]): Option[SValue] = d match {
      case HeightCall(_, _) => Some(Height)
      case InputsCall(_, _) => Some(Inputs)
      case OutputsCall(_, _) => Some(Outputs)
      case SelfCall(_, _) => Some(Self)
      case _ => None
    }
  }

  /** Recognizes constants in graph IR. */
  object IsConstantDef {
    def unapply(d: Def[_]): Option[Def[_]] = d match {
      case _: Const[_] => Some(d)
      case _ => None
    }
  }

  /** Emits a plain `MethodCall` ErgoTree node for a call of `m` without a lowering row. A Coll
    * receiver substitutes only the collection's type variables (`tIV`, and `tOV` for flatMap and
    * zip); any other receiver specialises the generic descriptor for the receiver and argument
    * types and applies the call's explicit type substitution.
    */
  def plainMethodCall(mc: MethodCall, m: SMethod, obj: SValue, args: Seq[SValue]): SValue = {
    if (mc.receiver.elem.isInstanceOf[CollElem[_]]) {
      val generic = m.objType.getMethodById(m.methodId).getOrElse(error(s"unknown method Coll.${m.name}"))
      val col = obj.asCollection[SType]
      val typeSubst = (generic, args) match {
        case (SCollectionMethods.FlatMapMethod, Seq(f)) =>
          Map(SCollection.tOV -> f.asFunc.tpe.tRange.asCollection.elemType)
        case (SCollectionMethods.ZipMethod, Seq(coll)) =>
          Map(SCollection.tOV -> coll.asCollection[SType].tpe.elemType)
        case _ => EmptySubst
      }
      val specMethod = generic.withConcreteTypes(typeSubst + (SCollection.tIV -> col.tpe.elemType))
      builder.mkMethodCall(col, specMethod, args.toIndexedSeq, Map())
    } else {
      val generic = m.objType.getMethodById(m.methodId)
        .getOrElse(error(s"Cannot find method '${m.name}' on receiver of type ${obj.tpe}"))
      val typeSubst = mc.typeSubst
      val specMethod = generic.specializeFor(obj.tpe, args.map(_.tpe)).withConcreteTypes(typeSubst)
      builder.mkMethodCall(obj, specMethod, args.toIndexedSeq, typeSubst)
    }
  }

  /** Transforms the given graph node into the corresponding ErgoTree node.
    * It is mutually recursive with processAstGraph, so it's part of the recursive
    * algorithms required by buildTree method.
    */
  private def buildValue(ctx: Ref[sigma.Context],
                 mainG: PGraph,
                 env: DefEnv,
                 s: Sym,
                 defId: Int,
                 constantsProcessing: Option[ConstantStore]): SValue = {
    import builder._
    def recurse[T <: SType](s: Sym) = buildValue(ctx, mainG, env, s, defId, constantsProcessing).asValue[T]
    object In { def unapply(s: Sym): Option[SValue] = Some(buildValue(ctx, mainG, env, s, defId, constantsProcessing)) }
    s match {
      case _ if s == ctx => sigma.ast.Context
      case _ if env.contains(s) =>
        val (id, tpe) = env(s)
        ValUse(id, tpe) // recursion base
      case Def(Lambda(lam, _, x, _)) =>
        val varId = defId + 1       // arguments are treated as ValDefs and occupy id space
        val env1 = env + (x -> (varId, elemToSType(x.elem)))
        val block = processAstGraph(ctx, mainG, env1, lam, varId + 1, constantsProcessing)
        val rhs = mkFuncValue(Array((varId, elemToSType(x.elem))), block)
        rhs
      case Def(Apply(fSym, xSym, _)) =>
        val Seq(f, x) = Seq(fSym, xSym).map(recurse)
        builder.mkApply(f, Array(x))
      case Def(th @ ThunkDef(_, _)) =>
        val block = processAstGraph(ctx, mainG, env, th, defId, constantsProcessing)
        block
      case Def(Const(x)) =>
        val tpe = elemToSType(s.elem)
        constantsProcessing match {
          case Some(s) =>
            val constant = mkConstant[tpe.type](x.asInstanceOf[tpe.WrappedType], tpe)
              .asInstanceOf[ConstantNode[SType]]
            s.put(constant)(builder)
          case None =>
            mkConstant[tpe.type](x.asInstanceOf[tpe.WrappedType], tpe)
        }
      case Def(IR.ConstantPlaceholder(id, elem)) =>
        val tpe = elemToSType(elem)
        mkConstantPlaceholder[tpe.type](id, tpe)

      case Def(wc: LiftedConst[a,_]) =>
        val tpe = elemToSType(s.elem)
        mkConstant[tpe.type](wc.constValue.asInstanceOf[tpe.WrappedType], tpe)

      case Def(DeserializeContextDef(d, _)) =>
        d

      case Def(DeserializeRegisterDef(d, _)) =>
        d

      case Def(IsContextProperty(v)) => v
      case s if s == sigmaDslBuilder => Global

      case Def(ApplyBinOp(op, xSym, ySym)) if op.isInstanceOf[NumericBitwiseOr[_]] =>
        val Seq(x, y) = Seq(xSym, ySym).map(recurse)
        val receiverType = x.asNumValue.tpe.asNumTypeOrElse(error(s"Expected numeric type, got: ${x.tpe}"))
        val m = SMethod.fromIds(receiverType.typeId, SNumericTypeMethods.BitwiseOrMethod.methodId)
        builder.mkMethodCall(x.asNumValue, m, IndexedSeq(y))

      case Def(ApplyBinOp(op, xSym, ySym)) if op.isInstanceOf[NumericBitwiseAnd[_]] =>
        val Seq(x, y) = Seq(xSym, ySym).map(recurse)
        val receiverType = x.asNumValue.tpe.asNumTypeOrElse(error(s"Expected numeric type, got: ${x.tpe}"))
        val m = SMethod.fromIds(receiverType.typeId, SNumericTypeMethods.BitwiseAndMethod.methodId)
        builder.mkMethodCall(x.asNumValue, m, IndexedSeq(y))

      case Def(ApplyBinOp(op, xSym, ySym)) if op.isInstanceOf[NumericBitwiseXor[_]] =>
        val Seq(x, y) = Seq(xSym, ySym).map(recurse)
        val receiverType = x.asNumValue.tpe.asNumTypeOrElse(error(s"Expected numeric type, got: ${x.tpe}"))
        val m = SMethod.fromIds(receiverType.typeId, SNumericTypeMethods.BitwiseXorMethod.methodId)
        builder.mkMethodCall(x.asNumValue, m, IndexedSeq(y))

      case Def(ApplyBinOpDiffArgs(op, xSym, ySym)) if op.isInstanceOf[NumericShiftLeft[_]] =>
        val Seq(x, y) = Seq(xSym, ySym).map(recurse)
        val receiverType = x.asNumValue.tpe.asNumTypeOrElse(error(s"Expected numeric type, got: ${x.tpe}"))
        val m = SMethod.fromIds(receiverType.typeId, SNumericTypeMethods.ShiftLeftMethod.methodId)
        builder.mkMethodCall(x.asNumValue, m, IndexedSeq(y))

      case Def(ApplyBinOpDiffArgs(op, xSym, ySym)) if op.isInstanceOf[NumericShiftRight[_]] =>
        val Seq(x, y) = Seq(xSym, ySym).map(recurse)
        val receiverType = x.asNumValue.tpe.asNumTypeOrElse(error(s"Expected numeric type, got: ${x.tpe}"))
        val m = SMethod.fromIds(receiverType.typeId, SNumericTypeMethods.ShiftRightMethod.methodId)
        builder.mkMethodCall(x.asNumValue, m, IndexedSeq(y))


      case Def(ApplyBinOp(IsArithOp(opCode), xSym, ySym)) =>
        val Seq(x, y) = Seq(xSym, ySym).map(recurse)
        mkArith(x.asNumValue, y.asNumValue, opCode)
      case Def(ApplyBinOp(IsRelationOp(mkNode), xSym, ySym)) =>
        val Seq(x, y) = Seq(xSym, ySym).map(recurse)
        mkNode(x, y)
      case Def(ApplyBinOp(IsLogicalBinOp(mkNode), xSym, ySym)) =>
        val Seq(x, y) = Seq(xSym, ySym).map(recurse)
        mkNode(x, y)
      case Def(ApplyBinOpLazy(IsLogicalBinOp(mkNode), xSym, ySym)) =>
        val Seq(x, y) = Seq(xSym, ySym).map(recurse)
        mkNode(x, y)
      case Def(ApplyUnOp(IsLogicalUnOp(mkNode), xSym)) =>
        mkNode(recurse(xSym))



      case Def(ApplyUnOp(IsNumericUnOp(mkNode), xSym)) =>
        mkNode(recurse(xSym))

      case Def(AnyZk(colSyms)) =>
        val col = colSyms.map(recurse(_).asSigmaProp)
        SigmaOr(col)
      case Def(AllZk(colSyms)) =>
        val col = colSyms.map(recurse(_).asSigmaProp)
        SigmaAnd(col)

      case Def(AnyOf(colSyms)) =>
        val col = colSyms.map(recurse(_).asBoolValue)
        mkAnyOf(col)
      case Def(AllOf(colSyms)) =>
        val col = colSyms.map(recurse(_).asBoolValue)
        mkAllOf(col)

      case Def(IfThenElseLazy(condSym, thenPSym, elsePSym)) =>
        val Seq(cond, thenP, elseP) = Seq(condSym, thenPSym, elsePSym).map(recurse)
        mkIf(cond, thenP, elseP)

      case Def(Tup(In(x), In(y))) =>
        mkTuple(Seq(x, y))
      case Def(First(pair)) =>
        mkSelectField(recurse(pair), 1)
      case Def(Second(pair)) =>
        mkSelectField(recurse(pair), 2)

      case Def(Downcast(inputSym, toSym)) =>
        mkDowncast(recurse(inputSym).asNumValue, elemToSType(toSym).asNumType)
      case Def(Upcast(inputSym, toSym)) =>
        mkUpcast(recurse(inputSym).asNumValue, elemToSType(toSym).asNumType)

      // Call nodes: the lowering row when the callee has a dedicated ErgoTree node, else a plain
      // MethodCall rebuilt from the method descriptor. A global builtin's receiver is the global
      // object, which recurses to Global above and which its row ignores.
      case Def(mc @ MethodCall(objSym, callee, argSyms, _)) =>
        val obj = recurse[SType](objSym)
        val args = argSyms.map(recurse[SType])
        loweringFor(callee) match {
          case Some(row) => row(mc, obj, args)
          case None => callee match {
            case MethodCallee(m) => plainMethodCall(mc, m, obj, args)
            case _ => error(s"No ErgoTree lowering for $callee")
          }
        }

      case Def(d) =>
        !!!(s"Don't know how to buildValue($mainG, $s -> $d, $env, $defId)")
    }
  }

  /** Transforms the given AstGraph node (Lambda of Thunk) into the corresponding ErgoTree node.
    * It is mutually recursive with buildValue, so it's part of the recursive
    * algorithms required by buildTree method.
    */
  private def processAstGraph(ctx: Ref[sigma.Context],
                              mainG: PGraph,
                              env: DefEnv,
                              subG: AstGraph,
                              defId: Int,
                              constantsProcessing: Option[ConstantStore]): SValue = {
    val valdefs = new ArrayBuffer[ValDef]
    var curId = defId
    var curEnv = env
    for (s <- subG.schedule) {
      val d = s.node
      if (mainG.hasManyUsagesGlobal(s)
        && IsContextProperty.unapply(d).isEmpty
          // to increase effect of constant segregation we need to treat the constants specially
          // and don't create ValDef even if the constant is used more than one time,
          // because two equal constants don't always have the same meaning.
        && IsConstantDef.unapply(d).isEmpty)
      {
        val rhs = buildValue(ctx, mainG, curEnv, s, curId, constantsProcessing)
        curId += 1
        val vd = ValDef(curId, Nil, rhs)
        curEnv = curEnv + (s -> (curId, vd.tpe))  // assign valId to s, so it can be use in ValUse
        valdefs += vd
      }
    }
    val root = subG.roots(0)
    val rhs = buildValue(ctx, mainG, curEnv, root, curId, constantsProcessing)
    val res = if (valdefs.nonEmpty) {
      (valdefs.toArray[BlockItem], rhs) match {
        // simple optimization to avoid producing block sub-expressions like:
        // `{ val idNew = id; idNew }` which this rules rewrites to just `id`
        case (Array(ValDef(idNew, _, source @ ValUse(_, tpe))), ValUse(idUse, tpeUse))
          if idUse == idNew && tpeUse == tpe => source
        case (items, _) =>
          BlockValue(items, rhs)
      }
    } else rhs
    res
  }

  /** Transforms the given function `f` from graph-based IR to ErgoTree expression.
    *
    * @param f                   reference to the graph node representing function from Context.
    * @param constantsProcessing if Some(store) is specified, then each constant is
    *                            segregated and a placeholder is inserted in the resulting expression.
    * @return expression of ErgoTree which corresponds to the function `f`
    */
  def buildTree[T <: SType](f: Ref[sigma.Context => Any],
                            constantsProcessing: Option[ConstantStore] = None): Value[T] = {
    val Def(Lambda(lam,_,_,_)) = f
    val mainG = new PGraph(lam.y)
    val block = processAstGraph(asRep[sigma.Context](lam.x), mainG, Map.empty, mainG, 0, constantsProcessing)
    block.asValue[T]
  }
}
