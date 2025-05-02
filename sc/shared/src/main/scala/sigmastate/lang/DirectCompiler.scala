package sigmastate.lang

import org.ergoplatform.ErgoBox
import sigma.ast.SigmaPredef.PredefinedFuncRegistry
import sigma.ast.Value.Typed
import sigma.ast._
import sigma.ast.syntax.{SValue, ValueOps}
import sigma.compiler.CompilerSettings
import sigma.data.Nullable
import sigma.exceptions.CompilerException
import sigma.kiama.==>
import sigma.kiama.rewriting.Rewriter
import sigma.kiama.rewriting.Rewriter.{Duplicator, strategy}
import sigmastate.lang.DirectCompiler.{DefEnv, emptyEnv}

import scala.collection.mutable.ArrayBuffer

case class ReducingTransformer[T](rwRules: T ==> T) {
  val transformRule = Rewriter.reduce(strategy[T](rwRules.andThen(Option(_))))

  def apply(e: T): T = {
    val res = Rewriter.rewrite(transformRule)(e)
    res
  }
}

/**
  * Type inference and analysis for Sigma expressions.
  */
class DirectCompiler(settings: CompilerSettings) {
  /** Constructs an instance for the given network type and with default settings. */
  def this(networkPrefix: Byte) = this(
    CompilerSettings(networkPrefix, TransformingSigmaBuilder, lowerMethodCalls = true))

  import settings.builder._

  def error(msg: String, srcCtx: Option[SourceContext]) = throw new CompilerException(msg, srcCtx)
  def error(msg: String, srcCtx: Nullable[SourceContext]): Nothing = error(msg, srcCtx.toOption)

  /** Arg name introduced when transforming `(x, y) => ...` lambdas to one argument lambda. */
  final val SyntheticArgNameInLambda = "__input__"

  /** Set of rules to lower ErgoScript expressions to ErgoTree nodes.
    * When a rule returns `null` then the original node should remain in the tree (i.e. no
    * rewriting is applied)
    */
  val transformer = ReducingTransformer[Any] {
    case Upcast(Constant(value, _), toTpe: SNumericType) =>
      mkConstant(toTpe.upcast(value.asInstanceOf[AnyVal]), toTpe)

    case Downcast(Constant(value, _), toTpe: SNumericType) =>
      mkConstant(toTpe.downcast(value.asInstanceOf[AnyVal]), toTpe)

    // Rule: col.size --> SizeOf(col)
    case Select(obj, "size", _) =>
      if (obj.tpe.isCollectionLike)
        mkSizeOf(obj.asValue[SCollection[SType]])
      else
        error(s"The type of $obj is expected to be Collection to select 'size' property", obj.sourceContext)

    // Rule: proof.isProven --> IsValid(proof)
    case Select(p, SSigmaPropMethods.IsProven, _) if p.tpe == SSigmaProp =>
      SigmaPropIsProven(p.asSigmaProp)

    // Rule: prop.propBytes --> SigmaProofBytes(prop)
    case Select(p, SSigmaPropMethods.PropBytes, _) if p.tpe == SSigmaProp =>
      SigmaPropBytes(p.asSigmaProp)

    // box.R$i[valType] =>
    case sel @ Select(Typed(box, SBox), regName, Some(SOption(valType))) if regName.startsWith("R") =>
      val reg = ErgoBox.registerByName.getOrElse(regName,
        error(s"Invalid register name $regName in expression $sel", sel.sourceContext.toOption))
      mkExtractRegisterAs(box.asBox, reg, SOption(valType)).asValue[SOption[valType.type]]

    case sel @ Select(obj, field, _) if obj.tpe == SBox =>
      (obj.asValue[SBox.type], field) match {
        case (box, SBoxMethods.Value) => mkExtractAmount(box)
        case (box, SBoxMethods.PropositionBytes) => mkExtractScriptBytes(box)
        case (box, SBoxMethods.Id) => mkExtractId(box)
        case (box, SBoxMethods.Bytes) => mkExtractBytes(box)
        case (box, SBoxMethods.BytesWithoutRef) => mkExtractBytesWithNoRef(box)
        case (box, SBoxMethods.CreationInfo) => mkExtractCreationInfo(box)
        case _ => error(s"Invalid access to Box property in $sel: field $field is not found", sel.sourceContext)
      }

    case Select(tuple, fn, _) if tuple.tpe.isTuple && fn.startsWith("_") =>
      val index = fn.substring(1).toByte
      mkSelectField(tuple.asTuple, index)

    case Select(obj, method, Some(tRes: SNumericType))
      if obj.tpe.isNumType && SNumericTypeMethods.isCastMethod(method) =>

      val numValue = obj.asNumValue
      if (numValue.tpe == tRes)
        numValue
      else if ((numValue.tpe max tRes) == numValue.tpe)
        mkDowncast(numValue, tRes)
      else
        mkUpcast(numValue, tRes)

    case Apply(col, Seq(index)) if col.tpe.isCollection =>
      mkByIndex(col.asCollection[SType], index.asValue[SInt.type], None)

    case SizeOf(xs) =>
      xs.tpe.asInstanceOf[SType] match {
        case _: SCollectionType[_] => null // means that no rule is applicable
        case _: STuple => IntConstant(2)
      }

    case Lambda(tpeParams, Seq((accN, accTpe), (n, tpe)), resTpe, Some(body)) =>
      val argTpe = SPair(accTpe, tpe)
      val arg = Ident(SyntheticArgNameInLambda, argTpe).asTuple
      val newVals = Seq(
        ValNode(accN, accTpe, SelectField(arg, 1)),
        ValNode(n,    tpe,    SelectField(arg, 2))
      )
      val newBody = body match {
        case b @ Block(vals, _) => b.copy(bindings = newVals ++ vals)
        case _ => Block(newVals, body)
      }
      Lambda(tpeParams,
        Vector((SyntheticArgNameInLambda, argTpe)),
        resTpe,
        Some(newBody))

    // fallback rule for MethodCall, should be the last case in the list
    case mc @ MethodCall(obj, method, args, _) =>
      (obj, method.objType) match {
        case (xs, SCollectionMethods) => method.name match {
          //            case SCollectionMethods.PatchMethod.name =>
          //              val from = asRep[Int](argsV(0))
          //              val patch = asRep[Coll[t]](argsV(1))
          //              val replaced = asRep[Int](argsV(2))
          //              xs.patch(from, patch, replaced)
          //            case SCollectionMethods.UpdatedMethod.name =>
          //              val index = asRep[Int](argsV(0))
          //              val value = asRep[t](argsV(1))
          //              xs.updated(index, value)
          //            case SCollectionMethods.AppendMethod.name =>
          //              val ys = asRep[Coll[t]](argsV(0))
          //              xs.append(ys)
          //            case SCollectionMethods.SliceMethod.name =>
          //              val from = asRep[Int](argsV(0))
          //              val until = asRep[Int](argsV(1))
          //              xs.slice(from, until)
          //            case SCollectionMethods.UpdateManyMethod.name =>
          //              val indexes = asRep[Coll[Int]](argsV(0))
          //              val values = asRep[Coll[t]](argsV(1))
          //              xs.updateMany(indexes, values)
          //            case SCollectionMethods.IndexOfMethod.name =>
          //              val elem = asRep[t](argsV(0))
          //              val from = asRep[Int](argsV(1))
          //              xs.indexOf(elem, from)
          //            case SCollectionMethods.ZipMethod.name =>
          //              val ys = asRep[Coll[Any]](argsV(0))
          //              xs.zip(ys)
          //            case SCollectionMethods.FlatMapMethod.name =>
          //              val f = asRep[Any => Coll[Any]](argsV(0))
          //              xs.flatMap(f)
          //            case SCollectionMethods.MapMethod.name =>
          //              val f = asRep[Any => Any](argsV(0))
          //              xs.map(f)
          case SCollectionMethods.FilterMethod.name =>
            val p = args(0).asFunc
            mkFilter(xs.asCollection[SType], p)
          //            case SCollectionMethods.ForallMethod.name =>
          //              val p = asRep[Any => Boolean](argsV(0))
          //              xs.forall(p)
          //            case SCollectionMethods.ExistsMethod.name =>
          //              val p = asRep[Any => Boolean](argsV(0))
          //              xs.exists(p)
          case SCollectionMethods.FoldMethod.name =>
            mkFold(xs.asCollection[SType], args(0), args(1).asFunc)
          //              val zero = asRep[Any](argsV(0))
          //              val op = asRep[((Any, Any)) => Any](argsV(1))
          //              xs.foldLeft(zero, op)
          //            case SCollectionMethods.GetOrElseMethod.name =>
          //              val i = asRep[Int](argsV(0))
          //              val d = asRep[t](argsV(1))
          //              xs.getOrElse(i, d)

          case _ => null  // means that no rule is applicable
        }
        case (opt, SOptionMethods) => method.name match {
        //            case SOptionMethods.GetMethod.name =>
        //              opt.get
          case SOptionMethods.GetOrElseMethod.name =>
            mkOptionGetOrElse(opt.asOption[SType], args(0))
        //            case SOptionMethods.IsDefinedMethod.name =>
        //              opt.isDefined
        //            case SOptionMethods.MapMethod.name =>
        //              opt.map(asRep[t => Any](argsV(0)))
        //            case SOptionMethods.FilterMethod.name =>
        //              opt.filter(asRep[t => Boolean](argsV(0)))
          case _ => null
        }
        //          case (ge: Ref[GroupElement]@unchecked, SGroupElementMethods) => method.name match {
        //            case SGroupElementMethods.GetEncodedMethod.name =>
        //              ge.getEncoded
        //            case SGroupElementMethods.NegateMethod.name =>
        //              ge.negate
        //            case SGroupElementMethods.MultiplyMethod.name =>
        //              val g2 = asRep[GroupElement](argsV(0))
        //              ge.multiply(g2)
        //            case SGroupElementMethods.ExponentiateMethod.name =>
        //              val k = asRep[BigInt](argsV(0))
        //              ge.exp(k)
        //            case _ => throwError
        //          }
        //          case (box: Ref[Box]@unchecked, SBoxMethods) => method.name match {
        //            case SBoxMethods.tokensMethod.name =>
        //              box.tokens
        //            case _ => throwError
        //          }
        //          case (ctx: Ref[Context]@unchecked, SContextMethods) => method.name match {
        //            case SContextMethods.dataInputsMethod.name =>
        //              ctx.dataInputs
        //            case SContextMethods.headersMethod.name =>
        //              ctx.headers
        //            case SContextMethods.preHeaderMethod.name =>
        //              ctx.preHeader
        //            case SContextMethods.inputsMethod.name =>
        //              ctx.INPUTS
        //            case SContextMethods.outputsMethod.name =>
        //              ctx.OUTPUTS
        //            case SContextMethods.heightMethod.name =>
        //              ctx.HEIGHT
        //            case SContextMethods.selfMethod.name =>
        //              ctx.SELF
        //            case SContextMethods.selfBoxIndexMethod.name =>
        //              ctx.selfBoxIndex
        //            case SContextMethods.lastBlockUtxoRootHashMethod.name =>
        //              ctx.LastBlockUtxoRootHash
        //            case SContextMethods.minerPubKeyMethod.name =>
        //              ctx.minerPubKey
        //            case _ => throwError
        //          }
        //          case (tree: Ref[AvlTree]@unchecked, SAvlTreeMethods) => method.name match {
        //            case SAvlTreeMethods.digestMethod.name =>
        //              tree.digest
        //            case SAvlTreeMethods.keyLengthMethod.name =>
        //              tree.keyLength
        //            case SAvlTreeMethods.valueLengthOptMethod.name =>
        //              tree.valueLengthOpt
        //            case SAvlTreeMethods.enabledOperationsMethod.name =>
        //              tree.enabledOperations
        //            case SAvlTreeMethods.isInsertAllowedMethod.name =>
        //              tree.isInsertAllowed
        //            case SAvlTreeMethods.isRemoveAllowedMethod.name =>
        //              tree.isRemoveAllowed
        //            case SAvlTreeMethods.isUpdateAllowedMethod.name =>
        //              tree.isUpdateAllowed
        //            case SAvlTreeMethods.updateDigestMethod.name =>
        //              val digest = asRep[Coll[Byte]](argsV(0))
        //              tree.updateDigest(digest)
        //            case SAvlTreeMethods.updateOperationsMethod.name =>
        //              val operations = asRep[Byte](argsV(0))
        //              tree.updateOperations(operations)
        //            case SAvlTreeMethods.getMethod.name =>
        //              val key = asRep[Coll[Byte]](argsV(0))
        //              val proof = asRep[Coll[Byte]](argsV(1))
        //              tree.get(key, proof)
        //            case SAvlTreeMethods.getManyMethod.name =>
        //              val keys = asRep[Coll[Coll[Byte]]](argsV(0))
        //              val proof = asRep[Coll[Byte]](argsV(1))
        //              tree.getMany(keys, proof)
        //            case SAvlTreeMethods.containsMethod.name =>
        //              val key = asRep[Coll[Byte]](argsV(0))
        //              val proof = asRep[Coll[Byte]](argsV(1))
        //              tree.contains(key, proof)
        //            case SAvlTreeMethods.insertMethod.name =>
        //              val operations = asRep[Coll[(Coll[Byte], Coll[Byte])]](argsV(0))
        //              val proof = asRep[Coll[Byte]](argsV(1))
        //              tree.insert(operations, proof)
        //            case SAvlTreeMethods.removeMethod.name =>
        //              val operations = asRep[Coll[Coll[Byte]]](argsV(0))
        //              val proof = asRep[Coll[Byte]](argsV(1))
        //              tree.remove(operations, proof)
        //            case SAvlTreeMethods.updateMethod.name =>
        //              val operations = asRep[Coll[(Coll[Byte], Coll[Byte])]](argsV(0))
        //              val proof = asRep[Coll[Byte]](argsV(1))
        //              tree.update(operations, proof)
        //            case _ => throwError
        //          }
        //          case (ph: Ref[PreHeader]@unchecked, SPreHeaderMethods) => method.name match {
        //            case SPreHeaderMethods.versionMethod.name =>
        //              ph.version
        //            case SPreHeaderMethods.parentIdMethod.name =>
        //              ph.parentId
        //            case SPreHeaderMethods.timestampMethod.name =>
        //              ph.timestamp
        //            case SPreHeaderMethods.nBitsMethod.name =>
        //              ph.nBits
        //            case SPreHeaderMethods.heightMethod.name =>
        //              ph.height
        //            case SPreHeaderMethods.minerPkMethod.name =>
        //              ph.minerPk
        //            case SPreHeaderMethods.votesMethod.name =>
        //              ph.votes
        //            case _ => throwError
        //          }
        //          case (h: Ref[Header]@unchecked, SHeaderMethods) => method.name match {
        //            case SHeaderMethods.idMethod.name =>
        //              h.id
        //            case SHeaderMethods.versionMethod.name =>
        //              h.version
        //            case SHeaderMethods.parentIdMethod.name =>
        //              h.parentId
        //            case SHeaderMethods.ADProofsRootMethod.name =>
        //              h.ADProofsRoot
        //            case SHeaderMethods.stateRootMethod.name =>
        //              h.stateRoot
        //            case SHeaderMethods.transactionsRootMethod.name =>
        //              h.transactionsRoot
        //            case SHeaderMethods.timestampMethod.name =>
        //              h.timestamp
        //            case SHeaderMethods.nBitsMethod.name =>
        //              h.nBits
        //            case SHeaderMethods.heightMethod.name =>
        //              h.height
        //            case SHeaderMethods.extensionRootMethod.name =>
        //              h.extensionRoot
        //            case SHeaderMethods.minerPkMethod.name =>
        //              h.minerPk
        //            case SHeaderMethods.powOnetimePkMethod.name =>
        //              h.powOnetimePk
        //            case SHeaderMethods.powNonceMethod.name =>
        //              h.powNonce
        //            case SHeaderMethods.powDistanceMethod.name =>
        //              h.powDistance
        //            case SHeaderMethods.votesMethod.name =>
        //              h.votes
        //            case _ => throwError
        //          }
        //          case (g: Ref[SigmaDslBuilder]@unchecked, SGlobalMethods) => method.name match {
        //            case SGlobalMethods.groupGeneratorMethod.name =>
        //              g.groupGenerator
        //            case SGlobalMethods.xorMethod.name =>
        //              val c1 = asRep[Coll[Byte]](argsV(0))
        //              val c2 = asRep[Coll[Byte]](argsV(1))
        //              g.xor(c1, c2)
        //            case _ => throwError
        //          }
        case _ => null // means that no rule is applicable
      }
  }

  def lowering(node: SValue): SValue = {
    val res = transformer(node)
    res.asInstanceOf[SValue]
  }

  def compileNode(env: DefEnv, node: SValue, defId: Int): SValue = {
    def recurse(node: SValue): SValue = compileNode(env, node, defId)
    def throwError =
      error(s"Don't know how to compileNode($node)", node.sourceContext)

    // recursive helper to process any child
    def recurseChild(env: DefEnv, child: Any, defId: Int): AnyRef = child match {
      case child: SValue =>
        compileNode(env, child, defId)
      case xs: Seq[_] =>
        xs.map(recurseChild(env, _, defId))
      case p: Product =>
        recurseProduct(env, p, defId).asInstanceOf[AnyRef]
      case x => x.asInstanceOf[AnyRef] // everything else is left as is (modulo boxing)
    }

    // recursive helper to process any Product
    def recurseProduct[T <: Product](env: DefEnv, node: T, defId: Int): T = {
      val children = node.productIterator
        .map(recurseChild(env, _, defId))
        .toArray
      Duplicator(node, children)
    }

    node match {
      case Ident(name, _) =>
        env.get(name) match {
          case Some((valId, tpe)) => ValUse(valId, tpe)
          case None => error(s"Undefined symbol $name", node.sourceContext)
        }
      case Lambda(tpeParams, args, resTpe, bodyOpt) =>
        val body = bodyOpt.getOrElse(error("Lambda body is missing", node.sourceContext))
        val valId = defId + 1       // arguments are treated as ValDefs and occupy id space
        val (argName, argTpe) = args(0)
        val env1 = env + (argName -> (valId, argTpe))
        val newBody = compileNode(env1, body, valId)
        mkFuncValue(Array((valId, argTpe)), newBody)
      case Block(bindings, result) =>
        val valdefs = new ArrayBuffer[ValDef]
        var curId = defId
        var curEnv = env
        for (v <- bindings) {
          val rhs = compileNode(curEnv, v.body, curId)
          curId += 1
          val vd = ValDef(curId, Nil, rhs)
          require(v.givenType == rhs.tpe,
            s"Given type ${v.givenType} does not match inferred type ${rhs.tpe}")
          curEnv = curEnv + (v.name -> (curId, rhs.tpe))  // assign valId to `name`, so it can be use in ValUse
          valdefs += vd
        }
        val rhs = compileNode(curEnv, result, curId)
        val res = if (valdefs.nonEmpty) {
          (valdefs.toArray[BlockItem], rhs) match {
            // simple optimization to avoid producing block sub-expressions like:
            // `{ val idNew = id; idNew }` which this rules rewrites to just `id`
            case (Array(ValDef(idNew, _, source @ ValUse(id, tpe))), ValUse(idUse, tpeUse))
              if idUse == idNew && tpeUse == tpe => source
            case (items, _) =>
              BlockValue(items, rhs)
          }
        } else rhs
        res

      case _ => // fallback to recursive descent into term structure
        recurseProduct(env, node, defId)
    }
  }

  def compileTyped(node: SValue): SValue = {
    val lowered = lowering(node)
    val res = compileNode(emptyEnv, lowered,
      defId = 0 // the ValDef numbering will start from 1
    )
    res
  }
}

object DirectCompiler {
  /** Describes assignment of valIds for symbols (ValNodes) which become ValDefs.
    * Each ValDef in current scope have entry in this map. */
  type DefEnv = Map[String, (Int, SType)]

  val emptyEnv: DefEnv = Map.empty
}
