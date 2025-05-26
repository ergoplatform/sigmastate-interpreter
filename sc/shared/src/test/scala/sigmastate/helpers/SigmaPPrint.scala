package sigmastate.helpers

import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.RegisterId
import org.ergoplatform.settings.ErgoAlgos
import pprint.{PPrinter, Tree}
import sigma.ast.SCollection.{SBooleanArray, SByteArray, SByteArray2}
import sigma.ast._
import sigma.crypto.EcPointType
import sigma.data.{AvlTreeData, AvlTreeFlags, CollType, PrimitiveType, TrivialProp}
import sigma.serialization.GroupElementSerializer
import sigma.{Coll, GroupElement}
import sigma.ast.{ConstantNode, FuncValue, ValueCompanion}
import sigmastate._
import sigmastate.crypto.GF2_192_Poly
import sigma.ast.MethodCall
import java.math.BigInteger
import scala.collection.compat.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

/** Pretty-printer customized to print [[sigma.ast.Value]] instances
  * into a valid Scala code (can be cut-and-pasted).*/
object SigmaPPrint extends PPrinter {
  override def showFieldNames = false
  
  /** Apply [[treeify]] for each element of the given sequence producing the iterator of resulting trees. */
  protected def treeifySeq(xs: Seq[Any]): Iterator[Tree] = {
    xs.iterator.map(_ match {
      case t: Tree => t
      case x => treeify(x)
    })
  }

  /** Helper overload to call [[treeifySeq]]. */
  protected def treeifyMany(head: Any, tail: Any*): Iterator[Tree] = {
    treeifySeq(head +: tail)
  }

  private def tpeName(tpe: SType): String = {
    val name = tpe.toTermString
    if (name == "Boolean") "Bool" else name
  }

  /** Valid Scala type for the given SType. */
  private[helpers] def typeName(tpe: SType): String = tpe match {
    case _: SPredefType =>
      val name = tpe.getClass.getSimpleName.replace("$", "")
      s"$name.type" // SByte.type, SInt.type, etc
    case ct: SCollectionType[_] =>
      s"SCollection[${typeName(ct.elemType)}]"
    case ot: SOption[_] =>
      s"SOption[${typeName(ot.elemType)}]"
    case _: STuple =>
      "STuple"
    case _ =>
      sys.error(s"Cannot get typeName($tpe)")
  }

  /** Valid Scala type of the Value with the given underlying SType. */
  private def valueType(tpe: SType): String = {
    val tn = typeName(tpe)
    s"Value[$tn]"
  }

  private val typeHandlers: PartialFunction[Any, Tree] = {
    case SByteArray =>
      Tree.Literal("SByteArray")
    case SByteArray2 =>
      Tree.Literal("SByteArray2")
    case SBooleanArray =>
      Tree.Literal("SBooleanArray")
    case SPair(l, r) =>
      Tree.Apply("SPair", treeifySeq(Array[Any](l, r)))
    case t: PrimitiveType[_] =>
      Tree.Literal(s"RType.${t.name}Type")
    case CollType(tItem) =>
      Tree.Apply("CollType", treeifySeq(Array[Any](tItem)))
  }

  private val exceptionHandlers: PartialFunction[Any, Tree] = {
    case ex: Exception =>
      Tree.Apply(s"new ${ex.getClass.getSimpleName}", treeifySeq(Seq(ex.getMessage)))
    case ex: Error =>
      Tree.Apply(s"new ${ex.getClass.getSimpleName}", treeifySeq(Seq(ex.getMessage)))
  }

  /** Generated Scala code which creates the given byte array from a hex string literal. */
  private def treeifyByteArray(bytes: Array[Byte]): Tree = {
    val hexString = ErgoAlgos.encode(bytes)
    Tree.Apply("ErgoAlgos.decodeUnsafe", treeifyMany(hexString))
  }

  private val dataHandlers: PartialFunction[Any, Tree] = {
    case v: Byte =>
      Tree.Literal(s"$v.toByte")

    case v: Short =>
      Tree.Literal(s"$v.toShort")

    case v: BigInteger =>
      Tree.Apply("new BigInteger", treeifyMany(v.toString(16), 16))

    case v: BigInt =>
      Tree.Apply("BigInt", treeifyMany(v.toString(16), 16))

    case poly: GF2_192_Poly =>
      val c0 = poly.coeff0Bytes
      val others = poly.toByteArray(false) // don't output
      Tree.Apply("GF2_192_Poly.fromByteArray", treeifyMany(c0, others))

    case wa: mutable.WrappedArray[Byte @unchecked] if wa.elemTag == ClassTag.Byte =>
      treeifyByteArray(wa.array.asInstanceOf[Array[Byte]])

    case wa: ArraySeq[Byte @unchecked] if wa.unsafeArray.elemTag == ClassTag.Byte =>
      treeifyByteArray(wa.toArray[Byte])

    case wa: mutable.WrappedArray[_] =>
      Tree.Apply("Array", treeifySeq(wa.toSeq))

    case wa: ArraySeq[_] =>
      Tree.Apply("Array", treeifySeq(wa))

    case arr: Array[Byte @unchecked] if arr.elemTag == ClassTag.Byte =>
      treeifyByteArray(arr)

    case arr: Array[_] =>
      Tree.Apply("Array", treeifySeq(arr))

    case buf: ArrayBuffer[_] =>
      Tree.Apply("Seq", treeifySeq(buf.toSeq))

    case ecp: EcPointType =>
      val hexString = ErgoAlgos.encode(GroupElementSerializer.toBytes(ecp))
      Tree.Apply("Helpers.decodeECPoint", treeifyMany(hexString))

    case ge: GroupElement =>
      val hexString = ErgoAlgos.encode(ge.getEncoded)
      Tree.Apply("Helpers.decodeGroupElement", treeifyMany(hexString))

    case Some(v) =>
      Tree.Apply("Some", treeifyMany(v))

    case coll: Coll[Byte @unchecked] if coll.tItem == sigma.ByteType =>
      val hexString = ErgoAlgos.encode(coll)
      Tree.Apply("Helpers.decodeBytes", treeifyMany(hexString))

    case coll: Coll[_] =>
      val elemTpe = coll.tItem.name
      Tree.Apply(s"Coll[$elemTpe]", treeifySeq(coll.toArray))

    case tp: TrivialProp =>
      Tree.Literal(s"TrivialProp.${if (tp.condition) "True" else "False"}Prop")

    case f: AvlTreeFlags =>
      Tree.Apply("AvlTreeFlags", treeifyMany(
        f.insertAllowed,
        f.updateAllowed,
        f.removeAllowed))

    case t: AvlTreeData =>
      Tree.Apply("AvlTreeData", treeifyMany(
        Tree.Apply("ADDigest @@ ", treeifyMany(t.digest)),
        t.treeFlags,
        t.keyLength,
        t.valueLengthOpt))

    case t: ErgoTree =>
      Tree.Apply("new ErgoTree", treeifyMany(
        t.header,
        Tree.Apply("Vector", t.constants.map(treeify).iterator),
        t.root
      ))

    case b: ErgoBox =>
      val tokens = Tree.Apply("Coll",
        b.additionalTokens.toArray.map { case (id, v) =>
          val idTree = Tree.Apply("Digest32 @@ ", treeifyMany(id))
          Tree.Apply("", treeifyMany(idTree, v))
        }.iterator)
      Tree.Apply("new ErgoBox", treeifyMany(
        b.value,
        b.ergoTree,
        tokens,
        b.additionalRegisters,
        Tree.Apply("ModifierId @@ ", treeifyMany(b.transactionId)),
        b.index,
        b.creationHeight
      ))
  }

  def methodLiteral(m: SMethod) = {
    val objType = apply(m.objType).plainText
    Tree.Literal(s"$objType.${m.name}")
  }

  override val additionalHandlers: PartialFunction[Any, Tree] =
    typeHandlers
     .orElse(exceptionHandlers)
     .orElse(dataHandlers)
     .orElse {
    case FixedCostItem(CompanionDesc(c), _) =>
      Tree.Apply("FixedCostItem", treeifySeq(Seq(c)))
    case FixedCostItem(MethodDesc(m), cost) =>
      Tree.Apply("FixedCostItem", Seq(methodLiteral(m)).iterator ++ treeifySeq(Seq(cost)))
    case FuncValue.AddToEnvironmentDesc =>
      Tree.Literal(s"FuncValue.AddToEnvironmentDesc")
    case MethodDesc(method) =>
      Tree.Apply("MethodDesc", Seq(methodLiteral(method)).iterator)
    case SGlobal =>
      Tree.Literal(s"SGlobal")
    case SCollection =>
      Tree.Literal(s"SCollection")
    case SOption =>
      Tree.Literal(s"SOption")
    case t: STypeCompanion if t.isInstanceOf[SType] =>
      Tree.Literal(s"S${t.typeName}")
    case c: ValueCompanion =>
      Tree.Literal(c.typeName)
    case r: RegisterId =>
      Tree.Literal(s"ErgoBox.R${r.number}")
    case sf: SelectField =>
      val resTpe = sf.input.tpe.items(sf.fieldIndex - 1)
      val resTpeName = valueType(resTpe)
      Tree.Apply(s"SelectField.typed[$resTpeName]", treeifySeq(Array[Any](sf.input, sf.fieldIndex)))

    case ConstantNode(v, SCollectionType(elemType)) if elemType.isInstanceOf[SPredefType] =>
      Tree.Apply(tpeName(elemType) + "ArrayConstant", treeifySeq(Seq(v)))

    case ConstantNode(true, SBoolean) =>
      Tree.Literal("TrueLeaf")

    case ConstantNode(false, SBoolean) =>
      Tree.Literal("FalseLeaf")

    case c: ConstantNode[_] if c.tpe.isInstanceOf[SPredefType] =>
      Tree.Apply(tpeName(c.tpe) + "Constant", treeifySeq(Seq(c.value)))

    case ArithOp(l, r, code) =>
      val args = treeifySeq(Seq(l, r)).toSeq :+ Tree.Apply("OpCode @@ ", treeifySeq(Seq(code)))
      Tree.Apply("ArithOp", args.iterator)

    case mc @ MethodCall(obj, method, args, typeSubst) =>
      val objType = apply(method.objType).plainText
      val methodTemplate = method.objType.getMethodByName(method.name)
      val methodT = unifyTypeLists(methodTemplate.stype.tDom, obj.tpe +: args.map(_.tpe)) match {
        case Some(subst) if subst.nonEmpty =>
          val getMethod = s"""$objType.getMethodByName("${method.name}").withConcreteTypes"""
          Tree.Apply(getMethod, treeifySeq(Seq(subst)))
        case _ =>
          val getMethod = s"$objType.getMethodByName"
          Tree.Apply(getMethod, Seq(treeify(method.name)).iterator)
      }

      val objT = treeify(obj)
      val argsT = treeify(args)
      val substT = treeify(typeSubst)
      val resTpe = mc.tpe
      val resTpeName = valueType(resTpe)
      Tree.Apply(s"MethodCall.typed[$resTpeName]", Seq(objT, methodT, argsT, substT).iterator)
  }
}

