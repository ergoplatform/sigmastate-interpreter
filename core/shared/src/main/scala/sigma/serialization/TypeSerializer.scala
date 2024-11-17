package sigma.serialization

import debox.cfor
import sigma.VersionContext
import sigma.ast.SCollectionType.{CollectionTypeCode, NestedCollectionTypeCode}
import sigma.ast._
import sigma.util.safeNewArray
import sigma.validation.ValidationRules.{CheckPrimitiveTypeCode, CheckPrimitiveTypeCodeV6, CheckTypeCode, CheckTypeCodeV6}

import java.nio.charset.StandardCharsets

/** Serialization of types according to specification in TypeSerialization.md. */
class TypeSerializer {
  import TypeSerializer._

  def getEmbeddableType(code: Int): SType = {
    // todo : add unsigned bit int to embeddable id to type
    if (VersionContext.current.isV6SoftForkActivated) {
      CheckPrimitiveTypeCodeV6(code.toByte)
    } else {
      CheckPrimitiveTypeCode(code.toByte)
    }
    embeddableIdToType(code)
  }

  def serialize(tpe: SType, w: CoreByteWriter): Unit = tpe match {
    case p: SEmbeddable => w.put(p.typeCode)
    case SString => w.put(SString.typeCode)
    case SAny => w.put(SAny.typeCode)
    case SUnit => w.put(SUnit.typeCode)
    case SBox => w.put(SBox.typeCode)
    case SAvlTree => w.put(SAvlTree.typeCode)
    case SContext => w.put(SContext.typeCode)
    case SGlobal => w.put(SGlobal.typeCode)
    case SHeader => w.put(SHeader.typeCode)
    case SPreHeader => w.put(SPreHeader.typeCode)
    case c: SCollectionType[a] => c.elemType match {
      case p: SEmbeddable =>
        val code = p.embedIn(CollectionTypeCode)
        w.put(code)
      case cn: SCollectionType[a] => cn.elemType match {
        case p: SEmbeddable =>
          val code = p.embedIn(NestedCollectionTypeCode)
          w.put(code)
        case _ =>
          w.put(CollectionTypeCode)
          serialize(cn, w)
      }
      case t =>
        w.put(CollectionTypeCode)
        serialize(t, w)
    }
    case o: SOption[a] => o.elemType match {
      case p: SEmbeddable =>
        val code = p.embedIn(SOption.OptionTypeCode)
        w.put(code)
      case c: SCollectionType[a] => c.elemType match {
        case p: SEmbeddable =>
          val code = p.embedIn(SOption.OptionCollectionTypeCode)
          w.put(code)
        case _ =>
          w.put(SOption.OptionTypeCode)
          serialize(c, w)
      }
      case t =>
        w.put(SOption.OptionTypeCode)
        serialize(t, w)
    }
    case _ @ STuple(Seq(t1, t2)) => (t1, t2) match {
      case (p: SEmbeddable, _) =>
        if (p == t2) {
          // Symmetric pair of primitive types (`(Int, Int)`, `(Byte,Byte)`, etc.)
          val code = p.embedIn(STuple.PairSymmetricTypeCode)
          w.put(code)
        } else {
          // Pair of types where first is primitive (`(_, Int)`)
          val code = p.embedIn(STuple.Pair1TypeCode)
          w.put(code)
          serialize(t2, w)
        }
      case (_, p: SEmbeddable) =>
        // Pair of types where second is primitive (`(Int, _)`)
        val code = p.embedIn(STuple.Pair2TypeCode)
        w.put(code)
        serialize(t1, w)
      case _ =>
        // Pair of non-primitive types (`((Int, Byte), (Boolean,Box))`, etc.)
        w.put(STuple.Pair1TypeCode)
        serialize(t1, w)
        serialize(t2, w)
    }
    case STuple(items) if items.length < 2 =>
      sys.error(s"Invalid Tuple type with less than 2 items $items")
    case tup: STuple => tup.items.length match {
      case 3 =>
        // Triple of types
        w.put(STuple.TripleTypeCode)
        for (i <- tup.items)
          serialize(i, w)
      case 4 =>
        // Quadruple of types
        w.put(STuple.QuadrupleTypeCode)
        for (i <- tup.items)
          serialize(i, w)
      case _ =>
        // `Tuple` type with more than 4 items `(Int, Byte, Box, Boolean, Int)`
        serializeTuple(tup, w)
    }
    case SFunc(tDom, tRange, tpeParams) =>
      w.put(SFunc.FuncTypeCode)
      w.putUByte(tDom.length)
      tDom.foreach { st =>
        serialize(st, w)
      }
      serialize(tRange, w)
      w.putUByte(tpeParams.length)
      tpeParams.foreach { tp =>
        serialize(tp.ident, w)
      }
    case typeIdent: STypeVar => {
      w.put(typeIdent.typeCode)
      val bytes = typeIdent.name.getBytes(StandardCharsets.UTF_8)
      w.putUByte(bytes.length)
        .putBytes(bytes)
    }
  }

  def deserialize(r: CoreByteReader): SType = deserialize(r, 0)

  private def deserialize(r: CoreByteReader, depth: Int): SType = {
    val c = r.getUByte()
    if (c <= 0)
      throw new InvalidTypePrefix(s"Cannot deserialize type prefix $c. Unexpected buffer $r with bytes ${r.getBytes(r.remaining)}")
    val tpe: SType = if (c < STuple.TupleTypeCode) {
      val constrId = c / SPrimType.PrimRange
      val primId   = c % SPrimType.PrimRange
      constrId match {
        case 0 => // primitive
          getEmbeddableType(c)
        case 1 => // Array[_]
          val tElem = getArgType(r, primId, depth)
          SCollection(tElem)
        case 2 => // Array[Array[_]]
          val tElem = getArgType(r, primId, depth)
          SCollection(SCollection(tElem))
        case 3 => // Option[_]
          val tElem = getArgType(r, primId, depth)
          SOption(tElem)
        case 4 => // Option[Collection[_]]
          val tElem = getArgType(r, primId, depth)
          SOption(SCollection(tElem))
        case STuple.Pair1TypeConstrId => // (_, t2)
          val (t1, t2) = if (primId == 0) {
            // Pair of non-primitive types (`((Int, Byte), (Boolean,Box))`, etc.)
            (deserialize(r, depth + 1), deserialize(r, depth + 1))
          } else {
            // Pair of types where first is primitive (`(_, Int)`)
            (getEmbeddableType(primId), deserialize(r, depth + 1))
          }
          STuple(t1, t2)
        case STuple.Pair2TypeConstrId => // (t1, _)
          if (primId == 0) {
            // Triple of types
            val (t1, t2, t3) = (deserialize(r, depth + 1), deserialize(r, depth + 1), deserialize(r, depth + 1))
            STuple(t1, t2, t3)
          } else {
            // Pair of types where second is primitive (`(Int, _)`)
            val t2 = getEmbeddableType(primId)
            val t1 = deserialize(r, depth + 1)
            STuple(t1, t2)
          }
        case STuple.PairSymmetricTypeConstrId => // (_, _)
          if (primId == 0) {
            // Quadruple of types
            val (t1, t2, t3, t4) = (deserialize(r, depth + 1), deserialize(r, depth + 1), deserialize(r, depth + 1), deserialize(r, depth + 1))
            STuple(t1, t2, t3, t4)
          } else {
            // Symmetric pair of primitive types (`(Int, Int)`, `(Byte,Byte)`, etc.)
            val t = getEmbeddableType(primId)
            STuple(t, t)
          }
      }
    }
    else {
      c match {
        case STuple.TupleTypeCode => {
          val len = r.getUByte()
          val items = safeNewArray[SType](len)
          cfor(0)(_ < len, _ + 1) { i =>
            items(i) = deserialize(r, depth + 1)
          }
          STuple(items)
        }
        case SAny.typeCode => SAny
        case SUnit.typeCode => SUnit
        case SBox.typeCode => SBox
        case SAvlTree.typeCode => SAvlTree
        case SContext.typeCode => SContext
        case SString.typeCode => SString
        case STypeVar.TypeCode => {
          val nameLength = r.getUByte()
          val name = new String(r.getBytes(nameLength), StandardCharsets.UTF_8)
          STypeVar(name)
        }
        case SHeader.typeCode => SHeader
        case SPreHeader.typeCode => SPreHeader
        case SGlobal.typeCode => SGlobal
        case SFunc.FuncTypeCode if VersionContext.current.isV6SoftForkActivated =>
          val tdLength = r.getUByte()

          val tDom = (1 to tdLength).map { _ =>
            deserialize(r)
          }
          val tRange = deserialize(r)
          val tpeParamsLength = r.getUByte()
          val tpeParams = (1 to tpeParamsLength).map { _ =>
            val ident = deserialize(r)
            require(ident.isInstanceOf[STypeVar])
            STypeParam(ident.asInstanceOf[STypeVar])
          }
          SFunc(tDom, tRange, tpeParams)
        case _ =>
          // the #1008 check replaced with one with identical behavior but different opcode (1018), to activate
          //  ReplacedRule(1008 -> 1018) during 6.0 activation
          if (VersionContext.current.isV6SoftForkActivated) {
            CheckTypeCodeV6(c.toByte)
          } else {
            CheckTypeCode(c.toByte)
          }
          NoType
      }
    }
    tpe
  }

  private def getArgType(r: CoreByteReader, primId: Int, depth: Int) =
    if (primId == 0)
      deserialize(r, depth + 1)
    else
      getEmbeddableType(primId)

  private def serializeTuple(t: STuple, w: CoreByteWriter) = {
    assert(t.items.length <= 255)
    w.put(STuple.TupleTypeCode)
    w.putUByte(t.items.length)
    for (i <- t.items)
      serialize(i, w)
  }
}

object TypeSerializer extends TypeSerializer {
  /** The list of embeddable types, i.e. types that can be combined with type constructor for optimized encoding.
    * For each embeddable type `T`, and type constructor `C`, the type `C[T]` can be represented by single byte. */
    def embeddableIdToType = {
      if (VersionContext.current.isV6SoftForkActivated) {
        embeddableV6
      } else {
        embeddableV5
      }
    }

  private val embeddableV5 = Array[SType](null, SBoolean, SByte, SShort, SInt, SLong, SBigInt, SGroupElement, SSigmaProp)

  private val embeddableV6 = Array[SType](null, SBoolean, SByte, SShort, SInt, SLong, SBigInt, SGroupElement, SSigmaProp, SUnsignedBigInt)

}