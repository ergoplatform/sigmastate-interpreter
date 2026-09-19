package sigma.compiler.ir.wrappers.sigma

import scala.language.{existentials, implicitConversions}
import scalan._
import sigma.compiler.ir.IRContext
import sigma.compiler.ir.wrappers.sigma.impl.SigmaDslDefs

import scala.collection.compat.immutable.ArraySeq

package impl {
  import sigma.ast.SType.tT
  import sigma.compiler.ir.wrappers.sigma.SigmaDsl
  import sigma.compiler.ir.{Base, GraphIRReflection, IRContext}
  import sigma.data.{Nullable, RType}
  import sigma.reflection.RClass

/** Implementation part of IR represenation related to Sigma types and methods. */
  // Abs -----------------------------------
trait SigmaDslDefs extends Base with SigmaDsl {
  self: IRContext =>

import AvlTree._
import BigInt._
import Box._
import Coll._
import CollBuilder._
import GroupElement._
import Header._
import PreHeader._
import SigmaProp._
import WOption._









object SigmaDslBuilder extends EntityObject("SigmaDslBuilder") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SSigmaDslBuilder = sigma.SigmaDslBuilder
  case class SigmaDslBuilderConst(
        constValue: SSigmaDslBuilder
      ) extends LiftedConst[SSigmaDslBuilder, SigmaDslBuilder] with SigmaDslBuilder
        with Def[SigmaDslBuilder] with SigmaDslBuilderConstMethods {
    val liftable: Liftable[SSigmaDslBuilder, SigmaDslBuilder] = LiftableSigmaDslBuilder
    val resultType: Elem[SigmaDslBuilder] = liftable.eW
  }

  trait SigmaDslBuilderConstMethods extends SigmaDslBuilder  { thisConst: Def[_] =>

    private val SigmaDslBuilderClass = RClass(classOf[SigmaDslBuilder])

    override def Colls: Ref[sigma.CollBuilder] = {
      asRep[sigma.CollBuilder](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("Colls"),
        ArraySeq.empty,
        true, false, element[sigma.CollBuilder]))
    }

    override def atLeast(bound: Ref[Int], props: Ref[sigma.Coll[sigma.SigmaProp]]): Ref[sigma.SigmaProp] = {
      asRep[sigma.SigmaProp](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("atLeast", classOf[Sym], classOf[Sym]),
        Array[AnyRef](bound, props),
        true, false, element[sigma.SigmaProp]))
    }

    override def allOf(conditions: Ref[sigma.Coll[Boolean]]): Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("allOf", classOf[Sym]),
        Array[AnyRef](conditions),
        true, false, element[Boolean]))
    }

    override def allZK(conditions: Ref[sigma.Coll[sigma.SigmaProp]]): Ref[sigma.SigmaProp] = {
      asRep[sigma.SigmaProp](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("allZK", classOf[Sym]),
        Array[AnyRef](conditions),
        true, false, element[sigma.SigmaProp]))
    }

    override def anyOf(conditions: Ref[sigma.Coll[Boolean]]): Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("anyOf", classOf[Sym]),
        Array[AnyRef](conditions),
        true, false, element[Boolean]))
    }

    override def anyZK(conditions: Ref[sigma.Coll[sigma.SigmaProp]]): Ref[sigma.SigmaProp] = {
      asRep[sigma.SigmaProp](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("anyZK", classOf[Sym]),
        Array[AnyRef](conditions),
        true, false, element[sigma.SigmaProp]))
    }

    override def xorOf(conditions: Ref[sigma.Coll[Boolean]]): Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("xorOf", classOf[Sym]),
        Array[AnyRef](conditions),
        true, false, element[Boolean]))
    }

    override def sigmaProp(b: Ref[Boolean]): Ref[sigma.SigmaProp] = {
      asRep[sigma.SigmaProp](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("sigmaProp", classOf[Sym]),
        Array[AnyRef](b),
        true, false, element[sigma.SigmaProp]))
    }

    override def blake2b256(bytes: Ref[sigma.Coll[Byte]]): Ref[sigma.Coll[Byte]] = {
      asRep[sigma.Coll[Byte]](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("blake2b256", classOf[Sym]),
        Array[AnyRef](bytes),
        true, false, element[sigma.Coll[Byte]]))
    }

    override def sha256(bytes: Ref[sigma.Coll[Byte]]): Ref[sigma.Coll[Byte]] = {
      asRep[sigma.Coll[Byte]](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("sha256", classOf[Sym]),
        Array[AnyRef](bytes),
        true, false, element[sigma.Coll[Byte]]))
    }

    override def byteArrayToBigInt(bytes: Ref[sigma.Coll[Byte]]): Ref[sigma.BigInt] = {
      asRep[sigma.BigInt](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("byteArrayToBigInt", classOf[Sym]),
        Array[AnyRef](bytes),
        true, false, element[sigma.BigInt]))
    }

    override def longToByteArray(l: Ref[Long]): Ref[sigma.Coll[Byte]] = {
      asRep[sigma.Coll[Byte]](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("longToByteArray", classOf[Sym]),
        Array[AnyRef](l),
        true, false, element[sigma.Coll[Byte]]))
    }

    override def byteArrayToLong(bytes: Ref[sigma.Coll[Byte]]): Ref[Long] = {
      asRep[Long](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("byteArrayToLong", classOf[Sym]),
        Array[AnyRef](bytes),
        true, false, element[Long]))
    }

    override def proveDlog(g: Ref[sigma.GroupElement]): Ref[sigma.SigmaProp] = {
      asRep[sigma.SigmaProp](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("proveDlog", classOf[Sym]),
        Array[AnyRef](g),
        true, false, element[sigma.SigmaProp]))
    }

    override def proveDHTuple(g: Ref[sigma.GroupElement], h: Ref[sigma.GroupElement], u: Ref[sigma.GroupElement], v: Ref[sigma.GroupElement]): Ref[sigma.SigmaProp] = {
      asRep[sigma.SigmaProp](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("proveDHTuple", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](g, h, u, v),
        true, false, element[sigma.SigmaProp]))
    }

    override def groupGenerator: Ref[sigma.GroupElement] = {
      asRep[sigma.GroupElement](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("groupGenerator"),
        ArraySeq.empty,
        true, false, element[sigma.GroupElement]))
    }

    override def substConstants[T](scriptBytes: Ref[sigma.Coll[Byte]], positions: Ref[sigma.Coll[Int]], newValues: Ref[sigma.Coll[T]]): Ref[sigma.Coll[Byte]] = {
      asRep[sigma.Coll[Byte]](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("substConstants", classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](scriptBytes, positions, newValues),
        true, false, element[sigma.Coll[Byte]]))
    }

    override def decodePoint(encoded: Ref[sigma.Coll[Byte]]): Ref[sigma.GroupElement] = {
      asRep[sigma.GroupElement](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("decodePoint", classOf[Sym]),
        Array[AnyRef](encoded),
        true, false, element[sigma.GroupElement]))
    }

    override def avlTree(operationFlags: Ref[Byte], digest: Ref[sigma.Coll[Byte]], keyLength: Ref[Int], valueLengthOpt: Ref[Option[Int]]): Ref[sigma.AvlTree] = {
      asRep[sigma.AvlTree](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("avlTree", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](operationFlags, digest, keyLength, valueLengthOpt),
        true, false, element[sigma.AvlTree]))
    }

    override def xor(l: Ref[sigma.Coll[Byte]], r: Ref[sigma.Coll[Byte]]): Ref[sigma.Coll[Byte]] = {
      asRep[sigma.Coll[Byte]](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("xor", classOf[Sym], classOf[Sym]),
        Array[AnyRef](l, r),
        true, false, element[sigma.Coll[Byte]]))
    }

    def serialize[T](value: Ref[T]): Ref[sigma.Coll[Byte]] = {
      asRep[sigma.Coll[Byte]](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("serialize", classOf[Sym]),
        Array[AnyRef](value),
        true, false, element[sigma.Coll[Byte]]))
    }

    override def deserializeTo[T](l: Ref[sigma.Coll[Byte]])(implicit cT: Elem[T]): Ref[T] = {
      asRep[T](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("deserializeTo", classOf[Sym], classOf[Elem[T]]),
        Array[AnyRef](l, cT),
        true, false, element[T](cT), Map(tT -> elemToSType(cT))))
    }
    override def fromBigEndianBytes[T](bytes: Ref[sigma.Coll[Byte]])(implicit cT: Elem[T]): Ref[T] = {
      asRep[T](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("fromBigEndianBytes", classOf[Sym], classOf[Elem[T]]),
        Array[AnyRef](bytes, cT),
        true, false, cT, Map(tT -> elemToSType(cT))))
    }

    override def some[T](value: Ref[T])(implicit cT: Elem[T]): Ref[Option[T]] = {
      asRep[Option[T]](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("some", classOf[Sym], classOf[Elem[T]]),
        Array[AnyRef](value, cT),
        true, false, element[Option[T]], Map(tT -> elemToSType(cT))))
    }

    override def none[T]()(implicit cT: Elem[T]): Ref[Option[T]] = {
      asRep[Option[T]](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("none", classOf[Elem[T]]),
        Array[AnyRef](cT),
        true, false, element[Option[T]], Map(tT -> elemToSType(cT))))
    }


    override def powHit(k: Ref[Int], msg: Ref[sigma.Coll[Byte]], nonce: Ref[sigma.Coll[Byte]], h: Ref[sigma.Coll[Byte]], N: Ref[Int]): Ref[sigma.UnsignedBigInt] = {
      asRep[sigma.UnsignedBigInt](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("powHit", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](k, msg, nonce, h, N),
        true, false, element[sigma.UnsignedBigInt](UnsignedBigInt.unsignedBigIntElement)))
    }

    override def encodeNbits(bi: Ref[sigma.BigInt]): Ref[Long] = {
      asRep[Long](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("encodeNbits", classOf[Sym]),
        Array[AnyRef](bi),
        true, false, element[Long]))
    }

    override def decodeNbits(l: Ref[Long]): Ref[sigma.BigInt] = {
      asRep[sigma.BigInt](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("decodeNbits", classOf[Sym]),
        Array[AnyRef](l),
        true, false, element[sigma.BigInt]))
    }
  }

  implicit object LiftableSigmaDslBuilder
    extends Liftable[SSigmaDslBuilder, SigmaDslBuilder] {
    lazy val eW: Elem[SigmaDslBuilder] = sigmaDslBuilderElement
    lazy val sourceType: RType[SSigmaDslBuilder] = {
      RType[SSigmaDslBuilder]
    }
    def lift(x: SSigmaDslBuilder): Ref[SigmaDslBuilder] = SigmaDslBuilderConst(x)
  }

  private val SigmaDslBuilderClass = RClass(classOf[SigmaDslBuilder])

  // entityAdapter for SigmaDslBuilder trait
  case class SigmaDslBuilderAdapter(source: Ref[SigmaDslBuilder])
      extends Node with SigmaDslBuilder
      with Def[SigmaDslBuilder] {
    val resultType: Elem[SigmaDslBuilder] = element[SigmaDslBuilder]
    override def transform(t: Transformer) = SigmaDslBuilderAdapter(t(source))

    def Colls: Ref[sigma.CollBuilder] = {
      asRep[sigma.CollBuilder](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("Colls"),
        ArraySeq.empty,
        true, true, element[sigma.CollBuilder]))
    }

    def atLeast(bound: Ref[Int], props: Ref[sigma.Coll[sigma.SigmaProp]]): Ref[sigma.SigmaProp] = {
      asRep[sigma.SigmaProp](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("atLeast", classOf[Sym], classOf[Sym]),
        Array[AnyRef](bound, props),
        true, true, element[sigma.SigmaProp]))
    }

    def allOf(conditions: Ref[sigma.Coll[Boolean]]): Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("allOf", classOf[Sym]),
        Array[AnyRef](conditions),
        true, true, element[Boolean]))
    }

    def allZK(conditions: Ref[sigma.Coll[sigma.SigmaProp]]): Ref[sigma.SigmaProp] = {
      asRep[sigma.SigmaProp](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("allZK", classOf[Sym]),
        Array[AnyRef](conditions),
        true, true, element[sigma.SigmaProp]))
    }

    def anyOf(conditions: Ref[sigma.Coll[Boolean]]): Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("anyOf", classOf[Sym]),
        Array[AnyRef](conditions),
        true, true, element[Boolean]))
    }

    def anyZK(conditions: Ref[sigma.Coll[sigma.SigmaProp]]): Ref[sigma.SigmaProp] = {
      asRep[sigma.SigmaProp](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("anyZK", classOf[Sym]),
        Array[AnyRef](conditions),
        true, true, element[sigma.SigmaProp]))
    }

    def xorOf(conditions: Ref[sigma.Coll[Boolean]]): Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("xorOf", classOf[Sym]),
        Array[AnyRef](conditions),
        true, true, element[Boolean]))
    }

    def sigmaProp(b: Ref[Boolean]): Ref[sigma.SigmaProp] = {
      asRep[sigma.SigmaProp](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("sigmaProp", classOf[Sym]),
        Array[AnyRef](b),
        true, true, element[sigma.SigmaProp]))
    }

    def blake2b256(bytes: Ref[sigma.Coll[Byte]]): Ref[sigma.Coll[Byte]] = {
      asRep[sigma.Coll[Byte]](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("blake2b256", classOf[Sym]),
        Array[AnyRef](bytes),
        true, true, element[sigma.Coll[Byte]]))
    }

    def sha256(bytes: Ref[sigma.Coll[Byte]]): Ref[sigma.Coll[Byte]] = {
      asRep[sigma.Coll[Byte]](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("sha256", classOf[Sym]),
        Array[AnyRef](bytes),
        true, true, element[sigma.Coll[Byte]]))
    }

    def byteArrayToBigInt(bytes: Ref[sigma.Coll[Byte]]): Ref[sigma.BigInt] = {
      asRep[sigma.BigInt](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("byteArrayToBigInt", classOf[Sym]),
        Array[AnyRef](bytes),
        true, true, element[sigma.BigInt]))
    }

    def longToByteArray(l: Ref[Long]): Ref[sigma.Coll[Byte]] = {
      asRep[sigma.Coll[Byte]](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("longToByteArray", classOf[Sym]),
        Array[AnyRef](l),
        true, true, element[sigma.Coll[Byte]]))
    }

    def byteArrayToLong(bytes: Ref[sigma.Coll[Byte]]): Ref[Long] = {
      asRep[Long](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("byteArrayToLong", classOf[Sym]),
        Array[AnyRef](bytes),
        true, true, element[Long]))
    }

    def proveDlog(g: Ref[sigma.GroupElement]): Ref[sigma.SigmaProp] = {
      asRep[sigma.SigmaProp](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("proveDlog", classOf[Sym]),
        Array[AnyRef](g),
        true, true, element[sigma.SigmaProp]))
    }

    def proveDHTuple(g: Ref[sigma.GroupElement], h: Ref[sigma.GroupElement], u: Ref[sigma.GroupElement], v: Ref[sigma.GroupElement]): Ref[sigma.SigmaProp] = {
      asRep[sigma.SigmaProp](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("proveDHTuple", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](g, h, u, v),
        true, true, element[sigma.SigmaProp]))
    }

    def groupGenerator: Ref[sigma.GroupElement] = {
      asRep[sigma.GroupElement](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("groupGenerator"),
        ArraySeq.empty,
        true, true, element[sigma.GroupElement]))
    }

    def substConstants[T](scriptBytes: Ref[sigma.Coll[Byte]], positions: Ref[sigma.Coll[Int]], newValues: Ref[sigma.Coll[T]]): Ref[sigma.Coll[Byte]] = {
      asRep[sigma.Coll[Byte]](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("substConstants", classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](scriptBytes, positions, newValues),
        true, true, element[sigma.Coll[Byte]]))
    }

    def decodePoint(encoded: Ref[sigma.Coll[Byte]]): Ref[sigma.GroupElement] = {
      asRep[sigma.GroupElement](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("decodePoint", classOf[Sym]),
        Array[AnyRef](encoded),
        true, true, element[sigma.GroupElement]))
    }

    def avlTree(operationFlags: Ref[Byte], digest: Ref[sigma.Coll[Byte]], keyLength: Ref[Int], valueLengthOpt: Ref[Option[Int]]): Ref[sigma.AvlTree] = {
      asRep[sigma.AvlTree](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("avlTree", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](operationFlags, digest, keyLength, valueLengthOpt),
        true, true, element[sigma.AvlTree]))
    }

    def xor(l: Ref[sigma.Coll[Byte]], r: Ref[sigma.Coll[Byte]]): Ref[sigma.Coll[Byte]] = {
      asRep[sigma.Coll[Byte]](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("xor", classOf[Sym], classOf[Sym]),
        Array[AnyRef](l, r),
        true, true, element[sigma.Coll[Byte]]))
    }

    def powHit(k: Ref[Int], msg: Ref[sigma.Coll[Byte]], nonce: Ref[sigma.Coll[Byte]], h: Ref[sigma.Coll[Byte]], N: Ref[Int]): Ref[sigma.UnsignedBigInt] = {
      asRep[sigma.UnsignedBigInt](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("powHit", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](k, msg, nonce, h, N),
        true, true, element[sigma.UnsignedBigInt](UnsignedBigInt.unsignedBigIntElement)))
    }

    def serialize[T](value: Ref[T]): Ref[sigma.Coll[Byte]] = {
      asRep[sigma.Coll[Byte]](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("serialize", classOf[Sym]),
        Array[AnyRef](value),
        true, true, element[sigma.Coll[Byte]]))
    }

    def deserializeTo[T](bytes: Ref[sigma.Coll[Byte]])(implicit cT: Elem[T]): Ref[T] = {
      asRep[T](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("deserializeTo", classOf[Sym], classOf[Elem[_]]),
        Array[AnyRef](bytes, cT),
        true, true, element[T](cT), Map(tT -> elemToSType(cT))))
    }

    def fromBigEndianBytes[T](bytes: Ref[sigma.Coll[Byte]])(implicit cT: Elem[T]): Ref[T] = {
      asRep[T](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("fromBigEndianBytes", classOf[Sym], classOf[Elem[T]]),
        Array[AnyRef](bytes, cT),
        true, true, cT, Map(tT -> elemToSType(cT))))
    }

    def some[T](value: Ref[T])(implicit cT: Elem[T]): Ref[Option[T]] = {
      asRep[Option[T]](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("some", classOf[Sym], classOf[Elem[T]]),
        Array[AnyRef](value, cT),
        true, true, element[Option[T]], Map(tT -> elemToSType(cT))))
    }

    def none[T]()(implicit cT: Elem[T]): Ref[Option[T]] = {
      asRep[Option[T]](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("none", classOf[Elem[T]]),
        Array[AnyRef](cT),
        true, true, element[Option[T]], Map(tT -> elemToSType(cT))))
    }


    override def encodeNbits(bi: Ref[sigma.BigInt]): Ref[Long] = {
      asRep[Long](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("encodeNbits", classOf[Sym]),
        Array[AnyRef](bi),
        true, true, element[Long]))
    }

    override def decodeNbits(l: Ref[Long]): Ref[sigma.BigInt] = {
      asRep[sigma.BigInt](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("decodeNbits", classOf[Sym]),
        Array[AnyRef](l),
        true, true, element[sigma.BigInt]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit final def unrefSigmaDslBuilder(p: Ref[SigmaDslBuilder]): SigmaDslBuilder = {
    if (p.node.isInstanceOf[SigmaDslBuilder]) p.node.asInstanceOf[SigmaDslBuilder]
    else
      SigmaDslBuilderAdapter(p)
  }

  // familyElem
  class SigmaDslBuilderElem[To <: SigmaDslBuilder]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = asLiftable[SSigmaDslBuilder, To](LiftableSigmaDslBuilder)

  }

  implicit lazy val sigmaDslBuilderElement: Elem[SigmaDslBuilder] =
    new SigmaDslBuilderElem[SigmaDslBuilder]

  object SigmaDslBuilderMethods {
    object Colls {
      def unapply(d: Def[_]): Nullable[Ref[SigmaDslBuilder]] = d match {
        case MethodCall(receiver, LegacyCallee(method), _, _) if method.getName == "Colls" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[SigmaDslBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[SigmaDslBuilder]] = unapply(exp.node)
    }

    object atLeast {
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[Int], Ref[sigma.Coll[sigma.SigmaProp]])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "atLeast" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[Int], Ref[sigma.Coll[sigma.SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[Int], Ref[sigma.Coll[sigma.SigmaProp]])] = unapply(exp.node)
    }

    object allOf {
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Boolean]])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "allOf" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Boolean]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Boolean]])] = unapply(exp.node)
    }

    object allZK {
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[sigma.SigmaProp]])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "allZK" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[sigma.SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[sigma.SigmaProp]])] = unapply(exp.node)
    }

    object anyOf {
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Boolean]])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "anyOf" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Boolean]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Boolean]])] = unapply(exp.node)
    }

    object anyZK {
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[sigma.SigmaProp]])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "anyZK" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[sigma.SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[sigma.SigmaProp]])] = unapply(exp.node)
    }

    object xorOf {
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Boolean]])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "xorOf" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Boolean]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Boolean]])] = unapply(exp.node)
    }

    object sigmaProp {
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[Boolean])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "sigmaProp" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[Boolean])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[Boolean])] = unapply(exp.node)
    }

    object blake2b256 {
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Byte]])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "blake2b256" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Byte]])] = unapply(exp.node)
    }

    object sha256 {
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Byte]])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "sha256" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Byte]])] = unapply(exp.node)
    }

    object byteArrayToBigInt {
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Byte]])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "byteArrayToBigInt" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Byte]])] = unapply(exp.node)
    }

    object longToByteArray {
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[Long])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "longToByteArray" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[Long])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[Long])] = unapply(exp.node)
    }

    object byteArrayToLong {
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Byte]])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "byteArrayToLong" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Byte]])] = unapply(exp.node)
    }

    object proveDlog {
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[sigma.GroupElement])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "proveDlog" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[sigma.GroupElement])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[sigma.GroupElement])] = unapply(exp.node)
    }

    object proveDHTuple {
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[sigma.GroupElement], Ref[sigma.GroupElement], Ref[sigma.GroupElement], Ref[sigma.GroupElement])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "proveDHTuple" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[sigma.GroupElement], Ref[sigma.GroupElement], Ref[sigma.GroupElement], Ref[sigma.GroupElement])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[sigma.GroupElement], Ref[sigma.GroupElement], Ref[sigma.GroupElement], Ref[sigma.GroupElement])] = unapply(exp.node)
    }

    object substConstants {
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Byte]], Ref[sigma.Coll[Int]], Ref[sigma.Coll[T]]) forSome {type T}] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "substConstants" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Byte]], Ref[sigma.Coll[Int]], Ref[sigma.Coll[T]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Byte]], Ref[sigma.Coll[Int]], Ref[sigma.Coll[T]]) forSome {type T}] = unapply(exp.node)
    }

    object decodePoint {
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Byte]])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "decodePoint" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Byte]])] = unapply(exp.node)
    }

    object deserializeTo {
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Byte]], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "deserializeTo" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Byte]], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Byte]], Elem[T]) forSome {type T}] = unapply(exp.node)
    }

    object serialize {
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[Any])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "serialize" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[Any])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[Any])] = unapply(exp.node)
    }

    /** This is necessary to handle CreateAvlTree in GraphBuilding (v6.0) */
    object avlTree {
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[Byte], Ref[sigma.Coll[Byte]], Ref[Int], Ref[Option[Int]])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "avlTree" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[Byte], Ref[sigma.Coll[Byte]], Ref[Int], Ref[Option[Int]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[Byte], Ref[sigma.Coll[Byte]], Ref[Int], Ref[Option[Int]])] = unapply(exp.node)
    }

    object xor {
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Byte]], Ref[sigma.Coll[Byte]])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "xor" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Byte]], Ref[sigma.Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[sigma.Coll[Byte]], Ref[sigma.Coll[Byte]])] = unapply(exp.node)
    }
  }
} // of object SigmaDslBuilder
  registerEntityObject("SigmaDslBuilder", SigmaDslBuilder)
}

}

trait SigmaDslModule extends SigmaDslDefs {self: IRContext =>}
