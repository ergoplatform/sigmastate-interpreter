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


object BigInt extends EntityObject("BigInt") {
  // entityConst: single const for each entity
  import Liftables._
  type SBigInt = sigma.BigInt
  case class BigIntConst(
        constValue: SBigInt
      ) extends LiftedConst[SBigInt, BigInt] with BigInt
        with Def[BigInt] with BigIntConstMethods {
    val liftable: Liftable[SBigInt, BigInt] = LiftableBigInt
    val resultType: Elem[BigInt] = liftable.eW
  }

  trait BigIntConstMethods extends BigInt  { thisConst: Def[_] =>

    private val BigIntClass = RClass(classOf[BigInt])

    override def add(that: Ref[BigInt]): Ref[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("add", classOf[Sym]),
        Array[AnyRef](that),
        true, false, element[BigInt]))
    }

    override def subtract(that: Ref[BigInt]): Ref[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("subtract", classOf[Sym]),
        Array[AnyRef](that),
        true, false, element[BigInt]))
    }

    override def multiply(that: Ref[BigInt]): Ref[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("multiply", classOf[Sym]),
        Array[AnyRef](that),
        true, false, element[BigInt]))
    }

    override def divide(that: Ref[BigInt]): Ref[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("divide", classOf[Sym]),
        Array[AnyRef](that),
        true, false, element[BigInt]))
    }

    override def mod(m: Ref[BigInt]): Ref[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("mod", classOf[Sym]),
        Array[AnyRef](m),
        true, false, element[BigInt]))
    }

    override def min(that: Ref[BigInt]): Ref[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("min", classOf[Sym]),
        Array[AnyRef](that),
        true, false, element[BigInt]))
    }

    override def max(that: Ref[BigInt]): Ref[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("max", classOf[Sym]),
        Array[AnyRef](that),
        true, false, element[BigInt]))
    }

    import UnsignedBigInt.unsignedBigIntElement

    override def toUnsigned(): Ref[UnsignedBigInt] = {
      asRep[UnsignedBigInt](mkMethodCall(self,
        BigIntClass.getMethod("toUnsigned"),
        Array[AnyRef](),
        true, false, element[UnsignedBigInt](unsignedBigIntElement)))
    }

    override def toUnsignedMod(m: Ref[UnsignedBigInt]): Ref[UnsignedBigInt] = {
      asRep[UnsignedBigInt](mkMethodCall(self,
        BigIntClass.getMethod("toUnsignedMod", classOf[Sym]),
        Array[AnyRef](m),
        true, false, element[UnsignedBigInt](unsignedBigIntElement)))
    }
  }

  implicit object LiftableBigInt
    extends Liftable[SBigInt, BigInt] {
    lazy val eW: Elem[BigInt] = bigIntElement
    lazy val sourceType: RType[SBigInt] = {
      RType[SBigInt]
    }
    def lift(x: SBigInt): Ref[BigInt] = BigIntConst(x)
  }

  private val BigIntClass = RClass(classOf[BigInt])

  // entityAdapter for BigInt trait
  case class BigIntAdapter(source: Ref[BigInt])
      extends Node with BigInt
      with Def[BigInt] {
    val resultType: Elem[BigInt] = element[BigInt]
    override def transform(t: Transformer) = BigIntAdapter(t(source))

    def add(that: Ref[BigInt]): Ref[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        BigIntClass.getMethod("add", classOf[Sym]),
        Array[AnyRef](that),
        true, true, element[BigInt]))
    }

    def subtract(that: Ref[BigInt]): Ref[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        BigIntClass.getMethod("subtract", classOf[Sym]),
        Array[AnyRef](that),
        true, true, element[BigInt]))
    }

    def multiply(that: Ref[BigInt]): Ref[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        BigIntClass.getMethod("multiply", classOf[Sym]),
        Array[AnyRef](that),
        true, true, element[BigInt]))
    }

    def divide(that: Ref[BigInt]): Ref[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        BigIntClass.getMethod("divide", classOf[Sym]),
        Array[AnyRef](that),
        true, true, element[BigInt]))
    }

    def mod(m: Ref[BigInt]): Ref[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        BigIntClass.getMethod("mod", classOf[Sym]),
        Array[AnyRef](m),
        true, true, element[BigInt]))
    }

    def min(that: Ref[BigInt]): Ref[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        BigIntClass.getMethod("min", classOf[Sym]),
        Array[AnyRef](that),
        true, true, element[BigInt]))
    }

    def max(that: Ref[BigInt]): Ref[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        BigIntClass.getMethod("max", classOf[Sym]),
        Array[AnyRef](that),
        true, true, element[BigInt]))
    }

    import UnsignedBigInt.unsignedBigIntElement

    def toUnsigned(): Ref[UnsignedBigInt] = {
      asRep[UnsignedBigInt](mkMethodCall(source,
        BigIntClass.getMethod("toUnsigned"),
        Array[AnyRef](),
        true, true, element[UnsignedBigInt](unsignedBigIntElement)))
    }

    def toUnsignedMod(that: Ref[UnsignedBigInt]): Ref[UnsignedBigInt] = {
      asRep[UnsignedBigInt](mkMethodCall(source,
        BigIntClass.getMethod("toUnsignedMod", classOf[Sym]),
        Array[AnyRef](that),
        true, true, element[UnsignedBigInt](unsignedBigIntElement)))
    }
  }

  // entityUnref: single unref method for each type family
  implicit final def unrefBigInt(p: Ref[BigInt]): BigInt = {
    if (p.node.isInstanceOf[BigInt]) p.node.asInstanceOf[BigInt]
    else
      BigIntAdapter(p)
  }

  // familyElem
  class BigIntElem[To <: BigInt]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = asLiftable[SBigInt, To](LiftableBigInt)

  }

  implicit lazy val bigIntElement: Elem[BigInt] =
    new BigIntElem[BigInt]

  object BigIntMethods {

    object add {
      def unapply(d: Def[_]): Nullable[(Ref[BigInt], Ref[BigInt])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "add" && receiver.elem.isInstanceOf[BigIntElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[BigInt], Ref[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[BigInt], Ref[BigInt])] = unapply(exp.node)
    }

    object subtract {
      def unapply(d: Def[_]): Nullable[(Ref[BigInt], Ref[BigInt])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "subtract" && receiver.elem.isInstanceOf[BigIntElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[BigInt], Ref[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[BigInt], Ref[BigInt])] = unapply(exp.node)
    }

    object multiply {
      def unapply(d: Def[_]): Nullable[(Ref[BigInt], Ref[BigInt])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "multiply" && receiver.elem.isInstanceOf[BigIntElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[BigInt], Ref[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[BigInt], Ref[BigInt])] = unapply(exp.node)
    }

    object divide {
      def unapply(d: Def[_]): Nullable[(Ref[BigInt], Ref[BigInt])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "divide" && receiver.elem.isInstanceOf[BigIntElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[BigInt], Ref[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[BigInt], Ref[BigInt])] = unapply(exp.node)
    }

    object mod {
      def unapply(d: Def[_]): Nullable[(Ref[BigInt], Ref[BigInt])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "mod" && receiver.elem.isInstanceOf[BigIntElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[BigInt], Ref[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[BigInt], Ref[BigInt])] = unapply(exp.node)
    }

    object min {
      def unapply(d: Def[_]): Nullable[(Ref[BigInt], Ref[BigInt])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "min" && receiver.elem.isInstanceOf[BigIntElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[BigInt], Ref[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[BigInt], Ref[BigInt])] = unapply(exp.node)
    }

    object max {
      def unapply(d: Def[_]): Nullable[(Ref[BigInt], Ref[BigInt])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "max" && receiver.elem.isInstanceOf[BigIntElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[BigInt], Ref[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[BigInt], Ref[BigInt])] = unapply(exp.node)
    }

  }

} // of object BigInt
  registerEntityObject("BigInt", BigInt)

object UnsignedBigInt extends EntityObject("UnsignedBigInt") {
  import Liftables._

  type SUnsignedBigInt = sigma.UnsignedBigInt
  unsignedBigIntElement

  case class UnsignedBigIntConst(constValue: SUnsignedBigInt)
      extends LiftedConst[SUnsignedBigInt, UnsignedBigInt] with UnsignedBigInt
        with Def[UnsignedBigInt] with UnsignedBigIntConstMethods {
    val liftable: Liftable[SUnsignedBigInt, UnsignedBigInt] = LiftableUnsignedBigInt
    val resultType: Elem[UnsignedBigInt] = liftable.eW
  }

  trait UnsignedBigIntConstMethods extends UnsignedBigInt  { thisConst: Def[_] =>

    private val UnsignedBigIntClass = RClass(classOf[UnsignedBigInt])

    override def add(that: Ref[UnsignedBigInt]): Ref[UnsignedBigInt] = {
      asRep[UnsignedBigInt](mkMethodCall(self,
        UnsignedBigIntClass.getMethod("add", classOf[Sym]),
        Array[AnyRef](that),
        true, false, element[UnsignedBigInt]))
    }

    override def subtract(that: Ref[UnsignedBigInt]): Ref[UnsignedBigInt] = {
      asRep[UnsignedBigInt](mkMethodCall(self,
        UnsignedBigIntClass.getMethod("subtract", classOf[Sym]),
        Array[AnyRef](that),
        true, false, element[UnsignedBigInt]))
    }

    override def multiply(that: Ref[UnsignedBigInt]): Ref[UnsignedBigInt] = {
      asRep[UnsignedBigInt](mkMethodCall(self,
        UnsignedBigIntClass.getMethod("multiply", classOf[Sym]),
        Array[AnyRef](that),
        true, false, element[UnsignedBigInt]))
    }

    override def divide(that: Ref[UnsignedBigInt]): Ref[UnsignedBigInt] = {
      asRep[UnsignedBigInt](mkMethodCall(self,
        UnsignedBigIntClass.getMethod("divide", classOf[Sym]),
        Array[AnyRef](that),
        true, false, element[UnsignedBigInt]))
    }

    override def mod(m: Ref[UnsignedBigInt]): Ref[UnsignedBigInt] = {
      asRep[UnsignedBigInt](mkMethodCall(self,
        UnsignedBigIntClass.getMethod("mod", classOf[Sym]),
        Array[AnyRef](m),
        true, false, element[UnsignedBigInt]))
    }

    override def min(that: Ref[UnsignedBigInt]): Ref[UnsignedBigInt] = {
      asRep[UnsignedBigInt](mkMethodCall(self,
        UnsignedBigIntClass.getMethod("min", classOf[Sym]),
        Array[AnyRef](that),
        true, false, element[UnsignedBigInt]))
    }

    override def max(that: Ref[UnsignedBigInt]): Ref[UnsignedBigInt] = {
      asRep[UnsignedBigInt](mkMethodCall(self,
        UnsignedBigIntClass.getMethod("max", classOf[Sym]),
        Array[AnyRef](that),
        true, false, element[UnsignedBigInt]))
    }

    override def modInverse(m: Ref[UnsignedBigInt]): Ref[UnsignedBigInt] = {
      asRep[UnsignedBigInt](mkMethodCall(self,
        UnsignedBigIntClass.getMethod("modInverse", classOf[Sym]),
        Array[AnyRef](m),
        true, false, element[UnsignedBigInt]))
    }

    override def plusMod(that: Ref[UnsignedBigInt], m: Ref[UnsignedBigInt]): Ref[UnsignedBigInt] = {
      asRep[UnsignedBigInt](mkMethodCall(self,
        UnsignedBigIntClass.getMethod("plusMod", classOf[Sym], classOf[Sym]),
        Array[AnyRef](that, m),
        true, false, element[UnsignedBigInt]))
    }

    override def subtractMod(that: Ref[UnsignedBigInt], m: Ref[UnsignedBigInt]): Ref[UnsignedBigInt] = {
      asRep[UnsignedBigInt](mkMethodCall(self,
        UnsignedBigIntClass.getMethod("subtractMod", classOf[Sym], classOf[Sym]),
        Array[AnyRef](that, m),
        true, false, element[UnsignedBigInt]))
    }

    override def multiplyMod(that: Ref[UnsignedBigInt], m: Ref[UnsignedBigInt]): Ref[UnsignedBigInt] = {
      asRep[UnsignedBigInt](mkMethodCall(self,
        UnsignedBigIntClass.getMethod("multiplyMod", classOf[Sym], classOf[Sym]),
        Array[AnyRef](that, m),
        true, false, element[UnsignedBigInt]))
    }

    override def toSigned(): Ref[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        UnsignedBigIntClass.getMethod("toSigned"),
        Array[AnyRef](),
        true, false, element[BigInt]))
    }
  }

  implicit object LiftableUnsignedBigInt extends Liftable[SUnsignedBigInt, UnsignedBigInt] {
    lazy val eW: Elem[UnsignedBigInt] = unsignedBigIntElement
    lazy val sourceType: RType[SUnsignedBigInt] = {
      RType[SUnsignedBigInt]
    }

    def lift(x: SUnsignedBigInt): Ref[UnsignedBigInt] = UnsignedBigIntConst(x)
  }

  private val UnsignedBigIntClass = RClass(classOf[UnsignedBigInt])

  // entityAdapter for BigInt trait
  case class UnsignedBigIntAdapter(source: Ref[UnsignedBigInt])
    extends Node with UnsignedBigInt
      with Def[UnsignedBigInt] {
    val resultType: Elem[UnsignedBigInt] = element[UnsignedBigInt]

    override def transform(t: Transformer) = UnsignedBigIntAdapter(t(source))

    def add(that: Ref[UnsignedBigInt]): Ref[UnsignedBigInt] = {
      asRep[UnsignedBigInt](mkMethodCall(source,
        UnsignedBigIntClass.getMethod("add", classOf[Sym]),
        Array[AnyRef](that),
        true, true, element[UnsignedBigInt]))
    }

    def subtract(that: Ref[UnsignedBigInt]): Ref[UnsignedBigInt] = {
      asRep[UnsignedBigInt](mkMethodCall(source,
        UnsignedBigIntClass.getMethod("subtract", classOf[Sym]),
        Array[AnyRef](that),
        true, true, element[UnsignedBigInt]))
    }

    def multiply(that: Ref[UnsignedBigInt]): Ref[UnsignedBigInt] = {
      asRep[UnsignedBigInt](mkMethodCall(source,
        UnsignedBigIntClass.getMethod("multiply", classOf[Sym]),
        Array[AnyRef](that),
        true, true, element[UnsignedBigInt]))
    }

    def divide(that: Ref[UnsignedBigInt]): Ref[UnsignedBigInt] = {
      asRep[UnsignedBigInt](mkMethodCall(source,
        UnsignedBigIntClass.getMethod("divide", classOf[Sym]),
        Array[AnyRef](that),
        true, true, element[UnsignedBigInt]))
    }

    def mod(m: Ref[UnsignedBigInt]): Ref[UnsignedBigInt] = {
      asRep[UnsignedBigInt](mkMethodCall(source,
        UnsignedBigIntClass.getMethod("mod", classOf[Sym]),
        Array[AnyRef](m),
        true, true, element[UnsignedBigInt]))
    }

    def min(that: Ref[UnsignedBigInt]): Ref[UnsignedBigInt] = {
      asRep[UnsignedBigInt](mkMethodCall(source,
        UnsignedBigIntClass.getMethod("min", classOf[Sym]),
        Array[AnyRef](that),
        true, true, element[UnsignedBigInt]))
    }

    def max(that: Ref[UnsignedBigInt]): Ref[UnsignedBigInt] = {
      asRep[UnsignedBigInt](mkMethodCall(source,
        UnsignedBigIntClass.getMethod("max", classOf[Sym]),
        Array[AnyRef](that),
        true, true, element[UnsignedBigInt]))
    }

    def modInverse(m: Ref[UnsignedBigInt]): Ref[UnsignedBigInt] = {
      asRep[UnsignedBigInt](mkMethodCall(source,
        UnsignedBigIntClass.getMethod("modInverse", classOf[Sym]),
        Array[AnyRef](m),
        true, true, element[UnsignedBigInt]))
    }

    def plusMod(that: Ref[UnsignedBigInt], m: Ref[UnsignedBigInt]): Ref[UnsignedBigInt] = {
      asRep[UnsignedBigInt](mkMethodCall(source,
        UnsignedBigIntClass.getMethod("plusMod", classOf[Sym], classOf[Sym]),
        Array[AnyRef](that, m),
        true, true, element[UnsignedBigInt]))
    }

    def subtractMod(that: Ref[UnsignedBigInt], m: Ref[UnsignedBigInt]): Ref[UnsignedBigInt] = {
      asRep[UnsignedBigInt](mkMethodCall(source,
        UnsignedBigIntClass.getMethod("subtractMod", classOf[Sym], classOf[Sym]),
        Array[AnyRef](that, m),
        true, true, element[UnsignedBigInt]))
    }

    def multiplyMod(that: Ref[UnsignedBigInt], m: Ref[UnsignedBigInt]): Ref[UnsignedBigInt] = {
      asRep[UnsignedBigInt](mkMethodCall(source,
        UnsignedBigIntClass.getMethod("multiplyMod", classOf[Sym], classOf[Sym]),
        Array[AnyRef](that, m),
        true, true, element[UnsignedBigInt]))
    }

    def toSigned(): Ref[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        UnsignedBigIntClass.getMethod("toSigned"),
        Array[AnyRef](),
        true, true, element[BigInt]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit final def unrefUnsignedBigInt(p: Ref[UnsignedBigInt]): UnsignedBigInt = {
    if (p.node.isInstanceOf[UnsignedBigInt]) p.node.asInstanceOf[UnsignedBigInt]
    else
      UnsignedBigIntAdapter(p)
  }

  class UnsignedBigIntElem[To <: UnsignedBigInt]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = asLiftable[SUnsignedBigInt, To](LiftableUnsignedBigInt)

  }

  implicit lazy val unsignedBigIntElement: Elem[UnsignedBigInt] = new UnsignedBigIntElem[UnsignedBigInt]
}   // of object BigInt
    registerEntityObject("UnsignedBigInt", UnsignedBigInt)


object SigmaProp extends EntityObject("SigmaProp") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SSigmaProp = sigma.SigmaProp
  case class SigmaPropConst(
        constValue: SSigmaProp
      ) extends LiftedConst[SSigmaProp, SigmaProp] with SigmaProp
        with Def[SigmaProp] with SigmaPropConstMethods {
    val liftable: Liftable[SSigmaProp, SigmaProp] = LiftableSigmaProp
    val resultType: Elem[SigmaProp] = liftable.eW
  }

  trait SigmaPropConstMethods extends SigmaProp  { thisConst: Def[_] =>

    private val SigmaPropClass = RClass(classOf[SigmaProp])

    override def isValid: Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        SigmaPropClass.getMethod("isValid"),
        ArraySeq.empty,
        true, false, element[Boolean]))
    }

    override def propBytes: Ref[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        SigmaPropClass.getMethod("propBytes"),
        ArraySeq.empty,
        true, false, element[Coll[Byte]]))
    }

    override def &&(other: Ref[SigmaProp]): Ref[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        SigmaPropClass.getMethod("$amp$amp", classOf[Sym]),
        Array[AnyRef](other),
        true, false, element[SigmaProp]))
    }

    override def ||(other: Ref[SigmaProp]): Ref[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        SigmaPropClass.getMethod("$bar$bar", classOf[Sym]),
        Array[AnyRef](other),
        true, false, element[SigmaProp]))
    }
  }

  implicit object LiftableSigmaProp
    extends Liftable[SSigmaProp, SigmaProp] {
    lazy val eW: Elem[SigmaProp] = sigmaPropElement
    lazy val sourceType: RType[SSigmaProp] = {
      RType[SSigmaProp]
    }
    def lift(x: SSigmaProp): Ref[SigmaProp] = SigmaPropConst(x)
  }

  private val SigmaPropClass = RClass(classOf[SigmaProp])

  // entityAdapter for SigmaProp trait
  case class SigmaPropAdapter(source: Ref[SigmaProp])
      extends Node with SigmaProp
      with Def[SigmaProp] {
    val resultType: Elem[SigmaProp] = element[SigmaProp]
    override def transform(t: Transformer) = SigmaPropAdapter(t(source))

    def isValid: Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        SigmaPropClass.getMethod("isValid"),
        ArraySeq.empty,
        true, true, element[Boolean]))
    }

    def propBytes: Ref[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        SigmaPropClass.getMethod("propBytes"),
        ArraySeq.empty,
        true, true, element[Coll[Byte]]))
    }

    def &&(other: Ref[SigmaProp]): Ref[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        SigmaPropClass.getMethod("$amp$amp", classOf[Sym]),
        Array[AnyRef](other),
        true, true, element[SigmaProp]))
    }

    def ||(other: Ref[SigmaProp]): Ref[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        SigmaPropClass.getMethod("$bar$bar", classOf[Sym]),
        Array[AnyRef](other),
        true, true, element[SigmaProp]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit final def unrefSigmaProp(p: Ref[SigmaProp]): SigmaProp = {
    if (p.node.isInstanceOf[SigmaProp]) p.node.asInstanceOf[SigmaProp]
    else
      SigmaPropAdapter(p)
  }

  // familyElem
  class SigmaPropElem[To <: SigmaProp]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = asLiftable[SSigmaProp, To](LiftableSigmaProp)

  }

  implicit lazy val sigmaPropElement: Elem[SigmaProp] =
    new SigmaPropElem[SigmaProp]

  object SigmaPropMethods {
    object isValid {
      def unapply(d: Def[_]): Nullable[Ref[SigmaProp]] = d match {
        case MethodCall(receiver, LegacyCallee(method), _, _) if method.getName == "isValid" && receiver.elem.isInstanceOf[SigmaPropElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[SigmaProp]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[SigmaProp]] = unapply(exp.node)
    }

    object propBytes {
      def unapply(d: Def[_]): Nullable[Ref[SigmaProp]] = d match {
        case MethodCall(receiver, LegacyCallee(method), _, _) if method.getName == "propBytes" && receiver.elem.isInstanceOf[SigmaPropElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[SigmaProp]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[SigmaProp]] = unapply(exp.node)
    }

    object and_sigma_&& {
      def unapply(d: Def[_]): Nullable[(Ref[SigmaProp], Ref[SigmaProp])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "$amp$amp" && receiver.elem.isInstanceOf[SigmaPropElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaProp], Ref[SigmaProp])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaProp], Ref[SigmaProp])] = unapply(exp.node)
    }

    object or_sigma_|| {
      def unapply(d: Def[_]): Nullable[(Ref[SigmaProp], Ref[SigmaProp])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "$bar$bar" && receiver.elem.isInstanceOf[SigmaPropElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaProp], Ref[SigmaProp])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaProp], Ref[SigmaProp])] = unapply(exp.node)
    }
  }
} // of object SigmaProp
  registerEntityObject("SigmaProp", SigmaProp)





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

    override def Colls: Ref[CollBuilder] = {
      asRep[CollBuilder](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("Colls"),
        ArraySeq.empty,
        true, false, element[CollBuilder]))
    }

    override def atLeast(bound: Ref[Int], props: Ref[Coll[SigmaProp]]): Ref[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("atLeast", classOf[Sym], classOf[Sym]),
        Array[AnyRef](bound, props),
        true, false, element[SigmaProp]))
    }

    override def allOf(conditions: Ref[Coll[Boolean]]): Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("allOf", classOf[Sym]),
        Array[AnyRef](conditions),
        true, false, element[Boolean]))
    }

    override def allZK(conditions: Ref[Coll[SigmaProp]]): Ref[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("allZK", classOf[Sym]),
        Array[AnyRef](conditions),
        true, false, element[SigmaProp]))
    }

    override def anyOf(conditions: Ref[Coll[Boolean]]): Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("anyOf", classOf[Sym]),
        Array[AnyRef](conditions),
        true, false, element[Boolean]))
    }

    override def anyZK(conditions: Ref[Coll[SigmaProp]]): Ref[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("anyZK", classOf[Sym]),
        Array[AnyRef](conditions),
        true, false, element[SigmaProp]))
    }

    override def xorOf(conditions: Ref[Coll[Boolean]]): Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("xorOf", classOf[Sym]),
        Array[AnyRef](conditions),
        true, false, element[Boolean]))
    }

    override def sigmaProp(b: Ref[Boolean]): Ref[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("sigmaProp", classOf[Sym]),
        Array[AnyRef](b),
        true, false, element[SigmaProp]))
    }

    override def blake2b256(bytes: Ref[Coll[Byte]]): Ref[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("blake2b256", classOf[Sym]),
        Array[AnyRef](bytes),
        true, false, element[Coll[Byte]]))
    }

    override def sha256(bytes: Ref[Coll[Byte]]): Ref[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("sha256", classOf[Sym]),
        Array[AnyRef](bytes),
        true, false, element[Coll[Byte]]))
    }

    override def byteArrayToBigInt(bytes: Ref[Coll[Byte]]): Ref[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("byteArrayToBigInt", classOf[Sym]),
        Array[AnyRef](bytes),
        true, false, element[BigInt]))
    }

    override def longToByteArray(l: Ref[Long]): Ref[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("longToByteArray", classOf[Sym]),
        Array[AnyRef](l),
        true, false, element[Coll[Byte]]))
    }

    override def byteArrayToLong(bytes: Ref[Coll[Byte]]): Ref[Long] = {
      asRep[Long](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("byteArrayToLong", classOf[Sym]),
        Array[AnyRef](bytes),
        true, false, element[Long]))
    }

    override def proveDlog(g: Ref[sigma.GroupElement]): Ref[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("proveDlog", classOf[Sym]),
        Array[AnyRef](g),
        true, false, element[SigmaProp]))
    }

    override def proveDHTuple(g: Ref[sigma.GroupElement], h: Ref[sigma.GroupElement], u: Ref[sigma.GroupElement], v: Ref[sigma.GroupElement]): Ref[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("proveDHTuple", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](g, h, u, v),
        true, false, element[SigmaProp]))
    }

    override def groupGenerator: Ref[sigma.GroupElement] = {
      asRep[sigma.GroupElement](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("groupGenerator"),
        ArraySeq.empty,
        true, false, element[sigma.GroupElement]))
    }

    override def substConstants[T](scriptBytes: Ref[Coll[Byte]], positions: Ref[Coll[Int]], newValues: Ref[Coll[T]]): Ref[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("substConstants", classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](scriptBytes, positions, newValues),
        true, false, element[Coll[Byte]]))
    }

    override def decodePoint(encoded: Ref[Coll[Byte]]): Ref[sigma.GroupElement] = {
      asRep[sigma.GroupElement](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("decodePoint", classOf[Sym]),
        Array[AnyRef](encoded),
        true, false, element[sigma.GroupElement]))
    }

    override def avlTree(operationFlags: Ref[Byte], digest: Ref[Coll[Byte]], keyLength: Ref[Int], valueLengthOpt: Ref[Option[Int]]): Ref[sigma.AvlTree] = {
      asRep[sigma.AvlTree](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("avlTree", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](operationFlags, digest, keyLength, valueLengthOpt),
        true, false, element[sigma.AvlTree]))
    }

    override def xor(l: Ref[Coll[Byte]], r: Ref[Coll[Byte]]): Ref[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("xor", classOf[Sym], classOf[Sym]),
        Array[AnyRef](l, r),
        true, false, element[Coll[Byte]]))
    }

    def serialize[T](value: Ref[T]): Ref[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("serialize", classOf[Sym]),
        Array[AnyRef](value),
        true, false, element[Coll[Byte]]))
    }

    override def deserializeTo[T](l: Ref[Coll[Byte]])(implicit cT: Elem[T]): Ref[T] = {
      asRep[T](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("deserializeTo", classOf[Sym], classOf[Elem[T]]),
        Array[AnyRef](l, cT),
        true, false, element[T](cT), Map(tT -> elemToSType(cT))))
    }
    override def fromBigEndianBytes[T](bytes: Ref[Coll[Byte]])(implicit cT: Elem[T]): Ref[T] = {
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


    override def powHit(k: Ref[Int], msg: Ref[Coll[Byte]], nonce: Ref[Coll[Byte]], h: Ref[Coll[Byte]], N: Ref[Int]): Ref[UnsignedBigInt] = {
      asRep[UnsignedBigInt](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("powHit", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](k, msg, nonce, h, N),
        true, false, element[UnsignedBigInt](UnsignedBigInt.unsignedBigIntElement)))
    }

    override def encodeNbits(bi: Ref[BigInt]): Ref[Long] = {
      asRep[Long](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("encodeNbits", classOf[Sym]),
        Array[AnyRef](bi),
        true, false, element[Long]))
    }

    override def decodeNbits(l: Ref[Long]): Ref[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("decodeNbits", classOf[Sym]),
        Array[AnyRef](l),
        true, false, element[BigInt]))
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

    def Colls: Ref[CollBuilder] = {
      asRep[CollBuilder](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("Colls"),
        ArraySeq.empty,
        true, true, element[CollBuilder]))
    }

    def atLeast(bound: Ref[Int], props: Ref[Coll[SigmaProp]]): Ref[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("atLeast", classOf[Sym], classOf[Sym]),
        Array[AnyRef](bound, props),
        true, true, element[SigmaProp]))
    }

    def allOf(conditions: Ref[Coll[Boolean]]): Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("allOf", classOf[Sym]),
        Array[AnyRef](conditions),
        true, true, element[Boolean]))
    }

    def allZK(conditions: Ref[Coll[SigmaProp]]): Ref[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("allZK", classOf[Sym]),
        Array[AnyRef](conditions),
        true, true, element[SigmaProp]))
    }

    def anyOf(conditions: Ref[Coll[Boolean]]): Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("anyOf", classOf[Sym]),
        Array[AnyRef](conditions),
        true, true, element[Boolean]))
    }

    def anyZK(conditions: Ref[Coll[SigmaProp]]): Ref[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("anyZK", classOf[Sym]),
        Array[AnyRef](conditions),
        true, true, element[SigmaProp]))
    }

    def xorOf(conditions: Ref[Coll[Boolean]]): Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("xorOf", classOf[Sym]),
        Array[AnyRef](conditions),
        true, true, element[Boolean]))
    }

    def sigmaProp(b: Ref[Boolean]): Ref[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("sigmaProp", classOf[Sym]),
        Array[AnyRef](b),
        true, true, element[SigmaProp]))
    }

    def blake2b256(bytes: Ref[Coll[Byte]]): Ref[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("blake2b256", classOf[Sym]),
        Array[AnyRef](bytes),
        true, true, element[Coll[Byte]]))
    }

    def sha256(bytes: Ref[Coll[Byte]]): Ref[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("sha256", classOf[Sym]),
        Array[AnyRef](bytes),
        true, true, element[Coll[Byte]]))
    }

    def byteArrayToBigInt(bytes: Ref[Coll[Byte]]): Ref[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("byteArrayToBigInt", classOf[Sym]),
        Array[AnyRef](bytes),
        true, true, element[BigInt]))
    }

    def longToByteArray(l: Ref[Long]): Ref[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("longToByteArray", classOf[Sym]),
        Array[AnyRef](l),
        true, true, element[Coll[Byte]]))
    }

    def byteArrayToLong(bytes: Ref[Coll[Byte]]): Ref[Long] = {
      asRep[Long](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("byteArrayToLong", classOf[Sym]),
        Array[AnyRef](bytes),
        true, true, element[Long]))
    }

    def proveDlog(g: Ref[sigma.GroupElement]): Ref[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("proveDlog", classOf[Sym]),
        Array[AnyRef](g),
        true, true, element[SigmaProp]))
    }

    def proveDHTuple(g: Ref[sigma.GroupElement], h: Ref[sigma.GroupElement], u: Ref[sigma.GroupElement], v: Ref[sigma.GroupElement]): Ref[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("proveDHTuple", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](g, h, u, v),
        true, true, element[SigmaProp]))
    }

    def groupGenerator: Ref[sigma.GroupElement] = {
      asRep[sigma.GroupElement](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("groupGenerator"),
        ArraySeq.empty,
        true, true, element[sigma.GroupElement]))
    }

    def substConstants[T](scriptBytes: Ref[Coll[Byte]], positions: Ref[Coll[Int]], newValues: Ref[Coll[T]]): Ref[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("substConstants", classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](scriptBytes, positions, newValues),
        true, true, element[Coll[Byte]]))
    }

    def decodePoint(encoded: Ref[Coll[Byte]]): Ref[sigma.GroupElement] = {
      asRep[sigma.GroupElement](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("decodePoint", classOf[Sym]),
        Array[AnyRef](encoded),
        true, true, element[sigma.GroupElement]))
    }

    def avlTree(operationFlags: Ref[Byte], digest: Ref[Coll[Byte]], keyLength: Ref[Int], valueLengthOpt: Ref[Option[Int]]): Ref[sigma.AvlTree] = {
      asRep[sigma.AvlTree](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("avlTree", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](operationFlags, digest, keyLength, valueLengthOpt),
        true, true, element[sigma.AvlTree]))
    }

    def xor(l: Ref[Coll[Byte]], r: Ref[Coll[Byte]]): Ref[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("xor", classOf[Sym], classOf[Sym]),
        Array[AnyRef](l, r),
        true, true, element[Coll[Byte]]))
    }

    def powHit(k: Ref[Int], msg: Ref[Coll[Byte]], nonce: Ref[Coll[Byte]], h: Ref[Coll[Byte]], N: Ref[Int]): Ref[UnsignedBigInt] = {
      asRep[UnsignedBigInt](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("powHit", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](k, msg, nonce, h, N),
        true, true, element[UnsignedBigInt](UnsignedBigInt.unsignedBigIntElement)))
    }

    def serialize[T](value: Ref[T]): Ref[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("serialize", classOf[Sym]),
        Array[AnyRef](value),
        true, true, element[Coll[Byte]]))
    }

    def deserializeTo[T](bytes: Ref[Coll[Byte]])(implicit cT: Elem[T]): Ref[T] = {
      asRep[T](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("deserializeTo", classOf[Sym], classOf[Elem[_]]),
        Array[AnyRef](bytes, cT),
        true, true, element[T](cT), Map(tT -> elemToSType(cT))))
    }

    def fromBigEndianBytes[T](bytes: Ref[Coll[Byte]])(implicit cT: Elem[T]): Ref[T] = {
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


    override def encodeNbits(bi: Ref[BigInt]): Ref[Long] = {
      asRep[Long](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("encodeNbits", classOf[Sym]),
        Array[AnyRef](bi),
        true, true, element[Long]))
    }

    override def decodeNbits(l: Ref[Long]): Ref[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("decodeNbits", classOf[Sym]),
        Array[AnyRef](l),
        true, true, element[BigInt]))
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
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[Int], Ref[Coll[SigmaProp]])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "atLeast" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[Int], Ref[Coll[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[Int], Ref[Coll[SigmaProp]])] = unapply(exp.node)
    }

    object allOf {
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Boolean]])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "allOf" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Boolean]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Boolean]])] = unapply(exp.node)
    }

    object allZK {
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[Coll[SigmaProp]])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "allZK" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[Coll[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[Coll[SigmaProp]])] = unapply(exp.node)
    }

    object anyOf {
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Boolean]])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "anyOf" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Boolean]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Boolean]])] = unapply(exp.node)
    }

    object anyZK {
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[Coll[SigmaProp]])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "anyZK" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[Coll[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[Coll[SigmaProp]])] = unapply(exp.node)
    }

    object xorOf {
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Boolean]])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "xorOf" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Boolean]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Boolean]])] = unapply(exp.node)
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
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Byte]])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "blake2b256" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Byte]])] = unapply(exp.node)
    }

    object sha256 {
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Byte]])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "sha256" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Byte]])] = unapply(exp.node)
    }

    object byteArrayToBigInt {
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Byte]])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "byteArrayToBigInt" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Byte]])] = unapply(exp.node)
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
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Byte]])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "byteArrayToLong" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Byte]])] = unapply(exp.node)
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
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Byte]], Ref[Coll[Int]], Ref[Coll[T]]) forSome {type T}] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "substConstants" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Byte]], Ref[Coll[Int]], Ref[Coll[T]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Byte]], Ref[Coll[Int]], Ref[Coll[T]]) forSome {type T}] = unapply(exp.node)
    }

    object decodePoint {
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Byte]])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "decodePoint" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Byte]])] = unapply(exp.node)
    }

    object deserializeTo {
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Byte]], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "deserializeTo" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Byte]], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Byte]], Elem[T]) forSome {type T}] = unapply(exp.node)
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
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[Byte], Ref[Coll[Byte]], Ref[Int], Ref[Option[Int]])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "avlTree" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[Byte], Ref[Coll[Byte]], Ref[Int], Ref[Option[Int]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[Byte], Ref[Coll[Byte]], Ref[Int], Ref[Option[Int]])] = unapply(exp.node)
    }

    object xor {
      def unapply(d: Def[_]): Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Byte]], Ref[Coll[Byte]])] = d match {
        case MethodCall(receiver, LegacyCallee(method), args, _) if method.getName == "xor" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Byte]], Ref[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SigmaDslBuilder], Ref[Coll[Byte]], Ref[Coll[Byte]])] = unapply(exp.node)
    }
  }
} // of object SigmaDslBuilder
  registerEntityObject("SigmaDslBuilder", SigmaDslBuilder)
}

}

trait SigmaDslModule extends SigmaDslDefs {self: IRContext =>}
