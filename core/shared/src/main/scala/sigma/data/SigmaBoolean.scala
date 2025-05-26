package sigma.data

import debox.cfor
import sigma.crypto.EcPointType
import sigma.data.SigmaPropCodes.{AndCode, AtLeastCode, OrCode, ProveDiffieHellmanTupleCode, ProveDlogCode, SPCode}
import sigma.data.TrivialProp.{FalseProp, TrueProp}
import sigma.serialization.{CoreByteReader, CoreByteWriter, CoreSerializer, GroupElementSerializer, ProveDHTupleSerializer, ProveDlogSerializer}
import sigma.util.safeNewArray

import scala.collection.mutable.ArrayBuffer

/** Algebraic data type of sigma proposition expressions.
  * Values of this type are used as values of SigmaProp type of SigmaScript and SigmaDsl
  */
sealed trait SigmaBoolean {
  /** Unique id of the node class used in serialization of SigmaBoolean. */
  val opCode: SPCode

  /** Size of the proposition tree (number of nodes). */
  def size: Int
}

object SigmaBoolean {
  /** Compute total size of the trees in the collection of children. */
  def totalSize(children: Seq[SigmaBoolean]): Int = {
    var res = 0
    val len = children.length
    cfor(0)(_ < len, _ + 1) { i =>
      res += children(i).size
    }
    res
  }

  /** HOTSPOT: don't beautify this code */
  object serializer extends CoreSerializer[SigmaBoolean, SigmaBoolean] {
    val dhtSerializer  = ProveDHTupleSerializer(ProveDHTuple.apply)

    val dlogSerializer = ProveDlogSerializer(ProveDlog.apply)

    override def serialize(data: SigmaBoolean, w: CoreByteWriter): Unit = {
      w.put(data.opCode)
      data match {
        case dlog: ProveDlog => dlogSerializer.serialize(dlog, w)
        case dht: ProveDHTuple => dhtSerializer.serialize(dht, w)
        case _: TrivialProp => // besides opCode no additional bytes
        case and: CAND =>
          val nChildren = and.children.length
          w.putUShort(nChildren)
          cfor(0)(_ < nChildren, _ + 1) { i =>
            val c = and.children(i)
            serializer.serialize(c, w)
          }
        case or: COR =>
          val nChildren = or.children.length
          w.putUShort(nChildren)
          cfor(0)(_ < nChildren, _ + 1) { i =>
            val c = or.children(i)
            serializer.serialize(c, w)
          }
        case th: CTHRESHOLD =>
          w.putUShort(th.k)
          val nChildren = th.children.length
          w.putUShort(nChildren)
          cfor(0)(_ < nChildren, _ + 1) { i =>
            val c = th.children(i)
            serializer.serialize(c, w)
          }
      }
    }

    override def parse(r: CoreByteReader): SigmaBoolean = {
      val depth = r.level
      r.level = depth + 1
      val opCode = r.getByte()
      val res    = opCode match {
        case FalseProp.opCode => FalseProp
        case TrueProp.opCode => TrueProp
        case ProveDlogCode => dlogSerializer.parse(r)
        case ProveDiffieHellmanTupleCode => dhtSerializer.parse(r)
        case AndCode =>
          val n        = r.getUShort()
          val children = safeNewArray[SigmaBoolean](n)
          cfor(0)(_ < n, _ + 1) { i =>
            children(i) = serializer.parse(r)
          }
          CAND(children)
        case OrCode =>
          val n        = r.getUShort()
          val children = safeNewArray[SigmaBoolean](n)
          cfor(0)(_ < n, _ + 1) { i =>
            children(i) = serializer.parse(r)
          }
          COR(children)
        case AtLeastCode =>
          val k        = r.getUShort()
          val n        = r.getUShort()
          val children = safeNewArray[SigmaBoolean](n)
          cfor(0)(_ < n, _ + 1) { i =>
            children(i) = serializer.parse(r)
          }
          CTHRESHOLD(k, children)
      }
      r.level = r.level - 1
      res
    }
  }
}

/**
  * Basic trait for inner nodes of crypto-trees, so AND/OR/THRESHOLD sigma-protocol connectives
  */
trait SigmaConjecture extends SigmaBoolean {
  def children: Seq[SigmaBoolean]
}

/**
  * Basic trait for leafs of crypto-trees, such as
  * [[sigmastate.crypto.DLogProtocol.ProveDlog]] and [[sigmastate.crypto.ProveDHTuple]]
  * instances.
  * It plays the same role as [[SigmaConjecture]]. It used in prover to distinguish leafs from
  * other nodes and have logic common to leaves regardless of the concrete leaf type.
  */
trait SigmaLeaf extends SigmaBoolean

/** Construct a new SigmaBoolean value representing public key of discrete logarithm signature protocol. */
case class ProveDlog(value: EcPointType) extends SigmaLeaf {
  override def size: Int = 1
  override val opCode : SPCode = SigmaPropCodes.ProveDlogCode

  /** Serialized bytes of the elliptic curve point (using GroupElementSerializer). */
  lazy val pkBytes: Array[Byte] = GroupElementSerializer.toBytes(value)
}

/** Construct a new SigmaProp value representing public key of Diffie Hellman signature protocol.
  * Common input: (g,h,u,v) */
case class ProveDHTuple(gv: EcPointType, hv: EcPointType, uv: EcPointType, vv: EcPointType)
    extends SigmaLeaf {
  override val opCode: SPCode = SigmaPropCodes.ProveDiffieHellmanTupleCode
  override def size: Int = 4  // one node for each EcPoint
  lazy val g = gv
  lazy val h = hv
  lazy val u = uv
  lazy val v = vv
}

/**
  * AND conjunction for sigma propositions
  */
case class CAND(override val children: Seq[SigmaBoolean]) extends SigmaConjecture {
  /** The same code is used for AND operation, but they belong to different type hierarchies. */
  override val opCode: SPCode = SigmaPropCodes.AndCode
  override val size: Int = SigmaBoolean.totalSize(children) + 1
}

object CAND {
  import TrivialProp._

  /** Connects the given sigma propositions into CAND proposition performing
    * partial evaluation when some of them are trivial propositioins.
    *
    * @param items propositions to combine into CAND
    * @return CAND, TrueProp, FalseProp or even one of the items depending on partial evaluation
    */
  def normalized(items: Seq[SigmaBoolean]): SigmaBoolean = {
    require(items.nonEmpty)
    val res = new ArrayBuffer[SigmaBoolean]()
    val nItems = items.length
    cfor(0)(_ < nItems, _ + 1) { i =>
      val x = items(i)
      x match {
        case FalseProp => return FalseProp
        case TrueProp => // skip
        case _ => res += x
      }
    }
    if (res.isEmpty) TrueProp
    else if (res.length == 1) res(0)
    else CAND(res.toSeq)
  }
}

/**
  * OR disjunction for sigma propositions
  */
case class COR(children: Seq[SigmaBoolean]) extends SigmaConjecture {
  /** The same code is also used for OR operation, but they belong to different type hierarchies. */
  override val opCode: SPCode = SigmaPropCodes.OrCode
  override val size: Int = SigmaBoolean.totalSize(children) + 1
}

object COR {
  import TrivialProp._

  /** Connects the given sigma propositions into COR proposition performing
    * partial evaluation when some of them are trivial propositioins.
    *
    * @param items propositions to combine into COR
    * @return COR, TrueProp, FalseProp or even one of the items depending on partial evaluation
    */
  def normalized(items: Seq[SigmaBoolean]): SigmaBoolean = {
    require(items.nonEmpty)
    val res = new ArrayBuffer[SigmaBoolean]()
    val nItems = items.length
    cfor(0)(_ < nItems, _ + 1) { i =>
      val x = items(i)
      x match {
        case FalseProp => // skip
        case TrueProp => return TrueProp
        case _ => res += x
      }
    }
    if (res.isEmpty) FalseProp
    else if (res.length == 1) res(0)
    else COR(res.toSeq)
  }
}

/**
  * THRESHOLD connector for sigma propositions
  */
case class CTHRESHOLD(k: Int, children: Seq[SigmaBoolean]) extends SigmaConjecture {
  // Our polynomial arithmetic can take only byte inputs
  require(k >= 0 && k <= children.length && children.length <= 255)

  override val opCode: SPCode = SigmaPropCodes.AtLeastCode
  override val size: Int = SigmaBoolean.totalSize(children) + 1
}


/** Represents boolean values (true/false) in SigmaBoolean tree.
  * Participates in evaluation of CAND, COR, THRESHOLD connectives over SigmaBoolean values.
  * See CAND.normalized, COR.normalized and AtLeast.reduce. */
abstract class TrivialProp(val condition: Boolean) extends SigmaBoolean with Product1[Boolean] {
  override def _1: Boolean = condition
  override def canEqual(that: Any): Boolean = that != null && that.isInstanceOf[TrivialProp]
}
object TrivialProp {
  // NOTE: the corresponding unapply is missing because any implementation (even using Nullable)
  // will lead to Boolean boxing, which we want to avoid
  // So, instead of `case TrivialProp(b) => ... b ...` use more efficient
  // `case p: TrivialProp => ... p.condition ...

  def apply(b: Boolean): TrivialProp = if (b) TrueProp else FalseProp

  val FalseProp = new TrivialProp(false) {
    override val opCode: SPCode = SigmaPropCodes.TrivialPropFalseCode
    override def size: Int = 1
    override def toString = "FalseProp"
  }
  val TrueProp = new TrivialProp(true) {
    override val opCode: SPCode = SigmaPropCodes.TrivialPropTrueCode
    override def size: Int = 1
    override def toString = "TrueProp"
  }
}


