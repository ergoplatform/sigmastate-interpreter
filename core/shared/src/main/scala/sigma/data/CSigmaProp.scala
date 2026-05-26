package sigma.data

import sigma.ast.SSigmaProp
import sigma.serialization.CoreSerializer
import sigma.util.Extensions.SigmaBooleanOps
import sigma.{Coll, Colls, GroupElement, SigmaProp}

/** A default implementation of [[SigmaProp]] interface.
  *
  * @see [[SigmaProp]] for detailed descriptions
  */
case class CSigmaProp(sigmaTree: SigmaBoolean) extends SigmaProp with WrapperOf[SigmaBoolean] {
  override def wrappedValue: SigmaBoolean = sigmaTree

  // TODO refactor: remove this (it shouldn't be used in interpreter)
  override def isValid: Boolean = sigmaTree match {
    case p: TrivialProp => p.condition
    case _ => sys.error(s"Method CostingSigmaProp.isValid is not defined for $sigmaTree")
  }

  override def propBytes: Coll[Byte] = {
    // Always v0 header. Changing this would alter Fiat-Shamir for deployed contracts.
    val w = CoreSerializer.startWriter()
    w.put(0)
    w.putType(SSigmaProp)
    SigmaBoolean.serializer.serialize(wrappedValue, w)
    Colls.fromArray(w.toBytes)
  }

  override def propBytes(version: Byte): Coll[Byte] = {
    val w = CoreSerializer.startWriter()
    if (version == 0) {
      w.put(0)
      w.putType(SSigmaProp)
      SigmaBoolean.serializer.serialize(wrappedValue, w)
    } else {
      // ergoTree.bytes for v1+: header (SizeFlag | version), UInt size, content.
      // SizeFlag (0x08) duplicated here to avoid a core->data dep on ErgoTree.SizeFlag.
      val contentW = CoreSerializer.startWriter()
      contentW.putType(SSigmaProp)
      SigmaBoolean.serializer.serialize(wrappedValue, contentW)
      val content = contentW.toBytes
      val header = (0x08 | version).toByte
      w.put(header)
      w.putUInt(content.length)
      w.putBytes(content)
    }
    Colls.fromArray(w.toBytes)
  }

  override def &&(other: SigmaProp): SigmaProp = other match {
    case other: CSigmaProp =>
      CSigmaProp(CAND.normalized(Array(sigmaTree, other.sigmaTree)))
  }

  override def ||(other: SigmaProp): SigmaProp = other match {
    case other: CSigmaProp =>
      CSigmaProp(COR.normalized(Array(sigmaTree, other.sigmaTree)))
  }

  override def toString: String = s"SigmaProp(${wrappedValue.showToString})"
}

object CSigmaProp {
  /** Create trivial sigma proposition with given boolean value. */
  def apply(b: Boolean): CSigmaProp =
    CSigmaProp(TrivialProp(b))
    
  /** Create SigmaProp value with underlying ProveDlog proposition. */
  def withProveDlog(ge: GroupElement) =
    CSigmaProp(ProveDlog(ge.asInstanceOf[CGroupElement].wrappedValue))

  /** Create SigmaProp value with underlying ProveDHTuple proposition. */
  def withProveDHTuple(g: GroupElement, h: GroupElement, u: GroupElement, v: GroupElement) =
    CSigmaProp(ProveDHTuple(
      g.asInstanceOf[CGroupElement].wrappedValue,
      h.asInstanceOf[CGroupElement].wrappedValue,
      u.asInstanceOf[CGroupElement].wrappedValue,
      v.asInstanceOf[CGroupElement].wrappedValue))
}
