package sigma.data

import sigma.crypto.{CryptoFacade, Ecp}
import sigma.serialization.GroupElementSerializer
import sigma.util.Extensions.EcpOps
import sigma.{BigInt, Coll, Colls, GroupElement, UnsignedBigInt}

/** A default implementation of [[GroupElement]] interface.
  *
  * @see [[GroupElement]] for detailed descriptions
  */
case class CGroupElement(override val wrappedValue: Ecp) extends GroupElement with WrapperOf[Ecp] {

  private var _encoded: Coll[Byte] = null
  override def toString: String = s"GroupElement(${wrappedValue.showECPoint})"

  override def getEncoded: Coll[Byte] = {
    if(_encoded == null) {
      _encoded = Colls.fromArray(GroupElementSerializer.toBytes(wrappedValue))
    }
    _encoded
  }

  override def isIdentity: Boolean = CryptoFacade.isInfinityPoint(wrappedValue)

  override def exp(k: BigInt): GroupElement =
    CGroupElement(CryptoFacade.exponentiatePoint(wrappedValue, k.asInstanceOf[CBigInt].wrappedValue))

  override def expUnsigned(k: UnsignedBigInt): GroupElement =
    CGroupElement(CryptoFacade.exponentiatePoint(wrappedValue, k.asInstanceOf[CUnsignedBigInt].wrappedValue))

  override def multiply(that: GroupElement): GroupElement =
    CGroupElement(CryptoFacade.multiplyPoints(wrappedValue, that.asInstanceOf[CGroupElement].wrappedValue))

  override def negate: GroupElement =
    CGroupElement(CryptoFacade.negatePoint(wrappedValue))

  override def equals(obj: Any): Boolean = {
    obj match {
      case cg: GroupElement => cg.getEncoded == this.getEncoded
      case _ => false
    }
  }
}
