package sigma.data

import sigma.crypto.{CryptoFacade, Ecp}
import sigma.serialization.GroupElementSerializer
import sigma.util.Extensions.EcpOps
import sigma.{BigInt, Coll, Colls, GroupElement}

/** A default implementation of [[GroupElement]] interface.
  *
  * @see [[GroupElement]] for detailed descriptions
  */
case class CGroupElement(override val wrappedValue: Ecp) extends GroupElement with WrapperOf[Ecp] {

  override def toString: String = s"GroupElement(${wrappedValue.showECPoint})"

  override def getEncoded: Coll[Byte] =
    Colls.fromArray(GroupElementSerializer.toBytes(wrappedValue))

  override def isIdentity: Boolean = CryptoFacade.isInfinityPoint(wrappedValue)

  override def exp(k: BigInt): GroupElement =
    CGroupElement(CryptoFacade.exponentiatePoint(wrappedValue, k.asInstanceOf[CBigInt].wrappedValue))

  override def multiply(that: GroupElement): GroupElement =
    CGroupElement(CryptoFacade.multiplyPoints(wrappedValue, that.asInstanceOf[CGroupElement].wrappedValue))

  override def negate: GroupElement =
    CGroupElement(CryptoFacade.negatePoint(wrappedValue))
}
