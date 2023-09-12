package sigma.data

import sigma.ast.SSigmaProp
import sigma.serialization.CoreSerializer
import sigma.util.Extensions.SigmaBooleanOps
import sigma.{Coll, Colls, SigmaProp}

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
    // in order to have comparisons like  `box.propositionBytes == pk.propBytes` we need to make sure
    // the same serialization method is used in both cases
    // TODO v6.0: add `pk.propBytes(version)` (see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/903)
    val w = CoreSerializer.startWriter()
    w.put(0)  // ErgoTree.header
    w.putType(SSigmaProp)
    SigmaBoolean.serializer.serialize(wrappedValue, w)
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


