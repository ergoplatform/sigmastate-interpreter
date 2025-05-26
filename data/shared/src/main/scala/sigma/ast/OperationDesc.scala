package sigma.ast

/** Each costable operation is described in one of the following ways:
  * 1) using [[ValueCompanion]] - operation with separate node class
  * 2) using [[SMethod]] - operation represented as method.
  * 3) using string name - intermediate sub-operation present in cost model, but
  *                        which is not a separate operation of ErgoTree.
  */
abstract class OperationDesc {
  def operationName: String
}

/** Operation descriptor based on [[ValueCompanion]]. */
case class CompanionDesc(companion: ValueCompanion) extends OperationDesc {
  override def operationName: String = companion.typeName
}

/** Operation descriptor based on [[SMethod]]. */
case class MethodDesc(method: SMethod) extends OperationDesc {
  override def operationName: String = method.opName

  override def toString: String = s"MethodDesc(${method.opName})"

  override def hashCode(): Int = (method.objType.typeId << 8) | method.methodId

  override def equals(obj: Any): Boolean =
    this.eq(obj.asInstanceOf[AnyRef]) || (obj != null && (obj match {
      case that: MethodDesc =>
        method.objType.typeId == that.method.objType.typeId &&
            method.methodId == that.method.methodId
      case _ => false
    }))
}

/** Operation descriptor based on name. */
case class NamedDesc(operationName: String) extends OperationDesc

/** Operation costing descriptors combined together. */
case class OperationCostInfo[C <: CostKind](costKind: C, opDesc: OperationDesc)

