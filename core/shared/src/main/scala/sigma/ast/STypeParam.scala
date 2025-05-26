package sigma.ast

/** Represents a type parameter in a type system.
  *
  * @param ident       The identifier for this type parameter.
  * @param upperBound  The upper bound of this type parameter, if exists.
  * @param lowerBound  The lower bound of this type parameter, if exists.
  * @note Type parameters with bounds are currently not supported.
  */
case class STypeParam(
    ident: STypeVar,
    upperBound: Option[SType] = None,
    lowerBound: Option[SType] = None) {
  assert(upperBound.isEmpty && lowerBound.isEmpty, s"Type parameters with bounds are not supported, but found $this")

  override def toString = ident.toString + upperBound.fold("")(u => s" <: $u") + lowerBound.fold("")(l => s" >: $l")
}

object STypeParam {
  /** Enables implicit conversion from [[STypeVar]] to [[STypeParam]].
    *
    * @param id The type variable to convert.
    * @return A type parameter constructed from the provided type variable.
    */
  implicit def typeIdentToTypeParam(id: STypeVar): STypeParam = STypeParam(id)
}