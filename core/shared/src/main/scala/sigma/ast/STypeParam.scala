package sigma.ast

/** Represents a type parameter in a type system.
  *
  * @param ident       The identifier for this type parameter
  */
case class STypeParam(ident: STypeVar) {
  override def toString = ident.toString
}

object STypeParam {
  /** Enables implicit conversion from [[STypeVar]] to [[STypeParam]].
    *
    * @param id The type variable to convert.
    * @return A type parameter constructed from the provided type variable.
    */
  implicit def typeIdentToTypeParam(id: STypeVar): STypeParam = STypeParam(id)
}