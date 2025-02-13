package sigma

import sigma.kiama.rewriting.Rewriter.{everywherebu, rewrite, rule}

package object ast {

  /** Type alias for a substitution of type variables with their corresponding types. */
  type STypeSubst = Map[STypeVar, SType]

  /** Immutable and sharable empty substitution. */
  val EmptySubst = Map.empty[STypeVar, SType]

  /** Performs pairwise type unification making sure each type variable is equally
    * substituted in all items. */
  def unifyTypeLists(items1: Seq[SType], items2: Seq[SType]): Option[STypeSubst] = {
    // unify items pairwise independently
    val itemsUni = (items1, items2).zipped.map((t1, t2) => unifyTypes(t1, t2))
    if (itemsUni.forall(_.isDefined)) {
      // merge substitutions making sure the same id is equally substituted in all items
      val merged = itemsUni.foldLeft(EmptySubst)((acc, subst) => {
        var res = acc
        for ( (id, t) <- subst.get ) {
          if (res.contains(id) && res(id) != t) return None
          res = res + (id -> t)
        }
        res
      })
      Some(merged)
    } else
      None
  }

  private val unifiedWithoutSubst = Some(EmptySubst)

  /** Finds a substitution `subst` of type variables such that unifyTypes(applySubst(t1, subst), t2) shouldBe Some(emptySubst) */
  def unifyTypes(t1: SType, t2: SType): Option[STypeSubst] = (t1, t2) match {
    case (_ @ STypeVar(n1), _ @ STypeVar(n2)) =>
      if (n1 == n2) unifiedWithoutSubst else None
    case (id1 @ STypeVar(_), _) =>
      Some(Map(id1 -> t2))
    case (e1: SCollectionType[_], e2: SCollectionType[_]) =>
      unifyTypes(e1.elemType, e2.elemType)
    case (e1: SCollectionType[_], _: STuple) =>
      unifyTypes(e1.elemType, SAny)
    case (e1: SOption[_], e2: SOption[_]) =>
      unifyTypes(e1.elemType, e2.elemType)
    case (e1: STuple, e2: STuple) if e1.items.length == e2.items.length =>
      unifyTypeLists(e1.items, e2.items)
    case (e1: SFunc, e2: SFunc) if e1.tDom.length == e2.tDom.length =>
      unifyTypeLists(e1.tDom :+ e1.tRange, e2.tDom :+ e2.tRange)
    case (STypeApply(name1, args1), STypeApply(name2, args2))
      if name1 == name2 && args1.length == args2.length =>
      unifyTypeLists(args1, args2)
    case (SBoolean, SSigmaProp) => // it is necessary for implicit conversion in Coll(bool, prop, bool)
      unifiedWithoutSubst
    case (SPrimType(e1), SPrimType(e2)) if e1 == e2 =>
      unifiedWithoutSubst
    case (SAny, _) =>
      unifiedWithoutSubst
    case _ => None
  }

  /** Applies a type substitution to a given type.
    *
    * @param tpe   the type to apply the substitution to
    * @param subst the type substitution to apply
    * @return the type after applying the substitution
    */
  def applySubst(tpe: SType, subst: STypeSubst): SType = tpe match {
    case SFunc(args, res, tparams) =>
      val remainingVars = tparams.filterNot { p => subst.contains(p.ident) }
      SFunc(args.map(applySubst(_, subst)), applySubst(res, subst), remainingVars)
    case _ =>
      val substRule = rule[Any] {
        case id: STypeVar if subst.contains(id) => subst(id)
      }
      rewrite(everywherebu(substRule))(tpe)
  }

  /** Computes the most general type given two types.
    *
    * @param t1 the first type
    * @param t2 the second type
    * @return the most general type if it exists, otherwise None
    */
  def msgType(t1: SType, t2: SType): Option[SType] = unifyTypes(t1, t2) match {
    case Some(_) => Some(t1)
    case None => unifyTypes(t2, t1).map(_ => t2)
  }

  /** Most Specific Generalized (MSG) type of ts.
    * Currently just the type of the first element as long as all the elements have the same type. */
  def msgTypeOf(ts: Seq[SType]): Option[SType] = {
    if (ts.isEmpty) None
    else {
      var res: SType = ts.head
      for ( t <- ts.iterator.drop(1) ) {
        msgType(t, res) match {
          case Some(msg) => res = msg //assign new
          case None => return None
        }
      }
      Some(res)
    }
  }

  implicit class STypeOps(val tpe: SType) extends AnyVal {
    def isCollectionLike: Boolean = tpe.isInstanceOf[SCollection[_]]

    def isCollection: Boolean = tpe.isInstanceOf[SCollectionType[_]]

    def isOption: Boolean = tpe.isInstanceOf[SOption[_]]

    def isBox: Boolean = tpe.isInstanceOf[SBox.type]

    def isGroupElement: Boolean = tpe.isInstanceOf[SGroupElement.type]

    def isSigmaProp: Boolean = tpe.isInstanceOf[SSigmaProp.type]

    def isAvlTree: Boolean = tpe.isInstanceOf[SAvlTree.type]

    def isFunc: Boolean = tpe.isInstanceOf[SFunc]

    def isTuple: Boolean = tpe.isInstanceOf[STuple]

    /** Returns true if this type is numeric (Byte, Short, etc.)
      *
      * @see [[sigmastate.SNumericType]]
      */
    def isNumType: Boolean = tpe.isInstanceOf[SNumericType]

    /** Returns true if this type is either numeric (Byte, Short, etc.) or is NoType.
      *
      * @see [[sigmastate.SNumericType]]
      */
    def isNumTypeOrNoType: Boolean = isNumType || tpe == NoType

    def asNumType: SNumericType = tpe.asInstanceOf[SNumericType]

    /** Cast this type to numeric type or else throws the given error. */
    def asNumTypeOrElse(error: => Exception): SNumericType = tpe match {
      case nt: SNumericType => nt
      case _ => throw error
    }

    def asFunc: SFunc = tpe.asInstanceOf[SFunc]

    def asProduct: SProduct = tpe.asInstanceOf[SProduct]

    def asTuple: STuple = tpe.asInstanceOf[STuple]

    def asOption[T <: SType]: SOption[T] = tpe.asInstanceOf[SOption[T]]

    def whenFunc[T](action: SFunc => Unit) = if (tpe.isInstanceOf[SFunc]) action(tpe.asFunc)

    def asCollection[T <: SType] = tpe.asInstanceOf[SCollection[T]]

    /** Applies a type substitution to this type.
      *
      * @param subst the type substitution to apply
      * @return the type after applying the substitution
      */
    def withSubstTypes(subst: Map[STypeVar, SType]): SType =
      if (subst.isEmpty) tpe
      else
        applySubst(tpe, subst)
  }
}
