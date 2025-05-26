/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package sigma.kiama
package rewriting

import sigma.reflection.{Platform, RClass, RConstructor}

/**
  * Strategy-based term rewriting in the style of Stratego (http://strategoxt.org/).
  * The implementation here is partially based on the semantics given in "Program
  * Transformation with Scoped Dynamic Rewrite Rules", by Bravenboer, van Dam, Olmos
  * and Visser, Fundamenta Informaticae, 69, 2005. The library strategies are mostly
  * based on the Stratego library, but also on combinators found in the Scrap Your
  * Boilerplate and Uniplate libraries for Haskell.
  */
trait Rewriter {

  import sigma.kiama.util.Comparison.same
  import sigma.kiama.util.Collections.{CanBuildFrom, Factory, newBuilder}

  /**
    * Rewrite a term.  Apply the strategy `s` to a term returning the result term
    * if `s` succeeds, otherwise return the original term.
    */
  def rewrite[T](s : Strategy)(t : T) : T = {
    s(t) match {
      case Some(t1) =>
        t1.asInstanceOf[T]
      case None =>
        t
    }
  }

  // Strategy creation

  /**
    * Make a strategy with the body `f`. By default, make a basic strategy.
    */
  def mkStrategy(f : Any => Option[Any]) : Strategy =
    new Strategy {
      def apply(t : Any) =
        f(t)
    }

  // Builder combinators.

  /**
    * Construct a strategy that always succeeds, changing the subject term to
    * the given term `t`. The term `t` is evaluated at most once.
    */
  def build(t : Any) : Strategy =
    rulef(_ => t)

  /**
    * A strategy that always fails.
    */
  val fail : Strategy =
    mkStrategy(_ => None)

  /**
    * A strategy that always succeeds.
    */
  val id : Strategy =
    mkStrategy(Some(_))

  /**
    * Perform a paramorphism over a value. This is a fold in which the
    * recursive step may refer to the recursive component of the value
    * and the results of folding over the children.  When the function `f`
    * is called, the first parameter is the value and the second is a
    * sequence of the values that `f` has returned for the children.  This
    * will work on any value, but will only decompose values that are
    * supported by the `Term` generic term deconstruction.  This operation
    * is similar to that used in the Uniplate library.
    */
  def para[T](f : (Any, Seq[T]) => T) : Any => T = {
    case Term(t, ts) => f(t, ts.map(para(f)))
  }

  /**
    * Define a term query by a partial function `f`. The query always succeeds
    * with no effect on the subject term but applies the given partial function
    * `f` to the subject term.  In other words, the strategy runs `f` for its
    * side-effects. If the subject term is not a `T` or the function is not
    * defined at the subject term, the strategy fails.
    *
    * Due to the type erasure performed on Scala programs the type test
    * will be imprecise for some types. E.g., it is not possible to tell
    * the difference between `List[Int]` and `List[String]`.
    */
  def query[T](f : T ==> Unit) : Strategy = {
    val anyf = f.asInstanceOf[===>[Any]]
    mkStrategy(
      (t : Any) => {
        val of = anyf andThen (_ => Some(t))
        try {
          of.applyOrElse(t, (_ : Any) => None)
        } catch {
          case _ : ClassCastException =>
            None
        }
      }
    )
  }

  /**
    * Define a rewrite rule using a partial function `f` defined on the type
    * `T`. If the subject term is a `T` and the function is defined at the
    * subject term, then the strategy succeeds with the return value of the
    * function applied to the subject term. Otherwise, the strategy fails.
    *
    * Due to the type erasure performed on Scala programs the type test
    * will be imprecise for some types. E.g., it is not possible to tell
    * the difference between `List[Int]` and `List[String]`.
    */
  def rule[T](f : ===>[T]) : Strategy = {
    val anyf = f.asInstanceOf[===>[Any]]
    val of = anyf andThen (Some(_))
    mkStrategy(
      (t : Any) =>
        try {
          of.applyOrElse(t, (_ : Any) => None)
        } catch {
          case _ : ClassCastException =>
            None
        }
    )
  }

  /**
    * Define a rewrite rule using a function `f` that returns a term.
    * The rule always succeeds with the return value of the function.
    */
  def rulef(f : Any => Any) : Strategy =
    strategyf(t => Some(f(t)))

  /**
    * Define a rewrite rule using a function `f` defined on type `T` that returns
    * a strategy. If the subject term is a `T` and the function is defined at the
    * subject term, the rule applies the function to the subject term to get a
    * strategy which is then applied again to the subject term. In other words,
    * the function is only used for effects such as pattern matching.  The whole
    * thing also fails if `f` is not defined at the term in the first place.
    */
  def rulefs[T](f : T ==> Strategy) : Strategy = {
    val anyf = f.asInstanceOf[Any ==> Strategy]
    mkStrategy(
      (t : Any) => {
        val of = anyf andThen (_.apply(t))
        try {
          of.applyOrElse(t, (_ : Any) => None)
        } catch {
          case _ : ClassCastException =>
            None
        }
      }
    )
  }

  /**
    * Make a strategy from a partial function `f` defined on the type `T`.
    * If the subject term is a `T` and the function is defined at the
    * subject term, then the function return value when applied to the
    * subject term determines whether the strategy succeeds or fails.
    * If the subject term is not a `T` or the function is not defined at
    * the subject term, the strategy fails.
    *
    * Due to the type erasure performed on Scala programs the type test
    * will be imprecise for some types. E.g., it is not possible to tell
    * the difference between `List[Int]` and `List[String]`.
    */
  def strategy[T](f : T ==> Option[T]) : Strategy = {
    val of = f.asInstanceOf[Any ==> Option[T]]
    mkStrategy(
      (t : Any) =>
        try {
          of.applyOrElse(t, (_ : Any) => None)
        } catch {
          case _ : ClassCastException =>
            None
        }
    )
  }

  /**
    * Make a strategy from a function `f`. The function return value
    * determines whether the strategy succeeds or fails.
    */
  def strategyf(f : Any => Option[Any]) : Strategy =
    mkStrategy(f)

  /**
    * Construct a strategy that succeeds only if the subject term matches
    * the given term `t`.
    */
  def term[T](t : T) : Strategy =
    rule[T]({
      case `t` => t
    })

  // Product duplication support

  /**
    * General product duplication functionality. This object is a function
    * that returns a product that applies the same constructor as the
    * product `t`, but with the given children instead of `t`'s children.
    * The function fails if a constructor cannot be found, there are the
    * wrong number of new children, or if one of the new children is not
    * of the appropriate type.
    */
  object Duplicator {

    /**
      * The type of a duplicator. A duplicator takes a product `t` and
      * an array of new children and returns a new product with the
      * same constructor as `t` but with the new children.
      */
    type Duper = (Any, Array[AnyRef]) => Any

    /**
      * This duper always returns the same product that it is given for singleton Scala objects.
      * It uses the `MODULE$` field to determine if a class is a singleton object.
      * Otherwise, it uses the first constructor it finds to make a new product.
      */
    object MakeDuper extends (RClass[_] => Duper) {

      /** Make a duper for the given class. */
      def apply(clazz : RClass[_]) : Duper =
        try {
          // See if this class has a MODULE$ field. This field is used by Scala
          // to hold a singleton instance and is only present in singleton classes
          // (e.g., ones arising from object declarations or case objects). If we
          // are trying to duplicate one of these then we want to return the same
          // singleton so we use an identity duper.
          clazz.getField("MODULE$")
          (t : Any, _ : Array[AnyRef]) => t
        } catch {
          // Otherwise, this is a normal class, so we try to make a
          // duper that uses the first constructor.
          case _ : NoSuchFieldException =>
            val ctors = clazz.getConstructors
            if (ctors.length == 0)
              sys.error(s"dup no constructors for ${clazz.getName}")
            else
              (_ : Any, children : Array[AnyRef]) =>
                makeInstance(ctors(0), children)
        }

      /** Make an instance using the given constructor with the given children as arguments. */
      def makeInstance(ctor : RConstructor[_], children : Array[AnyRef]) : Any =
        try {
          ctor.newInstance(unboxPrimitives(ctor, children) : _*)
        } catch {
          case _ : IllegalArgumentException =>
            sys.error(s"""dup illegal arguments: $ctor got (${children.mkString(",")})
                        |Common cause: term classes are nested in another class, move them to the top level""".stripMargin)
        }

      /** Unbox primitive values in the given array of children using type information of
        * the given constructor. */
      def unboxPrimitives(ctor : RConstructor[_], children : Array[AnyRef]) : Array[AnyRef] = {
        val childrenTypes = ctor.getParameterTypes()
        val numChildren = childrenTypes.length
        val newChildren = new Array[AnyRef](numChildren)
        var i = 0
        while (i < numChildren) {
          if (childrenTypes(i).isPrimitive())
            newChildren(i) = unboxAnyVal(children(i))
          else
            newChildren(i) = children(i)
          i = i + 1
        }
        newChildren
      }

      /** Unbox a primitive value. */
      def unboxAnyVal(s : AnyRef) : AnyRef =
        s match {
          case p : Product if p.productArity == 1 =>
            p.productElement(0).asInstanceOf[AnyRef]
          case _ =>
            s
        }

    }

    /** All memoized duppers. */
    private val dupers = new Platform.Cache[RClass[_], Duper]

    /** Obtains a duper for the given class and memoizes it in the `dupers` cache. */
    def getDuper(clazz: RClass[_]): Duper = {
      val duper = dupers.getOrElseUpdate(clazz, MakeDuper(clazz))
      duper
    }

    /** Apply the duplicator to the given product and children. */
    def apply[T <: Product](t : T, children : Array[AnyRef]) : T = {
      val clazz = RClass(t.getClass)
      val duper = getDuper(clazz)
      duper(t, children).asInstanceOf[T]
    }

  }

  /**
    * The duplicator used by the generic traversals. Needs to be defined
    * as a method so we can override it in other rewriting modules.
    */
  def dup[T <: Product](t : T, children : Array[AnyRef]) : T =
    Duplicator(t, children)

  /**
    * Copy a product node by creating a new node of the same class type
    * using the same children.
    */
  def copy[T <: Product](t : T) : T =
    Duplicator(t, t.productIterator.map(makechild).toArray)

  /**
    * Make an arbitrary value `c` into a term child, checking that it worked
    * properly. Object references will be returned unchanged; other values
    * will be boxed.
    */
  protected def makechild(c : Any) : AnyRef =
    c.asInstanceOf[AnyRef]

  // Generic traversals

  /**
    * Traversal to a single child.  Construct a strategy that applies `s` to
    * the ''ith'' child of the subject term (counting from one).  If `s` succeeds on
    * the ''ith'' child producing `t`, then succeed, forming a new term that is the
    * same as the original term except that the ''ith'' child is now `t`.  If `s` fails
    * on the ''ith'' child or the subject term does not have an ''ith'' child, then fail.
    * `child(i, s)` is equivalent to Stratego's `i(s)` operator.  If `s` succeeds on
    * the ''ith'' child producing the same term (by `eq` for references and by `==` for
    * other values), then the overall strategy returns the subject term.
    * This operation works for instances of `Product` or finite `Seq` values.
    * `s` is evaluated at most once.
    */
  def child(i : Int, s : => Strategy) : Strategy =
    mkStrategy({
      lazy val strat = s
      t =>
        t match {
          case p : Product =>
            childProduct(strat, i, p)
          case t : Seq[_] =>
            childSeq(strat, i, t.asInstanceOf[Seq[Any]])
          case _ =>
            None
        }
    })

  /**
    * Implementation of `child` for `Product` values.
    */
  def childProduct(s : Strategy, i : Int, p : Product) : Option[Any] = {
    val numchildren = p.productArity
    if ((i < 1) || (i > numchildren)) {
      None
    } else {
      val ct = p.productElement(i - 1)
      s(ct) match {
        case Some(ti) if same(ct, ti) =>
          Some(p)
        case Some(ti) =>
          val newchildren = p.productIterator.map(makechild).toArray
          newchildren(i - 1) = makechild(ti)
          Some(dup(p, newchildren))
        case None =>
          None
      }
    }
  }

  /**
    * Implementation of `child` for `Seq` values.
    */
  def childSeq[CC[U] <: Seq[U]](s : Strategy, i : Int, t : CC[Any])(implicit cbf : CanBuildFrom[CC[Any], Any, CC[Any]]) : Option[CC[Any]] = {
    val numchildren = t.size
    if ((i < 1) || (i > numchildren)) {
      None
    } else {
      val ct = t(i - 1)
      s(ct) match {
        case Some(ti) if same(ct, ti) =>
          Some(t)
        case Some(ti) =>
          val b = newBuilder(cbf, t)
          b.sizeHint(t.size)
          t.foldLeft(0) {
            case (j, ct) =>
              b += (if (j == i - 1) ti else ct)
              j + 1
          }
          Some(b.result())
        case None =>
          None
      }
    }
  }

  /**
    * Traversal to all children.  Construct a strategy that applies `s` to all
    * term children of the subject term.  If `s` succeeds on all of the children,
    * then succeed, forming a new term from the constructor
    * of the original term and the result of `s` for each child.  If `s` fails on any
    * child, fail. If there are no children, succeed.  If `s` succeeds on all
    * children producing the same terms (by `eq` for references and by `==` for
    * other values), then the overall strategy returns the subject term.
    * This operation works on finite `Rewritable`, `Product`, `Map` and `Iterable`
    * values, checked for in that order.
    * Children of a `Rewritable` (resp. Product, collection) value are processed
    * in the order returned by the value's deconstruct (resp. `productElement`,
    * `foreach`) method.
    * `s` is evaluated at most once.
    */
  def all(s : => Strategy) : Strategy =
    mkStrategy({
      lazy val strat = s
      t =>
        t match {
          case p : Product =>
            allProduct(strat, p)
          case m : Map[_, _] =>
            allMap(strat, m.asInstanceOf[Map[Any, Any]])
          case t : Iterable[_] =>
            allIterable(strat, t.asInstanceOf[Iterable[Any]])
          case _ =>
            Some(t)
        }
    })

  /**
    * Implementation of `all` for `Product` values.
    */
  def allProduct(s : Strategy, p : Product) : Option[Any] = {
    val numchildren = p.productArity
    if (numchildren == 0)
      Some(p)
    else {
      val newchildren = Array.newBuilder[AnyRef]
      val changed =
        p.productIterator.foldLeft(false) {
          case (changed, ct) =>
            s(ct) match {
              case Some(ti) =>
                newchildren += makechild(ti)
                changed || !same(ct, ti)
              case None =>
                return None
            }
        }
      if (changed)
        Some(dup(p, newchildren.result()))
      else
        Some(p)
    }
  }

  /**
    * Implementation of `all` for `Iterable` values.
    */
  def allIterable[CC[U] <: Iterable[U]](s : Strategy, t : CC[Any])(implicit cbf : CanBuildFrom[CC[Any], Any, CC[Any]]) : Option[CC[Any]] =
    if (t.size == 0)
      Some(t)
    else {
      val b = newBuilder(cbf, t)
      b.sizeHint(t.size)
      val (changed, _) =
        t.foldLeft((false, 0)) {
          case ((changed, i), ct) =>
            s(ct) match {
              case Some(ti) =>
                b += ti
                (changed || !same(ct, ti), i + 1)
              case None =>
                return None
            }
        }
      if (changed)
        Some(b.result())
      else
        Some(t)
    }

  /**
    * Implementation of `all` for `Map` values.
    */
  def allMap[CC[V, W] <: Map[V, W]](s : Strategy, t : CC[Any, Any])(implicit cbf : CanBuildFrom[CC[Any, Any], (Any, Any), CC[Any, Any]]) : Option[CC[Any, Any]] =
    if (t.size == 0)
      Some(t)
    else {
      val b = newBuilder(cbf, t)
      b.sizeHint(t.size)
      val (changed, _) =
        t.foldLeft((false, 0)) {
          case ((changed, i), ct) =>
            s(ct) match {
              case Some(ti @ (_, _)) =>
                b += ti
                (changed || !same(ct, ti), i + 1)
              case _ =>
                return None
            }
        }
      if (changed)
        Some(b.result())
      else
        Some(t)
    }

  /**
    * Traversal to one child.  Construct a strategy that applies `s` to the term
    * children of the subject term.  Assume that `c` is the
    * first child on which s succeeds.  Then stop applying `s` to the children and
    * succeed, forming a new term from the constructor of the original term and
    * the original children, except that `c` is replaced by the result of applying
    * `s` to `c`.  In the event that the strategy fails on all children, then fail.
    * If there are no children, fail.  If `s` succeeds on the one child producing
    * the same term (by `eq` for references and by `==` for other values), then
    * the overall strategy returns the subject term.
    * This operation works on instances of finite `Rewritable`, `Product`, `Map`
    * and `Iterable` values, checked for in that order.
    * Children of a `Rewritable` (resp. `Product`, collection) value are processed
    * in the order returned by the value's `deconstruct` (resp. `productElement`,
    * `foreach`) method.
    * `s` is evaluated at most once.
    */
  def one(s : => Strategy) : Strategy =
    mkStrategy({
      lazy val strat = s
      t =>
        t match {
          case p : Product =>
            oneProduct(strat, p)
          case m : Map[_, _] =>
            oneMap(strat, m.asInstanceOf[Map[Any, Any]])
          case t : Iterable[_] =>
            oneIterable(strat, t.asInstanceOf[Iterable[Any]])
          case _ =>
            None
        }
    })

  /**
    * Implementation of `one` for `Product` values.
    */
  def oneProduct(s : Strategy, p : Product) : Option[Any] = {
    p.productIterator.foldLeft(0) {
      case (i, ct) =>
        s(ct) match {
          case Some(ti) if same(ct, ti) =>
            return Some(p)
          case Some(ti) =>
            val newchildren = p.productIterator.toArray.map(makechild)
            newchildren(i) = makechild(ti)
            return Some(dup(p, newchildren))
          case None =>
            i + 1
        }
    }
    None
  }

  /**
    * Implementation of `one` for `Iterable` values.
    */
  def oneIterable[CC[U] <: Iterable[U]](s : Strategy, t : CC[Any])(implicit cbf : CanBuildFrom[CC[Any], Any, CC[Any]]) : Option[CC[Any]] = {
    val b = newBuilder(cbf, t)
    b.sizeHint(t.size)
    val add =
      t.foldLeft(true) {
        case (add, ct) =>
          if (add)
            s(ct) match {
              case Some(ti) if same(ct, ti) =>
                return Some(t)
              case Some(ti) =>
                b += ti
                false
              case None =>
                b += ct
                true
            }
          else {
            b += ct
            false
          }
      }
    if (add)
      None
    else
      Some(b.result())
  }

  /**
    * Implementation of `one` for `Map` values.
    */
  def oneMap[CC[V, W] <: Map[V, W]](s : Strategy, t : CC[Any, Any])(implicit cbf : CanBuildFrom[CC[Any, Any], (Any, Any), CC[Any, Any]]) : Option[CC[Any, Any]] = {
    val b = newBuilder(cbf, t)
    b.sizeHint(t.size)
    val add =
      t.foldLeft(true) {
        case (add, ct) =>
          if (add)
            s(ct) match {
              case Some(ti @ (_, _)) if same(ct, ti) =>
                return Some(t)
              case Some(ti @ (_, _)) =>
                b += ti
                false
              case Some(ti) =>
                sys.error(s"oneMap: got non-pair $ti")
              case None =>
                b += ct
                true
            }
          else {
            b += ct
            false
          }
      }
    if (add)
      None
    else
      Some(b.result())
  }

  /**
    * Traversal to as many children as possible, but at least one.  Construct a
    * strategy that applies `s` to the term children of the subject term.
    * If `s` succeeds on any of the children, then succeed,
    * forming a new term from the constructor of the original term and the result
    * of `s` for each succeeding child, with other children unchanged.  In the event
    * that `s` fails on all children, then fail. If there are no
    * children, fail.  If `s` succeeds on children producing the same terms (by `eq`
    * for references and by `==` for other values), then the overall strategy
    * returns the subject term.
    * This operation works on instances of finite `Rewritable`, `Product`, `Map` and
    * `Iterable` values, checked for in that order.
    * Children of a `Rewritable` (resp. `Product`, collection) value are processed
    * in the order returned by the value's `deconstruct` (resp. `productElement`,
    * `foreach`) method.
    * `s` is evaluated at most once.
    */
  def some(s : => Strategy) : Strategy =
    mkStrategy({
      lazy val strat = s
      t =>
        t match {
          case p : Product =>
            someProduct(strat, p)
          case m : Map[_, _] =>
            someMap(strat, m.asInstanceOf[Map[Any, Any]])
          case t : Iterable[_] =>
            someIterable(strat, t.asInstanceOf[Iterable[Any]])
          case _ =>
            None
        }
    })

  /**
    * Implementation of `some` for `Product` values.
    */
  def someProduct(s : Strategy, p : Product) : Option[Any] = {
    val numchildren = p.productArity
    if (numchildren == 0)
      None
    else {
      val newchildren = Array.newBuilder[AnyRef]
      val (success, changed) =
        p.productIterator.foldLeft((false, false)) {
          case ((success, changed), ct) =>
            s(ct) match {
              case Some(ti) =>
                newchildren += makechild(ti)
                (true, changed || !same(ct, ti))
              case None =>
                newchildren += makechild(ct)
                (success, changed)
            }
        }
      if (success)
        if (changed)
          Some(dup(p, newchildren.result()))
        else
          Some(p)
      else
        None
    }
  }

  /**
    * Implementation of `some` for `Iterable` values.
    */
  def someIterable[CC[U] <: Iterable[U]](s : Strategy, t : CC[Any])(implicit cbf : CanBuildFrom[CC[Any], Any, CC[Any]]) : Option[CC[Any]] =
    if (t.size == 0)
      None
    else {
      val b = newBuilder(cbf, t)
      b.sizeHint(t.size)
      val (success, changed) =
        t.foldLeft((false, false)) {
          case ((success, changed), ct) =>
            s(ct) match {
              case Some(ti) =>
                b += ti
                (true, changed || !same(ct, ti))
              case None =>
                b += ct
                (success, changed)
            }
        }
      if (success)
        if (changed)
          Some(b.result())
        else
          Some(t)
      else
        None
    }

  /**
    * Implementation of `some` for `Map` values.
    */
  def someMap[CC[V, W] <: Map[V, W]](s : Strategy, t : CC[Any, Any])(implicit cbf : CanBuildFrom[CC[Any, Any], (Any, Any), CC[Any, Any]]) : Option[CC[Any, Any]] =
    if (t.size == 0)
      None
    else {
      val b = newBuilder(cbf, t)
      b.sizeHint(t.size)
      val (success, changed) =
        t.foldLeft((false, false)) {
          case ((success, changed), ct) =>
            s(ct) match {
              case Some(ti @ (_, _)) =>
                b += ti
                (true, changed || !same(ct, ti))
              case _ =>
                b += ct
                (success, changed)
            }
        }
      if (success)
        if (changed)
          Some(b.result())
        else
          Some(t)
      else
        None
    }

  // Extractors

  /**
    * Generic term deconstruction.
    */
  object Term {

    /**
      * Generic term deconstruction. An extractor that decomposes `Product`
      * `Rewritable` or `Seq` values into the value itself and a vector of
      * its children.  Terms that are not of these types are not decomposable
      * (i.e., the children will be empty).
      */
    def unapply(t : Any) : Some[(Any, Vector[Any])] = {
      t match {
        case p : Product =>
          Some((p, p.productIterator.toVector))
        case s : Seq[_] =>
          Some((s, s.toVector))
        case _ =>
          Some((t, Vector()))
      }
    }

  }

  // Library combinators

  /**
    * `and(s1, s2)` applies `s1` and `s2` to the subject
    * term and succeeds if both succeed. `s2` will always
    * be applied, i.e., and is ''not'' a short-circuit
    * operator.
    */
  def and(s1 : Strategy, s2 : Strategy) : Strategy =
    where(s1) < (test(s2) + (test(s2) <* fail))

  /**
    * Construct a strategy that applies `s`, yielding the result of `s` if it
    * succeeds, otherwise leave the original subject term unchanged.  In
    * Stratego library this strategy is called `try`.
    */
  def attempt(s : Strategy) : Strategy =
    s <+ id

  /**
    * Construct a strategy that applies `s` in a bottom-up, postfix fashion
    * to the subject term.
    */
  def bottomup(s : Strategy) : Strategy =
    all(bottomup(s)) <* s

  /**
    * A strategy that tests whether the two sub-terms of a pair of terms are equal.
    */
  val eq : Strategy =
    rule[(Any, Any)]({
      case t @ (x, y) if x == y => t
    })

  /**
    * Construct a strategy that tests whether the two sub-terms of a
    * pair of terms are equal. Synonym for `eq`.
    */
  val equal : Strategy =
    eq

  /**
    * Same as `everywheretd`.
    */
  def everywhere(s : Strategy) : Strategy =
    everywheretd(s)

  /**
    * Construct a strategy that applies `s` at all terms in a bottom-up fashion
    * regardless of failure.  Terms for which the strategy fails are left
    * unchanged.
    */
  def everywherebu(s : Strategy) : Strategy =
    bottomup(attempt(s))

  /**
    * Construct a strategy that applies `s` at all terms in a top-down fashion
    * regardless of failure.  Terms for which the strategy fails are left
    * unchanged.
    */
  def everywheretd(s : Strategy) : Strategy =
    topdown(attempt(s))

  /**
    * Construct a strategy that while `r` succeeds applies `s`.  This operator
    * is called `while` in the Stratego library.
    */
  def loop(c : Strategy, s : Strategy) : Strategy = {
    lazy val result : Strategy = attempt(c <* s <* result)
    mkStrategy(result)
  }

  /**
    * Construct a strategy that applies `s(i)` for each integer `i` from `low` to
    * `high` (inclusive).  This operator is called `for` in the Stratego library.
    */
  def loopiter(s : Int => Strategy, low : Int, high : Int) : Strategy = {
    lazy val result =
      if (low <= high)
        s(low) <* loopiter(s, low + 1, high)
      else
        id
    mkStrategy(result)
  }

  /**
    * Construct a strategy that applies `s` to each element of a finite
    * sequence (type `Seq`) returning a new sequence of the results if
    * all of the applications succeed, otherwise fail.  If all of the
    * applications succeed without change, return the input sequence.
    */
  def map(s : Strategy) : Strategy =
    strategy[Seq[_]]({
      case l : Seq[_] =>
        allIterable[Seq](s, l)
    })

  /**
    * Construct a strategy that applies `s`, then fails if `s` succeeded or, if `s`
    * failed, succeeds with the subject term unchanged,  I.e., it tests if
    * `s` applies, but has no effect on the subject term.
    */
  def not(s : Strategy) : Strategy =
    s < (fail + id)

  /**
    * `or(s1, s2)` is similar to `ior(s1, s2)`, but the application
    * of the strategies is only tested.
    */
  def or(s1 : Strategy, s2 : Strategy) : Strategy =
    where(s1) < (attempt(test(s2)) + test(s2))

  /**
    * Construct a strategy that applies `s` repeatedly to subterms
    * until it fails on all of them.
    */
  def reduce(s : Strategy) : Strategy = {
    lazy val inner : Strategy = some(inner) + s
    repeat(inner)
  }

  /**
    * Construct a strategy that applies `s` repeatedly until it fails.
    */
  def repeat(s : Strategy) : Strategy = {
    lazy val result : Strategy = attempt(s <* result)
    mkStrategy(result)
  }

  /**
    * Construct a strategy that applies `s` repeatedly exactly `n` times. If
    * `s` fails at some point during the n applications, the entire strategy
    * fails. The result of the strategy is that of the ''nth'' application of
    * `s`.
    */
  def repeat(s : Strategy, n : Int) : Strategy = {
    lazy val result = if (n == 0) id else s <* repeat(s, n - 1)
    mkStrategy(result)
  }

  /**
    * Construct a strategy that tests whether strategy `s` succeeds,
    * restoring the original term on success.  A synonym for `where`.
    */
  def test(s : Strategy) : Strategy =
    where(s)

  /**
    * Construct a strategy that applies `s` in a top-down, prefix fashion
    * to the subject term.
    */
  def topdown(s : Strategy) : Strategy = {
    lazy val result : Strategy = s <* all(result)
    mkStrategy(result)
  }

  /**
    * Construct a strategy that tests whether strategy `s` succeeds,
    * restoring the original term on success.  This is similar
    * to Stratego's `where`, except that in this version any effects on
    * bindings are not visible outside `s`.
    */
  def where(s : Strategy) : Strategy =
    strategyf(t => (s <* build(t))(t))

  // Queries below here

  /**
    * Collect query results in a Iterable collection.  Run the function
    * `f` as a top-down left-to-right query on the subject term.  Each
    * application of `f` returns a single value. All of these values are
    * accumulated in the collection.
    */
  def collect[CC[X] <: Iterable[X], U](f : Any ==> U)(implicit cbf : Factory[U, CC[U]]) : Any => CC[U] =
    (t : Any) => {
      val b = newBuilder(cbf)
      val add = (u : U) => { b += u; () }
      (everywhere(query(f andThen add)))(t)
      b.result()
    }

  /**
    * Count function results.  Run the function `f` as a top-down query on
    * the subject term.  Sum the integer values returned by `f` from all
    * applications.
    */
  def count(f : Any ==> Int) : Any => Int =
    everything(0)(_ + _)(f)

  /**
    * Apply the function at every term in `t` in a top-down, left-to-right order.
    * Collect the resulting `T` values by accumulating them using `f` with initial
    * left value `v`.  Return the final value of the accumulation.
    */
  def everything[T](v : T)(f : (T, T) => T)(g : Any ==> T)(t : Any) : T = {
    val collector = collect[List, T](g)
    collector(t).foldLeft(v)(f)
  }

}

/**
  * Strategy-based term rewriting for arbitrary terms.
  */
object Rewriter extends Rewriter
