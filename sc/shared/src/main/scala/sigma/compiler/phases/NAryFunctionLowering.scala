package sigma.compiler.phases

import sigma.VersionContext
import sigma.ast._
import sigma.ast.syntax._
import sigma.exceptions.BuilderException
import sigma.kiama.rewriting.Rewriter.{everywherebu, rewrite, rule}

import java.util.concurrent.atomic.AtomicInteger

/** Desugars n-ary functions and applications into single-argument form over
  * right-nested pairs.
  *
  * ErgoTree's runtime supports only single-argument [[FuncValue]]s and only
  * 2-element [[Tuple]]s / [[SelectField]]s. To allow ErgoScript users to write
  * `def f(a, b, c) = ...; f(x, y, z)` we lower:
  *
  *  - n-ary lambda definitions into a 1-arg lambda taking a right-nested pair,
  *    with a [[Block]] that re-binds each parameter to a [[SelectField]] chain;
  *  - n-ary applications into a 1-arg application whose single argument is the
  *    right-nested pair built from the original arguments.
  *
  * Example (n=3):
  * {{{
  *   Lambda([(a, A), (b, B), (c, C)], R, body)
  * }}}
  * is rewritten to:
  * {{{
  *   Lambda([($tup, (A, (B, C)))], R,
  *     Block(
  *       Val(a, A, SelectField($tup, 1)),
  *       Val(b, B, SelectField(SelectField($tup, 2), 1)),
  *       Val(c, C, SelectField(SelectField($tup, 2), 2)),
  *       body
  *     )
  *   )
  * }}}
  *
  * Identifier references in the body are left untouched — the inserted
  * [[Block]] reintroduces the same names, so lexical scoping and inner-lambda
  * shadowing keep working.
  */
object NAryFunctionLowering {

  private val freshNamePrefix = "$tup"

  /** Lower n-ary lambdas and applications in `expr` to their single-argument
    * tuple-based equivalents. Other nodes are returned unchanged.
    */
  def lower(expr: SValue): SValue = {
    val counter = new AtomicInteger(0)
    def freshName(): String = freshNamePrefix + counter.getAndIncrement()

    val r = rule[Any]({
      case lam @ Lambda(tparams, args, givenResType, Some(body)) if args.length >= 2 =>
        val tupName = freshName()
        val tupTpe = nestPairType(args.map(_._2))
        val tupRef = Ident(tupName, tupTpe)
        val bindings: Seq[Val] = args.zipWithIndex.map { case ((name, tpe), i) =>
          ValNode(name, tpe, projectAt(tupRef, i, args.length))
        }
        val newBody = Block(bindings, body)
        Lambda(tparams, IndexedSeq((tupName, tupTpe)), givenResType, Some(newBody))
          .withPropagatedSrcCtx(lam.sourceContext)

      case app @ Apply(f, args) if args.length >= 2 && f.tpe.isFunc =>
        if (!VersionContext.current.isV6Activated) {
          throw new BuilderException(
            "n-ary user-defined functions require ErgoTree v6+ " +
              "(activated script version 3 or later)",
            app.sourceContext.toOption)
        }
        Apply(f, IndexedSeq(nestPairValue(args)))
          .withPropagatedSrcCtx(app.sourceContext)
    })

    rewrite(everywherebu(r))(expr).asInstanceOf[SValue]
  }

  /** Right-nested pair type for `types`. Pre: `types.length >= 1`. */
  private[phases] def nestPairType(types: IndexedSeq[SType]): SType = {
    require(types.nonEmpty, "nestPairType requires at least one type")
    types.reduceRight((t, acc) => STuple(t, acc))
  }

  /** Right-nested pair value for `items`. Pre: `items.length >= 1`. */
  private[phases] def nestPairValue(items: IndexedSeq[SValue]): SValue = {
    require(items.nonEmpty, "nestPairValue requires at least one item")
    items.reduceRight((v, acc) => Tuple(IndexedSeq(v, acc)))
  }

  /** SelectField chain projecting the `index`-th of `count` items out of the
    * right-nested pair `root`. Indices are 0-based; `count >= 2`.
    *
    * For (A, (B, (C, D))):
    *   index=0 → root._1
    *   index=1 → root._2._1
    *   index=2 → root._2._2._1
    *   index=3 → root._2._2._2
    *
    * Walk `_2` `min(index, count-2)` times into the nested pair, then pick
    * `_2` if `index` is the last element, otherwise `_1`.
    */
  private[phases] def projectAt(root: SValue, index: Int, count: Int): SValue = {
    require(count >= 2, "projectAt expects nested pair (count >= 2)")
    require(0 <= index && index < count, s"index $index out of bounds for count $count")
    val walks = math.min(index, count - 2)
    var node: Value[STuple] = root.asInstanceOf[Value[STuple]]
    var i = 0
    while (i < walks) {
      node = SelectField(node, 2.toByte).asInstanceOf[Value[STuple]]
      i += 1
    }
    val fieldIdx: Byte = if (index == count - 1) 2 else 1
    SelectField(node, fieldIdx)
  }
}
