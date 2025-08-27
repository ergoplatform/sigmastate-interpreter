/**
 * Author: Alexander Slesarenko
 * Date: 7/25/12
 */
package sigma.compiler.ir.primitives

import sigma.compiler.ir.{Base, IRContext}
import sigma.data.AVHashMap

import scala.language.implicitConversions

/** Slice in [[IRContext]] cake with definitions of pairs and tuples. */
trait Tuples extends Base { self: IRContext =>
  object Pair {
    def apply[A, B](a: Ref[A], b: Ref[B]) = zipPair[A, B]((a, b))
    def unapply[A, B](p: Ref[(A, B)]) = Some(unzipPair[A, B](p))
  }

  object IsPair {
    def unapply[A,B](s: Sym): Option[Ref[(A,B)]] = s.elem match {
      case pe: PairElem[_,_] => Some(s.asInstanceOf[Ref[(A,B)]])
      case _ => None
    }
  }

  implicit class ListOps[A, B](t: Ref[(A, B)]) {
    def head: Ref[A] = { val Pair(x, _) = t; x }
    def tail: Ref[B] = { val Pair(_, x) = t; x }
  }

  implicit class TupleOps2[A, B](t: Ref[(A, B)]) {
    def _1: Ref[A] = { val Pair(x, _) = t; x }
    def _2: Ref[B] = { val Pair(_, x) = t; x }
  }

  implicit class TupleOps3[A,B,C](t: Ref[(A,(B,C))]) {
    def _1: Ref[A] = { val Pair(x, _) = t; x }
    def _2: Ref[B] = { val Pair(_, Pair(x, _)) = t; x }
    def _3: Ref[C] = { val Pair(_, Pair(_, x)) = t; x }
  }

  implicit class TupleOps4[A,B,C,D](t: Ref[(A,(B,(C,D)))]) {
    def _1: Ref[A] = { val Pair(x, _) = t; x }
    def _2: Ref[B] = { val Pair(_, Pair(x, _)) = t; x }
    def _3: Ref[C] = { val Pair(_, Pair(_, Pair(x, _))) = t; x }
    def _4: Ref[D] = { val Pair(_, Pair(_, Pair(_, x))) = t; x }
  }

  implicit class TupleOps5[A,B,C,D,E](t: Ref[(A,(B,(C,(D,E))))]) {
    def _1: Ref[A] = { val Pair(x, _) = t; x }
    def _2: Ref[B] = { val Pair(_, Pair(x, _)) = t; x }
    def _3: Ref[C] = { val Pair(_, Pair(_, Pair(x, _))) = t; x }
    def _4: Ref[D] = { val Pair(_, Pair(_, Pair(_, Pair(x, _)))) = t; x }
    def _5: Ref[E] = { val Pair(_, Pair(_, Pair(_, Pair(_, x)))) = t; x }
  }

  val tuplesCache = AVHashMap[Ref[_], (Ref[_], Ref[_])](1000)

  def unzipPair[A, B](p: Ref[(A, B)]): (Ref[A], Ref[B]) = p.node match {
    case Tup(a, b) => (a, b)
    case _ => p.elem match {
      case pe: PairElem[_, _] =>
        if (cachePairs) {
          if (!tuplesCache.containsKey(p)) {
            tuplesCache.put(p, (First(p), Second(p)))
          }
          tuplesCache(p).asInstanceOf[(Ref[A], Ref[B])]
        }
        else
          (First(p), Second(p))
      case _ =>
        !!!(s"expected Tup[A,B] or Sym with type (A,B) but got ${p.toStringWithDefinition}", p)
    }
  }

  implicit def zipPair[A, B](p: (Ref[A], Ref[B])): Ref[(A, B)] = {
    Tup(p._1, p._2)
  }

  case class Tup[A, B](a: Ref[A], b: Ref[B]) extends Def[(A, B)] {
    implicit val eA: Elem[A] = a.elem
    implicit val eB: Elem[B] = b.elem
    assert(null != eA && null != eB)
    lazy val resultType = element[(A,B)]
    override def transform(t: Transformer): Def[(A, B)] = Tup(t(a), t(b))
  }

  case class First[A, B](pair: Ref[(A, B)]) extends Def[A] {
    val resultType: Elem[A] = pair.elem.eFst
    override def transform(t: Transformer): Def[A] = First(t(pair))
  }

  case class Second[A, B](pair: Ref[(A, B)]) extends Def[B] {
    val resultType: Elem[B] = pair.elem.eSnd
    override def transform(t: Transformer): Def[B] = Second(t(pair))
  }
}
