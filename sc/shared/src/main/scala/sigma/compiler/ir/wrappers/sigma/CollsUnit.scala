package sigma.compiler.ir.wrappers.sigma

import sigma.compiler.ir.{Base, IRContext}

  /** Staged version of collection interfaces which is used in graph-based IR to represent
    * methods of Coll and CollBuilder.
    * Each method of Coll and CollBuilder in Colls corresponds to a method of the original
    * non-staged class [[sigma.Coll]] and [[sigma.CollBuilder]].
    * The semantics of each method is the same as in the original class, please look there
    * for details.
    */
  trait Colls extends Base { self: IRContext =>
    trait Coll[A] extends Def[Coll[A]] {
      implicit def eA: Elem[A];
      def length: Ref[Int];
      def apply(i: Ref[Int]): Ref[A];
      def getOrElse(index: Ref[Int], default: Ref[A]): Ref[A];
      def map[B](f: Ref[scala.Function1[A, B]]): Ref[Coll[B]];
      def zip[B](ys: Ref[Coll[B]]): Ref[Coll[scala.Tuple2[A, B]]];
      def exists(p: Ref[scala.Function1[A, Boolean]]): Ref[Boolean];
      def forall(p: Ref[scala.Function1[A, Boolean]]): Ref[Boolean];
      def filter(p: Ref[scala.Function1[A, Boolean]]): Ref[Coll[A]];
      def foldLeft[B](zero: Ref[B], op: Ref[scala.Function1[scala.Tuple2[B, A], B]]): Ref[B];
      def indices: Ref[Coll[Int]];
      def flatMap[B](f: Ref[scala.Function1[A, Coll[B]]]): Ref[Coll[B]];
      def indexOf(elem: Ref[A], from: Ref[Int]): Ref[Int];
      def patch(from: Ref[Int], patch: Ref[Coll[A]], replaced: Ref[Int]): Ref[Coll[A]];
      def updated(index: Ref[Int], elem: Ref[A]): Ref[Coll[A]];
      def updateMany(indexes: Ref[Coll[Int]], values: Ref[Coll[A]]): Ref[Coll[A]];
      def slice(from: Ref[Int], until: Ref[Int]): Ref[Coll[A]];
      def append(other: Ref[Coll[A]]): Ref[Coll[A]];
    };
    trait CollBuilder extends Def[CollBuilder] {
      def fromItems[T](items: Ref[T]*)(implicit cT: Elem[T]): Ref[Coll[T]];
      def xor(left: Ref[Coll[Byte]], right: Ref[Coll[Byte]]): Ref[Coll[Byte]];
      def replicate[T](n: Ref[Int], v: Ref[T]): Ref[Coll[T]];
    };
  }