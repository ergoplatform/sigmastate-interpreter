package sigma.compiler.ir

import debox.{cfor, Buffer => DBuffer}
import sigma.data.{DFunc, Nullable}
import sigma.util.GraphUtil

import scala.collection.compat.immutable.ArraySeq

/** Defines implementation of [[ProgramGraph]] and related utilities. */
trait ProgramGraphs extends AstGraphs { self: IRContext =>

  type PGraph = ProgramGraph

  /** Deboxed function to obtain usages of a given node.
    * Represents adjacency matrix of the reversed graph `g`.
    * @param  g  original graph whose usages are computed */
  class PGraphUsages(g: AstGraph) extends DFunc[Int, DBuffer[Int]] {
    override def apply(nodeId: Int) = {
      val us = g.usagesOf(nodeId)
      us
    }
  }

  /** Immutable graph collected from `roots` following Ref.node.deps links. */
  case class ProgramGraph(roots: Seq[Sym], mapping: Nullable[Transformer], filterNode: Nullable[Sym => Boolean])
  	  extends AstGraph {
    def this(roots: Seq[Sym], filterNode: Nullable[Sym => Boolean] = Nullable.None) { this(roots, Nullable.None, filterNode) }
    def this(root: Sym) { this(List(root)) }

    override lazy val rootIds: DBuffer[Int] = super.rootIds

    override def boundVars = Nil
    override def isIdentity: Boolean = false
    override def freeVars = ArraySeq.empty[Sym]
    override lazy val scheduleIds = {
      val neighbours: DFunc[Int, DBuffer[Int]] = filterNode match {
        case Nullable(pred) =>
          new DFunc[Int, DBuffer[Int]] { def apply(id: Int) = {
            val deps = getSym(id).node.deps
            val len = deps.length
            val res = DBuffer.ofSize[Int](len)
            cfor(0)(_ < len, _ + 1) { i =>
              val sym = deps(i)
              if (pred(sym) && !sym.isVar)  // TODO optimize: remove isVar condition here and below
                res += sym.node.nodeId
            }
            res
          }}
        case _ =>
          new DFunc[Int, DBuffer[Int]] { def apply(id: Int) = {
            val deps = getSym(id).node.deps
            val len = deps.length
            val res = DBuffer.ofSize[Int](len)
            cfor(0)(_ < len, _ + 1) { i =>
              val sym = deps(i)
              if (!sym.isVar)
                res += sym.node.nodeId
            }
            res
          }}
      }
      val sch = GraphUtil.depthFirstOrderFrom(rootIds, neighbours)
      sch
    }

    override def toString: String = {
      val mappingStr = if (mapping.isEmpty) "None" else mapping.toString
      val filterNodeStr = if (filterNode.isDefined) filterNode.toString else "None"
      s"ProgramGraph($roots, $mappingStr, $filterNodeStr)"
    }
  }

}
