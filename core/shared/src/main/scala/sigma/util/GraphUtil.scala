package sigma.util

import sigma.data.DFunc
import debox.{Set => DSet, Buffer => DBuffer}
import debox.cfor
import scala.reflect.ClassTag

object GraphUtil {

  /** Build a set of reachable nodes starting form the given nodes and following
    * the given neighbours relations. Depth first search algorithm is used.
    * @tparam A type of value representing node, should implement equality which is used in debox.Set
    * @param  starts     starting nodes for the search
    * @param  neighbours a function representing the adjacency matrix of the graph
    * @return   Set of nodes reachable from the `starts` including `starts` themselves
    */
  def depthFirstSetFrom[@specialized(Int) A: ClassTag](starts: DBuffer[A])(neighbours: DFunc[A, DBuffer[A]]): DSet[A] = {
    val visited = DSet.ofSize[A](starts.length)

    def visit(s: A): Unit = {
      if (!(visited(s))) {
        visited += s
        val ns = neighbours(s)
        cfor(0)(_ < ns.length, _ + 1) { i =>
          visit(ns(i))
        }
      }
    }

   cfor(0)(_ < starts.length, _ + 1) { i =>
     visit(starts(i))
   }
   visited
 }

  /** Collect and topologically order all reachable nodes starting form the given nodes and following
    * the given neighbours relations. Depth first search algorithm is used.
    * @tparam  A          type of value representing node, should implement equality which is used in debox.Set
    * @param   starts     starting nodes for the search
    * @param   neighbours a function representing the adjacency matrix of the graph
    * @return  Topologically ordered sequence of nodes reachable from the `starts` including `starts` themselves
    */
  def depthFirstOrderFrom[@specialized(Int) A: ClassTag](starts: DBuffer[A], neighbours: DFunc[A, DBuffer[A]]): DBuffer[A] = {
    val visited = DSet.ofSize[A](starts.length)
    val res = DBuffer.ofSize[A](starts.length)
    def visit(s: A): Unit = {
      if (!(visited(s))) {
        visited += s
        // first visit all deps recursively
        val ns = neighbours(s)
        cfor(0)(_ < ns.length, _ + 1) { i =>
          visit(ns(i))
        }
        // then append this node to result
        res += s
      }
    }

    val len = starts.length
    cfor(0)(_ < len, _ + 1) { i =>
      visit(starts(i))
    }
    res
  }

}



