package sigma.util

import sigma.{BaseNestedTests}
import debox.{Set => DSet, Buffer => DBuffer}
import sigma.data.DFunc

class GraphUtilTests extends BaseNestedTests {
  import GraphUtil._

  describe("Collecting dependencies") {
    val graph = Array(
      List(1, 2), // 0
      List(3),    // 1
      List(4),    // 2
      List(5, 6), // 3
      List(6),    // 4
      List(6),    // 5
      List()      // 6
    )

    val neighbours: DFunc[Int, DBuffer[Int]] = new DFunc[Int, DBuffer[Int]] {
      def apply(node: Int) = {
        val ns = DBuffer.empty[Int]
        graph(node) foreach (ns.+=)
        ns
      }
    }

    it("depthFirstSetFrom") {
      depthFirstSetFrom(DBuffer(6))(neighbours) shouldBe (DSet(6))
      depthFirstSetFrom(DBuffer(5))(neighbours) shouldBe (DSet(5, 6))
      depthFirstSetFrom(DBuffer(3))(neighbours) shouldBe (DSet(3, 5, 6))
      depthFirstSetFrom(DBuffer(2))(neighbours) shouldBe (DSet(2, 4, 6))
      depthFirstSetFrom(DBuffer(0))(neighbours) shouldBe (DSet(0, 1, 2, 3, 4, 5, 6))
    }
    it("depthFirstOrderFrom") {
      val succ: DFunc[Int, DBuffer[Int]] = new DFunc[Int, DBuffer[Int]] {
        def apply(id: Int) = DBuffer(graph(id):_*)
      }
      depthFirstOrderFrom(DBuffer(6), succ) shouldBe (DBuffer(6))
      depthFirstOrderFrom(DBuffer(5), succ) shouldBe (DBuffer(6, 5))
      depthFirstOrderFrom(DBuffer(3), succ) shouldBe (DBuffer(6, 5, 3))
      depthFirstOrderFrom(DBuffer(2), succ) shouldBe (DBuffer(6, 4, 2))
      depthFirstOrderFrom(DBuffer(0), succ) shouldBe (DBuffer(6, 5, 3, 1, 4, 2, 0))
    }
  }
}
