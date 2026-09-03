package sigma.stark

import sigma.BaseTests

/** Differential and ownership checks for allocation-minimal STARK primitive
  * implementations. Oracle KAT coverage remains in the JVM suites; these
  * tests also run on Scala.js and pin the semantics of the optimized paths.
  */
class StarkPrimitiveAllocationSpec extends BaseTests {

  test("Ext4 scalar multiplication matches the schoolbook reduction") {
    val boundary = Array(0, 1, 2, BabyBear.P - 2, BabyBear.P - 1)
    var state = 0x13579bdf

    def nextElem(): Int = {
      state = state * 1103515245 + 12345
      (state & 0x7fffffff) % BabyBear.P
    }

    def reference(a: Ext4, b: Ext4): Ext4 = {
      val left = Array(a.c0, a.c1, a.c2, a.c3)
      val right = Array(b.c0, b.c1, b.c2, b.c3)
      val product = new Array[Int](7)
      var i = 0
      while (i < 4) {
        var j = 0
        while (j < 4) {
          product(i + j) = BabyBear.add(product(i + j), BabyBear.mul(left(i), right(j)))
          j += 1
        }
        i += 1
      }
      var degree = 6
      while (degree >= 4) {
        product(degree - 4) = BabyBear.add(
          product(degree - 4),
          BabyBear.mul(product(degree), Ext4.NBeta))
        degree -= 1
      }
      Ext4(product(0), product(1), product(2), product(3))
    }

    var i = 0
    while (i < boundary.length) {
      var j = 0
      while (j < boundary.length) {
        val a = Ext4(boundary(i), nextElem(), boundary(j), nextElem())
        val b = Ext4(nextElem(), boundary(j), nextElem(), boundary(i))
        (a * b) shouldBe reference(a, b)
        j += 1
      }
      i += 1
    }
  }

  test("Poseidon2 scalar external layer preserves the zero permutation KAT") {
    val cells = new Array[Int](Poseidon2Constants.Cells)
    Poseidon2.mix(cells)
    cells shouldBe Array(
      972705262, 946791486, 1172739502, 607725896, 1443562977, 10371933,
      1256364390, 832646779, 324608513, 1218088384, 1927362941,
      1316083208, 1247749003, 494661501, 219252024, 979706958, 417250331,
      1789792672, 422984860, 1807101920, 1567038995, 1949574701,
      1240162431, 1775282439)
  }

  test("Poseidon2 hashPair equals a one-block unpadded hash and owns its output") {
    val left = Array.tabulate(Poseidon2.CellsOut)(i => i * 7919 + 17)
    val right = Array.tabulate(Poseidon2.CellsOut)(i => BabyBear.P - 1 - i * 3571)
    val leftBefore = left.clone()
    val rightBefore = right.clone()
    val expected = Poseidon2.unpaddedHash(left ++ right)
    val actual = Poseidon2.hashPair(left, right)

    actual shouldBe expected
    left shouldBe leftBefore
    right shouldBe rightBefore
    actual(0) ^= 1
    Poseidon2.hashPair(left, right) shouldBe expected
  }

  test("ReadIop direct canonical conversion preserves cursor and ownership semantics") {
    val canonical = Array(0, 1, BabyBear.P - 1)
    val raw = canonical.map(BabyBear.toRaw)
    val proof = raw ++ Array(BabyBear.P, 7)
    val iop = new ReadIop(proof)

    val decoded = iop.readFieldElemSlice(canonical.length).get
    decoded shouldBe canonical
    decoded(0) = 42
    proof(0) shouldBe raw(0)

    // A complete invalid slice is consumed before checked-cast rejection,
    // matching the former readU32s(...).flatMap implementation.
    iop.readFieldElemSlice(1) shouldBe None
    iop.remaining shouldBe 1
    iop.readU32s(1).get shouldBe Array(7)
    iop.verifyComplete shouldBe true
  }

  test("ReadIop single-digest specialization returns an owned raw copy") {
    val proof = Array.tabulate(Poseidon2.CellsOut)(i => i * 13 + 5)
    val iop = new ReadIop(proof)
    val digest = iop.readDigestRaw().get
    digest shouldBe proof
    digest(0) ^= 1
    proof(0) shouldBe 5
    iop.verifyComplete shouldBe true
    iop.readDigestRaw() shouldBe None
  }
}
