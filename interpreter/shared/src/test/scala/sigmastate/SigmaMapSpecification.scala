package sigmastate

import sigma.ast.IntConstant
import sigma.interpreter.SigmaMap
import sigmastate.helpers.TestingCommons

class SigmaMapSpecification extends TestingCommons {
  property("SigmaMap keys ordering test") {
    val map1 = Map(1.toByte -> IntConstant(1))
    val smap1 = SigmaMap(map1)

    val map2 = Map(1.toByte -> IntConstant(1), 2.toByte -> IntConstant(2))
    val smap2 = SigmaMap(map2)

    val map3 = Map(1.toByte -> IntConstant(1), 2.toByte -> IntConstant(2), 3.toByte -> IntConstant(3))
    val smap3 = SigmaMap(map3)

    val map4 = Map(1.toByte -> IntConstant(1), 2.toByte -> IntConstant(2), 3.toByte -> IntConstant(3), 4.toByte -> IntConstant(4))
    val smap4 = SigmaMap(map4)

    val res1 = map1.keys.iterator.toArray
    val sres1 = smap1.keysIterator.toArray
    val res2 = map2.keys.iterator.toArray
    val sres2 = smap2.keysIterator.toArray
    val res3 = map3.keys.iterator.toArray
    val sres3 = smap3.keysIterator.toArray
    val res4 = map4.keys.iterator.toArray
    val sres4 = smap4.keysIterator.toArray

    res1(0) == sres1(0) &&
    res2(0) == sres2(0) && res2(1) == sres2(1) &&
    res3(0) == sres3(0) && res3(1) == sres3(1) && res3(2) == sres3(2) &&
    res4(0) == sres4(0) && res4(1) == sres4(1) && res4(2) == sres4(2) && res4(3) == sres4(3)
  }

  property("SigmaMap values ordering test") {
    val map1 = Map(1.toByte -> IntConstant(1))
    val smap1 = SigmaMap(map1)

    val map2 = Map(1.toByte -> IntConstant(1), 2.toByte -> IntConstant(2))
    val smap2 = SigmaMap(map2)

    val map3 = Map(1.toByte -> IntConstant(1), 2.toByte -> IntConstant(2), 3.toByte -> IntConstant(3))
    val smap3 = SigmaMap(map3)

    val map4 = Map(1.toByte -> IntConstant(1), 2.toByte -> IntConstant(2), 3.toByte -> IntConstant(3), 4.toByte -> IntConstant(4))
    val smap4 = SigmaMap(map4)

    val res1 = map1.iterator.map(x => x._2).toArray
    val sres1 = smap1.iterator.map(x => x._2).toArray
    val res2 = map2.iterator.map(x => x._2).toArray
    val sres2 = smap2.iterator.map(x => x._2).toArray
    val res3 = map3.iterator.map(x => x._2).toArray
    val sres3 = smap3.iterator.map(x => x._2).toArray
    val res4 = map4.iterator.map(x => x._2).toArray
    val sres4 = smap4.iterator.map(x => x._2).toArray

    res1(0) == sres1(0) &&
      res2(0) == sres2(0) && res2(1) == sres2(1) &&
      res3(0) == sres3(0) && res3(1) == sres3(1) && res3(2) == sres3(2) &&
      res4(0) == sres4(0) && res4(1) == sres4(1) && res4(2) == sres4(2) && res4(3) == sres4(3)
  }
}
