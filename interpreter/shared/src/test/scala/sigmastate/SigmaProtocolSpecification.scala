package sigmastate

import sigmastate.crypto.VerifierMessage.Challenge
import sigmastate.crypto.{GF2_192, GF2_192_Poly}
import sigma.SigmaTestingData

class SigmaProtocolSpecification extends SigmaTestingData {

  property("CThresholdUncheckedNode equality") {
    val c1 = Challenge @@ Coll[Byte](1)
    val c2 = Challenge @@ Coll[Byte](2)
    val n0 = CThresholdUncheckedNode(c1, Seq(), 0, None)
    val n1 = CThresholdUncheckedNode(c1, Seq(), 0, None)
    val n2 = CThresholdUncheckedNode(c2, Seq(), 0, None)
    val n3 = CThresholdUncheckedNode(c2, Seq(n1), 0, None)
    val n4 = CThresholdUncheckedNode(c2, Seq(n1), 1, None)
    val p = GF2_192_Poly.interpolate(new Array[Byte](0), new Array[GF2_192](0), new GF2_192(0))
    val n5 = CThresholdUncheckedNode(c2, Seq(n1), 1, Some(p))

    assertResult(true)(n0 == n1)
    assertResult(true)(n1 != n2)
    assertResult(true)(n2 != n3)
    assertResult(true)(n3 != n4)
    assertResult(true)(n4 != n5)
  }

}
