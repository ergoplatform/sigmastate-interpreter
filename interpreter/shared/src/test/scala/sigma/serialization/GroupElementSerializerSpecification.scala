package sigma.serialization

import sigma.crypto.{CryptoConstants, CryptoFacade}
import sigma.util.Extensions.{EcpOps, GroupElementOps}

class GroupElementSerializerSpecification extends SerializationSpecification {

  property("Identity point serialization") {
    val group = CryptoConstants.dlogGroup
    val identity = group.identity

    val bytes = GroupElementSerializer.toBytes(identity)
    bytes.length shouldBe CryptoConstants.EncodedGroupElementLength
    bytes.forall(_ == 0) shouldBe true
    GroupElementSerializer.parse(SigmaSerializer.startReader(bytes, 0)).toGroupElement.isIdentity shouldBe true
  }

  property("point roundtrip") {
    forAll(groupElementConstGen){ge =>
      val bytes = GroupElementSerializer.toBytes(ge.value.toECPoint)
      bytes.length shouldBe CryptoConstants.EncodedGroupElementLength
      val restored = GroupElementSerializer.parse(SigmaSerializer.startReader(bytes, 0))
      CryptoFacade.getAffineXCoord(CryptoFacade.normalizePoint(restored)) shouldBe
        CryptoFacade.getAffineXCoord(CryptoFacade.normalizePoint(ge.value.toECPoint))
      CryptoFacade.getAffineYCoord(CryptoFacade.normalizePoint(restored)) shouldBe
        CryptoFacade.getAffineYCoord(CryptoFacade.normalizePoint(ge.value.toECPoint))
    }
  }
}
