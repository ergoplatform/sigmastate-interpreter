package org.ergoplatform.sdk.utils

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sigma.ast.ErgoTree.HeaderType

class ErgoTreeUtilsSpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers {
  property("explainTreeHeader") {
    ErgoTreeUtils.explainTreeHeader(HeaderType @@ 26.toByte) shouldBe
      """|
         |Header: Ox1a (26)
         |Bit 0: 0 \
         |Bit 1: 1	-- ErgoTree version 2
         |Bit 2: 0 /
         |Bit 3: 1 	-- size of the whole tree is serialized after the header byte
         |Bit 4: 1 	-- constant segregation is used for this ErgoTree
         |Bit 5: 0 	-- reserved (should be 0)
         |Bit 6: 0 	-- reserved (should be 0)
         |Bit 7: 0 	-- header contains more than 1 byte (default == 0)
         |""".stripMargin
  }
}
