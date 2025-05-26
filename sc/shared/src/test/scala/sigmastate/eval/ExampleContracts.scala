package sigmastate.eval

import scalan.BaseCtxTests
import sigma.data.CAnyValue

trait ExampleContracts extends ErgoScriptTestkit { self: BaseCtxTests =>

  val envCF = Map(
    "timeout" -> timeout,
    "minToRaise" -> minToRaise,
    "backerPubKeyId" -> CAnyValue(backerPubKeyId),
    "projectPubKeyId" -> CAnyValue(projectPubKeyId)
  )

  val crowdFundingScript =
    """{
     | //let projectPubKey = proveDlog(project)  //getVar[SigmaProp](projectPubKeyId)
     | //let backerPubKey = proveDlog(backer)  //getVar[SigmaProp](backerPubKeyId)
     | val projectBytes = projectPubKey.propBytes
     | val c1 = HEIGHT >= timeout && backerPubKey
     | val c2 = allOf(Coll(
     |   HEIGHT < timeout,
     |   projectPubKey,
     |   OUTPUTS.exists { (out: Box) =>
     |     out.value >= minToRaise && out.propositionBytes == projectBytes
     |   }
     | ))
     | c1 || c2
     | }
    """.stripMargin

  val demurragePeriod = 100
  val demurrageCost = 2L
  val regScriptId = 1.toByte
  val envDem = Map(
    "demurragePeriod" -> demurragePeriod,
    "demurrageCost" -> demurrageCost,
    "regScriptId" -> regScriptId
  )

  val demurrageScript =
    """{
     | val selfBytes = SELF.propositionBytes
     | val selfValue = SELF.value
     | val c2 = allOf(Coll(
     |   HEIGHT >= SELF.R4[Int].get + demurragePeriod,
     |   OUTPUTS.exists { (out: Box) =>
     |     out.value >= selfValue - demurrageCost && out.propositionBytes == selfBytes
     |   }
     | ))
     | //getVar[SigmaProp](regScriptId) || c2
     | regScript || c2
     | }
    """.stripMargin

}
