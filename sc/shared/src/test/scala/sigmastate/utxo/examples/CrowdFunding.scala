package sigmastate.utxo.examples

import sigma.{Context, Box}
import org.ergoplatform.dsl.{SigmaContractSyntax, ContractSpec, StdContracts}

/** @param backer    The party, who wants to fund some amount of Ergs to the project.
  * @param project   The party, who wants to collect at least `minToRaise` Ergs before `deadline`.
  * */
case class CrowdFunding[Spec <: ContractSpec]
    (deadline: Int, minToRaise: Long,
     backer: Spec#ProvingParty,
     project: Spec#ProvingParty)
    (implicit val spec: Spec) extends SigmaContractSyntax with StdContracts
{
  def pkBacker = backer.pubKey
  def pkProject = project.pubKey
  lazy val contractEnv = Env("pkBacker" -> pkBacker, "pkProject" -> pkProject, "deadline" -> deadline, "minToRaise" -> minToRaise)

  lazy val holderProp = proposition("holder", { ctx: Context =>
    import ctx._
    val fundraisingFailure = HEIGHT >= deadline && pkBacker
    val enoughRaised = {(outBox: Box) =>
      outBox.value >= minToRaise &&
          outBox.propositionBytes == pkProject.propBytes
    }
    val fundraisingSuccess = HEIGHT < deadline &&
        pkProject &&
        sigmaProp(OUTPUTS.exists(enoughRaised))

    fundraisingFailure || fundraisingSuccess
  },
  """
   |{
   |  val fundraisingFailure = HEIGHT >= deadline && pkBacker
   |    val enoughRaised = {(outBox: Box) =>
   |      outBox.value >= minToRaise &&
   |          outBox.propositionBytes == pkProject.propBytes
   |    }
   |    val fundraisingSuccess = HEIGHT < deadline &&
   |        pkProject &&
   |        OUTPUTS.exists(enoughRaised)
   |
   |    fundraisingFailure || fundraisingSuccess
   |}
  """.stripMargin)

  lazy val backerSignature  = proposition("backerSignature", _ => pkBacker, "pkBacker")
  lazy val projectSignature = proposition("projectSignature", _ => pkProject, "pkProject")

  lazy val oldProp = proposition("old", { ctx: Context =>
    import ctx._
    val c1 = HEIGHT >= deadline && pkBacker
    val c2 =
      HEIGHT < deadline && pkProject &&
      sigmaProp(OUTPUTS.exists({ (out: Box) =>
        out.value >= minToRaise && out.propositionBytes == pkProject.propBytes
      }))
    c1 || c2
  },
  """
   |{
   |  val c1 = HEIGHT >= deadline && pkBacker
   |  val c2 =
   |    HEIGHT < deadline && pkProject &&
   |    OUTPUTS.exists({ (out: Box) =>
   |      out.value >= minToRaise && out.propositionBytes == pkProject.propBytes
   |    })
   |  c1 || c2
   |}
  """.stripMargin)

}
