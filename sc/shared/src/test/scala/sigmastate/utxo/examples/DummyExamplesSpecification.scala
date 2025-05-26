package sigmastate.utxo.examples

import org.ergoplatform.ErgoBox
import org.ergoplatform.dsl.{ContractSpec, SigmaContractSyntax, StdContracts, TestContractSpec}
import scorex.crypto.hash
import sigma.ast.{ByteArrayConstant, CollectionConstant, SByte, SCollection}
import sigmastate.helpers.CompilerTestingCommons
import sigma.{Box, Coll, Context}

class DummyExamplesSpecification extends CompilerTestingCommons { suite =>
  implicit lazy val IR = new TestingIRContext

  private val reg1 = ErgoBox.nonMandatoryRegisters(0)

  case class DummyContract[Spec <: ContractSpec]
    (alice: Spec#ProvingParty)(implicit val spec: Spec) extends SigmaContractSyntax with StdContracts {
    def pkA = alice.pubKey
    lazy val contractEnv = Env("pkA" -> pkA)

    lazy val prop = proposition("dummyContract", { CONTEXT: Context =>
      import CONTEXT._
      val oldRoot = SELF.R4[Coll[Byte]].get
      val oldItem: Coll[Byte] = getVar[Coll[Byte]](1).get
      val newItem: Coll[Byte] = getVar[Coll[Byte]](2).get
      val newRoot = OUTPUTS(0).R4[Coll[Byte]].get
      val path = getVar[Coll[Coll[Byte]]](0).get

      def xor(a: (Coll[Byte], Coll[Byte])):Coll[Byte] = {
        a._1.zip(a._2).map({ (c: (Byte, Byte)) => (c._1 ^ c._2).toByte })
      }

      def getRoot(item: Coll[Byte]):Coll[Byte] = path.foldLeft(item, xor)
      pkA && sigmaProp(getRoot(oldItem) == oldRoot) && sigmaProp(getRoot(newItem) == newRoot)

      val dummy = Coll[Byte]()
      val isValid = {(b:Box) => b.propositionBytes == dummy}
      val x = OUTPUTS.foldLeft(0L, {(x:(Long, Box)) => if (isValid(x._2)) x._1 + x._2.value else x._1 }) // works

      val f = {(x:(Long, Box)) => if (isValid(x._2)) x._1 + x._2.value else x._1 }
      OUTPUTS.foldLeft(0L, f) // error
      //OUTPUTS.foldLeft(0L, f) // error
      ???
    },
      """{
        |      val oldRoot = SELF.R4[Coll[Byte]].get
        |      val oldItem: Coll[Byte] = getVar[Coll[Byte]](1).get
        |      val newItem: Coll[Byte] = getVar[Coll[Byte]](2).get
        |      val newRoot = OUTPUTS(0).R4[Coll[Byte]].get
        |      val path = getVar[Coll[Coll[Byte]]](0).get
        |
        |      val isFee = {(out:Box) => out.propositionBytes == pkA.propBytes}
        |
        |      //val accumulate = {(x:Long, b:Box) => if (isFee(b)) x + b.value else x }
        |      val totalFee = OUTPUTS.fold(0L, {(x:Long, b:Box) => if (isFee(b)) x + b.value else x })
        |
        |      val dummy = Coll[Byte]()
        |      val isValid = {(b:Box) => b.propositionBytes == dummy}
        |      val x = OUTPUTS.fold(0L, {(x:Long, b:Box) => if (isValid(b)) x + b.value else x }) // works
        |      //val f1 = {(x:(Long, Box)) => if (isValid(x._2)) x._1 + x._2.value else x._1 }
        |      val f2 = {(x:Long, b:Box) => if (isValid(b)) x + b.value else x } // error
        |      val y = OUTPUTS.fold(0L, f2) // error
        |
        |      //val totalFee = OUTPUTS.fold(0L, {(x:Long, b:Box) => if (isFee(b)) x + b.value else x })
        |
        |      //val getRoot = {(item: Coll[Byte]) => path.fold(item, {
        |        //(a: (Coll[Byte], Coll[Byte])) =>
        |        //a._1.zip(a._2).map({ (c: (Byte, Byte)) => (c._1 ^ c._2).toByte })
        |      // })}
        |      pkA && totalFee < 100 //&& getRoot(oldItem) == oldRoot && getRoot(newItem) == newRoot
        |}
      """.stripMargin)

  }
  lazy val spec = TestContractSpec(suite)(new TestingIRContext)
  lazy val alice = spec.ProvingParty("Alice")

  ignore("Dummy contract") {
    val contract = DummyContract[spec.type](alice)(spec)
    import contract.spec._

    val mockTx = candidateBlock(0).newTransaction()

    val dummyBytes:Coll[Byte] = hash.Blake2b256("".getBytes)
    val dummyPath:Coll[Coll[Byte]] = Array(dummyBytes)
    val deposit = mockTx.outBox(100, contract.prop).withRegs(reg1 -> dummyBytes)

    val tx = candidateBlock(10).newTransaction().spending(deposit)

    tx.outBox(70, contract.prop).withRegs(reg1 -> dummyBytes)

    val in = tx.inputs(0)

    val res = in.runDsl(
      Map(
        0.toByte -> CollectionConstant[SCollection[SByte.type]](dummyPath, SCollection(SByte)),
        1.toByte -> ByteArrayConstant(dummyBytes),
        2.toByte -> ByteArrayConstant(dummyBytes)
      )
    )

    val pr = alice.prove(in).get
    contract.verifier.verify(in, pr) shouldBe true
  }
}
