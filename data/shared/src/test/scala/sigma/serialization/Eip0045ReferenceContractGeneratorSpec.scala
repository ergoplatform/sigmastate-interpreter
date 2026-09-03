package sigma.serialization

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base16
import sigma.VersionContext
import sigma.ast.ErgoTree.ZeroHeader
import sigma.ast.SCollection.SByteArray
import sigma.ast._
import sigma.serialization.ErgoTreeSerializer.DefaultSerializer

/** Canonical source for the EIP-0045 candidate B4 foundation proposition bytes. */
object Eip0045ReferenceContract {
  val TreeVersion: Byte = 4

  val PropositionHex: String =
    "1c53020e209490a07414919c7eca0176d4ff9614523beecc8746ae7ffd4916f29b2edb9fe5" +
    "0e2023c4a123ffb33a1c8db89436fe0e7972bd8e4e289459ee5fd71be5440607d383" +
    "d1b9e4e3001ae4e3010e73007301"

  val ContractIdHex: String =
    "f3582418f41ba6920c83758e56ac4475bf6084a039f91a762388e774258a6c61"

  private val ProgramIdBytes: Array[Byte] = Base16.decode(
    "9490a07414919c7eca0176d4ff9614523beecc8746ae7ffd4916f29b2edb9fe5").get

  private val ProfileIdBytes: Array[Byte] = Base16.decode(
    "23c4a123ffb33a1c8db89436fe0e7972bd8e4e289459ee5fd71be5440607d383").get

  def programId: Array[Byte] = ProgramIdBytes.clone()

  def profileId: Array[Byte] = ProfileIdBytes.clone()

  def buildTree(): ErgoTree = {
    val proofChunks = OptionGet(
      GetVar(0.toByte, SCollection(SByteArray)))
    val applicationPayload = OptionGet(
      GetVar(1.toByte, SByteArray))
    val proposition = VerifyStark(
      proofChunks,
      applicationPayload,
      ByteArrayConstant(programId),
      ByteArrayConstant(profileId)
    ).toSigmaProp
    val header = ErgoTree.headerWithVersion(ZeroHeader, TreeVersion)
    ErgoTree.withSegregation(header, proposition)
  }

  def propositionBytes: Array[Byte] =
    DefaultSerializer.serializeErgoTree(buildTree())

  def contractId: Array[Byte] = Blake2b256(propositionBytes)
}

/**
 * Emits the canonical proposition bytes as Base16 so a JVM test run can be
 * captured by the external, create-only B4 artifact builder.
 */
object Eip0045ReferenceContractGenerator {
  def main(args: Array[String]): Unit = {
    require(args.isEmpty, "this generator takes no arguments")
    val bytes = Eip0045ReferenceContract.propositionBytes
    println(s"EIP0045_REFERENCE_PROPOSITION_LENGTH=${bytes.length}")
    println(s"EIP0045_REFERENCE_PROPOSITION_HEX=${Base16.encode(bytes)}")
    println(s"EIP0045_REFERENCE_CONTRACT_ID=${Base16.encode(Blake2b256(bytes))}")
  }
}

class Eip0045ReferenceContractGeneratorSpec extends AnyPropSpec with Matchers {
  property("reference contract has the exact v4 segregated four-child shape") {
    val tree = Eip0045ReferenceContract.buildTree()

    tree.version shouldBe Eip0045ReferenceContract.TreeVersion
    tree.hasSize shouldBe true
    tree.isConstantSegregation shouldBe true
    tree.constants shouldBe IndexedSeq(
      ByteArrayConstant(Eip0045ReferenceContract.programId),
      ByteArrayConstant(Eip0045ReferenceContract.profileId))

    tree.root.right.get shouldBe BoolToSigmaProp(VerifyStark(
      OptionGet(GetVar(0.toByte, SCollection(SByteArray))),
      OptionGet(GetVar(1.toByte, SByteArray)),
      ConstantPlaceholder(0, SByteArray),
      ConstantPlaceholder(1, SByteArray)))
  }

  property("serialize-deserialize-serialize is byte exact under v4") {
    val bytes = Eip0045ReferenceContract.propositionBytes
    val reparsed = VersionContext.withVersions(
      Eip0045ReferenceContract.TreeVersion,
      Eip0045ReferenceContract.TreeVersion) {
      DefaultSerializer.deserializeErgoTree(bytes)
    }
    val reserialized = DefaultSerializer.serializeErgoTree(reparsed)

    reparsed.isRightParsed shouldBe true
    reserialized shouldBe bytes
    Blake2b256(reserialized) shouldBe Eip0045ReferenceContract.contractId
  }

  property("canonical proposition bytes and contract id match the frozen golden values") {
    val bytes = Eip0045ReferenceContract.propositionBytes

    Base16.encode(bytes) shouldBe Eip0045ReferenceContract.PropositionHex
    Base16.encode(Blake2b256(bytes)) shouldBe Eip0045ReferenceContract.ContractIdHex
  }

  property("callers cannot mutate the frozen identifier sources") {
    val programId = Eip0045ReferenceContract.programId
    val profileId = Eip0045ReferenceContract.profileId
    programId(0) = (programId(0) ^ 1).toByte
    profileId(0) = (profileId(0) ^ 1).toByte

    Base16.encode(Eip0045ReferenceContract.propositionBytes) shouldBe
      Eip0045ReferenceContract.PropositionHex
  }
}
