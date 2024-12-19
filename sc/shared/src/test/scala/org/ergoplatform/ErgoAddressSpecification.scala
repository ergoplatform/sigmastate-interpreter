package org.ergoplatform

import org.ergoplatform.ErgoAddressEncoder.{MainnetNetworkPrefix, TestnetNetworkPrefix, hash256}
import org.scalatest.propspec.AnyPropSpecLike
import org.scalatest.{Assertion, TryValues}
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base58
import sigma.ast.{ByteArrayConstant, Constant, ErgoTree, IntConstant, SigmaAnd, UnparsedErgoTree}
import sigmastate.crypto.DLogProtocol.DLogProverInput
import sigmastate.helpers.TestingHelpers._
import sigmastate.helpers._
import sigma.interpreter.ContextExtension.VarBinding
import sigma.crypto.CryptoConstants.dlogGroup
import sigmastate.interpreter.Interpreter.{ScriptEnv, ScriptNameProp}
import sigma.ast.syntax.ValueOps
import sigma.serialization.ErgoTreeSerializer.DefaultSerializer
import sigma.serialization.ValueSerializer
import sigmastate.utils.Helpers._
import sigmastate.CompilerCrossVersionProps
import sigma.{SigmaDslTesting, VersionContext}
import sigma.ast.ErgoTree.{ZeroHeader, setConstantSegregation}
import sigma.ast.SType
import sigma.data.ProveDlog
import sigma.exceptions.{CostLimitException, InvalidType}
import sigma.interpreter.{ContextExtension, CostedProverResult}
import sigma.serialization.GroupElementSerializer
import sigma.validation.ValidationException
import sigma.validation.ValidationRules.CheckTypeCode

import java.math.BigInteger

class ErgoAddressSpecification extends SigmaDslTesting
  with TryValues with CompilerCrossVersionProps with AnyPropSpecLike {

  private implicit val ergoAddressEncoder: ErgoAddressEncoder =
    new ErgoAddressEncoder(TestnetNetworkPrefix)

  def addressRoundtrip(addr: ErgoAddress): Assertion = {
    val addrStr = ergoAddressEncoder.toString(addr)
    val parsedAddr = ergoAddressEncoder.fromString(addrStr).get
    parsedAddr shouldBe addr
    ergoAddressEncoder.toString(parsedAddr) shouldBe addrStr
  }

  property("P2PK roundtrip") {
    forAll(proveDlogGen) { pk =>
      addressRoundtrip(P2PKAddress(pk))
    }
  }

  property("SHA roundtrip") {
    forAll(proveDlogGen) { pk =>
      addressRoundtrip(Pay2SHAddress(ErgoTree.fromSigmaBoolean(ergoTreeHeaderInTests, pk)))
    }
  }

  property("SA roundtrip") {
    forAll(proveDlogGen) { pk =>
      addressRoundtrip(Pay2SAddress(ErgoTree.fromSigmaBoolean(ergoTreeHeaderInTests, pk)))
    }
  }

  property("P2SH proper bytes to track") {
    forAll(proveDlogGen) { pk =>
      val p2sh = Pay2SHAddress(ErgoTree.fromSigmaBoolean(ergoTreeHeaderInTests, pk))

      //search we're doing to find a box potentially corresponding to some address
      DefaultSerializer.serializeErgoTree(p2sh.script).containsSlice(p2sh.contentBytes) shouldBe true
    }
  }

  property("P2S proper bytes to track") {
    forAll(proveDlogGen) { pk =>
      val p2s = Pay2SAddress(ErgoTree.fromSigmaBoolean(ergoTreeHeaderInTests, pk))

      //search we're doing to find a box potentially corresponding to some address
      DefaultSerializer.serializeErgoTree(p2s.script).containsSlice(p2s.contentBytes) shouldBe true
    }
  }

  def testFromProposition(scriptVersion: Byte,
                          expectedP2S: String, expectedP2SH: String, expectedP2PK: String) = {
    val pk: ProveDlog = DLogProverInput(BigInteger.ONE).publicImage
    val pk10: ProveDlog = DLogProverInput(BigInteger.TEN).publicImage

    val p2s: Pay2SAddress = Pay2SAddress(
      ErgoTree.fromProposition(
        ErgoTree.headerWithVersion(ZeroHeader, scriptVersion),
        SigmaAnd(pk, pk10)))
    val p2sh: Pay2SHAddress = Pay2SHAddress(pk)
    val p2pk: P2PKAddress = P2PKAddress(pk)

    p2s.toString shouldBe expectedP2S
    p2sh.toString shouldBe expectedP2SH
    p2pk.toString shouldBe expectedP2PK

    assertResult(true)(p2s != p2sh && p2sh != p2s)
    assertResult(true)(p2sh != p2pk && p2pk != p2sh)
    assertResult(true)(p2pk != p2s && p2s != p2pk)

    val parsed_p2s = ergoAddressEncoder.fromProposition(p2s.script).success.value
    assertResult(false)(parsed_p2s eq p2s)
    assertResult(true)(parsed_p2s == p2s && p2s == parsed_p2s)
    parsed_p2s.hashCode() shouldBe p2s.hashCode()

    val parsed_p2sh = ergoAddressEncoder.fromProposition(p2sh.script).success.value
    assertResult(false)(parsed_p2sh eq p2sh)
    assertResult(true)(parsed_p2sh == p2sh && p2sh == parsed_p2sh)
    parsed_p2sh.hashCode() shouldBe p2sh.hashCode()

    val parsed_p2pk = ergoAddressEncoder.fromProposition(p2pk.script).success.value
    assertResult(false)(parsed_p2pk eq p2pk)
    assertResult(true)(parsed_p2pk == p2pk && p2pk == parsed_p2pk)
    parsed_p2pk.hashCode() shouldBe p2pk.hashCode()

    val tree = mkTestErgoTree(pk)
    val p2s_2: Pay2SAddress = Pay2SAddress(tree) // address created via P2S constructor method
    assertResult(true)(p2s_2 != p2pk && p2pk != p2s_2)

    // not equal to created via encoder (how "good" is that!)
    val from_tree = ergoAddressEncoder.fromProposition(tree).success.value
    assertResult(true)(from_tree != p2s_2)
    assertResult(true)(from_tree == p2pk)
  }

  property("derivation from private key") {
    val w = new BigInteger("bb2e6f44a38052b3f564fafcd477c4eb8cda1a8a553a4a5f38f1e1084d6a69f0", 16)
    val g = dlogGroup.generator
    val pk = dlogGroup.exponentiate(g, w)
    val pkBytes = GroupElementSerializer.toBytes(pk)
    val encoder = new ErgoAddressEncoder(MainnetNetworkPrefix)
    val p2pk =  P2PKAddress(ProveDlog(pk))(encoder)
    val addrStr = p2pk.toString

    val prefix = (encoder.networkPrefix + P2PKAddress.addressTypePrefix).toByte
    val bytes = prefix +: pkBytes

    val checksum = Blake2b256(bytes).take(ErgoAddressEncoder.ChecksumLength)
    val expectedAddrStr = Base58.encode(bytes ++ checksum)

    addrStr shouldBe expectedAddrStr
    addrStr shouldBe "9iJd9drp1KR3R7HLi7YmQbB5sJ5HFKZoPb5MxGepamggJs5vDHm"
  }

  property("fromProposition() should properly distinct all types of addresses from script AST") {
    testFromProposition(scriptVersion = 0,
      expectedP2S = "JryiCXrZf5VDetH1PM7rKDX3q4sLF34AdErWJFqG87Hf5ikTDf636b35Nr7goWMdRUKA3ZPxdeqFNbQzBjhnDR9SUMYwDX1tdV8ZuGgXwQPaRVcB9",
      expectedP2SH = "qETVgcEctaXurNbFRgGUcZEGg4EKa8R4a5UNHY7",
      expectedP2PK = "3WwXpssaZwcNzaGMv3AgxBdTPJQBt5gCmqBsg3DykQ39bYdhJBsN")

    testFromProposition(scriptVersion = 1,
      expectedP2S = "2MzJLjzX6UNfJHSVvioB6seYZ99FpWHB4Ds1gekHPv5KtNmLJUecgRWwvcGEqbt8ZAokUxGvKMuNgMZFzkPPdTGiYzPQoSR55NT5isCidMywgp52LYV",
      expectedP2SH = "qETVgcEctaXurNbFRgGUcZEGg4EKa8R4a5UNHY7",
      expectedP2PK = "3WwXpssaZwcNzaGMv3AgxBdTPJQBt5gCmqBsg3DykQ39bYdhJBsN")

    testFromProposition(scriptVersion = 2,
      expectedP2S = "2N1Egpu5R9XtomV7x343LTXGrBLEkC8pvMVtjm6V3iHryxVfc6LUJhd1JsswhXMpPXUMatoBgnJ4qMGAC7dha27WkjqVBUsebWBDhig97zhmKS8T4YS",
      expectedP2SH = "qETVgcEctaXurNbFRgGUcZEGg4EKa8R4a5UNHY7",
      expectedP2PK = "3WwXpssaZwcNzaGMv3AgxBdTPJQBt5gCmqBsg3DykQ39bYdhJBsN")
  }

  property("decode with wrong address") {
    assertExceptionThrown(
      ergoAddressEncoder.fromString(
        "JryiCXrZf5VDetH1PM7rKDX3q4sLF34AdErWJFqG87Hf5ikTDf636b35Nr7goWMdRUKA3ZPxdeqFNbQzBjhnDR9SUMYwDX1tdV8ZuGgXwQPaRVcB8")
          .getOrThrow,
      t => t.getMessage.contains("Checksum check fails")
    )
    assertExceptionThrown(
      ergoAddressEncoder.fromString(
        "2MzJLjzX6UNfJHSVvioB6seYZ99FpWHB4Ds1gekHPv5KtNmLJUecgRWwvcGEqbt8ZAokUxGvKMuNgMZFzkPPdTGiYzPQoSR55NT5isCidMywgp52LYU")
          .getOrThrow,
      t => t.getMessage.contains("Checksum check fails")
    )

    {
      val invalid_p2sh = new Pay2SHAddress(Array[Byte](1, 2, 3))
      val addrStr = ergoAddressEncoder.toString(invalid_p2sh)

      assertExceptionThrown(
        ergoAddressEncoder.fromString(addrStr).getOrThrow,
        t => t.getMessage.contains("Invalid length of the hash bytes in P2SH address: 41fKjb7zWNw")
      )
    }

    {
      val pk: ProveDlog = DLogProverInput(BigInteger.ONE).publicImage
      val p2pk = P2PKAddress(pk)(ergoAddressEncoder)

      val invalidAddrType = 4.toByte
      val withNetworkByte = (ergoAddressEncoder.networkPrefix + invalidAddrType).toByte +: p2pk.contentBytes

      val checksum = hash256(withNetworkByte).take(ErgoAddressEncoder.ChecksumLength)
      val invalidAddrStr = Base58.encode(withNetworkByte ++ checksum)

      assertExceptionThrown(
        ergoAddressEncoder.fromString(invalidAddrStr).getOrThrow,
        t => t.getMessage.contains("Unsupported address type: 4")
      )
    }

    {
      val unparsedTree = new ErgoTree(
        setConstantSegregation(ergoTreeHeaderInTests),
        Array[Constant[SType]](),
        Left(UnparsedErgoTree(Array[Byte](), ValidationException("", CheckTypeCode, Seq())))
      )
      assertExceptionThrown(
        ergoAddressEncoder.fromProposition(unparsedTree).getOrThrow,
        t => t.getMessage.contains("Cannot create ErgoAddress form unparsed ergo tree")
      )
    }

    {
      val invalidTree = new ErgoTree(
        setConstantSegregation(ergoTreeHeaderInTests),
        Array[Constant[SType]](),
        Right(IntConstant(10).asSigmaProp)
      )
      assertExceptionThrown(
        ergoAddressEncoder.fromProposition(invalidTree).getOrThrow,
        t => t.getMessage.contains("Cannot create ErgoAddress form proposition")
      )
    }
  }

  property("decode with wrong network prefix") {
    forAll(proveDlogGen) { pk =>
      val mainnetEncoder = new ErgoAddressEncoder(MainnetNetworkPrefix)
      val testnetEncoder = new ErgoAddressEncoder(TestnetNetworkPrefix)
      val mnAddr = P2PKAddress(pk)(mainnetEncoder)
      val tnAddr = P2PKAddress(pk)(testnetEncoder)

      an[RuntimeException] should be thrownBy mainnetEncoder.fromString(tnAddr.toString).get
      an[RuntimeException] should be thrownBy testnetEncoder.fromString(mnAddr.toString).get
    }
  }

  def testPay2SHAddress(address: Pay2SHAddress, scriptBytes: Array[Byte]) = {
    val scriptId = Pay2SHAddress.scriptId
    val boxToSpend = testBox(10, address.script, creationHeight = 5)
    val ctx = ErgoLikeContextTesting.dummy(boxToSpend, activatedVersionInTests)
        .withExtension(ContextExtension(Seq(
          scriptId -> ByteArrayConstant(scriptBytes)  // provide script bytes in context variable
        ).toMap))

    val env: ScriptEnv = Map()
    val prover = new ErgoLikeTestProvingInterpreter()
    val pr = prover.prove(env + (ScriptNameProp -> s"prove"), address.script, ctx, fakeMessage).getOrThrow

    val verifier = new ErgoLikeTestInterpreter
    verifier.verify(env + (ScriptNameProp -> s"verify_ext"), address.script, ctx, pr.proof, fakeMessage).getOrThrow._1 shouldBe true
  }

  def createPropAndScriptBytes() = {
    implicit lazy val IR = new TestingIRContext
    val script = "{ 1 < 2 }"
    val prop = compile(Map.empty, script).asBoolValue.toSigmaProp
    val scriptBytes = ValueSerializer.serialize(prop)
    (prop, scriptBytes)
  }

  property("spending a box protected by P2SH contract") {
    val (prop, scriptBytes) = createPropAndScriptBytes()

    testPay2SHAddress(Pay2SHAddress(prop), scriptBytes)

    val tree = mkTestErgoTree(prop)
    testPay2SHAddress(Pay2SHAddress(tree), scriptBytes) // NOTE: same scriptBytes regardless of ErgoTree version
  }

  property("Pay2SHAddress.script should create ErgoTree v0") {
    val (prop, _) = createPropAndScriptBytes()

    val address = VersionContext.withVersions(activatedVersionInTests, ergoTreeVersionInTests) {
      Pay2SHAddress(prop)
    }
    address.script.version shouldBe 0
  }

  // create non-versioned property which is executed under default version context
  // see VersionContext._defaultContext
  super[AnyPropSpecLike].property("using default VersionContext still creates ErgoTree v0") {
    val (prop, _) = createPropAndScriptBytes()
    val address = Pay2SHAddress(prop)
    address.script.version shouldBe 0
  }

  property("negative cases: deserialized script + costing exceptions") {

    def testPay2SHAddress(address: Pay2SHAddress, script: VarBinding, costLimit: Int = scriptCostLimitInTests): CostedProverResult = {
      val boxToSpend = testBox(10, address.script, creationHeight = 5)
      val ctx = copyContext(ErgoLikeContextTesting.dummy(boxToSpend, activatedVersionInTests)
          .withExtension(ContextExtension(Seq(
            script // provide script bytes in context variable
          ).toMap)))(costLimit = costLimit)

      val prover = new ErgoLikeTestProvingInterpreter()
      val res = prover.prove(address.script, ctx, fakeMessage).getOrThrow
      res
    }

    val scriptVarId = Pay2SHAddress.scriptId
    val (prop, scriptBytes) = createPropAndScriptBytes()
    val addr = Pay2SHAddress(prop)

    // when everything is ok
    testPay2SHAddress(addr, script = scriptVarId -> ByteArrayConstant(scriptBytes))

    val expectedCost = 88

    // when limit is low
    {
      val deliberatelySmallLimit = 24
      assertExceptionThrown(
        testPay2SHAddress(addr,
          script = scriptVarId -> ByteArrayConstant(scriptBytes),
          costLimit = deliberatelySmallLimit),
        rootCauseLike[CostLimitException](
          s"Estimated execution cost $expectedCost exceeds the limit $deliberatelySmallLimit")
      )
    }


    // when limit is even lower than tree complexity
    {
      val deliberatelySmallLimit = 2

      assertExceptionThrown(
      {
        testPay2SHAddress(addr,
          script = scriptVarId -> ByteArrayConstant(scriptBytes),
          costLimit = deliberatelySmallLimit)
      },
      rootCauseLike[CostLimitException](
        s"Estimated execution cost $expectedCost exceeds the limit $deliberatelySmallLimit")
      )
    }

    // when script var have invalid type
    assertExceptionThrown(
      testPay2SHAddress(addr, script = scriptVarId -> IntConstant(10)),
      rootCauseLike[InvalidType](s"invalid type of value")
    )

    // when script var have invalid id
    val invalidId: Byte = 2
    assertExceptionThrown(
      testPay2SHAddress(addr, script = invalidId -> IntConstant(10)),
      rootCauseLike[NoSuchElementException]("None.get")
    )
  }

}