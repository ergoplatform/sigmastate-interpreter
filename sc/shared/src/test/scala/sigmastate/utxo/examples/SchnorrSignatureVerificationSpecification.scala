package sigmastate.utxo.examples

import org.ergoplatform._
import sigma.ast.ErgoTree.ZeroHeader
import sigma.ast.syntax.ValueOps
import sigma.data.AvlTreeData
import sigma.VersionContext
import sigmastate._
import sigmastate.helpers._
import sigmastate.helpers.TestingHelpers._
import sigma.ast.{ByteArrayConstant, ErgoTree, GroupElementConstant}
import sigma.interpreter.ContextExtension
import sigma.crypto.{BigIntegers, CryptoConstants}
import sigma.serialization.GroupElementSerializer
import scorex.crypto.hash.Blake2b256

import java.math.BigInteger


class SchnorrSignatureVerificationSpecification extends CompilerTestingCommons with CompilerCrossVersionProps {
  private implicit lazy val IR: TestingIRContext = new TestingIRContext



  /**
    * Strong Fiat-Shamir Schnorr Signature Verification Example:
    * A contract that verifies Schnorr signatures using strong Fiat-Shamir transform
    * with ErgoTree v3 and UnsignedBigInt type.
    * 
    * Based on the ErgoForum post: https://www.ergoforum.org/t/verifying-schnorr-signatures-in-ergoscript/3407
    * and ChainCash implementation: https://github.com/BetterMoneyLabs/chaincash/blob/master/contracts/offchain/basis.es
    */
  property("Schnorr signature verification contract compiles and verifies correctly") {
    // Run with V6 activation to properly handle rule #1011 replacement
    forEachActivatedScriptVersion(Seq(VersionContext.V6SoftForkVersion)) {
    VersionContext.withVersions(VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion) {
    // Strong Fiat-Shamir Schnorr signature verification contract
    val schnorrScript = """
      |{
      |  // Strong Fiat-Shamir Schnorr signature verification
      |  val g: GroupElement = groupGenerator
      |  
      |  // Public key stored in R4
      |  val publicKey = SELF.R4[GroupElement].get
      |  
      |  // Message to verify stored in R5
      |  val message = SELF.R5[Coll[Byte]].get
      |  
      |  // Signature components from context variables
      |  // Signature format: (aBytes || zBytes) where:
      |  // - aBytes: 33 bytes (group element)
      |  // - zBytes: 32 bytes (big integer)
      |  val signatureBytes = getVar[Coll[Byte]](0).get
      |  
      |  // Extract signature components
      |  val aBytes = signatureBytes.slice(0, 33)
      |  val zBytes = signatureBytes.slice(33, signatureBytes.size)
      |  
      |  // Decode group element a
      |  val a = decodePoint(aBytes)
      |  
      |  // Convert z to UnsignedBigInt (fits within 256 bits)
      |  val z = Global.fromBigEndianBytes[UnsignedBigInt](zBytes)
      |  
      |  // Strong Fiat-Shamir challenge computation
      |  // Includes public key in the hash to prevent certain attacks
      |  val e: Coll[Byte] = blake2b256(aBytes ++ message ++ publicKey.getEncoded)
      |  val eInt = byteArrayToBigInt(e)
      |  
      |  // Schnorr signature verification: g^z = a * Y^e
      |  val properSignature = g.exp(z) == a.multiply(publicKey.exp(eInt))
      |  sigmaProp(properSignature)
      |}
      |""".stripMargin

    // Test that the contract compiles successfully
    val h = ErgoTree.setVersionBits(ZeroHeader, 3)
    val prop = compile(Map.empty, schnorrScript, 3).asSigmaProp
    val schnorrTree = ErgoTree.fromProposition(h, prop)

    // The contract should compile without errors
    schnorrTree should not be null

    // Full verification: Test the Schnorr signature contract with proper verification flow
    // The contract implements strong Fiat-Shamir Schnorr verification
    
    val prover = new ErgoLikeTestProvingInterpreter

    // Use the proving interpreter's secret for the public key
    val secret = prover.dlogSecrets.head
    val publicKey = secret.publicImage

    // Create test message
    val messageBytes = "Test message for Schnorr signature verification".getBytes("UTF-8")

    // Create test box with public key and message
    val inputBox = testBox(
      1000000L, 
      schnorrTree, 
      100000,
      additionalRegisters = Map(
        ErgoBox.R4 -> GroupElementConstant(publicKey.value),
        ErgoBox.R5 -> ByteArrayConstant(messageBytes)
      )
    )
    
    val outputBox = testBox(1000000L, schnorrTree, 100000)
    
    val tx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(inputBox.id)),
      IndexedSeq(outputBox)
    )

    // Real Schnorr signing off-chain, following the algorithm from the forum post.
    // Important detail: the contract decodes the challenge with byteArrayToBigInt, which is a
    // SIGNED two's-complement interpretation (see CSigmaDslBuilder.byteArrayToBigInt), so the
    // challenge hash must be interpreted as a signed big-endian integer both off-chain and
    // on-chain. In contrast, z is decoded with UnsignedBigInt (unsigned, 256 bits), so
    // z = (r + x*e) mod q always fits without the "retry until z fits 255 bits" loop that the
    // forum version needs (there z was decoded with signed byteArrayToBigInt as well).
    val group = CryptoConstants.dlogGroup
    val q = CryptoConstants.groupOrder
    val x = secret.w
    val publicKeyPoint = publicKey.value

    /** Sign msgBytes with the strong Fiat-Shamir Schnorr scheme, returning aBytes(33) || zBytes(32). */
    def sign(msgBytes: Array[Byte]): Array[Byte] = {
      val r = BigIntegers.createRandomInRange(BigInteger.ONE, q.subtract(BigInteger.ONE), group.secureRandom)
      val aPoint = group.exponentiate(group.generator, r)
      val aBytes = GroupElementSerializer.toBytes(aPoint)
      val pkBytes = GroupElementSerializer.toBytes(publicKeyPoint)
      val eBytes = Blake2b256(aBytes ++ msgBytes ++ pkBytes)
      val e = new BigInteger(eBytes) // signed interpretation, same as byteArrayToBigInt on-chain
      val z = (BigInt(r) + BigInt(x) * BigInt(e)).mod(BigInt(q)).bigInteger
      val zRaw = z.toByteArray // two's-complement, may carry a leading 0x00 byte
      val zBytes = new Array[Byte](32)
      val copyLen = math.min(32, zRaw.length)
      System.arraycopy(zRaw, zRaw.length - copyLen, zBytes, 32 - copyLen, copyLen)
      aBytes ++ zBytes
    }

    def contextWithSignature(sigBytes: Array[Byte], box: ErgoBox = inputBox, transaction: UnsignedErgoLikeTransaction = tx): ErgoLikeContext = {
      val baseCtx = ErgoLikeContextTesting(
        currentHeight = 100000,
        lastBlockUtxoRoot = AvlTreeData.dummy,
        minerPubkey = ErgoLikeContextTesting.dummyPubkey,
        boxesToSpend = IndexedSeq(box),
        transaction,
        self = box,
        activatedVersionInTests
      )
      baseCtx.withExtension(ContextExtension(Map(
        0.toByte -> ByteArrayConstant(sigBytes)
      )))
    }

    // Test 1: a genuine Schnorr signature must be accepted (positive case)
    val validSignature = sign(messageBytes)
    validSignature.length shouldBe 65

    val extendedCtx = contextWithSignature(validSignature)

    prover.prove(schnorrTree, extendedCtx, fakeMessage).isSuccess shouldBe true

    // Test 2: signature over a different message must be rejected.
    // The box's R5 message differs from what was signed.
    val otherMessageBytes = "A different message".getBytes("UTF-8")
    val otherMessageBox = testBox(
      1000000L,
      schnorrTree,
      100000,
      additionalRegisters = Map(
        ErgoBox.R4 -> GroupElementConstant(publicKey.value),
        ErgoBox.R5 -> ByteArrayConstant(otherMessageBytes)
      )
    )
    val otherMessageTx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(otherMessageBox.id)),
      IndexedSeq(outputBox)
    )
    prover.prove(schnorrTree, contextWithSignature(validSignature, otherMessageBox, otherMessageTx), fakeMessage).isSuccess shouldBe false

    // Test 3: tampered signature (last byte of z changed) must be rejected
    val tamperedSignature = validSignature.clone()
    tamperedSignature(64) = (tamperedSignature(64) ^ 0x01).toByte
    prover.prove(schnorrTree, contextWithSignature(tamperedSignature), fakeMessage).isSuccess shouldBe false

    // Test 4: empty signature - should fail
    val emptySignature = Array.empty[Byte]
    prover.prove(schnorrTree, contextWithSignature(emptySignature), fakeMessage).isSuccess shouldBe false

    // Test 5: Test with wrong public key - should fail
    val wrongProver = new ErgoLikeTestProvingInterpreter
    val wrongSecret = wrongProver.dlogSecrets.head
    val wrongPublicKey = wrongSecret.publicImage

    val wrongInputBox = testBox(
      1000000L,
      schnorrTree,
      100000,
      additionalRegisters = Map(
        ErgoBox.R4 -> GroupElementConstant(wrongPublicKey.value),
        ErgoBox.R5 -> ByteArrayConstant(messageBytes)
      )
    )

    val wrongTx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(wrongInputBox.id)),
      IndexedSeq(outputBox)
    )

    // signature computed for the right key, but the box carries a different public key
    prover.prove(schnorrTree, contextWithSignature(validSignature, wrongInputBox, wrongTx), fakeMessage).isSuccess shouldBe false

    // Test 6: signature of wrong total length must be rejected.
    // Note: the contract has no explicit length checks; a truncated/padded signature is rejected
    // because the bytes don't decode to a valid point or don't satisfy the verification equation.
    val wrongLengthSignature = Array.fill[Byte](64)(0x42)
    prover.prove(schnorrTree, contextWithSignature(wrongLengthSignature), fakeMessage).isSuccess shouldBe false
    }
    }
  }

}