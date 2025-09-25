package sigmastate.utxo

import org.ergoplatform.ErgoBox.{AdditionalRegisters, R6, R8}
import org.ergoplatform._
import org.scalatest.Assertion
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert, InsertOrUpdate}
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util.{ByteArrayBuilder, idToBytes}
import scorex.util.encode.Base16
import scorex.utils.Ints
import scorex.util.serialization.VLQByteBufferWriter
import scorex.utils.Longs
import sigma.{Coll, Colls, GroupElement, SigmaTestingData, VersionContext}
import sigma.Extensions.ArrayOps
import sigma.VersionContext.{V6SoftForkVersion, withVersions}
import sigma.ast.SCollection.SByteArray
import sigma.ast.SType.{AnyOps, tD}
import sigma.data.{AvlTreeData, AvlTreeFlags, CAND, CAnyValue, CBigInt, CGroupElement, CHeader, CSigmaDslBuilder, CSigmaProp}
import sigma.ast.SOption
import sigma.util.StringUtil._
import sigma.ast._
import sigma.ast.syntax._
import sigma.crypto.{CryptoConstants, SecP256K1Group}
import sigmastate._
import sigmastate.helpers.TestingHelpers._
import sigmastate.helpers.{CompilerTestingCommons, ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTestInterpreter}
import sigma.interpreter.ContextExtension.VarBinding
import sigmastate.interpreter.CErgoTreeEvaluator.DefaultEvalSettings
import sigmastate.interpreter.Interpreter._
import sigma.ast.Apply
import sigma.eval.{EvalSettings, SigmaDsl}
import sigma.exceptions.InvalidType
import sigma.serialization.ErgoTreeSerializer
import sigma.serialization.{DataSerializer, SigmaByteWriter, ValueSerializer}
import sigma.interpreter.{ContextExtension, ProverResult}
import sigma.validation.ValidationException
import sigma.util.Extensions
import sigmastate.utils.Helpers
import sigma.exceptions.InterpreterException
import sigmastate.utils.Helpers._

import java.math.BigInteger
import scala.collection.compat.immutable.ArraySeq
import java.security.SecureRandom
import scala.annotation.tailrec
import scala.util.Try

class GlobalTransferPoliciesSpecification extends CompilerTestingCommons
  with CompilerCrossVersionProps {
  override val printVersions: Boolean = false
  implicit lazy val IR = new TestingIRContext {
  }

  private val reg1 = ErgoBox.nonMandatoryRegisters.head
  private val reg2 = ErgoBox.nonMandatoryRegisters.tail.head
  val intVar1 = 1.toByte
  val intVar2 = 2.toByte
  val byteVar1 = 3.toByte
  val byteVar2 = 4.toByte
  val bigIntVar1 = 5.toByte
  val bigIntVar2 = 6.toByte
  val bigIntVar3 = 7.toByte
  val byteVar3 = 8.toByte
  val booleanVar = 9.toByte
  val propVar1 = 10.toByte
  val propVar2 = 11.toByte
  val propVar3 = 12.toByte
  val propBytesVar1 = 13.toByte
  val lastExtVar = propBytesVar1

  val ext: Seq[VarBinding] = Seq(
    (intVar1, IntConstant(1)), (intVar2, IntConstant(2)),
    (byteVar1, ByteConstant(1)), (byteVar2, ByteConstant(2)),
    (bigIntVar1, BigIntConstant(BigInt(10).underlying())), (bigIntVar2, BigIntConstant(BigInt(20).underlying())),
    (booleanVar, TrueLeaf))
  // to support both JVM and JS we need to wrap numeric values into CAnyValue
  val env = Map(
    "intVar1" -> CAnyValue(intVar1), "intVar2" -> CAnyValue(intVar2),
    "byteVar1" -> CAnyValue(byteVar1), "byteVar2" -> CAnyValue(byteVar2), "byteVar3" -> CAnyValue(byteVar3),
    "bigIntVar1" -> CAnyValue(bigIntVar1), "bigIntVar2" -> CAnyValue(bigIntVar2), "bigIntVar3" -> CAnyValue(bigIntVar3),
    "trueVar" -> CAnyValue(booleanVar),
    "proofVar1" -> CAnyValue(propVar1),
    "proofVar2" -> CAnyValue(propVar2)
    )

  def test(name: String,
           env: ScriptEnv,
           ext: Seq[VarBinding],
           script: String,
           propExp: SValue,
           onlyPositive: Boolean = true,
           testExceededCost: Boolean = true,
           additionalRegistersOpt: Option[AdditionalRegisters] = None) = {
    val prover = new ContextEnrichingTestProvingInterpreter() {
      override lazy val contextExtenders: Map[Byte, EvaluatedValue[_ <: SType]] = {
        val p1 = dlogSecrets(0).publicImage
        val p2 = dlogSecrets(1).publicImage
        val d1 = dhSecrets(0).publicImage

        (ext ++ Seq(
          propVar1 -> SigmaPropConstant(p1),
          propVar2 -> SigmaPropConstant(p2),
          propVar3 -> SigmaPropConstant(CSigmaProp(CAND(Seq(p1, d1)))),
          propBytesVar1 -> ByteArrayConstant(CSigmaProp(CAND(Seq(p1, d1))).propBytes)
        )).toMap
      }
      override val evalSettings: EvalSettings = DefaultEvalSettings.copy(
        isMeasureOperationTime = true,
        isDebug = true,
        isTestRun = testExceededCost)
    }

    val prop = if (script.isNullOrEmpty) {
      // for some testcases the script cannot be compiled (i.e. the corresponding syntax
      // is not supported by ErgoScript Compiler)
      // In such cases we use expected property as the property to test
      propExp.asSigmaProp
    } else {
      // compile with the latest compiler version, to get validation exception during execution, not compilation error
      withVersions(VersionContext.MaxSupportedScriptVersion, VersionContext.MaxSupportedScriptVersion) {
        compile(env, script).asBoolValue.toSigmaProp
      }
    }

    if (propExp != null)
      prop shouldBe propExp

    val tree = ErgoTree.fromProposition(ergoTreeHeaderInTests, prop)

    // check ErgoTree roundtrip
    val tBytes = ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(tree)
    val tBytes2 = ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(tBytes))
    tBytes.sameElements(tBytes2) shouldBe true

    val p3 = prover.dlogSecrets(2).publicImage
    val boxToSpend = testBox(10, tree,
      additionalRegisters = additionalRegistersOpt.getOrElse(Map(
        reg1 -> SigmaPropConstant(p3),
        reg2 -> IntConstant(1))
      ),
      creationHeight = 5)

    val newBox1 = testBox(10, tree, creationHeight = 0, boxIndex = 0, additionalRegisters = Map(
      reg1 -> IntConstant(1),
      reg2 -> IntConstant(10)))
    val ce = ContextExtension(prover.contextExtenders)
    val tx = new ErgoLikeTransaction(IndexedSeq(Input(boxToSpend.id, ProverResult(Array.empty, ce))), ArraySeq.empty, IndexedSeq(newBox1))

    val ctx = ErgoLikeContextTesting(currentHeight = 0,
      lastBlockUtxoRoot = AvlTreeData.dummy, ErgoLikeContextTesting.dummyPubkey, boxesToSpend = IndexedSeq(boxToSpend),
      spendingTransaction = tx, self = boxToSpend, ergoTreeVersionInTests)

    val pr = prover.prove(env + (ScriptNameProp -> s"${name}_prove"), tree, ctx, fakeMessage).getOrThrow

    val ctxExt = ctx.withExtension(pr.extension)

    val testVerifier = new ErgoLikeTestInterpreter

    if (!onlyPositive) {
      // test negative case
      testVerifier.verify(
          env + (ScriptNameProp -> s"${name}_verify"),
          tree, ctx, pr.proof, fakeMessage)
        .map(_._1)
        .getOrElse(false) shouldBe false //context w/out extensions
    }

    // this is helper verifier which respects the requested parameter testExceededCost for
    // some test cases (when testExceededCost == false) it emit message in the console
    // instead of failing the test and the failing case is tested separately in that case
    val flexVerifier = new ErgoLikeTestInterpreter {
      override val evalSettings: EvalSettings = DefaultEvalSettings.copy(
        isMeasureOperationTime = true,
        isDebug = true,
        isTestRun = testExceededCost)
    }
    val verifyEnv = env + (ScriptNameProp -> s"${name}_verify_ext")
    flexVerifier.verify(verifyEnv, tree, ctxExt, pr.proof, fakeMessage).get._1 shouldBe true
  }

  property("script hash verification from context var and data input R4") {
    // Test that the script compiles correctly
    val compiledScript = compile(env, 
      """{
        |  // Get script from context variable #1
        |  val scriptBytes = getVar[Coll[Byte]](1).get
        |  
        |  // Compute script hash
        |  val computedHash = blake2b256(scriptBytes)
        |  
        |  // Get expected hash from R4 of first data input
        |  val expectedHash = CONTEXT.dataInputs(0).R4[Coll[Byte]].get
        |  
        |  // Verify they match
        |  sigmaProp(computedHash == expectedHash)
        |}""".stripMargin)
    
    // If we get here without exceptions, the script compiles correctly
    compiledScript should not be null
  }

}