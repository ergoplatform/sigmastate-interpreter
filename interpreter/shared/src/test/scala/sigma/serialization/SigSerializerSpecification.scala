package sigma.serialization

import java.math.BigInteger
import org.ergoplatform.settings.ErgoAlgos
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Assertion
import sigma.Extensions.ArrayOps
import sigma.data.{AvlTreeData, CAND, COR, CTHRESHOLD, ProveDHTuple, ProveDlog, SigmaBoolean, TrivialProp}
import sigma.eval.Extensions.SigmaBooleanOps
import sigmastate._
import sigmastate.crypto.DLogProtocol.SecondDLogProverMessage
import sigmastate.crypto.VerifierMessage.Challenge
import sigmastate.crypto.SecondDHTupleProverMessage
import sigmastate.crypto.GF2_192_Poly
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTransactionTesting, TestingCommons}
import sigmastate.interpreter.Interpreter
import sigma.serialization.generators.ObjectGenerators
import sigmastate.utils.Helpers

import scala.util.Random

class SigSerializerSpecification extends TestingCommons
  with ObjectGenerators with CrossVersionProps {
  private lazy implicit val arbExprGen: Arbitrary[SigmaBoolean] = Arbitrary(exprTreeGen)

  private lazy val prover = new ContextEnrichingTestProvingInterpreter()

  private lazy val interpreterProveDlogGen: Gen[ProveDlog] =
    Gen.oneOf(prover.dlogSecrets.map(_.publicImage))

  private lazy val interpreterProveDHTGen =
    Gen.oneOf(prover.dhSecrets.map(_.publicImage))

  private def exprTreeNodeGen: Gen[SigmaBoolean] = for {
    left <- exprTreeGen
    right <- exprTreeGen
    third <- exprTreeGen
    node <- Gen.oneOf(
      COR(Array(left, right)),
      CAND(Array(left, right)),
      CTHRESHOLD(2, Array(left, right, third))
    ) if Interpreter.estimateCryptoVerifyCost(node).value <= 100000 // limit size of the generated tree
                                                   // to fit into Box size limit
  } yield node

  private def exprTreeGen: Gen[SigmaBoolean] =
    Gen.oneOf(interpreterProveDlogGen, interpreterProveDHTGen, Gen.delay(exprTreeNodeGen))

  private def isEquivalent(expected: ProofTree, actual: ProofTree): Boolean = (expected, actual) match {
    case (NoProof, NoProof) => true
    case (dht1: UncheckedDiffieHellmanTuple, dht2: UncheckedDiffieHellmanTuple) =>
      // `firstMessageOpt` is not serialized
      dht1.copy(commitmentOpt = None) == dht2
    case (sch1: UncheckedSchnorr, sch2: UncheckedSchnorr) =>
      // `firstMessageOpt` is not serialized
      sch1.copy(commitmentOpt = None) == sch2
    case (conj1: UncheckedConjecture, conj2: UncheckedConjecture) =>
      conj1.challenge == conj2.challenge &&
        conj1.children.zip(conj2.children).forall(t => isEquivalent(t._1, t._2))
    case _ => false
  }


  private def roundTrip(uncheckedTree: UncheckedTree, exp: SigmaBoolean): Assertion = {
    val proof = SigSerializer.toProofBytes(uncheckedTree)
    val parsedUncheckedTree = SigSerializer.parseAndComputeChallenges(exp, proof)(null)
    isEquivalent(uncheckedTree, parsedUncheckedTree) shouldBe true
  }

  property("SigSerializer no proof round trip") {
    roundTrip(NoProof, TrivialProp.TrueProp)
  }

  property("SigSerializer round trip") {
    forAll(configParams = MinSuccessful(100)) { sb: SigmaBoolean =>
      val expr = sb.toSigmaPropValue
      val challenge = Array.fill(32)(Random.nextInt(100).toByte)

      val ctx = ErgoLikeContextTesting(
        currentHeight = 1,
        lastBlockUtxoRoot = AvlTreeData.dummy,
        minerPubkey = ErgoLikeContextTesting.dummyPubkey,
        boxesToSpend = IndexedSeq(fakeSelf),
        spendingTransaction = ErgoLikeTransactionTesting.dummy,
        self = fakeSelf, activatedVersionInTests)
        .withCostLimit(Long.MaxValue) // To avoid occasional cost limit exceptions which are irrelevant here

      try {
        // get sigma conjectures out of transformers
        val tree = mkTestErgoTree(expr)
        val prop = prover.fullReduction(tree, ctx).value

        val proof = prover.prove(tree, ctx, challenge).get.proof
        val uncheckedTree = SigSerializer.parseAndComputeChallenges(prop, proof)(null)
        roundTrip(uncheckedTree, prop)

// uncomment the following lines to print test cases with test vectors
// they can be added to property("SigSerializer test vectors")
//        val fiatShamirHex = getFiatShamirHex(uncheckedTree)
//        val testCase = ProofTestCase(prop, proof, uncheckedTree, fiatShamirHex)
//        println(
//          s"""-------------------------------
//            |${sigmastate.helpers.SigmaPPrint(testCase, width = 150, height = 150)}
//            |""".stripMargin)

      } catch {
        case t: Throwable =>
          t.printStackTrace()
          throw t
      }
    }
  }

  /** This is used to represent related test vectors to test:
    * - SigSerializer.toBytes
    * - SigSerializer.parseAndComputeChallenges
    * - FiatShamirTree.toBytes
    */
  case class ProofTestCase(
    prop: SigmaBoolean,
    proof: Array[Byte],
    uncheckedTree: UncheckedTree,
    fiatShamirHex: String = ""
  )

  /** This steps are performed in Interpreter.checkCommitments and should be in sync with
    * that code. */
  def getFiatShamirHex(uncheckedTree: UncheckedTree): String = {
    val newRoot = prover.computeCommitments(uncheckedTree).get.asInstanceOf[UncheckedSigmaTree]
    val fiatShamirBytes = FiatShamirTree.toBytes(newRoot)(null/* no profiling */)
    val hex = ErgoAlgos.encode(fiatShamirBytes)
    hex
  }

  property("SigSerializer test vectors")  {
    // this list of test cases contains test vectors generated by v4.0.2
    // as such they can be used to catch changes in behaviour of later versions
    val cases = Seq(
      ProofTestCase(
        prop = ProveDlog(
          Helpers.decodeECPoint("02e8e77123e300f8324e7b5c4cbe0f7ac616e0b78fc45f28f54fa6696231fc8ec3")
        ),
        proof = ErgoAlgos.decodeUnsafe(
          "c6429b70f4926a3ba1454f1aec116075f9e9fbe8a8f72114b277b8462a8b9098f5d4c934ab2876eb1b5707f3119e209bdbbad831e7cc4a41"
        ),
        uncheckedTree = UncheckedSchnorr(
          ProveDlog(
            Helpers.decodeECPoint("02e8e77123e300f8324e7b5c4cbe0f7ac616e0b78fc45f28f54fa6696231fc8ec3")
          ),
          None,
          Challenge @@ ErgoAlgos.decodeUnsafe("c6429b70f4926a3ba1454f1aec116075f9e9fbe8a8f72114").toColl,
          SecondDLogProverMessage(
            BigInt("b277b8462a8b9098f5d4c934ab2876eb1b5707f3119e209bdbbad831e7cc4a41", 16)
          )
        ),
        fiatShamirHex = "010027100108cd02e8e77123e300f8324e7b5c4cbe0f7ac616e0b78fc45f28f54fa6696231fc8ec373000021021d30cef8084f8659e9734099bf8e6faa89d81f908c3a62e7638da7b2a33822fc"
      ),
      ProofTestCase(
        ProveDHTuple(
          Helpers.decodeECPoint("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"),
          Helpers.decodeECPoint("024ebfeb5a2b6ad997e40efb4888b3f091a611df8298cf7fb24315b4d112ad7c3c"),
          Helpers.decodeECPoint("03d41afc8c5875a8d52439d088b66ed63a5d64f16e1efd7f17c6036a923c637e5c"),
          Helpers.decodeECPoint("034132d4c7eb387f12ef40ba3ec03723bda0ee5707f7471185aafc316167e85137")
        ),
        ErgoAlgos.decodeUnsafe(
          "9ec740b57353cb2f6035bb1a481b0066b2fdc0406a6fa67ebb2e6f44a38052b3f564fafcd477c4eb8cda1a8a553a4a5f38f1e1084d6a69f0"
        ),
        UncheckedDiffieHellmanTuple(
          ProveDHTuple(
            Helpers.decodeECPoint("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"),
            Helpers.decodeECPoint("024ebfeb5a2b6ad997e40efb4888b3f091a611df8298cf7fb24315b4d112ad7c3c"),
            Helpers.decodeECPoint("03d41afc8c5875a8d52439d088b66ed63a5d64f16e1efd7f17c6036a923c637e5c"),
            Helpers.decodeECPoint("034132d4c7eb387f12ef40ba3ec03723bda0ee5707f7471185aafc316167e85137")
          ),
          None,
          Challenge @@ ErgoAlgos.decodeUnsafe("9ec740b57353cb2f6035bb1a481b0066b2fdc0406a6fa67e").toColl,
          SecondDHTupleProverMessage(
            new BigInteger("bb2e6f44a38052b3f564fafcd477c4eb8cda1a8a553a4a5f38f1e1084d6a69f0", 16)
          )
        ),
        "01008a100108ce0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798024ebfeb5a2b6ad997e40efb4888b3f091a611df8298cf7fb24315b4d112ad7c3c03d41afc8c5875a8d52439d088b66ed63a5d64f16e1efd7f17c6036a923c637e5c034132d4c7eb387f12ef40ba3ec03723bda0ee5707f7471185aafc316167e851377300004203724c887a207569c4648a81bc2d66bddeefe14b3ed259ee320773ec0c7df714490338360ba3350c16c42839878fc13b641c0a67372c4217e2ceafe1d40405f03708"
      ),
      ProofTestCase(
        CAND(
          List(
            ProveDlog(
              Helpers.decodeECPoint("03670a10fcf68531423e3aa8bdad2d755eb5363ac53068e80d44578861f80abef3")
            ),
            ProveDlog(
              Helpers.decodeECPoint("0249829d9ca70fa3974c1354d7d112390e07b826032c5a7c3bc39e56b3f480bb87")
            )
          )
        ),
        ErgoAlgos.decodeUnsafe(
          "a00b476899e583aefc18b237a7a70e73baace72aa533271a561d3432c347dcaec8975fdefb36389abe21656aadcfda0a0259681ce17bc47c9539ae1e7068292bb9646a9ffe4e11653495bd67588cfd6454d82cc455036e5b"
        ),
        CAndUncheckedNode(
          Challenge @@ ErgoAlgos.decodeUnsafe("a00b476899e583aefc18b237a7a70e73baace72aa533271a").toColl,
          List(
            UncheckedSchnorr(
              ProveDlog(
                Helpers.decodeECPoint(
                  "03670a10fcf68531423e3aa8bdad2d755eb5363ac53068e80d44578861f80abef3"
                )
              ),
              None,
              Challenge @@ ErgoAlgos.decodeUnsafe("a00b476899e583aefc18b237a7a70e73baace72aa533271a").toColl,
              SecondDLogProverMessage(
                BigInt("561d3432c347dcaec8975fdefb36389abe21656aadcfda0a0259681ce17bc47c", 16)
              )
            ),
            UncheckedSchnorr(
              ProveDlog(
                Helpers.decodeECPoint(
                  "0249829d9ca70fa3974c1354d7d112390e07b826032c5a7c3bc39e56b3f480bb87"
                )
              ),
              None,
              Challenge @@ ErgoAlgos.decodeUnsafe("a00b476899e583aefc18b237a7a70e73baace72aa533271a").toColl,
              SecondDLogProverMessage(
                BigInt("9539ae1e7068292bb9646a9ffe4e11653495bd67588cfd6454d82cc455036e5b", 16)
              )
            )
          )
        ),
        "00000002010027100108cd03670a10fcf68531423e3aa8bdad2d755eb5363ac53068e80d44578861f80abef373000021024fc32f5fc7dad49005dc86b8ad95975d62ee4336cdddd4de8868414211370320010027100108cd0249829d9ca70fa3974c1354d7d112390e07b826032c5a7c3bc39e56b3f480bb877300002103f084eb45540454909d3e793d876262fa184f90412c33a046a0b1c4d7c8933f67"
      ),
      ProofTestCase(
        COR(
          List(
            ProveDlog(
              Helpers.decodeECPoint("0344789e3a797e713103f2a8edd673fac35e56d414c584e575aaa750f3e8728b5b")
            ),
            ProveDlog(
              Helpers.decodeECPoint("0249829d9ca70fa3974c1354d7d112390e07b826032c5a7c3bc39e56b3f480bb87")
            )
          )
        ),
        ErgoAlgos.decodeUnsafe(
          "c617e65a2ca62ac97bc33a33b76cb669622129ba0e094ad96287d97c2c6d6c8e48790d7c44961f7d958d59222ab4d7c814808a466a3e66e6f98e02d421757baa2842288b8d02787b5111db2e8924623790175e5bf27a2e4513e8eb196c22c8cf26a9d7b51cd7e386508db9c12b070d84"
        ),
        COrUncheckedNode(
          Challenge @@ ErgoAlgos.decodeUnsafe("c617e65a2ca62ac97bc33a33b76cb669622129ba0e094ad9").toColl,
          List(
            UncheckedSchnorr(
              ProveDlog(
                Helpers.decodeECPoint(
                  "0344789e3a797e713103f2a8edd673fac35e56d414c584e575aaa750f3e8728b5b"
                )
              ),
              None,
              Challenge @@ ErgoAlgos.decodeUnsafe("6287d97c2c6d6c8e48790d7c44961f7d958d59222ab4d7c8").toColl,
              SecondDLogProverMessage(
                BigInt("14808a466a3e66e6f98e02d421757baa2842288b8d02787b5111db2e89246237", 16)
              )
            ),
            UncheckedSchnorr(
              ProveDlog(
                Helpers.decodeECPoint(
                  "0249829d9ca70fa3974c1354d7d112390e07b826032c5a7c3bc39e56b3f480bb87"
                )
              ),
              None,
              Challenge @@ ErgoAlgos.decodeUnsafe("a4903f2600cb464733ba374ff3faa914f7ac709824bd9d11").toColl,
              SecondDLogProverMessage(
                BigInt("90175e5bf27a2e4513e8eb196c22c8cf26a9d7b51cd7e386508db9c12b070d84", 16)
              )
            )
          )
        ),
        "00010002010027100108cd0344789e3a797e713103f2a8edd673fac35e56d414c584e575aaa750f3e8728b5b730000210207700723f7cf3a94782a56d9366f3916c548616edf9bcbaecb892f8a52b28836010027100108cd0249829d9ca70fa3974c1354d7d112390e07b826032c5a7c3bc39e56b3f480bb8773000021039072557976001866ac8a1a6a8bd921e5b18171b195e6dabffff667f9a88ab9a2"
      ),
      ProofTestCase(
        COR(
          Array(
            ProveDHTuple(
              Helpers.decodeECPoint("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"),
              Helpers.decodeECPoint("03a5f4c3b8217557514df3df8537ca13f991b11538935b2ea407e8b24afcabe509"),
              Helpers.decodeECPoint("029837d12c86c29c92e74229dfd3fcb10933b696685209b14baa74dbabacb2dee5"),
              Helpers.decodeECPoint("03f17cefec3911966dc9952090325267a5cf7f9b0be76b02623021989d7f0007a2")
            ),
            COR(
              Array(
                ProveDlog(Helpers.decodeECPoint("03f997167c03aa234732e3a68126b371dffa1e409f62ca8fa18cea6acd1dbe54d5")),
                ProveDHTuple(
                  Helpers.decodeECPoint("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"),
                  Helpers.decodeECPoint("03f5921dde02233135665d006838fcb783deca634ee333c5541cc05a9012e684ee"),
                  Helpers.decodeECPoint("039b65625db7aad6d86599355b7cac785e6b5ac85b8a32e0d6927b324704d0a261"),
                  Helpers.decodeECPoint("02fc58b939b105231da101540c87e56f5703460c179935aaee47137f3c367904f1")
                )
              )
            )
          )
        ),
        ErgoAlgos.decodeUnsafe(
          "96addfddcc197bdbacf5c0142fb16c39384b3699fa47da7dffd3149193b042fda134c0e208fefcb791379959ac6fc731adf47e32000fc75e2923dba482c843c7f6b684cbf2ceec5bfdf5fe6d13cabe5d15f8295ca4e8094fba3c4716bfdfc3c462417a79a61fcc487d6997a42739d533eebffa3b420a6e2e44616a1341e5baa1165c6c22e91a81addd97c3bd2fe40ecdbbda6f43bf71240da8dac878c044c16d42a4b34c536bbb1b"
        ),
        COrUncheckedNode(
          Challenge @@ ErgoAlgos.decodeUnsafe("96addfddcc197bdbacf5c0142fb16c39384b3699fa47da7d").toColl,
          List(
            UncheckedDiffieHellmanTuple(
              ProveDHTuple(
                Helpers.decodeECPoint("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"),
                Helpers.decodeECPoint("03a5f4c3b8217557514df3df8537ca13f991b11538935b2ea407e8b24afcabe509"),
                Helpers.decodeECPoint("029837d12c86c29c92e74229dfd3fcb10933b696685209b14baa74dbabacb2dee5"),
                Helpers.decodeECPoint("03f17cefec3911966dc9952090325267a5cf7f9b0be76b02623021989d7f0007a2")
              ),
              None,
              Challenge @@ ErgoAlgos.decodeUnsafe("ffd3149193b042fda134c0e208fefcb791379959ac6fc731").toColl,
              SecondDHTupleProverMessage(new BigInteger("adf47e32000fc75e2923dba482c843c7f6b684cbf2ceec5bfdf5fe6d13cabe5d", 16))
            ),
            COrUncheckedNode(
              Challenge @@ ErgoAlgos.decodeUnsafe("697ecb4c5fa939260dc100f6274f908ea97cafc056281d4c").toColl,
              List(
                UncheckedSchnorr(
                  ProveDlog(Helpers.decodeECPoint("03f997167c03aa234732e3a68126b371dffa1e409f62ca8fa18cea6acd1dbe54d5")),
                  None,
                  Challenge @@ ErgoAlgos.decodeUnsafe("15f8295ca4e8094fba3c4716bfdfc3c462417a79a61fcc48").toColl,
                  SecondDLogProverMessage(BigInt("7d6997a42739d533eebffa3b420a6e2e44616a1341e5baa1165c6c22e91a81ad", 16))
                ),
                UncheckedDiffieHellmanTuple(
                  ProveDHTuple(
                    Helpers.decodeECPoint("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"),
                    Helpers.decodeECPoint("03f5921dde02233135665d006838fcb783deca634ee333c5541cc05a9012e684ee"),
                    Helpers.decodeECPoint("039b65625db7aad6d86599355b7cac785e6b5ac85b8a32e0d6927b324704d0a261"),
                    Helpers.decodeECPoint("02fc58b939b105231da101540c87e56f5703460c179935aaee47137f3c367904f1")
                  ),
                  None,
                  Challenge @@ ErgoAlgos.decodeUnsafe("7c86e210fb413069b7fd47e09890534acb3dd5b9f037d104").toColl,
                  SecondDHTupleProverMessage(new BigInteger("dd97c3bd2fe40ecdbbda6f43bf71240da8dac878c044c16d42a4b34c536bbb1b", 16))
                )
              )
            )
          )
        ),
        "0001000201008a100108ce0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f8179803a5f4c3b8217557514df3df8537ca13f991b11538935b2ea407e8b24afcabe509029837d12c86c29c92e74229dfd3fcb10933b696685209b14baa74dbabacb2dee503f17cefec3911966dc9952090325267a5cf7f9b0be76b02623021989d7f0007a273000042035e192266f309bebe0a3e50f96f8161ad4dbd6136771619b7560b8ea4271ff2be036ebbb7e8e91546c16a41d79971d81513813416c06ef4ee92825065c484f43c6f00010002010027100108cd03f997167c03aa234732e3a68126b371dffa1e409f62ca8fa18cea6acd1dbe54d573000021031a93fccb6536b097682276ec047138f95ad05369b8bd24d73ecdc46571b5a7e601008a100108ce0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f8179803f5921dde02233135665d006838fcb783deca634ee333c5541cc05a9012e684ee039b65625db7aad6d86599355b7cac785e6b5ac85b8a32e0d6927b324704d0a26102fc58b939b105231da101540c87e56f5703460c179935aaee47137f3c367904f1730000420359bc0180bf8e1df00dd5021dd43cb52acd5612fa5baa3d517222a514ed7e4d1903de533bce02969892436e113cad7270de0b3d051035629abd645d3470928f3700"
      ),
      ProofTestCase(
        COR(
          Array(
            CAND(
              Array(
                ProveDlog(Helpers.decodeECPoint("0368c0d88d9eb2972bbfc23c961de6307f6a944352cbfe316f262401feabdaa87d")),
                ProveDHTuple(
                  Helpers.decodeECPoint("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"),
                  Helpers.decodeECPoint("02badd523c2f5c12f4a3d4d667efb6ce8e95c8ad007cc697c34e91884335b55249"),
                  Helpers.decodeECPoint("02c455e55dc7bc731c5a487778e84814080fb70cc59957bc2a40c45373fe1ce14c"),
                  Helpers.decodeECPoint("029d4ec275379f9212a53e15994aef203dcec43a177c0b1f40afcf592e5753ce67")
                )
              )
            ),
            COR(
              Array(
                ProveDHTuple(
                  Helpers.decodeECPoint("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"),
                  Helpers.decodeECPoint("02a5af61e5c0eaad47ffdf29a91afffb1791295fff434831802e7f36b885fc2aa7"),
                  Helpers.decodeECPoint("03a9aa914199bb0e3b00ff4dd6ff82ec34d5f451825c28c41dc6432c763b6061e2"),
                  Helpers.decodeECPoint("0315d84dba1b29074f766e57bb11843687da899180cf2487ccecd0a3ec5f05365a")
                ),
                ProveDHTuple(
                  Helpers.decodeECPoint("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"),
                  Helpers.decodeECPoint("02a5af61e5c0eaad47ffdf29a91afffb1791295fff434831802e7f36b885fc2aa7"),
                  Helpers.decodeECPoint("03a9aa914199bb0e3b00ff4dd6ff82ec34d5f451825c28c41dc6432c763b6061e2"),
                  Helpers.decodeECPoint("0315d84dba1b29074f766e57bb11843687da899180cf2487ccecd0a3ec5f05365a")
                )
              )
            )
          )
        ),
        ErgoAlgos.decodeUnsafe(
          "4fdc76711fd844de0831d8e90ebaf9c622117a062b2f8b63ff8b9c2a4eed345a11c697f6850cf3a38763d738539ad2d2e0a3e44384f23eee260931d88e1f5241a2600a7c98545ada675fd5e627e8e84f140fc95e28775cde52e71bb4d7b5ee2564553fac5b52202530fcbcdf205b7cca145202fb2a5bb181a890eb15536b08b747ea163f6b5d32a116fa9e1eb6b348fd82d3ebc11c125e5bc3f09c499aa0a8db14dc1780b4181f9bae5ed0f743f71b82b18784380814507d810cbef61ebc0b30e7f324083e2d3d08"
        ),
        COrUncheckedNode(
          Challenge @@ ErgoAlgos.decodeUnsafe("4fdc76711fd844de0831d8e90ebaf9c622117a062b2f8b63").toColl,
          List(
            CAndUncheckedNode(
              Challenge @@ ErgoAlgos.decodeUnsafe("ff8b9c2a4eed345a11c697f6850cf3a38763d738539ad2d2").toColl,
              List(
                UncheckedSchnorr(
                  ProveDlog(Helpers.decodeECPoint("0368c0d88d9eb2972bbfc23c961de6307f6a944352cbfe316f262401feabdaa87d")),
                  None,
                  Challenge @@ ErgoAlgos.decodeUnsafe("ff8b9c2a4eed345a11c697f6850cf3a38763d738539ad2d2").toColl,
                  SecondDLogProverMessage(BigInt("e0a3e44384f23eee260931d88e1f5241a2600a7c98545ada675fd5e627e8e84f", 16))
                ),
                UncheckedDiffieHellmanTuple(
                  ProveDHTuple(
                    Helpers.decodeECPoint("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"),
                    Helpers.decodeECPoint("02badd523c2f5c12f4a3d4d667efb6ce8e95c8ad007cc697c34e91884335b55249"),
                    Helpers.decodeECPoint("02c455e55dc7bc731c5a487778e84814080fb70cc59957bc2a40c45373fe1ce14c"),
                    Helpers.decodeECPoint("029d4ec275379f9212a53e15994aef203dcec43a177c0b1f40afcf592e5753ce67")
                  ),
                  None,
                  Challenge @@ ErgoAlgos.decodeUnsafe("ff8b9c2a4eed345a11c697f6850cf3a38763d738539ad2d2").toColl,
                  SecondDHTupleProverMessage(new BigInteger("140fc95e28775cde52e71bb4d7b5ee2564553fac5b52202530fcbcdf205b7cca", 16))
                )
              )
            ),
            COrUncheckedNode(
              Challenge @@ ErgoAlgos.decodeUnsafe("b057ea5b5135708419f74f1f8bb60a65a572ad3e78b559b1").toColl,
              List(
                UncheckedDiffieHellmanTuple(
                  ProveDHTuple(
                    Helpers.decodeECPoint("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"),
                    Helpers.decodeECPoint("02a5af61e5c0eaad47ffdf29a91afffb1791295fff434831802e7f36b885fc2aa7"),
                    Helpers.decodeECPoint("03a9aa914199bb0e3b00ff4dd6ff82ec34d5f451825c28c41dc6432c763b6061e2"),
                    Helpers.decodeECPoint("0315d84dba1b29074f766e57bb11843687da899180cf2487ccecd0a3ec5f05365a")
                  ),
                  None,
                  Challenge @@ ErgoAlgos.decodeUnsafe("145202fb2a5bb181a890eb15536b08b747ea163f6b5d32a1").toColl,
                  SecondDHTupleProverMessage(new BigInteger("16fa9e1eb6b348fd82d3ebc11c125e5bc3f09c499aa0a8db14dc1780b4181f9b", 16))
                ),
                UncheckedDiffieHellmanTuple(
                  ProveDHTuple(
                    Helpers.decodeECPoint("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"),
                    Helpers.decodeECPoint("02a5af61e5c0eaad47ffdf29a91afffb1791295fff434831802e7f36b885fc2aa7"),
                    Helpers.decodeECPoint("03a9aa914199bb0e3b00ff4dd6ff82ec34d5f451825c28c41dc6432c763b6061e2"),
                    Helpers.decodeECPoint("0315d84dba1b29074f766e57bb11843687da899180cf2487ccecd0a3ec5f05365a")
                  ),
                  None,
                  Challenge @@ ErgoAlgos.decodeUnsafe("a405e8a07b6ec105b167a40ad8dd02d2e298bb0113e86b10").toColl,
                  SecondDHTupleProverMessage(new BigInteger("ae5ed0f743f71b82b18784380814507d810cbef61ebc0b30e7f324083e2d3d08", 16))
                )
              )
            )
          )
        ),
        "0001000200000002010027100108cd0368c0d88d9eb2972bbfc23c961de6307f6a944352cbfe316f262401feabdaa87d7300002102628c0e33748b2e120536945d77e69b3fb5fb3878c5697c20bf70b8459f476e4b01008a100108ce0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f8179802badd523c2f5c12f4a3d4d667efb6ce8e95c8ad007cc697c34e91884335b5524902c455e55dc7bc731c5a487778e84814080fb70cc59957bc2a40c45373fe1ce14c029d4ec275379f9212a53e15994aef203dcec43a177c0b1f40afcf592e5753ce6773000042030064fb366dce2d424a14308d2c3d562f3558b042250cc28eab47a594efdec9c603787ca34604619468b3c677e2887a0de382a093264729a15ace38f1e3be20fd0f0001000201008a100108ce0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f8179802a5af61e5c0eaad47ffdf29a91afffb1791295fff434831802e7f36b885fc2aa703a9aa914199bb0e3b00ff4dd6ff82ec34d5f451825c28c41dc6432c763b6061e20315d84dba1b29074f766e57bb11843687da899180cf2487ccecd0a3ec5f05365a7300004202f718c7ac18b4d942056b7237809f843811a04314044d6dd517022f6d15198c4e0329ecb543ea406fda96d3088196f4490cacdea49286c954ea2924523a57847a7d01008a100108ce0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f8179802a5af61e5c0eaad47ffdf29a91afffb1791295fff434831802e7f36b885fc2aa703a9aa914199bb0e3b00ff4dd6ff82ec34d5f451825c28c41dc6432c763b6061e20315d84dba1b29074f766e57bb11843687da899180cf2487ccecd0a3ec5f05365a73000042021cc93df3de5b405ee306f5c7003b6abbfb7b4d0ab356c36d0c454a4c6b3403f603608a98d8aa0781ec96970353b4e22ae05454833528ae8f8468d0a8fc3f92675a"
      ),
      ProofTestCase(
        CTHRESHOLD(
          2,
          Array(
            ProveDlog(Helpers.decodeECPoint("03a5a5234701fff48be4ed1b3e1fab446657eeddb52e2573c52b9c4021f2403866")),
            ProveDHTuple(
              Helpers.decodeECPoint("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"),
              Helpers.decodeECPoint("036b52166c82e61d0954d521a8a8a20af0eb1adb7f23a9c3ee1ebac1242e35ac18"),
              Helpers.decodeECPoint("0339a5debbb2bb67aa560e98dbfc4050e8ca0643683314cd1bc911f11c5477a312"),
              Helpers.decodeECPoint("02730455ebb8c01a89dced09c5253c9bfa4b1471d1068ba30ab226104a6551c461")
            ),
            ProveDHTuple(
              Helpers.decodeECPoint("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"),
              Helpers.decodeECPoint("029775461de9886800dac39ef14f535e38dc3b2719770d69627007aee9f638e918"),
              Helpers.decodeECPoint("02fdc07913da5615db53a0351a47131832d77e29c6520248e82a568d02b09d4604"),
              Helpers.decodeECPoint("03cefefa1511430ca2a873759107085f269f6fbcd4e836db7760749f52b7f7923a")
            )
          )
        ),
        ErgoAlgos.decodeUnsafe(
          "c94696c3e3089d9fd1174c18e6dd22f1be8003bbea08011fcf39310e7c9049c1c9966198b8d63a2f19e98843b81b74399f662dba4e764cd548406dd180453dd1bc0e24562f0184d189ca25a41ca8b54ada857dd649d3228a8c359ac499d430ecada3f92d5206cddeffb16248068c1003477d717e04afbf206c87a59ce5263ee7cc4020b5772d91b1df00bd72b15347fd"
        ),
        CThresholdUncheckedNode(
          Challenge @@ ErgoAlgos.decodeUnsafe("c94696c3e3089d9fd1174c18e6dd22f1be8003bbea08011f").toColl,
          List(
            UncheckedSchnorr(
              ProveDlog(Helpers.decodeECPoint("03a5a5234701fff48be4ed1b3e1fab446657eeddb52e2573c52b9c4021f2403866")),
              None,
              Challenge @@ ErgoAlgos.decodeUnsafe("067fa7cd9f98d45e18812d805e0b18dea7698bf852137526").toColl,
              SecondDLogProverMessage(BigInt("9f662dba4e764cd548406dd180453dd1bc0e24562f0184d189ca25a41ca8b54a", 16))
            ),
            UncheckedDiffieHellmanTuple(
              ProveDHTuple(
                Helpers.decodeECPoint("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"),
                Helpers.decodeECPoint("036b52166c82e61d0954d521a8a8a20af0eb1adb7f23a9c3ee1ebac1242e35ac18"),
                Helpers.decodeECPoint("0339a5debbb2bb67aa560e98dbfc4050e8ca0643683314cd1bc911f11c5477a312"),
                Helpers.decodeECPoint("02730455ebb8c01a89dced09c5253c9bfa4b1471d1068ba30ab226104a6551c461")
              ),
              None,
              Challenge @@ ErgoAlgos.decodeUnsafe("5735f4df1b280e1d423a8f28977057af8c52123c9a3fe96d").toColl,
              SecondDHTupleProverMessage(new BigInteger("da857dd649d3228a8c359ac499d430ecada3f92d5206cddeffb16248068c1003", 16))
            ),
            UncheckedDiffieHellmanTuple(
              ProveDHTuple(
                Helpers.decodeECPoint("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"),
                Helpers.decodeECPoint("029775461de9886800dac39ef14f535e38dc3b2719770d69627007aee9f638e918"),
                Helpers.decodeECPoint("02fdc07913da5615db53a0351a47131832d77e29c6520248e82a568d02b09d4604"),
                Helpers.decodeECPoint("03cefefa1511430ca2a873759107085f269f6fbcd4e836db7760749f52b7f7923a")
              ),
              None,
              Challenge @@ ErgoAlgos.decodeUnsafe("980cc5d167b847dc8baceeb02fa66d8095bb9a7f22249d54").toColl,
              SecondDHTupleProverMessage(new BigInteger("477d717e04afbf206c87a59ce5263ee7cc4020b5772d91b1df00bd72b15347fd", 16))
            )
          ),
          2,
          Some(
            GF2_192_Poly.fromByteArray(
              ErgoAlgos.decodeUnsafe("c94696c3e3089d9fd1174c18e6dd22f1be8003bbea08011f"),
              ErgoAlgos.decodeUnsafe("cf39310e7c9049c1c9966198b8d63a2f19e98843b81b7439")
            )
          )
        ),
        "0002020003010027100108cd03a5a5234701fff48be4ed1b3e1fab446657eeddb52e2573c52b9c4021f2403866730000210337af2d4066efa710279bcf8e1e53bb0cc7a164d04b5b1734f182fc33517a1aa701008a100108ce0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798036b52166c82e61d0954d521a8a8a20af0eb1adb7f23a9c3ee1ebac1242e35ac180339a5debbb2bb67aa560e98dbfc4050e8ca0643683314cd1bc911f11c5477a31202730455ebb8c01a89dced09c5253c9bfa4b1471d1068ba30ab226104a6551c46173000042033569a1a0dc21c510fb415b8cf11bc79d40ba0c9344ac4f6d031e0cefb0c5a86a039e35b6fde9f8443e18527807007619874abcc5a6d28d8ad9324bf89749d2f72901008a100108ce0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798029775461de9886800dac39ef14f535e38dc3b2719770d69627007aee9f638e91802fdc07913da5615db53a0351a47131832d77e29c6520248e82a568d02b09d460403cefefa1511430ca2a873759107085f269f6fbcd4e836db7760749f52b7f7923a7300004203e4ffc0fd026f9b9ab6d80d13e453f14caf09b1157ffca8d3da899b52f6b10d1603e119655077c1fda14ab47138496b50f4e7305edbfd0a6925952a4650d4f2729f"
      )
    )

    cases.zipWithIndex.foreach { case (c, _) =>
      val sigBytes = SigSerializer.toProofBytes(c.uncheckedTree)
      sigBytes shouldBe c.proof
      val uncheckedTree = SigSerializer.parseAndComputeChallenges(c.prop, c.proof)(null)
      uncheckedTree shouldBe c.uncheckedTree

      val hex = getFiatShamirHex(c.uncheckedTree)

      if (c.fiatShamirHex.isEmpty) {
        // uncomment to run on JVM only and print output test vector
//        val vector = sigmastate.helpers.SigmaPPrint(hex, width = 150, height = 150)
//        println(
//          s"""case $iCase: -------------------------
//            |hex: $vector
//            |""".stripMargin)
      }

      hex shouldBe c.fiatShamirHex
    }
  }

  property("Invalid signature parsing") {
    forAll { bytes: Array[Byte] =>
      val r = SigmaSerializer.startReader(bytes)
      val nRequested = bytes.length + 1 // request more than present
      val readBytes = r.getBytesUnsafe(nRequested)
      readBytes shouldBe bytes

      // we now at the limit position of reader, and still can get all buffer bytes
      val allBytes = r.getAllBufferBytes
      allBytes shouldBe bytes

      r.position = 0
      var reported = false
      val res = SigSerializer.readBytesChecked(r,
        numRequestedBytes = nRequested,
        onError = _ => reported = true)
      res shouldBe bytes
      reported shouldBe true
    }
  }
}
