package sigmastate.utxo

import scorex.util.encode.Base16
import scorex.crypto.hash.Blake2b256
import sigma.ast._
import sigma.ast.syntax._
import sigmastate._
import sigmastate.helpers.{CompilerTestingCommons, ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTestInterpreter}
import sigma.{Coll, Colls}


class ContextEnrichingSpecification extends CompilerTestingCommons
  with CompilerCrossVersionProps {

  implicit lazy val IR: TestingIRContext = new TestingIRContext

  property("context enriching mixed w. crypto") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val preimage = prover.contextExtenders(1).value.asInstanceOf[Coll[Byte]]
    val pubkey = prover.dlogSecrets.head.publicImage

    val env = Map("blake" -> Colls.fromArray(Blake2b256(preimage.toArray)), "pubkey" -> pubkey)
    val prop = compile(env,
      """{
        |  pubkey && blake2b256(getVar[Coll[Byte]](1).get) == blake
        |}
      """.stripMargin).asSigmaProp
    val propTree = mkTestErgoTree(prop)

    val propExpected = SigmaAnd(
      pubkey,
      EQ(CalcBlake2b256(GetVarByteArray(1).get), ByteArrayConstant(Blake2b256(preimage.toArray))).toSigmaProp
    )
    prop shouldBe propExpected

    val ctx = ErgoLikeContextTesting.dummy(fakeSelf, activatedVersionInTests)
    val pr = prover.prove(propTree, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new ErgoLikeTestInterpreter
    verifier.verify(env, propTree, ctx, pr.proof, fakeMessage).map(_._1).getOrElse(false) shouldBe false //context w/out extensions
    verifier.verify(env, propTree, ctxv, pr.proof, fakeMessage).get._1 shouldBe true
  }

  property("context enriching mixed w. crypto 2") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val preimage1 = prover.contextExtenders(1).value.asInstanceOf[Coll[Byte]]
    val preimage2 = prover.contextExtenders(2).value.asInstanceOf[Coll[Byte]]
    val pubkey = prover.dlogSecrets.head.publicImage

    val env = Map("blake" -> Colls.fromArray(Blake2b256(preimage1.append(preimage2).toArray)), "pubkey" -> pubkey)
    val prop = compile(env,
      """{
        |  pubkey && blake2b256(getVar[Coll[Byte]](1).get ++ getVar[Coll[Byte]](2).get) == blake
        |}
      """.stripMargin).asSigmaProp
    val propTree = mkTestErgoTree(prop)

    val propExpected = SigmaAnd(
      pubkey,
      EQ(
        CalcBlake2b256(Append(GetVarByteArray(1).get, GetVarByteArray(2).get)),
        ByteArrayConstant(Blake2b256(preimage1.append(preimage2).toArray))
      ).toSigmaProp
    )
    prop shouldBe propExpected

    val ctx = ErgoLikeContextTesting.dummy(fakeSelf, activatedVersionInTests)
    val pr = prover.prove(propTree, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new ErgoLikeTestInterpreter
    verifier.verify(env, propTree, ctx, pr.proof, fakeMessage).map(_._1).getOrElse(false) shouldBe false //context w/out extensions
    verifier.verify(env, propTree, ctxv, pr.proof, fakeMessage).get._1 shouldBe true
  }

  property("prover enriching context - xor") {
    val v1 = Base16.decode("abcdef7865").get
    val k1 = 21: Byte

    val v2 = Base16.decode("10abdca345").get
    val k2 = 22: Byte

    val r = Base16.decode("bb6633db20").get

    val prover = new ContextEnrichingTestProvingInterpreter()
      .withContextExtender(k1, ByteArrayConstant(v1))
      .withContextExtender(k2, ByteArrayConstant(v2))

    val env = Map("k1" -> k1.toInt, "k2" -> k2.toInt, "r" -> Colls.fromArray(r))
    val prop = compile(env,
      "{ xor(getVar[Coll[Byte]](k1).get, getVar[Coll[Byte]](k2).get) == r }").asBoolValue.toSigmaProp
    val propTree = mkTestErgoTree(prop)

    val propExpected = EQ(Xor(GetVarByteArray(k1).get, GetVarByteArray(k2).get), ByteArrayConstant(r)).toSigmaProp
    prop shouldBe propExpected

    val ctx = ErgoLikeContextTesting.dummy(fakeSelf, activatedVersionInTests)
    val pr = prover.prove(propTree, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new ErgoLikeTestInterpreter
    verifier.verify(env, propTree, ctxv, pr.proof, fakeMessage).get._1 shouldBe true

    // negative tests: context w/out extensions
    assertExceptionThrown(
      verifier.verify(env, propTree, ctx, pr.proof, fakeMessage).get,
      rootCause(_).isInstanceOf[NoSuchElementException]
    )
  }

  /**
    * The script is asking for a hash function preimage. The "proof" could be replayed, so not really a proof.
    */
  property("prover enriching context") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val preimage = prover.contextExtenders(1).value.asInstanceOf[Coll[Byte]]

    val env = Map("blake" -> Colls.fromArray(Blake2b256(preimage.toArray)))
    val prop = compile(env,
      """{
        |  blake2b256(getVar[Coll[Byte]](1).get) == blake
        |}
      """.stripMargin).asBoolValue.toSigmaProp
    val propTree = mkTestErgoTree(prop)

    val propExpected = EQ(CalcBlake2b256(GetVarByteArray(1).get), ByteArrayConstant(Blake2b256(preimage.toArray))).toSigmaProp
    prop shouldBe propExpected

    val ctx = ErgoLikeContextTesting.dummy(fakeSelf, activatedVersionInTests)
    val pr = prover.prove(propTree, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new ErgoLikeTestInterpreter
    verifier.verify(env, propTree, ctxv, pr.proof, fakeMessage).get._1 shouldBe true

    // negative tests: context w/out extensions
    assertExceptionThrown(
      verifier.verify(env, propTree, ctx, pr.proof, fakeMessage).get,
      rootCause(_).isInstanceOf[NoSuchElementException])
  }

  property("prover enriching context 2") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val preimage1 = prover.contextExtenders(1).value.asInstanceOf[Coll[Byte]]
    val preimage2 = prover.contextExtenders(2).value.asInstanceOf[Coll[Byte]]

    val env = Map("blake" -> Colls.fromArray(Blake2b256(preimage2.append(preimage1).toArray)))
    val prop = compile(env,
      """{
        |  blake2b256(getVar[Coll[Byte]](2).get ++ getVar[Coll[Byte]](1).get) == blake
        |}
      """.stripMargin).asBoolValue.toSigmaProp
    val propTree = mkTestErgoTree(prop)

    val propExpected = EQ(CalcBlake2b256(Append(GetVarByteArray(2).get, GetVarByteArray(1).get)),
      ByteArrayConstant(Blake2b256(preimage2.append(preimage1).toArray))).toSigmaProp
    prop shouldBe propExpected

    val ctx = ErgoLikeContextTesting.dummy(fakeSelf, activatedVersionInTests)
    val pr = prover.prove(propTree, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new ErgoLikeTestInterpreter
    verifier.verify(env, propTree, ctxv, pr.proof, fakeMessage).get._1 shouldBe true

    // negative tests: context w/out extensions
    assertExceptionThrown(
      verifier.verify(env, propTree, ctx, pr.proof, fakeMessage).get,
      rootCause(_).isInstanceOf[NoSuchElementException]
    )
  }
}
