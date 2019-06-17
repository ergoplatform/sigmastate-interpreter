package sigmastate.utxo

import org.ergoplatform.ErgoBox.{R4, R5, R6, R7, R8, R9}
import org.ergoplatform.ErgoConstants.ScriptCostLimit
import org.ergoplatform._
import org.ergoplatform.validation.ValidationRules
import org.scalacheck.Gen
import scalan.util.BenchmarkUtil
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert, Lookup}
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util.encode.Base16
import scorex.utils.Random
import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate.lang.Terms._
import sigmastate._
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.eval._
import sigmastate.interpreter.Interpreter._
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, SigmaTestingCommons, ErgoLikeTestInterpreter}
import sigmastate.interpreter.ContextExtension
import sigmastate.lang.exceptions.CosterException
import special.collection.Coll


/**
  * Suite of tests where a malicious prover tries to feed a verifier with a script which is costly to verify
  */
class SpamSpecification extends SigmaTestingCommons {
  implicit lazy val IR: TestingIRContext = new TestingIRContext
  //we assume that verifier must finish verification of any script in less time than 3M hash calculations
  // (for the Blake2b256 hash function over a single block input)
  val Timeout: Long = {
    val block = Array.fill(16)(0: Byte)
    val hf = Blake2b256

    //just in case to heat up JVM
    (1 to 2000000).foreach(_ => hf(block))

    val t0 = System.currentTimeMillis()
    (1 to 4000000).foreach(_ => hf(block))
    val t = System.currentTimeMillis()
    t - t0
  }

  def termination[T](fn: () => T): (T, Boolean) = {
    val t0 = System.currentTimeMillis()
    val res = fn()
    val t = System.currentTimeMillis()
    (res, (t - t0) < Timeout)
  }

  property("nested loops 1") {
    val alice = new ContextEnrichingTestProvingInterpreter
    val alicePubKey:ProveDlog = alice.dlogSecrets.head.publicImage
    val largeColl = Colls.fromArray((1 to 50).toArray)
    val env = Map(
      ScriptNameProp -> "Script",
      "alice" -> alicePubKey,
      "largeColl" -> largeColl
    )
    val spamScript = compile(env,
      """{
        |  val valid  = largeColl.forall({(i:Int) =>
        |     largeColl.exists({(j:Int) =>
        |       i != j
        |     }
        |     ) &&
        |     largeColl.exists({(j:Int) =>
        |       largeColl.forall({(k:Int) =>
        |         k != i + j
        |       }
        |       ) &&
        |       i != j
        |     }
        |     ) &&
        |     OUTPUTS.exists({(x:Box) =>
        |       x.propositionBytes.size >= i
        |     }
        |     )
        |   }
        |  )
        |  ! valid
        |}
      """.stripMargin).asBoolValue.toSigmaProp

    //todo: make value dependent on CostTable constants, not magic constant
    val ba = Random.randomBytes(10000000)

    val id = 11: Byte
    val id2 = 12: Byte

    val prover = new ContextEnrichingTestProvingInterpreter()
      .withContextExtender(id, ByteArrayConstant(ba))
      .withContextExtender(id2, ByteArrayConstant(ba))

    //val spamScript = EQ(CalcBlake2b256(GetVarByteArray(id).get), CalcBlake2b256(GetVarByteArray(id2).get)).toSigmaProp

    val ctx = ErgoLikeContext.dummy(fakeSelf)

    val pr = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), spamScript, ctx, fakeMessage).get

    val verifier = new ErgoLikeTestInterpreter
    val (_, calcTime) = BenchmarkUtil.measureTime {
      verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), spamScript, ctx, pr, fakeMessage)
    }
    println(s"Verify time: $calcTime millis")
    println("Timeout: " + Timeout)
    (calcTime < Timeout) shouldBe true
  }

  property("nested loops 2") {
    val alice = new ContextEnrichingTestProvingInterpreter
    val alicePubKey:ProveDlog = alice.dlogSecrets.head.publicImage
    val largeColl = Colls.fromArray((1 to 50).toArray)
    val env = Map(
      ScriptNameProp -> "Script",
      "alice" -> alicePubKey,
      "largeColl" -> largeColl
    )
    val spamScript = compile(env,
      """{
        |  val valid  = largeColl.forall({(i:Int) =>
        |     largeColl.exists({(j:Int) =>
        |       largeColl.forall({(k:Int) =>
        |         k != i + j
        |       }
        |       )
        |     }
        |     )
        |   }
        |  )
        |  alice && valid
        |}
      """.stripMargin).asBoolValue.toSigmaProp

    //todo: make value dependent on CostTable constants, not magic constant
    val ba = Random.randomBytes(10000000)

    val id = 11: Byte
    val id2 = 12: Byte

    val prover = new ContextEnrichingTestProvingInterpreter()
      .withContextExtender(id, ByteArrayConstant(ba))
      .withContextExtender(id2, ByteArrayConstant(ba))

    //val spamScript = EQ(CalcBlake2b256(GetVarByteArray(id).get), CalcBlake2b256(GetVarByteArray(id2).get)).toSigmaProp

    val ctx = ErgoLikeContext.dummy(fakeSelf)

    val pr = prover.withSecrets(alice.dlogSecrets).prove(emptyEnv + (ScriptNameProp -> "prove"), spamScript, ctx, fakeMessage).get

    val verifier = new ErgoLikeTestInterpreter

    val (_, calcTime) = BenchmarkUtil.measureTime {
      verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), spamScript, ctx, pr, fakeMessage)
    }
    println(s"Verify time: $calcTime millis")
    println("Timeout: " + Timeout)
    (calcTime < Timeout) shouldBe true

  }

  property("large num of inputs 1") {
    // runtime is high, test failing
    /*
      TIMEOUT IS 2150
      TIME IS 598
     */
    val alice = new ContextEnrichingTestProvingInterpreter
    val alicePubKey:ProveDlog = alice.dlogSecrets.head.publicImage
    val minNumInputs = 10
    val minNumOutputs = 10
    val env = Map(
      ScriptNameProp -> "Script",
      "alice" -> alicePubKey,
      "minNumInputs" -> minNumInputs,
      "minNumOutputs" -> minNumOutputs
    )
    val spamScript = compile(env,
      """{
        |  val valid  = INPUTS.exists({(ib:Box) =>
        |     OUTPUTS.exists({(ob:Box) =>
        |       OUTPUTS.exists({(ob2:Box) =>
        |         val ib_r4 = ib.R4[Byte].get
        |         val ib_r5 = ib.R5[SigmaProp].get
        |         val ib_r6 = ib.R6[Int].get
        |         val ib_r7 = ib.R7[Coll[Long]].get
        |         val ib_r8 = ib.R8[Coll[Byte]].get
        |         val ob_r4 = ob.R4[Byte].get
        |         val ob_r5 = ob.R5[SigmaProp].get
        |         val ob_r6 = ob.R6[Int].get
        |         val ob_r7 = ob.R7[Coll[Long]].get
        |         val ob_r8 = ob.R8[Coll[Byte]].get
        |         val ob2_r4 = ob2.R4[Byte].get
        |         val ob2_r5 = ob2.R5[SigmaProp].get
        |         val ob2_r6 = ob2.R6[Int].get
        |         val ob2_r7 = ob2.R7[Coll[Long]].get
        |         val ob2_r8 = ob2.R8[Coll[Byte]].get
        |         ib.propositionBytes == ob.propositionBytes && ob2.propositionBytes.size <= SELF.propositionBytes.size &&
        |         ib_r4 == ob_r4 && ob_r4 == ob2_r4 &&
        |         ib_r5 == ob_r5 && ob_r5 == ob2_r5 &&
        |         ib_r6 == ob_r6 && ob_r6 == ob2_r6 &&
        |         ib_r7 == ob_r7 && ob_r7 == ob2_r7 &&
        |         ib_r8 == ob_r8 && ob_r8 == ob2_r8
        |       }
        |       )
        |     }
        |     )
        |  }
        |  ) && INPUTS.size >= minNumInputs && OUTPUTS.size >= minNumOutputs
        |  alice && !valid
        |}
      """.stripMargin).asBoolValue.toSigmaProp
    val longs = Array[Long](1, 2, 3)

    val output = ErgoBox(1, alicePubKey, 10, Nil,
      Map(
        R4 -> ByteConstant(1),
        R5 -> SigmaPropConstant(alicePubKey),
        R6 -> IntConstant(10),
        R7 -> LongArrayConstant(longs),
        R8 -> ByteArrayConstant(Base16.decode("123456123456123456123456123456123456123456123456123456123456123456").get),
      )
    )

    val input = ErgoBox(1, spamScript, 10, Nil,
      Map(
        R4 -> ByteConstant(1),
        R5 -> SigmaPropConstant(alicePubKey),
        R6 -> IntConstant(10),
        R7 -> LongArrayConstant(longs),
        R8 -> ByteArrayConstant(Base16.decode("123456123456123456123456123456123456123456123456123456123456123456").get)
      )
    )
    val outBoxes:IndexedSeq[ErgoBoxCandidate] = IndexedSeq.fill(minNumOutputs)(output)
    val inBoxes:IndexedSeq[ErgoBox] = IndexedSeq.fill(minNumOutputs)(input)
    //normally this transaction would invalid (why?), but we're not checking it in this test
    val tx = createTransaction(outBoxes)

    val context = ErgoLikeContext(
      currentHeight = 10,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = inBoxes,
      spendingTransaction = tx,
      self = inBoxes(0) // what is the use of self?
    )


    val prover = new ContextEnrichingTestProvingInterpreter()

    val pr = prover.withSecrets(alice.dlogSecrets).prove(emptyEnv + (ScriptNameProp -> "prove"), spamScript, context, fakeMessage).get

    val verifier = new ErgoLikeTestInterpreter
    val (res, terminated) = termination(() =>
      verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), spamScript, context, pr, fakeMessage)
    )
    res.isFailure shouldBe true
    terminated shouldBe true
  }

  property("large num of inputs 2") {
    /*
    Also failing, but time is high

    TIMEOUT IS 2062
    TIME IS 1476

     */
    val alice = new ContextEnrichingTestProvingInterpreter
    val alicePubKey:ProveDlog = alice.dlogSecrets.head.publicImage
    val minNumInputs = 60
    val minNumOutputs = 60
    val env = Map(
      ScriptNameProp -> "Script",
      "alice" -> alicePubKey,
      "minNumInputs" -> minNumInputs,
      "minNumOutputs" -> minNumOutputs
    )
    val spamScript = compile(env,
      """{
          |  val valid  = INPUTS.exists({(ib:Box) =>
          |     OUTPUTS.exists({(ob:Box) =>
          |         val ib_r4 = ib.R4[Byte].get
          |         val ib_r5 = ib.R5[SigmaProp].get
          |         val ib_r6 = ib.R6[Int].get
          |         val ib_r7 = ib.R7[Coll[Long]].get
          |         val ib_r8 = ib.R8[Coll[Byte]].get
          |         val ib_r9 = ib.R9[Coll[Coll[Byte]]].get
          |         val ob_r4 = ob.R4[Byte].get
          |         val ob_r5 = ob.R5[SigmaProp].get
          |         val ob_r6 = ob.R6[Int].get
          |         val ob_r7 = ob.R7[Coll[Long]].get
          |         val ob_r8 = ob.R8[Coll[Byte]].get
          |         val ob_r9 = ob.R9[Coll[Coll[Byte]]].get
          |         ib.propositionBytes == ob.propositionBytes && ob.propositionBytes.size <= SELF.propositionBytes.size &&
          |         ib_r4 == ob_r4 &&
          |         ib_r5 == ob_r5 &&
          |         ib_r6 == ob_r6 &&
          |         ib_r7 == ob_r7 &&
          |         ib_r8 == ob_r8 &&
          |         ib_r9 != ob_r9
          |     }
          |     )
          |  }
          |  ) && INPUTS.size >= minNumInputs && OUTPUTS.size >= minNumOutputs
          |  alice && !valid
          |}
        """.stripMargin).asBoolValue.toSigmaProp

    val collCollByte = Colls.fromItems(Colls.fromArray((1 to 100).map(_.toByte).toArray))
    val longs = (1 to 100).map(_.toLong).toArray

    object ByteArrayArrayConstant {
      def apply(value: Coll[Coll[Byte]]): CollectionConstant[SByteArray.type] = CollectionConstant[SByteArray.type](value, SByteArray)
    }


    val output = ErgoBox(1, alicePubKey, 10, Nil,
      Map(
        R4 -> ByteConstant(1),
        R5 -> SigmaPropConstant(alicePubKey),
        R6 -> IntConstant(10),
        R7 -> LongArrayConstant(longs),
        R8 -> ByteArrayConstant(Base16.decode("123456123456123456123456123456123456123456123456123456123456123456").get),
        R9 -> ByteArrayArrayConstant(collCollByte)
    )
    )

    val input = ErgoBox(1, spamScript, 10, Nil,
      Map(
        R4 -> ByteConstant(1),
        R5 -> SigmaPropConstant(alicePubKey),
        R6 -> IntConstant(10),
        R7 -> LongArrayConstant(longs),
        R8 -> ByteArrayConstant(Base16.decode("123456123456123456123456123456123456123456123456123456123456123456").get),
        R9 -> ByteArrayArrayConstant(collCollByte)
      )
    )
    val outBoxes:IndexedSeq[ErgoBoxCandidate] = IndexedSeq.fill(minNumOutputs)(output)
    val inBoxes:IndexedSeq[ErgoBox] = IndexedSeq.fill(minNumOutputs)(input)
    //normally this transaction would invalid (why?), but we're not checking it in this test
    val tx = createTransaction(outBoxes)

    val context = ErgoLikeContext(
      currentHeight = 10,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = inBoxes,
      spendingTransaction = tx,
      self = inBoxes(0) // what is the use of self?
    )


    val prover = new ContextEnrichingTestProvingInterpreter()

    val pr = prover.withSecrets(alice.dlogSecrets).prove(emptyEnv + (ScriptNameProp -> "prove"), spamScript, context, fakeMessage).get

    val verifier = new ErgoLikeTestInterpreter
    val (res, terminated) = termination(() =>
      verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), spamScript, context, pr, fakeMessage)
    )
    res.isFailure shouldBe true
    terminated shouldBe true
  }


  property("huge byte array") {
    //TODO coverage: make value dependent on CostTable constants, not magic constant
    val ba = Random.randomBytes(10000000)

    val id = 11: Byte
    val id2 = 12: Byte

    val prover = new ContextEnrichingTestProvingInterpreter()
      .withContextExtender(id, ByteArrayConstant(ba))
      .withContextExtender(id2, ByteArrayConstant(ba))

    val spamScript = EQ(CalcBlake2b256(GetVarByteArray(id).get), CalcBlake2b256(GetVarByteArray(id2).get)).toSigmaProp

    val ctx = ErgoLikeContext.dummy(fakeSelf)

    val pr = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), spamScript, ctx.withCostLimit(CostTable.ScriptLimit * 10), fakeMessage).get

    val verifier = new ErgoLikeTestInterpreter
    val (res, terminated) = termination(() =>
      verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), spamScript, ctx, pr, fakeMessage)
    )

    res.isFailure shouldBe true
    terminated shouldBe true
  }

  /** This case verifies behavior of script interpreter when given enormously deep tree.
    * Below it is at least 150 levels.
    * When transaction is validated the script is deserialized for execution.
    * It should be checked by deserializer for it's depth.
    * The scripts with more than 150 levels are considered malicious.
  */
  property("big byte array with a lot of operations") {

    val ba = Random.randomBytes(5000000)

    val id = 21: Byte

    val prover = new ContextEnrichingTestProvingInterpreter().withContextExtender(id, ByteArrayConstant(ba))

    val bigSubScript = (1 to 100).foldLeft(CalcBlake2b256(GetVarByteArray(id).get)) { case (script, _) =>
      CalcBlake2b256(script)
    }

    val spamScript = NEQ(bigSubScript, CalcBlake2b256(ByteArrayConstant(Array.fill(32)(0: Byte)))).toSigmaProp

    val ctx = ErgoLikeContext.dummy(fakeSelf).withCostLimit(CostTable.ScriptLimit * 10)

    val prt = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), spamScript, ctx, fakeMessage)
    prt.isSuccess shouldBe true

    val pr = prt.get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new ErgoLikeTestInterpreter
    val (_, terminated) = termination(() =>
      verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), spamScript, ctxv, pr.proof, fakeMessage)
    )
    terminated shouldBe true
  }

  property("ring signature - maximum ok ring size") {
    val prover = new ContextEnrichingTestProvingInterpreter()
    val verifier = new ErgoLikeTestInterpreter
    val secret = prover.dlogSecrets.head

    val simulated = (1 to 98).map { _ =>
      new ContextEnrichingTestProvingInterpreter().dlogSecrets.head.publicImage
    }

    val ctx = ErgoLikeContext.dummy(fakeSelf).withCostLimit(CostTable.ScriptLimit * 2)

    val publicImages = secret.publicImage +: simulated
    val prop = OR(publicImages.map(image => SigmaPropConstant(image).isProven)).toSigmaProp

    val proof = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), prop, ctx, fakeMessage).get

    val (_, terminated) = termination(() =>
      verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), prop, ctx, proof, fakeMessage)
    )
    terminated shouldBe true
  }

  property("transaction with many outputs") {
    forAll(Gen.choose(10, 200), Gen.choose(200, 5000)) { case (orCnt, outCnt) =>
      whenever(orCnt > 10 && outCnt > 200) {
    val orCnt = 10
    val outCnt = 5
    val prover = new ContextEnrichingTestProvingInterpreter()

    val propToCompare = OR((1 to orCnt).map(_ => EQ(LongConstant(6), LongConstant(5)))).toSigmaProp

    val spamProp = OR((1 until orCnt).map(_ => EQ(LongConstant(6), LongConstant(5))) :+
      EQ(LongConstant(6), LongConstant(6))).toSigmaProp

    val spamScript =
      Exists(Outputs,
        FuncValue(Vector((1, SBox)),
          AND(
            GE(ExtractAmount(ValUse(1, SBox)), LongConstant(10)),
            EQ(
              ExtractScriptBytes(ValUse(1, SBox)),
              ByteArrayConstant(propToCompare.treeWithSegregation.bytes))
          )
        )
      ).toSigmaProp

    val txOutputs = ((1 to outCnt) map (_ => ErgoBox(11, spamProp, 0))) :+ ErgoBox(11, propToCompare, 0)
    val tx = createTransaction(txOutputs)

    val ctx = ErgoLikeContext.dummy(createBox(0, propToCompare))
      .withTransaction(tx)
      .withCostLimit(CostTable.ScriptLimit * 1000000L)

    val pt0 = System.currentTimeMillis()
    val proof = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), spamScript, ctx, fakeMessage).get
    val pt = System.currentTimeMillis()
    println(s"Prover time: ${(pt - pt0) / 1000.0} seconds")

    val verifier = new ErgoLikeTestInterpreter
    val (_, terminated) = termination(() =>
      verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), spamScript, ctx, proof, fakeMessage))
    terminated shouldBe true
      }
    }
  }

  property("transaction with many inputs and outputs") {
    implicit lazy val IR = new TestingIRContext {
      override val okPrintEvaluatedEntries = false
      override def onEvaluatedGraphNode(env: DataEnv, sym: Sym, value: AnyRef): Unit = {
        if (okPrintEvaluatedEntries)
          println(printEnvEntry(sym, value))
      }
    }
    val prover = new ContextEnrichingTestProvingInterpreter()
    val limitlessProver = new ContextEnrichingTestProvingInterpreter()

    val prop = Exists(Inputs,
      FuncValue(Vector((1, SBox)),
        Exists(Outputs,
          FuncValue(Vector((2, SBox)), EQ(ExtractScriptBytes(ValUse(1, SBox)), ExtractScriptBytes(ValUse(2, SBox))))))).toSigmaProp

    val inputScript = OR((1 to 200).map(_ => EQ(LongConstant(6), LongConstant(5)))).toSigmaProp
    val outputScript = OR((1 to 200).map(_ => EQ(LongConstant(6), LongConstant(6)))).toSigmaProp

    val inputs = ((1 to 999) map (_ => ErgoBox(11, inputScript, 0))) :+ ErgoBox(11, outputScript, 0)
    val outputs = (1 to 1000) map (_ => ErgoBox(11, outputScript, 0))

    val tx = createTransaction(outputs)

    val ctx = new ErgoLikeContext(currentHeight = 0,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      dataBoxes = ErgoLikeContext.noBoxes,
      headers = ErgoLikeContext.noHeaders,
      preHeader = ErgoLikeContext.dummyPreHeader,
      boxesToSpend = inputs,
      spendingTransaction = tx,
      self = inputs(0),
      extension = ContextExtension.empty,
      validationSettings = ValidationRules.currentSettings,
      costLimit = ScriptCostLimit.value)

    println(s"Timeout: ${Timeout / 1000.0} seconds")

    // check that execution terminated within timeout due to costing exception and cost limit
    val pt0 = System.currentTimeMillis()
    val (res, terminated) = termination(() =>
      prover.prove(emptyEnv + (ScriptNameProp -> "prove"), prop, ctx, fakeMessage)
    )
    val pt = System.currentTimeMillis()
    println(s"Prover time: ${(pt - pt0) / 1000.0} seconds")
    terminated shouldBe true
    assertExceptionThrown(
      res.fold(t => throw t, identity),
      {
        case se: IR.StagingException =>
          val cause = rootCause(se)
          cause.isInstanceOf[CosterException] && cause.getMessage.contains("Estimated expression complexity")
        case _ => false
      }
    )

    // measure time required to execute the script itself and it is more then timeout
    val (_, calcTime) = BenchmarkUtil.measureTime {
      import limitlessProver.IR._
      val Pair(calcF, _) = doCostingEx(emptyEnv + (ScriptNameProp -> "compute"), prop, true)
      val calcCtx = ctx.toSigmaContext(limitlessProver.IR, isCost = false)
      limitlessProver.calcResult(calcCtx, calcF)
    }
    println(s"Full time to execute the script: ${calcTime / 1000.0} seconds")
    assert(calcTime > Timeout)
  }

  property("too heavy avl tree lookup") {
    val reg1 = ErgoBox.nonMandatoryRegisters.head
    def genKey(str: String): ADKey = ADKey @@ Blake2b256("key: " + str)
    def genValue(str: String): ADValue = ADValue @@ Blake2b256("val: " + str)

    val prover = new ContextEnrichingTestProvingInterpreter()
    val verifier = new ErgoLikeTestInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)

    (1 to 100000).foreach {i =>
      avlProver.performOneOperation(Insert(genKey(s"key$i"), genValue(s"value$i")))
    }
    avlProver.generateProof()

    val digest = avlProver.digest

    (1 to 100000).foreach { i =>
      avlProver.performOneOperation(Lookup(genKey(s"key$i")))
    }

    val proof = avlProver.generateProof()

    println("proof size: " + proof.length)

    val treeData = SigmaDsl.avlTree(new AvlTreeData(digest, AvlTreeFlags.ReadOnly, 32, None))

    val key1 = genKey("key1")
    val value1 = genValue("value1")

    val prop = ErgoTree(ErgoTree.DefaultHeader, ErgoTree.EmptyConstants,
      EQ(
        IR.builder.mkMethodCall(
          ExtractRegisterAs[SAvlTree.type](Self, reg1).get,
          SAvlTree.getMethod,
          IndexedSeq(ByteArrayConstant(key1), ByteArrayConstant(proof))).asOption[SByteArray].get,
          ByteArrayConstant(value1)
      ).toSigmaProp
    )

    val newBox1 = ErgoBox(10, pubkey, 0)
    val newBoxes = IndexedSeq(newBox1)

    val spendingTransaction = createTransaction(newBoxes)

    val s = ErgoBox(20, ErgoScriptPredef.TrueProp, 0, Seq(), Map(reg1 -> AvlTreeConstant(treeData)))

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(s),
      spendingTransaction,
      self = s)

    val pr = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), prop, ctx.withCostLimit(Long.MaxValue), fakeMessage).get
    println("Cost: " + pr.cost)
    verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), prop, ctx, pr, fakeMessage).isFailure shouldBe true
  }

  property("nested loops") {
    val alice = new ContextEnrichingTestProvingInterpreter
    val alicePubKey:ProveDlog = alice.dlogSecrets.head.publicImage
    val largeColl = Colls.fromArray((1 to 50).toArray)
    val env = Map(
      ScriptNameProp -> "Script",
      "alice" -> alicePubKey,
      "largeColl" -> largeColl
    )
    val spamScript = compile(env,
      """{
        |  val valid  = largeColl.forall({(i:Int) =>
        |     largeColl.exists({(j:Int) =>
        |       i != j
        |     }
        |     ) &&
        |     largeColl.exists({(j:Int) =>
        |       largeColl.forall({(k:Int) =>
        |         k != i + j
        |       }
        |       ) &&
        |       i != j
        |     }
        |     ) &&
        |     OUTPUTS.exists({(x:Box) =>
        |       x.propositionBytes.size >= i
        |     }
        |     )
        |   }
        |  )
        |  ! valid
        |}
      """.stripMargin).asBoolValue.toSigmaProp

    //todo: make value dependent on CostTable constants, not magic constant
    val ba = Random.randomBytes(10000000)

    val id = 11: Byte
    val id2 = 12: Byte

    val prover = new ContextEnrichingTestProvingInterpreter()
      .withContextExtender(id, ByteArrayConstant(ba))
      .withContextExtender(id2, ByteArrayConstant(ba))

    //val spamScript = EQ(CalcBlake2b256(GetVarByteArray(id).get), CalcBlake2b256(GetVarByteArray(id2).get)).toSigmaProp

    val ctx = ErgoLikeContext.dummy(fakeSelf)

    val pr = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), spamScript, ctx, fakeMessage).get

    val verifier = new ErgoLikeTestInterpreter
    val (_, calcTime) = BenchmarkUtil.measureTime {
      verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), spamScript, ctx, pr, fakeMessage)
    }
    println(s"calc time: $calcTime millis")
    calcTime < Timeout shouldBe true
  }
}