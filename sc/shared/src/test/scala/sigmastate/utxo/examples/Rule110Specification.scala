package sigmastate.utxo.examples

import org.ergoplatform._
import scorex.crypto.hash.Blake2b256
import scorex.util._
import sigma.Colls
import sigma.ast.syntax.{BooleanConstant, GetVarByteArray, OptionValueOps}
import sigma.ast._
import sigma.data.AvlTreeData
import sigmastate._
import sigmastate.helpers.TestingHelpers._
import sigmastate.helpers.{CompilerTestingCommons, ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTestInterpreter}
import sigma.ast.syntax._
import sigma.interpreter.ContextExtension
import sigma.serialization.ValueSerializer
import sigmastate.utxo.blockchain.BlockchainSimulationTestingCommons._

/**
  * Wolfram's Rule110 implementations
  *
  */
class Rule110Specification extends CompilerTestingCommons
  with CompilerCrossVersionProps {
  implicit lazy val IR = new TestingIRContext
  private val reg1 = ErgoBox.nonMandatoryRegisters.head
  private val reg2 = ErgoBox.nonMandatoryRegisters(1)
  private val reg3 = ErgoBox.nonMandatoryRegisters(2)
  private val reg4 = ErgoBox.nonMandatoryRegisters(3)

  /**
    * Rule 110 example implementation.
    * Current rule 110 layer of fixed size (6 in this example), is kept in register R3 as an array of bytes
    * (one byte represents 1 bit in rule 110).
    * Script checks, that
    * - register R3 first output contains correct updated layer based on rule 110 with boundary conditions
    * - first output contains the same protecting script, allowing to calculate further layers
    */
  property("rule110 - one layer in register") {
    val maxCost: Long = 2000000
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val prop = compile(Map(),
      """{
        |  val indices: Coll[Int] = Coll(0, 1, 2, 3, 4, 5)
        |  val inLayer: Coll[Byte] = SELF.R4[Coll[Byte]].get
        |  val procCell = {(i: Int) =>
        |    val l = inLayer((if (i == 0) 5 else (i - 1)))
        |    val c = inLayer(i)
        |    val r = inLayer((i + 1) % 6)
        |    ((l * c * r + c * r + c + r) % 2).toByte
        |  }
        |  (OUTPUTS(0).R4[Coll[Byte]].get == indices.map(procCell)) &&
        |   (OUTPUTS(0).propositionBytes == SELF.propositionBytes)
         }""".stripMargin).asBoolValue.toSigmaProp

    val input = testBox(1, mkTestErgoTree(prop), 0, Seq(), Map(reg1 -> ByteArrayConstant(Array[Byte](0, 1, 1, 0, 1, 0))))
    val output = testBox(1, mkTestErgoTree(prop), 0, Seq(), Map(reg1 -> ByteArrayConstant(Array[Byte](1, 1, 1, 1, 1, 0))))
    val tx = UnsignedErgoLikeTransaction(IndexedSeq(new UnsignedInput(input.id)), IndexedSeq(output))

    val ctx = ErgoLikeContextTesting(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(input),
      tx,
      self = input, activatedVersionInTests).withCostLimit(maxCost)

    val pr = prover.prove(mkTestErgoTree(prop), ctx, fakeMessage).get
    verifier.verify(mkTestErgoTree(prop), ctx, pr, fakeMessage).get._1 shouldBe true
  }


  /**
    * Rule 110 example implementation on unlimited greed.
    *
    * Every output’s register R6 contains its bit value
    * register R4 contains the column index x,
    * register R5 contains the minimal x index at the current step n and
    * register R3 contains flag mid.
    *
    * Protecting script is the MAST script that only defines script hashes for different conditions,
    * while prover should provide a validation script and satisfy it's requirements.
    *
    * Normal case script works as follows:
    * As the grid expands by one at every step, −n also serves as the row number.
    * By default the transaction spends three inputs (corresponding to the three neighboring bits
    * from the previous row), and creates three outputs with the same bit value by
    * the automaton rule. One output flagged by mid is supposed to be spent for new
    * value with the column number x, and another two — for the columns x ± 1.
    * The script checks the correctness of the positions of inputs and
    * outputs, correspondence of of the bit values, the correctness
    * of the mid flag assignment for inputs and the fact that all outputs are
    * identical except the mid flag, which is set only once
    *
    * In case the transaction creates the boundary cells, either one or two
    * inputs are emulated to have zero bit values. This cases are implemented as
    * a separate scripts in a MAST.
    */
  property("rule110 - one bit in register") {
    val t = ByteConstant(1)
    val f = ByteConstant(0)

    // function correctPayload(in, out) from the paper parametrized for boundary conditions
    def checkPayloadCorrect(in0Mid: Value[SByte.type],
                            in1Mid: Value[SByte.type],
                            in2Mid: Value[SByte.type],
                            in0X: Value[SByte.type],
                            in1X: Value[SByte.type],
                            in2X: Value[SByte.type],
                            in0Y: Value[SByte.type],
                            in1Y: Value[SByte.type],
                            in2Y: Value[SByte.type],
                            l: Value[SByte.type],
                            c: Value[SByte.type],
                            r: Value[SByte.type],
                            out0X: Value[SByte.type],
                            out0Y: Value[SByte.type],
                            out0V: Value[SByte.type],
                            outsSize: Value[SInt.type]) = {
      val inMidCorrect = AND(EQ(in0Mid, f), EQ(in1Mid, t), EQ(in2Mid, f))
      val inYCorrect = AND(EQ(in0Y, in1Y), EQ(in0Y, in2Y))
      val inXCorrect = AND(EQ(in1X, Plus(in0X, ByteConstant(1))), EQ(in1X, Minus(in2X, ByteConstant(1))))
      val calculatedBit = Modulo(Plus(Plus(Plus(Multiply(Multiply(l, c), r), Multiply(c, r)), c), r), ByteConstant(2))
      val inValCorrect = EQ(calculatedBit, out0V)
      val outPosCorrect = AND(EQ(out0X, in1X), EQ(out0Y, Minus(in0Y, ByteConstant(1))))
      val sizesCorrect = EQ(SizeOf(Inputs), outsSize)
      AND(inValCorrect, inYCorrect, inXCorrect, inMidCorrect, outPosCorrect, sizesCorrect)
    }

    val verifier = new ErgoLikeTestInterpreter

    val MidReg = reg1
    val XReg = reg2
    val YReg = reg3
    val ValReg = reg4
    val scriptId = 21.toByte
    val scriptHash = CalcBlake2b256(GetVarByteArray(scriptId).get)

    // extract required values of for all outputs
    val in0Mid = ExtractRegisterAs[SByte.type](ByIndex(Inputs, 0), MidReg).get
    val in1Mid = ExtractRegisterAs[SByte.type](ByIndex(Inputs, 1), MidReg).get
    val in2Mid = ExtractRegisterAs[SByte.type](ByIndex(Inputs, 2), MidReg).get
    val out0Mid = ExtractRegisterAs[SByte.type](ByIndex(Outputs, 0), MidReg).get
    val out1Mid = ExtractRegisterAs[SByte.type](ByIndex(Outputs, 1), MidReg).get
    val out2Mid = ExtractRegisterAs[SByte.type](ByIndex(Outputs, 2), MidReg).get

    val in0X = ExtractRegisterAs[SByte.type](ByIndex(Inputs, 0), XReg).get
    val in1X = ExtractRegisterAs[SByte.type](ByIndex(Inputs, 1), XReg).get
    val in2X = ExtractRegisterAs[SByte.type](ByIndex(Inputs, 2), XReg).get
    val out0X = ExtractRegisterAs[SByte.type](ByIndex(Outputs, 0), XReg).get
    val out1X = ExtractRegisterAs[SByte.type](ByIndex(Outputs, 1), XReg).get
    val out2X = ExtractRegisterAs[SByte.type](ByIndex(Outputs, 2), XReg).get

    val in0Y = ExtractRegisterAs[SByte.type](ByIndex(Inputs, 0), YReg).get
    val in1Y = ExtractRegisterAs[SByte.type](ByIndex(Inputs, 1), YReg).get
    val in2Y = ExtractRegisterAs[SByte.type](ByIndex(Inputs, 2), YReg).get
    val out0Y = ExtractRegisterAs[SByte.type](ByIndex(Outputs, 0), YReg).get
    val out1Y = ExtractRegisterAs[SByte.type](ByIndex(Outputs, 1), YReg).get
    val out2Y = ExtractRegisterAs[SByte.type](ByIndex(Outputs, 2), YReg).get

    val in0Val = ExtractRegisterAs[SByte.type](ByIndex(Inputs, 0), ValReg).get
    val in1Val = ExtractRegisterAs[SByte.type](ByIndex(Inputs, 1), ValReg).get
    val in2Val = ExtractRegisterAs[SByte.type](ByIndex(Inputs, 2), ValReg).get
    val out0V = ExtractRegisterAs[SByte.type](ByIndex(Outputs, 0), ValReg).get
    val out1V = ExtractRegisterAs[SByte.type](ByIndex(Outputs, 0), ValReg).get
    val out2V = ExtractRegisterAs[SByte.type](ByIndex(Outputs, 0), ValReg).get

    val in0Script = ExtractScriptBytes(ByIndex(Inputs, 0))
    val out0Script = ExtractScriptBytes(ByIndex(Outputs, 0))
    val out1Script = ExtractScriptBytes(ByIndex(Outputs, 1))
    val out2Script = ExtractScriptBytes(ByIndex(Outputs, 2))

    val normalPayloadCorrect = checkPayloadCorrect(in0Mid, in1Mid, in2Mid, in0X, in1X, in2X, in0Y, in1Y, in2Y, in0Val, in1Val, in2Val, out0X, out0Y, out0V, IntConstant(3))
    val rightmostPayloadCorrect = checkPayloadCorrect(in0Mid, in1Mid, f, in0X, in1X, ByteConstant(1), in0Y, in1Y, in0Y, in0Val, in1Val, f, out0X, out0Y, out0V, IntConstant(2))
    val nLeftmostPayloadCorrect = checkPayloadCorrect(f, in0Mid, in1Mid, Minus(in0X, ByteConstant(1)), in0X, in1X, in0Y, in0Y, in1Y, f, in0Val, in1Val, out0X, out0Y, out0V, IntConstant(2))
    val leftmostPayloadCorrect = checkPayloadCorrect(f, t, in0Mid, Minus(in0X, ByteConstant(2)), Minus(in0X, ByteConstant(1)), in0X, in0Y, in0Y, in0Y, f, f, in0Val, out0X, out0Y, out0V, IntConstant(1))

    // function outCorrect(out, script) from the paper
    val scriptsCorrect = AND(EQ(in0Script, out0Script), EQ(in0Script, out1Script), EQ(in0Script, out2Script))
    val outXCorrect = AND(EQ(out0X, out1X), EQ(out1X, out2X))
    val outYCorrect = AND(EQ(out0Y, out1Y), EQ(out1Y, out2Y))
    val outValCorrect = AND(EQ(out0V, out1V), EQ(out1V, out2V))
    val outMidCorrect = AND(EQ(out0Mid, f), EQ(out1Mid, t), EQ(out2Mid, f))
    val outsSizeCorrect = EQ(SizeOf(Outputs), IntConstant(3))
    val outputsCorrect = AND(scriptsCorrect, outXCorrect, outYCorrect, outValCorrect, outMidCorrect, outsSizeCorrect)

    val normalCaseProp = AND(normalPayloadCorrect, outputsCorrect)
    val normalCaseBytes = ValueSerializer.serialize(normalCaseProp)
    val normalCaseHash = Blake2b256(normalCaseBytes)
    val normalCaseConditions = AND(EQ(SizeOf(Inputs), 3), EQ(scriptHash, normalCaseHash))

    val rightmostProp = AND(rightmostPayloadCorrect, outputsCorrect)
    val rightmostBytes = ValueSerializer.serialize(rightmostProp)
    val rightmostHash = Blake2b256(rightmostBytes)
    val rightmostConditions = AND(EQ(SizeOf(Inputs), 2), EQ(in0X, ByteConstant(-1)), EQ(scriptHash, rightmostHash))

    val nLeftmostProp = AND(nLeftmostPayloadCorrect, outputsCorrect)
    val nLeftmostBytes = ValueSerializer.serialize(nLeftmostProp)
    val nLeftmostHash = Blake2b256(nLeftmostBytes)
    val nLeftmostConditions = AND(EQ(SizeOf(Inputs), 2), EQ(in0X, in0Y), EQ(scriptHash, nLeftmostHash))

    val leftmostProp = AND(leftmostPayloadCorrect, outputsCorrect)
    val leftmostBytes = ValueSerializer.serialize(leftmostProp)
    val leftmostHash = Blake2b256(leftmostBytes)
    val leftmostConditions = AND(EQ(SizeOf(Inputs), 1), EQ(in0X, in0Y), EQ(scriptHash, leftmostHash))

    val scriptIsCorrect = DeserializeContext(scriptId, SBoolean)
    val prop = AND(scriptIsCorrect, OR(normalCaseConditions, rightmostConditions, nLeftmostConditions, leftmostConditions)).toSigmaProp
    val propTree = mkTestErgoTree(prop)

    // test normal case
    val nIn0 = testBox(1, propTree, 0, Seq(), Map(MidReg -> f, XReg -> ByteConstant(-2), YReg -> ByteConstant(0), ValReg -> t))
    val nIn1 = testBox(1, propTree, 0, Seq(), Map(MidReg -> t, XReg -> ByteConstant(-1), YReg -> ByteConstant(0), ValReg -> f))
    val nIn2 = testBox(1, propTree, 0, Seq(), Map(MidReg -> f, XReg -> ByteConstant(0), YReg -> ByteConstant(0), ValReg -> t))
    val nOut0 = testBox(1, propTree, 0, Seq(), Map(MidReg -> f, XReg -> ByteConstant(-1), YReg -> ByteConstant(-1), ValReg -> t))
    val nOut1 = testBox(1, propTree, 0, Seq(), Map(MidReg -> t, XReg -> ByteConstant(-1), YReg -> ByteConstant(-1), ValReg -> t))
    val nOut2 = testBox(1, propTree, 0, Seq(), Map(MidReg -> f, XReg -> ByteConstant(-1), YReg -> ByteConstant(-1), ValReg -> t))

    val nTx = UnsignedErgoLikeTransaction(IndexedSeq(nIn0, nIn1, nIn2).map(i => new UnsignedInput(i.id)), IndexedSeq(nOut0, nOut1, nOut2))
    val nProver = new ContextEnrichingTestProvingInterpreter()
      .withContextExtender(scriptId, ByteArrayConstant(normalCaseBytes))

    val nCtx = ErgoLikeContextTesting(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(nIn0, nIn1, nIn2),
      nTx,
      self = nIn0, activatedVersionInTests)

    val nProof = nProver.prove(propTree, nCtx, fakeMessage).get
    verifier.verify(propTree, nCtx, nProof, fakeMessage).get._1 shouldBe true

    // test rightmost case
    val rIn0 = testBox(1, propTree, 0, Seq(), Map(MidReg -> f, XReg -> ByteConstant(-1), YReg -> ByteConstant(0), ValReg -> t))
    val rIn1 = testBox(1, propTree, 0, Seq(), Map(MidReg -> t, XReg -> ByteConstant(0), YReg -> ByteConstant(0), ValReg -> t))
    val rOut0 = testBox(1, propTree, 0, Seq(), Map(MidReg -> f, XReg -> ByteConstant(0), YReg -> ByteConstant(-1), ValReg -> t))
    val rOut1 = testBox(1, propTree, 0, Seq(), Map(MidReg -> t, XReg -> ByteConstant(0), YReg -> ByteConstant(-1), ValReg -> t))
    val rOut2 = testBox(1, propTree, 0, Seq(), Map(MidReg -> f, XReg -> ByteConstant(0), YReg -> ByteConstant(-1), ValReg -> t))

    val rTx = UnsignedErgoLikeTransaction(IndexedSeq(rIn0, rIn1).map(i => new UnsignedInput(i.id)), IndexedSeq(rOut0, rOut1, rOut2))
    val rProver = new ContextEnrichingTestProvingInterpreter()
      .withContextExtender(scriptId, ByteArrayConstant(rightmostBytes))

    val rCtx = ErgoLikeContextTesting(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(rIn0, rIn1),
      rTx,
      self = rIn0, activatedVersionInTests)

    val rProof = rProver.prove(propTree, rCtx, fakeMessage).get
    verifier.verify(propTree, rCtx, rProof, fakeMessage).get._1 shouldBe true

    // test next to leftmost case
    val lnIn0 = testBox(1, propTree, 0, Seq(), Map(MidReg -> t, XReg -> ByteConstant(-6), YReg -> ByteConstant(-6), ValReg -> t))
    val lnIn1 = testBox(1, propTree, 0, Seq(), Map(MidReg -> f, XReg -> ByteConstant(-5), YReg -> ByteConstant(-6), ValReg -> t))
    val lnOut0 = testBox(1, propTree, 0, Seq(), Map(MidReg -> f, XReg -> ByteConstant(-6), YReg -> ByteConstant(-7), ValReg -> t))
    val lnOut1 = testBox(1, propTree, 0, Seq(), Map(MidReg -> t, XReg -> ByteConstant(-6), YReg -> ByteConstant(-7), ValReg -> t))
    val lnOut2 = testBox(1, propTree, 0, Seq(), Map(MidReg -> f, XReg -> ByteConstant(-6), YReg -> ByteConstant(-7), ValReg -> t))

    val lnTx = UnsignedErgoLikeTransaction(IndexedSeq(lnIn0, lnIn1).map(i => new UnsignedInput(i.id)), IndexedSeq(lnOut0, lnOut1, lnOut2))
    val lnProver = new ContextEnrichingTestProvingInterpreter()
      .withContextExtender(scriptId, ByteArrayConstant(nLeftmostBytes))

    val lnCtx = ErgoLikeContextTesting(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(lnIn0, lnIn1),
      lnTx,
      self = lnIn0, activatedVersionInTests)

    val lnProof = lnProver.prove(propTree, lnCtx, fakeMessage).get
    verifier.verify(propTree, lnCtx, lnProof, fakeMessage).get._1 shouldBe true

    // test  leftmost case
    val lIn0 = testBox(1, propTree, 0, Seq(), Map(MidReg -> f, XReg -> ByteConstant(-6), YReg -> ByteConstant(-6), ValReg -> t))
    val lOut0 = testBox(1, propTree, 0, Seq(), Map(MidReg -> f, XReg -> ByteConstant(-7), YReg -> ByteConstant(-7), ValReg -> t))
    val lOut1 = testBox(1, propTree, 0, Seq(), Map(MidReg -> t, XReg -> ByteConstant(-7), YReg -> ByteConstant(-7), ValReg -> t))
    val lOut2 = testBox(1, propTree, 0, Seq(), Map(MidReg -> f, XReg -> ByteConstant(-7), YReg -> ByteConstant(-7), ValReg -> t))

    val lTx = UnsignedErgoLikeTransaction(IndexedSeq(lIn0).map(i => new UnsignedInput(i.id)), IndexedSeq(lOut0, lOut1, lOut2))
    val lProver = new ContextEnrichingTestProvingInterpreter()
      .withContextExtender(scriptId, ByteArrayConstant(leftmostBytes))

    val lCtx = ErgoLikeContextTesting(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(lIn0),
      lTx,
      self = lIn0, activatedVersionInTests)

    val lProof = lProver.prove(propTree, lCtx, fakeMessage).get
    verifier.verify(propTree, lCtx, lProof, fakeMessage).get._1 shouldBe true
  }


  /**
    * A coin holds following data:
    *
    * R4 - index of row
    * R5 - index of column
    * R6 - bit value (represented as a boolean)
    *
    * Each transaction have 3 inputs and 4 outputs. 3 outputs are just copies of inputs, 1 output is a bit on
    * new layer of rule 110
    */
  property("rule110 - one bit per output (old version)") {
    val prover = new ContextEnrichingTestProvingInterpreter()

    val RowReg = reg1
    val ColumnReg = reg2
    val ValueReg = reg3
    val bitsInString = 31
    val lastBitIndex = bitsInString - 1

    val midBitColumn = EQ(ExtractRegisterAs[SLong.type](ByIndex(Outputs, 0), ColumnReg).get,
      ExtractRegisterAs[SLong.type](ByIndex(Inputs, 1), ColumnReg).get)

    val leftBitColumn =
      OR(
        AND(
          EQ(ExtractRegisterAs[SLong.type](ByIndex(Outputs, 0), ColumnReg).get, LongConstant(0)),
          EQ(ExtractRegisterAs[SLong.type](ByIndex(Inputs, 0), ColumnReg).get, LongConstant(lastBitIndex))
        ),
        EQ(ExtractRegisterAs[SLong.type](ByIndex(Outputs, 0), ColumnReg).get,
          Plus(ExtractRegisterAs[SLong.type](ByIndex(Inputs, 0), ColumnReg).get, LongConstant(1)))
      )

    val rightBitColumn =
      OR(
        AND(
          EQ(ExtractRegisterAs[SLong.type](ByIndex(Outputs, 0), ColumnReg).get, LongConstant(lastBitIndex)),
          EQ(ExtractRegisterAs[SLong.type](ByIndex(Inputs, 2), ColumnReg).get, LongConstant(0))
        ),
        EQ(Plus(ExtractRegisterAs[SLong.type](ByIndex(Outputs, 0), ColumnReg).get, LongConstant(1)),
          ExtractRegisterAs[SLong.type](ByIndex(Inputs, 2), ColumnReg).get)
      )

    val row0 = EQ(ExtractRegisterAs[SLong.type](ByIndex(Outputs, 0), RowReg).get,
      Plus(ExtractRegisterAs[SLong.type](ByIndex(Inputs, 0), RowReg).get, LongConstant(1)))

    val row1 = EQ(ExtractRegisterAs[SLong.type](ByIndex(Outputs, 0), RowReg).get,
      Plus(ExtractRegisterAs[SLong.type](ByIndex(Inputs, 1), RowReg).get, LongConstant(1)))

    val row2 = EQ(ExtractRegisterAs[SLong.type](ByIndex(Outputs, 0), RowReg).get,
      Plus(ExtractRegisterAs[SLong.type](ByIndex(Inputs, 2), RowReg).get, LongConstant(1)))

    val input0 = ExtractRegisterAs[SBoolean.type](ByIndex(Inputs, 0), ValueReg).get
    val input1 = ExtractRegisterAs[SBoolean.type](ByIndex(Inputs, 1), ValueReg).get
    val input2 = ExtractRegisterAs[SBoolean.type](ByIndex(Inputs, 2), ValueReg).get

    val output = ExtractRegisterAs[SBoolean.type](ByIndex(Outputs, 0), ValueReg).get

    val t = TrueLeaf
    val f = FalseLeaf

    val rule110 = OR(Array(
      AND(EQ(input0, t), EQ(input1, t), EQ(input2, t), EQ(output, f)),
      AND(EQ(input0, t), EQ(input1, t), EQ(input2, f), EQ(output, t)),
      AND(EQ(input0, t), EQ(input1, f), EQ(input2, t), EQ(output, t)),
      AND(EQ(input0, t), EQ(input1, f), EQ(input2, f), EQ(output, f)),
      AND(EQ(input0, f), EQ(input1, t), EQ(input2, t), EQ(output, t)),
      AND(EQ(input0, f), EQ(input1, t), EQ(input2, f), EQ(output, t)),
      AND(EQ(input0, f), EQ(input1, f), EQ(input2, t), EQ(output, t)),
      AND(EQ(input0, f), EQ(input1, f), EQ(input2, f), EQ(output, f))
    ))

    val prop = AND(Array(
      EQ(SizeOf(Inputs), IntConstant(3)),
      EQ(SizeOf(Outputs), IntConstant(4)),

      //We're checking that the outputs are indeed contain the same script
      EQ(ExtractScriptBytes(Self), ExtractScriptBytes(ByIndex(Outputs, 0))),
      EQ(ExtractScriptBytes(Self), ExtractScriptBytes(ByIndex(Outputs, 1))),
      EQ(ExtractScriptBytes(Self), ExtractScriptBytes(ByIndex(Outputs, 2))),
      EQ(ExtractScriptBytes(Self), ExtractScriptBytes(ByIndex(Outputs, 2))),

      EQ(ExtractBytesWithNoRef(ByIndex(Inputs, 0)), ExtractBytesWithNoRef(ByIndex(Outputs, 1))),
      EQ(ExtractBytesWithNoRef(ByIndex(Inputs, 1)), ExtractBytesWithNoRef(ByIndex(Outputs, 2))),
      EQ(ExtractBytesWithNoRef(ByIndex(Inputs, 2)), ExtractBytesWithNoRef(ByIndex(Outputs, 3))),

      midBitColumn,
      leftBitColumn,
      rightBitColumn,
      row0,
      row1,
      row2,
      rule110
    )).toSigmaProp
    val propTree = mkTestErgoTree(prop)

    val hash = Blake2b256
    val txId = hash.hash(scala.util.Random.nextString(12).getBytes)

    // further we fill the genesis row as in the first example at http://mathworld.wolfram.com/Rule110.html,
    // and check that the first row (after the genesis one) is satisfying the example

    val coins = (0 until bitsInString).map { col =>
      val row = RowReg -> LongConstant(0)
      val column = ColumnReg -> LongConstant(col)
      val value = if (col == 15) ValueReg -> TrueLeaf else ValueReg -> FalseLeaf
      testBox(0L,
        propTree,
        0, Nil, Map(row, column, value), txId.toModifierId, col.toShort)
    }

    val initBlock = FullBlock(
      IndexedSeq(createTransaction(coins)),
      ErgoLikeContextTesting.dummyPubkey
    )

    val genesisState = ValidationState.initialState(activatedVersionInTests, initBlock)

    def byPos(state: ValidationState, row: Int, pos: Int) =
      state.boxesReader.byTwoInts(RowReg, row, ColumnReg, pos).get

    def generateTransactionsForRow(state: ValidationState, row: Int): IndexedSeq[ErgoLikeTransaction] = {
      require(row >= 1)

      (0 until bitsInString).map { col =>
        val leftColl = if (col == 0) lastBitIndex else col - 1
        val centerColl = col
        val rightColl = if (col == lastBitIndex) 0 else col + 1

        val left = byPos(state, row - 1, leftColl)
        val center = byPos(state, row - 1, centerColl)
        val right = byPos(state, row - 1, rightColl)

        val lv = left.get(ValueReg).get.asInstanceOf[BooleanConstant].value
        val cv = center.get(ValueReg).get.asInstanceOf[BooleanConstant].value
        val rv = right.get(ValueReg).get.asInstanceOf[BooleanConstant].value

        val value = ValueReg -> BooleanConstant.fromBoolean(calcRule110(lv, cv, rv))

        val c = new ErgoBoxCandidate(0L, propTree, row, Colls.emptyColl, Map(RowReg -> LongConstant(row), ColumnReg -> LongConstant(col), value))

        val ut = UnsignedErgoLikeTransaction(
          IndexedSeq(new UnsignedInput(left.id), new UnsignedInput(center.id), new UnsignedInput(right.id)),
          IndexedSeq(c, left.toCandidate, center.toCandidate, right.toCandidate)
        )

        val contextLeft = ErgoLikeContextTesting(row,
          state.state.lastBlockUtxoRoot,
          ErgoLikeContextTesting.dummyPubkey,
          IndexedSeq(left, center, right),
          ut,
          left,
          activatedVersionInTests,
          ContextExtension.empty)
        val proverResultLeft = prover.prove(left.ergoTree, contextLeft, ut.messageToSign).get

        val contextCenter = ErgoLikeContextTesting(row,
          state.state.lastBlockUtxoRoot,
          ErgoLikeContextTesting.dummyPubkey,
          IndexedSeq(left, center, right),
          ut,
          center,
          activatedVersionInTests,
          ContextExtension.empty)
        val proverResultCenter = prover.prove(center.ergoTree, contextCenter, ut.messageToSign).get

        val contextRight = ErgoLikeContextTesting(row,
          state.state.lastBlockUtxoRoot,
          ErgoLikeContextTesting.dummyPubkey,
          IndexedSeq(left, center, right),
          ut,
          right,
          activatedVersionInTests,
          ContextExtension.empty)
        val proverResultRight = prover.prove(right.ergoTree, contextRight, ut.messageToSign).get
        ut.toSigned(IndexedSeq(proverResultLeft, proverResultCenter, proverResultRight))
      }
    }

    val firstRowBlock = FullBlock(generateTransactionsForRow(genesisState, 1), ErgoLikeContextTesting.dummyPubkey)

    val t0 = System.currentTimeMillis()
    val firstRowState = genesisState.applyBlock(firstRowBlock, 10000000).get
    val t1 = System.currentTimeMillis()

    printDebug(s"First row time ${t1 - t0} ms.")

    firstRowState.boxesReader.byTwoInts(RowReg, 1, ColumnReg, 13).get.get(ValueReg).get.asInstanceOf[BooleanConstant].value shouldBe false
    firstRowState.boxesReader.byTwoInts(RowReg, 1, ColumnReg, 14).get.get(ValueReg).get.asInstanceOf[BooleanConstant].value shouldBe true
    firstRowState.boxesReader.byTwoInts(RowReg, 1, ColumnReg, 15).get.get(ValueReg).get.asInstanceOf[BooleanConstant].value shouldBe true
    firstRowState.boxesReader.byTwoInts(RowReg, 1, ColumnReg, 16).get.get(ValueReg).get.asInstanceOf[BooleanConstant].value shouldBe false
  }


  def calcRule110(left: Boolean, center: Boolean, right: Boolean): Boolean =
    (left, center, right) match {
      case (true, true, true) => false
      case (true, true, false) => true
      case (true, false, true) => true
      case (true, false, false) => false
      case (false, true, true) => true
      case (false, true, false) => true
      case (false, false, true) => true
      case (false, false, false) => false
    }
}
