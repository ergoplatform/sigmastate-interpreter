package org.ergoplatform.dsl

import sigmastate.interpreter.Interpreter.ScriptNameProp

import scala.collection.mutable
import sigma.interpreter.{CostedProverResult, ProverResult}

import scala.collection.mutable.ArrayBuffer
import org.ergoplatform.ErgoBox.{NonMandatoryRegisterId, TokenId}
import sigma.data.{AvlTreeData, CAnyValue, CSigmaProp, Nullable}

import scala.util.Try
import org.ergoplatform.{ErgoBox, ErgoLikeContext}
import org.ergoplatform.dsl.ContractSyntax.{ErgoScript, Proposition, Token}
import sigma.ast.{ErgoTree, SType}
import sigma.ast.EvaluatedValue
import sigma.interpreter.ContextExtension
import sigmastate.helpers.{CompilerTestingCommons, ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTestInterpreter}
import sigmastate.helpers.TestingHelpers._
import sigma.ast.syntax.ValueOps
import sigma.{AnyValue, Evaluation, SigmaProp}
import ErgoTree.ZeroHeader
import sigma.compiler.ir.IRContext

case class TestContractSpec(testSuite: CompilerTestingCommons)(implicit val IR: IRContext) extends ContractSpec {

  case class TestPropositionSpec(name: String, dslSpec: Proposition, scriptSpec: ErgoScript) extends PropositionSpec {
    lazy val ergoTree: ErgoTree = {
      val value = testSuite.compile(scriptSpec.env, scriptSpec.code)
      val header = scriptSpec.scriptVersion match {
        case Some(version) => ErgoTree.headerWithVersion(ZeroHeader, version)
        case None => testSuite.ergoTreeHeaderInTests
      }
      val tree: ErgoTree = ErgoTree.fromProposition(header, value.asSigmaProp)
      tree
    }
  }

  override private[dsl] def mkPropositionSpec(name: String, dslSpec: Proposition, scriptSpec: ErgoScript) =
    TestPropositionSpec(name, dslSpec, scriptSpec)


  case class TestProvingParty(name: String) extends ProvingParty {
    private val prover = new ContextEnrichingTestProvingInterpreter

    val pubKey: SigmaProp = CSigmaProp(prover.dlogSecrets.head.publicImage)

    import SType.AnyOps
    def prove(inBox: InputBox, extensions: Map[Byte, EvaluatedValue[_ <: SType]] = Map()): Try[CostedProverResult] = {
      val boxToSpend = inBox.utxoBox
      val propSpec: PropositionSpec = boxToSpend.propSpec
      val ctx = inBox.toErgoContext
      val env = propSpec.scriptSpec.env + (ScriptNameProp -> (propSpec.name + "_prove"))
      val prop = propSpec.ergoTree
      val p = extensions.foldLeft(prover) { (p, b) => p.withContextExtender(b._1, b._2) }
      val proof = p.prove(env, prop, ctx, testSuite.fakeMessage)
      proof
    }
  }

  override protected def mkProvingParty(name: String): ProvingParty = TestProvingParty(name)

  case class TestVerifyingParty(name: String) extends VerifyingParty {
    private val verifier = new ErgoLikeTestInterpreter

    def verify(inBox: InputBox, proverResult: ProverResult) = {
      val boxToSpend = inBox.utxoBox
      val propSpec = boxToSpend.propSpec
      val ctx = inBox.toErgoContext
      val env = propSpec.scriptSpec.env + (ScriptNameProp -> (propSpec.name + "_verify"))
      val prop = propSpec.ergoTree
      verifier.verify(env, prop, ctx, proverResult, testSuite.fakeMessage).get._1
    }
  }

  override protected def mkVerifyingParty(name: String): VerifyingParty = TestVerifyingParty(name)

  case class TestInputBox(tx: TransactionCandidate, utxoBox: OutBox) extends InputBox {
    private [dsl] def toErgoContext: ErgoLikeContext = {
      val boxesToSpend = tx.inputs.map(_.utxoBox.ergoBox).toIndexedSeq
      val dataBoxes = tx.dataInputs.map(_.utxoBox.ergoBox).toIndexedSeq
      val ctx = ErgoLikeContextTesting(
        currentHeight = tx.block.height,
        lastBlockUtxoRoot = AvlTreeData.dummy,
        minerPubkey = ErgoLikeContextTesting.dummyPubkey,
        dataBoxes   = dataBoxes,
        boxesToSpend = boxesToSpend,
        spendingTransaction = createTransaction(dataBoxes, tx.outputs.map(_.ergoBox).toIndexedSeq),
        selfIndex = boxesToSpend.indexOf(utxoBox.ergoBox),
        activatedVersion = testSuite.activatedVersionInTests)
              .withErgoTreeVersion(testSuite.ergoTreeVersionInTests)
      ctx
    }
    def runDsl(extensions: Map[Byte, EvaluatedValue[_ <: SType]] = Map()): SigmaProp = {
      val ctx = toErgoContext.withExtension(ContextExtension(extensions)).toSigmaContext()
      val res = utxoBox.propSpec.dslSpec(ctx)
      res
    }
  }

  case class TestOutBox(tx: TransactionCandidate, boxIndex: Int, value: Long, propSpec: PropositionSpec) extends OutBox {
    private var _tokens: Seq[Token] = Seq()
    private var _regs: Map[NonMandatoryRegisterId, _ <: EvaluatedValue[_ <: SType]] = Map()

    def withTokens(tokens: Token*) = { _tokens = tokens.toSeq; this }
    def withRegs(regs: (NonMandatoryRegisterId, Any)*) = {
      _regs = regs.map { case (id, v) =>
        val lifted = IR.builder.liftAny(v) match {
          case Nullable(v) => v
          case _ =>
            sys.error(s"Invalid value for register R${id.number}: $v")
        }
        (id, lifted.asInstanceOf[EvaluatedValue[_ <: SType]])
      }.toMap
      this
    }

    override def token(id: TokenId): Token = {
      val optToken = _tokens.collectFirst { case t if t.id == id => t }
      optToken.getOrElse(sys.error(s"Token with id=$id not found in the box $this"))
    }

    private[dsl] lazy val ergoBox: ErgoBox = {
      val tokens = _tokens.map(t => (t.id, t.value))
      testBox(value, propSpec.ergoTree, tx.block.height, tokens, _regs)
    }
    def id = ergoBox.id
  }

  case class MockTransaction(block: BlockCandidate) extends TransactionCandidate {
    private val _inputs: ArrayBuffer[InputBox] = mutable.ArrayBuffer.empty[InputBox]
    def inputs: Seq[InputBox] = _inputs.toSeq

    private val _dataInputs: ArrayBuffer[InputBox] = mutable.ArrayBuffer.empty[InputBox]
    def dataInputs: Seq[InputBox] = _dataInputs.toSeq

    private val _outputs = mutable.ArrayBuffer.empty[OutBox]
    def outputs: Seq[OutBox] = _outputs.toSeq

    def inBox(utxoBox: OutBox) = {
      val box = TestInputBox(this, utxoBox)
      _inputs += box
      box
    }

    def dataBox(dataBox: OutBox) = {
      val box = TestInputBox(this, dataBox)
      _dataInputs += box
      box
    }

    def outBox(value: Long, propSpec: PropositionSpec) = {
      val box = TestOutBox(this, _outputs.size, value, propSpec)
      _outputs += box
      box
    }

    def spending(utxos: OutBox*) = {
      for (b <- utxos) inBox(b)
      this
    }

    def withDataInputs(dataBoxes: OutBox*) = {
      for (b <- dataBoxes) dataBox(b)
      this
    }

  }

  case class BBlockCandidate(height: Int) extends BlockCandidate {
    def newTransaction() = MockTransaction(this)
  }

  def candidateBlock(height: Int): BlockCandidate = BBlockCandidate(height)
}
