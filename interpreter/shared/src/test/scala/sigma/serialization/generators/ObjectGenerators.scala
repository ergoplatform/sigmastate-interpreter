package sigma.serialization.generators

import org.ergoplatform.ErgoBox._
import sigma.data.SigmaConstants.MaxPropositionBytes
import org.ergoplatform.validation._
import org.ergoplatform._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen.{choose, frequency}
import org.scalacheck.util.Buildable
import org.scalacheck.{Arbitrary, Gen}
import sigma.data._
import scorex.crypto.authds.{ADDigest, ADKey}
import scorex.util.encode.{Base58, Base64}
import scorex.util.{ModifierId, bytesToId}
import sigma.ast._
import sigma.ast.syntax._
import sigmastate.eval._
import sigma.crypto.CryptoConstants.dlogGroup
import TransformingSigmaBuilder._
import sigmastate._
import sigma.Coll
import sigma.Extensions.ArrayOps
import sigma._
import sigma.ast.syntax.SigmaPropValue
import sigma.crypto.{CryptoConstants, EcPointType}
import sigma.util.Extensions.EcpOps
import sigma.validation.{ChangedRule, DisabledRule, EnabledRule, ReplacedRule, RuleStatus}
import sigma.validation.ValidationRules.FirstRuleId
import ErgoTree.ZeroHeader
import sigma.eval.Extensions.{EvalIterableOps, SigmaBooleanOps}
import sigma.eval.SigmaDsl
import sigma.interpreter.{ContextExtension, ProverResult}

import java.math.BigInteger
import scala.collection.compat.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

trait ObjectGenerators extends TypeGenerators
  with ValidationSpecification
  with ConcreteCollectionGenerators
  with TestsBase {

  val ThresholdLimit = 10

  implicit lazy val statusArb: Arbitrary[RuleStatus] = Arbitrary(statusGen)
  implicit lazy val arbMapCollection: Arbitrary[MapCollection[SInt.type, SInt.type]] = Arbitrary(mapCollectionGen)
  implicit lazy val arbExists: Arbitrary[Exists[SInt.type]] = Arbitrary(existsGen)
  implicit lazy val arbForAll: Arbitrary[ForAll[SInt.type]] = Arbitrary(forAllGen)
  implicit lazy val arbFold: Arbitrary[Fold[SInt.type, SBoolean.type]] = Arbitrary(foldGen)
  implicit lazy val arbAppend: Arbitrary[Append[SInt.type]] = Arbitrary(appendGen)
  implicit lazy val arbSlice: Arbitrary[Slice[SInt.type]] = Arbitrary(sliceGen)
  implicit lazy val arbAtLeast: Arbitrary[AtLeast] = Arbitrary(atLeastGen)
  implicit lazy val arbFilter: Arbitrary[Filter[SInt.type]] = Arbitrary(filterGen)
  implicit lazy val arbSizeOf: Arbitrary[SizeOf[SInt.type]] = Arbitrary(sizeOfGen)
  implicit lazy val arbExtractAmount: Arbitrary[ExtractAmount] = Arbitrary(extractAmountGen)
  implicit lazy val arbExtractCreationInfo: Arbitrary[ExtractCreationInfo] = Arbitrary(extractCreationInfoGen)
  implicit lazy val arbExtractScriptBytes: Arbitrary[ExtractScriptBytes] = Arbitrary(extractScriptBytesGen)
  implicit lazy val arbExtractBytes: Arbitrary[ExtractBytes] = Arbitrary(extractBytesGen)
  implicit lazy val arbExtractBytesWithNoRef: Arbitrary[ExtractBytesWithNoRef] = Arbitrary(extractBytesWithNoRefGen)
  implicit lazy val arbExtractId: Arbitrary[ExtractId] = Arbitrary(extractIdGen)
  implicit lazy val arbExtractRegisterAs: Arbitrary[ExtractRegisterAs[SInt.type]] = Arbitrary(extractRegisterAsGen)
  implicit lazy val arbIntToByteArray: Arbitrary[LongToByteArray] = Arbitrary(longToByteArrayGen)
  implicit lazy val arbByteArrayToBigInt: Arbitrary[ByteArrayToBigInt] = Arbitrary(byteArrayToBigIntGen)
  implicit lazy val arbCalcBlake2b256: Arbitrary[CalcBlake2b256] = Arbitrary(calcBlake2b256Gen)
  implicit lazy val arbCalcSha256: Arbitrary[CalcSha256] = Arbitrary(calcSha256Gen)
  implicit lazy val arbByIndex: Arbitrary[ByIndex[SInt.type]] = Arbitrary(byIndexGen)
  implicit lazy val arbDeserializeContext: Arbitrary[DeserializeContext[SBoolean.type]] = Arbitrary(deserializeContextGen)
  implicit lazy val arbDeserializeRegister: Arbitrary[DeserializeRegister[SBoolean.type]] = Arbitrary(deserializeRegisterGen)
  implicit lazy val arbByteConstants: Arbitrary[ByteConstant] = Arbitrary(byteConstGen)
  implicit lazy val arbIntConstants: Arbitrary[IntConstant] = Arbitrary(intConstGen)
  implicit lazy val arbLongConstants: Arbitrary[LongConstant] = Arbitrary(longConstGen)
  implicit lazy val arbByteArrayConstant: Arbitrary[CollectionConstant[SByte.type]] = Arbitrary(byteArrayConstGen)
  implicit lazy val arbGroupElementConstant: Arbitrary[GroupElementConstant] = Arbitrary(groupElementConstGen)
  implicit lazy val arbBoxConstant: Arbitrary[BoxConstant] = Arbitrary(boxConstantGen)
  implicit lazy val arbAvlTreeConstant: Arbitrary[AvlTreeConstant] = Arbitrary(avlTreeConstantGen)
  implicit lazy val arbBigIntConstant: Arbitrary[BigIntConstant] = Arbitrary(bigIntConstGen)
  implicit lazy val arbGetVarBox: Arbitrary[BoxValue] = Arbitrary(getVar[SBox.type])
  implicit lazy val arbGetVarAvlTree : Arbitrary[AvlTreeValue]   = Arbitrary(getVarAvlTreeGen)
  implicit lazy val arbProveDlog     : Arbitrary[ProveDlog]      = Arbitrary(proveDlogGen)
  implicit lazy val arbProveDHT: Arbitrary[ProveDHTuple] = Arbitrary(proveDHTGen)
  implicit lazy val arbRegisterIdentifier: Arbitrary[RegisterId] = Arbitrary(registerIdentifierGen)
  implicit lazy val arbBigInteger: Arbitrary[BigInteger] = Arbitrary(Arbitrary.arbBigInt.arbitrary.map(_.bigInteger))
  implicit lazy val arbBigInt: Arbitrary[BigInt] = Arbitrary(arbBigInteger.arbitrary.map(SigmaDsl.BigInt(_)))
  implicit lazy val arbEcPointType: Arbitrary[dlogGroup.ElemType] = Arbitrary(Gen.const(()).flatMap(_ => CryptoConstants.dlogGroup.createRandomGenerator()))
  implicit lazy val arbGroupElement: Arbitrary[GroupElement] = Arbitrary(arbEcPointType.arbitrary.map(SigmaDsl.GroupElement(_)))
  implicit lazy val arbSigmaBoolean: Arbitrary[SigmaBoolean] = Arbitrary(Gen.oneOf(proveDHTGen, proveDHTGen))
  implicit lazy val arbSigmaProp: Arbitrary[SigmaProp] = Arbitrary(sigmaPropGen)
  implicit lazy val arbSigmaPropValue: Arbitrary[SigmaPropValue] = Arbitrary(sigmaPropValueGen)
  implicit lazy val arbErgoBox: Arbitrary[ErgoBox] = Arbitrary(ergoBoxGen)
  implicit lazy val arbBox: Arbitrary[Box] = Arbitrary(ergoBoxGen.map(SigmaDsl.Box))
  implicit lazy val arbAvlTreeData: Arbitrary[AvlTreeData] = Arbitrary(avlTreeDataGen)
  implicit lazy val arbAvlTree: Arbitrary[AvlTree] = Arbitrary(avlTreeGen)
  implicit lazy val arbBoxCandidate: Arbitrary[ErgoBoxCandidate] = Arbitrary(ergoBoxCandidateGen(tokensGen.sample.get))
  implicit lazy val arbTransaction: Arbitrary[ErgoLikeTransaction] = Arbitrary(ergoTransactionGen)
  implicit lazy val arbContextExtension: Arbitrary[ContextExtension] = Arbitrary(contextExtensionGen)
  implicit lazy val arbSerializedProverResult: Arbitrary[ProverResult] = Arbitrary(serializedProverResultGen)
  implicit lazy val arbUnsignedInput: Arbitrary[UnsignedInput] = Arbitrary(unsignedInputGen)
  implicit lazy val arbDataInput: Arbitrary[DataInput] = Arbitrary(dataInputGen)
  implicit lazy val arbInput: Arbitrary[Input] = Arbitrary(inputGen)
  implicit lazy val arbUnsignedErgoLikeTransaction: Arbitrary[UnsignedErgoLikeTransaction] = Arbitrary(unsignedErgoLikeTransactionGen)

  def arrayOfN[T](n: Int, g: Gen[T])
      (implicit evb: Buildable[T, Array[T]]): Gen[Array[T]] = {
    Gen.containerOfN[Array, T](n, g)
  }

  def arrayOfRange[T](minLength: Int, maxLength: Int, g: Gen[T])
      (implicit evb: Buildable[T, Array[T]]): Gen[Array[T]] = for {
    length <- Gen.chooseNum(minLength, maxLength)
    bytes <- arrayOfN(length, g)
  } yield bytes

  implicit def arrayGen[T: Arbitrary : ClassTag]: Gen[Array[T]] =
    arrayOfRange(1, 100, arbitrary[T])

  def collOfN[T: RType](n: Int, g: Gen[T])
      (implicit b: Buildable[T, Array[T]]): Gen[Coll[T]] = {
    arrayOfN[T](n, g).map(Colls.fromArray(_))
  }

  def collOfRange[T: RType](minLength: Int, maxLength: Int, g: Gen[T])
      (implicit evb: Buildable[T, Array[T]]): Gen[Coll[T]] =
    arrayOfRange(minLength, maxLength, g).map(_.toColl)

  implicit def collGen[T: Arbitrary : RType]: Gen[Coll[T]] = {
    implicit val cT = RType[T].classTag
    arrayGen[T].map(Colls.fromArray[T](_))
  }

  lazy val byteConstGen: Gen[ByteConstant] =
    arbByte.arbitrary.map { v => mkConstant[SByte.type](v, SByte) }
  lazy val booleanConstGen: Gen[Value[SBoolean.type]] = Gen.oneOf(TrueLeaf, FalseLeaf)
  lazy val shortConstGen: Gen[ShortConstant] =
    arbShort.arbitrary.map { v => mkConstant[SShort.type](v, SShort) }
  lazy val intConstGen: Gen[IntConstant] =
    arbInt.arbitrary.map { v => mkConstant[SInt.type](v, SInt) }
  lazy val longConstGen: Gen[LongConstant] =
    arbLong.arbitrary.map { v => mkConstant[SLong.type](v, SLong) }
  lazy val stringConstGen: Gen[StringConstant] =
    arbString.arbitrary.map { v => mkConstant[SString.type](v, SString) }
  lazy val bigIntConstGen: Gen[BigIntConstant] =
    arbBigInt.arbitrary.map { v => mkConstant[SBigInt.type](v, SBigInt) }

  lazy val byteArrayConstGen: Gen[CollectionConstant[SByte.type]] = for {
    bytes <- arrayOfRange(1, 100, arbByte.arbitrary)
  } yield mkCollectionConstant[SByte.type](bytes, SByte)

  lazy val intArrayConstGen: Gen[CollectionConstant[SInt.type]] = for {
    ints <- arrayOfRange(1, 100, arbInt.arbitrary)
  } yield mkCollectionConstant[SInt.type](ints, SInt)

  lazy val heightGen: Gen[Int] = Gen.chooseNum(0, 1000000)

  lazy val groupElementGen: Gen[EcPointType] = for {
    _ <- Gen.const(1)
  } yield CryptoConstants.dlogGroup.createRandomElement()

  lazy  val groupElementConstGen: Gen[GroupElementConstant] = for {
    p <- groupElementGen
  } yield mkConstant[SGroupElement.type](p.toGroupElement, SGroupElement)

  lazy val constantGen: Gen[Constant[SType]] =
    Gen.oneOf(booleanConstGen, byteConstGen,
      shortConstGen, intConstGen, longConstGen, bigIntConstGen, byteArrayConstGen,
      intArrayConstGen, groupElementConstGen).asInstanceOf[Gen[Constant[SType]]]

  def getVar[T <: SType](implicit aT: Arbitrary[T]): Gen[Value[T]] = for {
    t <- aT.arbitrary
    id <- arbByte.arbitrary
  } yield mkGetVar(id, t).get


  lazy val proveDlogGen: Gen[ProveDlog] = for {v <- groupElementGen} yield ProveDlog(v)
  lazy val proveDHTGen: Gen[ProveDHTuple] = for {
    gv <- groupElementGen
    hv <- groupElementGen
    uv <- groupElementGen
    vv <- groupElementGen
  } yield ProveDHTuple(gv, hv, uv, vv)

  lazy val sigmaTreeNodeGen: Gen[SigmaBoolean] = for {
    itemsNum <- Gen.choose(2, ThresholdLimit)
    items <- if (itemsNum <= 2) {
      arrayOfN(itemsNum, sigmaBooleanGen)
    } else {
      arrayOfN(itemsNum, nonRecursiveSigmaBoolean)
    }
    threshold <- Gen.choose(1, itemsNum)
    node <- Gen.oneOf(
      CTHRESHOLD(threshold, items),
      COR(items),
      CAND(items)
    )
  } yield node

  lazy val nonRecursiveSigmaBoolean: Gen[SigmaBoolean] = Gen.oneOf(proveDlogGen, proveDHTGen)

  lazy val sigmaBooleanGen: Gen[SigmaBoolean] = Gen.oneOf(
    nonRecursiveSigmaBoolean,
    Gen.delay(sigmaTreeNodeGen)
  )

  lazy val sigmaPropGen: Gen[SigmaProp] = sigmaBooleanGen.map(SigmaDsl.SigmaProp)
  lazy val sigmaPropValueGen: Gen[SigmaPropValue] =
    Gen.oneOf(proveDlogGen.map(SigmaPropConstant(_)), proveDHTGen.map(SigmaPropConstant(_)))

  lazy val registerIdentifierGen: Gen[RegisterId] = Gen.oneOf(R0, R1, R2, R3, R4, R5, R6, R7, R8, R9)

  lazy val getVarAvlTreeGen: Gen[AvlTreeValue] =
    arbByte.arbitrary.map { v => mkGetVar(v, SAvlTree).get }

  lazy val evaluatedValueGen: Gen[EvaluatedValue[SType]] =
    Gen.oneOf(booleanConstGen.asInstanceOf[Gen[EvaluatedValue[SType]]], byteArrayConstGen, longConstGen)

  def additionalRegistersGen(cnt: Byte): Seq[Gen[(NonMandatoryRegisterId, EvaluatedValue[_ <: SType])]] = {
    scala.util.Random.shuffle((0 until cnt).toList)
      .map(_ + ErgoBox.startingNonMandatoryIndex)
      .map(rI => ErgoBox.registerByIndex(rI).asInstanceOf[NonMandatoryRegisterId])
      .map { r =>
        for {
          v <- evaluatedValueGen
        } yield r -> v
      }
  }

  def additionalTokensGen(cnt: Int): Seq[Gen[(Digest32Coll, Long)]] =
    (0 until cnt).map { _ =>
      for {
        id <- boxIdGen
        amt <- Gen.oneOf(1, 500, 20000, 10000000, Long.MaxValue)
      } yield (Digest32Coll @@@ id.toColl) -> amt
    }

  val smallIntGen: Gen[Int] = Gen.chooseNum(2, 16)
  val smallIntOptGen: Gen[Option[Int]] = for {
    int <- smallIntGen
    opt <- Gen.oneOf(Some(int), None)
  } yield opt
  val unsignedIntGen: Gen[Int] = Gen.chooseNum(0, Int.MaxValue)
  val unsignedShortGen: Gen[Short] = Gen.chooseNum(0, Short.MaxValue).map(_.toShort)

  lazy val contextExtensionGen: Gen[ContextExtension] = for {
    values: scala.collection.Seq[(Byte, EvaluatedValue[SType])] <- Gen.sequence(contextExtensionValuesGen(0, 5))(Buildable.buildableSeq)
  } yield ContextExtension(mutable.LinkedHashMap[Byte, EvaluatedValue[SType]](values.sortBy(_._1).toSeq:_*))

  lazy val serializedProverResultGen: Gen[ProverResult] = for {
    bytes <- arrayOfRange(1, 100, arbByte.arbitrary)
    contextExt <- contextExtensionGen
  } yield ProverResult(bytes, contextExt)

  val boxIdGen: Gen[BoxId] = for {
    bytes <- arrayOfN(BoxId.size, arbByte.arbitrary)
  } yield ADKey @@ bytes

  lazy val unsignedInputGen: Gen[UnsignedInput] = for {
    boxId <- boxIdGen
    contextExt <- contextExtensionGen
  } yield new UnsignedInput(boxId, contextExt)

  lazy val dataInputGen: Gen[DataInput] = for {
    boxId <- boxIdGen
  } yield DataInput(boxId)

  lazy val inputGen: Gen[Input] = for {
    boxId <- boxIdGen
    proof <- serializedProverResultGen
  } yield Input(boxId, proof)

  def contextExtensionValuesGen(min: Int, max: Int): Seq[Gen[(Byte, EvaluatedValue[SType])]] =
    (0 until Gen.chooseNum(min, max).sample.get)
      .map(_ + 1)
      .map { varId =>
        for {
          v <- evaluatedValueGen
        }
          yield varId.toByte -> v
      }

  def avlTreeFlagsGen: Gen[AvlTreeFlags] = for {
    insert <- arbBool.arbitrary
    update <- arbBool.arbitrary
    remove <- arbBool.arbitrary
  } yield AvlTreeFlags(insert, update, remove)

  lazy val aDDigestGen: Gen[ADDigest] =
    arrayOfN(AvlTreeData.DigestSize, arbByte.arbitrary).map(ADDigest @@ _)

  def avlTreeDataGen: Gen[AvlTreeData] = for {
    digest <- aDDigestGen
    flags <- avlTreeFlagsGen
    keyLength <- unsignedIntGen
    vl <- arbOption[Int](Arbitrary(unsignedIntGen)).arbitrary
  } yield AvlTreeData(Colls.fromArray(digest), flags, keyLength, vl)

  def avlTreeGen: Gen[AvlTree] = avlTreeDataGen.map(SigmaDsl.avlTree)

  def avlTreeConstantGen: Gen[AvlTreeConstant] = avlTreeGen.map { v => AvlTreeConstant(v) }

  def wrappedTypeGen[T <: SType](tpe: T): Gen[T#WrappedType] = (tpe match {
    case SBoolean => arbBool
    case SByte => arbByte
    case SShort => arbShort
    case SInt => arbInt
    case SLong => arbLong
    case SBigInt => arbBigInt
    case SGroupElement => arbGroupElement
    case SSigmaProp => arbSigmaProp
    case SBox => arbBox
    case SAvlTree => arbAvlTree
    case SAny => arbAnyVal
    case SUnit => arbUnit
    case opt: SOption[a] =>
      Arbitrary(frequency((5, None), (5, for (x <- wrappedTypeGen(opt.elemType)) yield Some(x))))
  }).asInstanceOf[Arbitrary[T#WrappedType]].arbitrary

  def tupleGen(min: Int, max: Int): Gen[Tuple] = for {
    values <- arrayOfRange(min, max, Gen.oneOf(
      byteArrayConstGen,
      byteConstGen,
      shortConstGen,
      intConstGen,
      longConstGen,
      booleanConstGen,
      bigIntConstGen,
      groupElementConstGen,
      getVar[SInt.type],
      getVar[SLong.type],
      getVar[SBox.type],
      getVar(Arbitrary(sTupleGen(2, 10)))
    ))
  } yield mkTuple(values).asInstanceOf[Tuple]

  lazy val modifierIdGen: Gen[ModifierId] =
    arrayOfN(sigma.crypto.hashLength, arbByte.arbitrary)
    .map(bytesToId)

  lazy val modifierIdBytesGen: Gen[Coll[Byte]] =
    collOfN(sigma.crypto.hashLength, arbByte.arbitrary)

  val MaxTokens = 10

  lazy val ergoBoxGen: Gen[ErgoBox] = for {
    tId <- modifierIdGen
    boxId <- unsignedShortGen
    tokensCount <- Gen.chooseNum[Int](0, MaxTokens)
    tokens <- Gen.sequence(additionalTokensGen(tokensCount))(Buildable.buildableSeq).map(_.map(_._1))
    candidate <- ergoBoxCandidateGen(tokens.toSeq)
  } yield candidate.toBox(tId, boxId)

  lazy val additionalRegistersGen: Gen[AdditionalRegisters] = for {
    regNum <- Gen.chooseNum[Byte](0, ErgoBox.nonMandatoryRegistersCount)
    regs <- Gen.sequence(additionalRegistersGen(regNum))(Buildable.buildableSeq)
  } yield
    Map(regs.toIndexedSeq:_*)

  def ergoBoxTokens(availableTokens: Seq[TokenId]): Gen[Coll[Token]] = for {
    tokens <-
      if(availableTokens.nonEmpty) {
        for {
          tokensCount <- Gen.chooseNum[Int](0, MaxTokens)
          ts <- arrayOfN(tokensCount, Gen.oneOf(availableTokens))
        } yield ts.distinct
      } else {
        Gen.const(Array.empty[TokenId])
      }
    tokenAmounts <- arrayOfN(tokens.length, Gen.oneOf(1, 500, 20000, 10000000, Long.MaxValue))
  } yield tokens.toColl.zip(tokenAmounts.toColl)

  def ergoBoxCandidateGen(availableTokens: Seq[TokenId]): Gen[ErgoBoxCandidate] = for {
    l <- arbLong.arbitrary
    b <- ergoTreeGen.filter(t => t.bytes.length < MaxPropositionBytes.value)
    ar <- additionalRegistersGen
    tokens <- ergoBoxTokens(availableTokens)
    creationHeight <- heightGen
  } yield new ErgoBoxCandidate(l, b, creationHeight, tokens, ar)

  lazy val boxConstantGen: Gen[BoxConstant] = ergoBoxGen.map { v => BoxConstant(CBox(v)) }

  lazy val digest32Gen: Gen[TokenId] = for {
    bytes <- collOfN(TokenId.size, arbByte.arbitrary)
  } yield Digest32Coll @@@ bytes

  lazy val tokenIdGen: Gen[TokenId] = digest32Gen

  lazy val tokensGen: Gen[Seq[TokenId]] = for {
    count <- Gen.chooseNum(1, MaxTokens)
    tokens <- arrayOfN(count, tokenIdGen)
  } yield tokens

  lazy val ergoTransactionGen: Gen[ErgoLikeTransaction] = for {
    inputBoxIds <- Gen.nonEmptyListOf(boxIdGen)
    dataInputBoxIds <- Gen.listOf(boxIdGen)
    tx <- ergoLikeTransactionGen(inputBoxIds, dataInputBoxIds)
  } yield tx

  // distinct list of elements from a given generator
  // with a maximum number of elements to discard
  // source https://gist.github.com/etorreborre/d0616e704ed85d7276eb12b025df8ab0
  def distinctListOfGen[T](gen: Gen[T], maxDiscarded: Int = 1000): Gen[List[T]] = {
    val seen: ListBuffer[T] = new ListBuffer[T]
    var discarded = 0

    Gen.sized { size =>
      if (size == seen.size) seen.toList
      else {
        while (seen.size <= size && discarded < maxDiscarded)
          gen.sample match {
            case Some(t) if !seen.contains(t) =>
              seen.+=:(t)
            case _ => discarded += 1
          }
        seen.toList
      }
    }
  }

  lazy val minerVotesGen: Gen[Coll[Byte]] = collOfN(CHeader.VotesSize, arbByte.arbitrary)

  lazy val nonceBytesGen: Gen[Coll[Byte]] = collOfN(CHeader.NonceSize, arbByte.arbitrary)

  import ValidationRules._

  val numRules = currentSettings.size

  val replacedRuleIdGen = Gen.chooseNum((FirstRuleId + numRules).toShort, Short.MaxValue)

  val ruleIdGen = Gen.chooseNum(FirstRuleId, (FirstRuleId + numRules - 1).toShort)

  val statusGen: Gen[RuleStatus] = Gen.oneOf(
    Gen.oneOf(EnabledRule, DisabledRule),
    replacedRuleIdGen.map(id => ReplacedRule(id)),
    arrayOfRange(1, 10, arbitrary[Byte]).map(xs => ChangedRule(xs))
  )

  lazy val mapCollectionGen: Gen[MapCollection[SInt.type, SInt.type]] = for {
    input <- arbCCOfIntConstant.arbitrary
    mapper <- funcValueGen
  } yield mkMapCollection(input, mapper).asInstanceOf[MapCollection[SInt.type, SInt.type]]

  lazy val existsGen: Gen[Exists[SInt.type]] = for {
    input <- arbCCOfIntConstant.arbitrary
    condition <- funcValueGen
  } yield mkExists(input, condition).asInstanceOf[Exists[SInt.type]]

  lazy val forAllGen: Gen[ForAll[SInt.type]] = for {
    input <- arbCCOfIntConstant.arbitrary
    condition <- funcValueGen
  } yield mkForAll(input, condition).asInstanceOf[ForAll[SInt.type]]

  lazy val foldGen: Gen[Fold[SInt.type, SBoolean.type]] = for {
    input <- arbCCOfIntConstant.arbitrary
    foldOp <- funcValueGen
  } yield
    mkFold(input, TrueLeaf, foldOp).asInstanceOf[Fold[SInt.type, SBoolean.type]]

  lazy val sliceGen: Gen[Slice[SInt.type]] = for {
    col1 <- arbCCOfIntConstant.arbitrary
    from <- intConstGen
    until <- intConstGen
  } yield mkSlice(col1, from, until).asInstanceOf[Slice[SInt.type]]

  lazy val atLeastGen: Gen[AtLeast] = for {
    bound <- intConstGen
    input <- arbCCOfSigmaPropConstant.arbitrary
  } yield mkAtLeast(bound, input).asInstanceOf[AtLeast]

  lazy val filterGen: Gen[Filter[SInt.type]] = for {
    col1 <- arbCCOfIntConstant.arbitrary
    condition <- funcValueGen
  } yield mkFilter(col1, condition).asInstanceOf[Filter[SInt.type]]

  lazy val appendGen: Gen[Append[SInt.type]] = for {
    col1 <- arbCCOfIntConstant.arbitrary
    col2 <- arbCCOfIntConstant.arbitrary
  } yield mkAppend(col1, col2).asInstanceOf[Append[SInt.type]]

  lazy val sizeOfGen: Gen[SizeOf[SInt.type]] = for {
    input <- arbCCOfIntConstant.arbitrary
  } yield mkSizeOf(input).asInstanceOf[SizeOf[SInt.type]]

  lazy val extractAmountGen: Gen[ExtractAmount] =
    arbGetVarBox.arbitrary.map { b => mkExtractAmount(b).asInstanceOf[ExtractAmount] }
  lazy val extractScriptBytesGen: Gen[ExtractScriptBytes] =
    arbGetVarBox.arbitrary.map { b => mkExtractScriptBytes(b).asInstanceOf[ExtractScriptBytes] }
  lazy val extractBytesGen: Gen[ExtractBytes] =
    arbGetVarBox.arbitrary.map { b => mkExtractBytes(b).asInstanceOf[ExtractBytes] }
  lazy val extractBytesWithNoRefGen: Gen[ExtractBytesWithNoRef] =
    arbGetVarBox.arbitrary.map { b =>
      mkExtractBytesWithNoRef(b).asInstanceOf[ExtractBytesWithNoRef]
    }
  lazy val extractIdGen: Gen[ExtractId] =
    arbGetVarBox.arbitrary.map { b => mkExtractId(b).asInstanceOf[ExtractId] }
  lazy val extractRegisterAsGen: Gen[ExtractRegisterAs[SInt.type]] = for {
    input <- arbGetVarBox.arbitrary
    r <- arbRegisterIdentifier.arbitrary
  } yield ExtractRegisterAs(input, r)(SInt)
  lazy val extractCreationInfoGen: Gen[ExtractCreationInfo] =
    arbGetVarBox.arbitrary.map { b => mkExtractCreationInfo(b).asInstanceOf[ExtractCreationInfo] }

  lazy val deserializeContextGen: Gen[DeserializeContext[SBoolean.type]] =
    Arbitrary.arbitrary[Byte].map(b =>
      mkDeserializeContext(b, SBoolean).asInstanceOf[DeserializeContext[SBoolean.type]])

  lazy val deserializeRegisterGen: Gen[DeserializeRegister[SBoolean.type]] = for {
    r <- arbRegisterIdentifier.arbitrary
    default <- booleanConstGen
    isDefined <- Arbitrary.arbitrary[Boolean]
    defaultOpt = if (isDefined) Some(default) else None
  } yield mkDeserializeRegister(r, SBoolean, defaultOpt)
    .asInstanceOf[DeserializeRegister[SBoolean.type]]

  lazy val longToByteArrayGen: Gen[LongToByteArray] = arbLongConstants.arbitrary.map { v => LongToByteArray(v) }
  lazy val byteArrayToBigIntGen: Gen[ByteArrayToBigInt] =
    arbByteArrayConstant.arbitrary.map { v =>
      mkByteArrayToBigInt(v).asInstanceOf[ByteArrayToBigInt]
    }
  lazy val calcBlake2b256Gen: Gen[CalcBlake2b256] = arbByteArrayConstant.arbitrary.map { v => CalcBlake2b256(v) }
  lazy val calcSha256Gen: Gen[CalcSha256] = arbByteArrayConstant.arbitrary.map { v => CalcSha256(v) }

  lazy val byIndexGen: Gen[ByIndex[SInt.type]] = for {
    input <- Gen.oneOf(intConstCollectionGen, intArrayConstGen)
    index <- arbInt.arbitrary
    defaultValue <- arbOption(arbIntConstants).arbitrary
  } yield ByIndex(input, index, defaultValue)

  lazy val booleanExprGen: Gen[Value[SBoolean.type]] =
    Gen.oneOf(
      Gen.oneOf(
        EQ(IntConstant(1), IntConstant(1)), // true
        EQ(IntConstant(1), IntConstant(2)) // false
      ),
      booleanConstGen
    )

  private type LogicalTransformerCons =
    Seq[Value[SBoolean.type]] => Transformer[SCollection[SBoolean.type], SBoolean.type]

  def logicalExprTreeNodeGen(nodeCons: Seq[LogicalTransformerCons]): Gen[Transformer[SCollection[SBoolean.type], SBoolean.type]] = for {
    left <- logicalExprTreeGen(nodeCons)
    right <- logicalExprTreeGen(nodeCons)
    node <- Gen.oneOf(nodeCons.map(cons => cons(Array(left, right))))
  } yield node

  def logicalExprTreeGen(nodeCons: Seq[LogicalTransformerCons]): Gen[Value[SBoolean.type]] =
    Gen.oneOf(booleanExprGen, booleanConstGen, Gen.delay(logicalExprTreeNodeGen(nodeCons)))

  def numExprTreeNodeGen: Gen[Value[SNumericType]] = for {
    left <- numExprTreeGen
    right <- numExprTreeGen
    node <- Gen.oneOf(
      mkPlus(left, right),
      mkMinus(left, right),
      mkMultiply(left, right),
      mkDivide(left, right),
      mkModulo(left, right),
      mkMin(left, right),
      mkMax(left, right)
    )
  } yield node

  def numExprTreeGen: Gen[Value[SNumericType]] =
    Gen.oneOf(arbByteConstants.arbitrary,
      arbIntConstants.arbitrary,
      arbLongConstants.arbitrary,
      arbBigIntConstant.arbitrary,
      Gen.delay(numExprTreeNodeGen))

  def comparisonExprTreeNodeGen: Gen[Value[SBoolean.type]] = for {
    left <- numExprTreeNodeGen
    right <- numExprTreeNodeGen
    node <- Gen.oneOf(
      mkEQ(left, right),
      mkNEQ(left, right),
      mkLE(left, right),
      mkGE(left, right),
      mkLT(left, right),
      mkGT(left, right)
    )
  } yield node

  lazy val downcastGen: Gen[Downcast[SNumericType, SNumericType]] = for {
    numVal <- Gen.oneOf(numExprTreeNodeGen, shortConstGen, intConstGen, longConstGen)
  } yield mkDowncast(numVal, SByte).asInstanceOf[Downcast[SNumericType, SNumericType]]

  lazy val base58StringGen: Gen[String] = for {
    s <- Gen.someOf(Base58.Alphabet).suchThat(_.nonEmpty)
  } yield s.toString

  lazy val base64StringGen: Gen[String] = for {
    s <- Gen.someOf(Base64.Alphabet).suchThat(_.length > 1)
  } yield s.toString

  def p2pkAddressGen(networkPrefix: Byte): Gen[P2PKAddress] = for {
    pd <- proveDlogGen
  } yield P2PKAddress(pd)(new ErgoAddressEncoder(networkPrefix))

  lazy val getVarIntGen: Gen[GetVar[SInt.type]] = for {
    varId <- arbByte.arbitrary
  } yield GetVarInt(varId)

  lazy val optionGetGen: Gen[OptionGet[SInt.type]] = for {
    getVar <- getVarIntGen
  } yield OptionGet(getVar)

  lazy val optionGetOrElseGen: Gen[OptionGetOrElse[SInt.type]] = for {
    getVar <- getVarIntGen
  } yield OptionGetOrElse(getVar, IntConstant(1))

  lazy val optionIsDefinedGen: Gen[OptionIsDefined[SInt.type]] = for {
    getVar <- getVarIntGen
  } yield OptionIsDefined(getVar)

  lazy val valDefGen: Gen[ValDef] = for {
    id <- unsignedIntGen
    rhs <- booleanExprGen
  } yield ValDef(id, Nil, rhs)

  lazy val funDefGen: Gen[ValDef] = for {
    id <- unsignedIntGen
    tpeArgs <- Gen.nonEmptyListOf(sTypeIdentGen)
    rhs <- booleanExprGen
  } yield ValDef(id, tpeArgs, rhs)

  lazy val valOrFunDefGen: Gen[ValDef] = for {
    v <- Gen.oneOf(valDefGen, funDefGen)
  } yield v

  lazy val valUseGen: Gen[ValUse[SType]] = for {
    id <- unsignedIntGen
    tpe <- predefTypeGen
  } yield ValUse(id, tpe)

  lazy val blockValueGen: Gen[BlockValue] = for {
    items <- Gen.listOf(valDefGen)
  } yield BlockValue(items.toIndexedSeq,
    EQ(
      SizeOf(Tuple(items.toIndexedSeq.map(valDef => ValUse(valDef.id, valDef.tpe)))),
      IntConstant(items.length)))

  lazy val constantPlaceholderGen: Gen[ConstantPlaceholder[SType]] = for {
    id <- unsignedIntGen
    tpe <- predefTypeGen
  } yield ConstantPlaceholder(id, tpe)

  lazy val funcValueArgsGen: Gen[IndexedSeq[(Int, SType)]] = for {
    num <- Gen.chooseNum(1, 10)
    indices <- arrayOfN(num, unsignedIntGen)
    tpes <- arrayOfN(num, predefTypeGen)
  } yield indices.zip(tpes).toIndexedSeq

  lazy val funcValueGen: Gen[FuncValue] = for {
    args <- funcValueArgsGen
    body <- logicalExprTreeNodeGen(ArraySeq(AND.apply))
  } yield FuncValue(args, body)

  lazy val sigmaAndGen: Gen[SigmaAnd] = for {
    items <- arrayOfRange(1, ThresholdLimit, sigmaPropValueGen)
  } yield mkSigmaAnd(items).asInstanceOf[SigmaAnd]

  lazy val sigmaOrGen: Gen[SigmaOr] = for {
    items <- arrayOfRange(1, ThresholdLimit, sigmaPropValueGen)
  } yield mkSigmaOr(items).asInstanceOf[SigmaOr]

  lazy val sigmaThresholdGen: Gen[CTHRESHOLD] = for {
    num <- Gen.chooseNum(1, ThresholdLimit)
    threshold <- Gen.choose(0, num)
    items: Seq[SigmaBoolean] <- arrayOfN(num, sigmaBooleanGen).map(_.toSeq)
  } yield CTHRESHOLD(threshold, items)


  lazy val boolToSigmaPropGen: Gen[BoolToSigmaProp] = for {
    b <- booleanConstGen
  } yield mkBoolToSigmaProp(b).asInstanceOf[BoolToSigmaProp]

  lazy val byteArrayToLongGen: Gen[ByteArrayToLong] =
    arbByteArrayConstant.arbitrary.map { v =>
      mkByteArrayToLong(v).asInstanceOf[ByteArrayToLong]
    }

  lazy val ergoTreeGen: Gen[ErgoTree] = for {
    sigmaBoolean <- Gen.delay(sigmaBooleanGen)
    propWithConstants <- Gen.delay(logicalExprTreeNodeGen(Seq(AND.apply, OR.apply, XorOf.apply)).map(_.toSigmaProp))
    prop <- Gen.oneOf(propWithConstants, sigmaBoolean.toSigmaPropValue)
    treeBuilder <- Gen.oneOf(Seq[SigmaPropValue => ErgoTree](ErgoTree.withSegregation(ZeroHeader, _),
      ErgoTree.withoutSegregation(ZeroHeader, _)))
  } yield treeBuilder(prop)

  lazy val ergoTreeWithSegregationGen: Gen[ErgoTree] = for {
    sigmaBoolean <- Gen.delay(sigmaBooleanGen)
    propWithConstants <- Gen.delay(logicalExprTreeNodeGen(Seq(AND.apply, OR.apply, XorOf.apply)).map(_.toSigmaProp))
    prop <- Gen.oneOf(propWithConstants, sigmaBoolean.toSigmaPropValue)
  } yield ErgoTree.withSegregation(ZeroHeader, prop)

  def headerGen(stateRoot: AvlTree, parentId: Coll[Byte]): Gen[Header] = for {
    id <- modifierIdBytesGen
    version <- arbByte.arbitrary
    adProofsRoot <- digest32Gen
    transactionRoot <- digest32Gen
    timestamp <- arbLong.arbitrary
    nBits <- arbLong.arbitrary
    height <- heightGen
    extensionRoot <- digest32Gen
    minerPk <- groupElementGen
    powOnetimePk <- groupElementGen
    powNonce <- nonceBytesGen
    powDistance <- arbBigInt.arbitrary
    votes <- minerVotesGen
  } yield CHeader(id, version, parentId, adProofsRoot, stateRoot, transactionRoot, timestamp, nBits,
    height, extensionRoot, minerPk.toGroupElement, powOnetimePk.toGroupElement, powNonce, powDistance, votes)

  lazy val headerGen: Gen[Header] = for {
    stateRoot <- avlTreeGen
    parentId <- modifierIdBytesGen
    header <- headerGen(stateRoot, parentId)
  } yield header

  implicit lazy val arbHeader: Arbitrary[Header] = Arbitrary(headerGen)

  val MaxHeaders = 2
  def headersGen(stateRoot: AvlTree): Gen[Seq[Header]] = for {
    size <- Gen.chooseNum(0, MaxHeaders)
  } yield
    if (size == 0) Seq()
    else {
      val h = headerGen(stateRoot, modifierIdBytesGen.sample.get).sample.get
      (0 to size)
        .foldLeft(List[Header](h)) { (hs, _) =>
          headerGen(stateRoot, hs.head.id).sample.get :: hs
        }
    }

  def preHeaderGen(parentId: Coll[Byte]): Gen[PreHeader] = for {
    version <- arbByte.arbitrary
    timestamp <- arbLong.arbitrary
    nBits <- arbLong.arbitrary
    height <- heightGen
    minerPk <- groupElementGen
    votes <- minerVotesGen
  } yield CPreHeader(version, parentId, timestamp, nBits, height, minerPk.toGroupElement, votes)

  lazy val preHeaderGen: Gen[PreHeader] = for {
    parentId <- modifierIdBytesGen
    preHeader <- preHeaderGen(parentId)
  } yield preHeader

  implicit lazy val arbPreHeader: Arbitrary[PreHeader] = Arbitrary(preHeaderGen)

  lazy val ergoLikeTransactionGen: Gen[ErgoLikeTransaction] = for {
    inputBoxesIds <- Gen.nonEmptyListOf(boxIdGen)
    dataInputBoxIds <- Gen.listOf(boxIdGen)
    tx <- ergoLikeTransactionGen(inputBoxesIds, dataInputBoxIds)
  } yield tx

  def ergoLikeTransactionGen(inputBoxesIds: Seq[BoxId], dataInputBoxIds: Seq[BoxId]): Gen[ErgoLikeTransaction] = for {
    tokens <- tokensGen
    outputsCount <- Gen.chooseNum(1, MaxOutputBoxes)
    outputCandidates <- arrayOfN(outputsCount, ergoBoxCandidateGen(tokens))
    proofs <- arrayOfN(inputBoxesIds.length, serializedProverResultGen)
  } yield new ErgoLikeTransaction(
    inputs = inputBoxesIds.zip(proofs).map(t => Input(t._1, t._2)).toIndexedSeq,
    dataInputs = dataInputBoxIds.map(DataInput).toIndexedSeq,
    outputCandidates = outputCandidates.toIndexedSeq
  )

  lazy val unsignedErgoLikeTransactionGen: Gen[UnsignedErgoLikeTransaction] = for {
    inputBoxesIds <- Gen.nonEmptyListOf(boxIdGen)
    dataInputBoxIds <- Gen.listOf(boxIdGen)
    tx <- unsignedErgoLikeTransactionGen(inputBoxesIds, dataInputBoxIds)
  } yield tx

  def unsignedErgoLikeTransactionGen(inputBoxesIds: Seq[BoxId], dataInputBoxIds: Seq[BoxId]): Gen[UnsignedErgoLikeTransaction] = for {
    tokens <- tokensGen
    outputsCount <- Gen.chooseNum(1, MaxOutputBoxes)
    outputCandidates <- arrayOfN(outputsCount, ergoBoxCandidateGen(tokens))
    contextExtensions <- arrayOfN(inputBoxesIds.length, contextExtensionGen)
  } yield new UnsignedErgoLikeTransaction(
    inputs = inputBoxesIds.zip(contextExtensions).map(t => new UnsignedInput(t._1, t._2)).toIndexedSeq,
    dataInputs = dataInputBoxIds.map(DataInput).toIndexedSeq,
    outputCandidates = outputCandidates.toIndexedSeq
  )

  lazy val ergoLikeTransactionTemplateGen: Gen[ErgoLikeTransactionTemplate[_ <: UnsignedInput]] =
    Gen.oneOf(unsignedErgoLikeTransactionGen, ergoLikeTransactionGen)

  val MaxDataBoxes = 5
  val MaxInputBoxes = 5
  val MaxOutputBoxes = 100

  lazy val ergoLikeContextGen: Gen[ErgoLikeContext] = for {
    stateRoot <- avlTreeGen
    headers <- headersGen(stateRoot)
    preHeader <- preHeaderGen(headers.headOption.map(_.id).getOrElse(modifierIdBytesGen.sample.get))
    dataBoxes <- choose(0, MaxDataBoxes).flatMap(arrayOfN(_, ergoBoxGen))
    boxesToSpend <- choose(1, MaxInputBoxes).flatMap(arrayOfN(_, ergoBoxGen)).suchThat(_.size > 0)
    extension <- contextExtensionGen
    costLimit <- arbLong.arbitrary
    initCost <- arbLong.arbitrary
    avlTreeFlags <- avlTreeFlagsGen
    spendingTransaction <- Gen.oneOf(
      unsignedErgoLikeTransactionGen(boxesToSpend.map(_.id), dataBoxes.map(_.id)),
      ergoLikeTransactionGen(boxesToSpend.map(_.id), dataBoxes.map(_.id))
    )
  } yield new ErgoLikeContext(
    lastBlockUtxoRoot = AvlTreeData(stateRoot.digest, avlTreeFlags, unsignedIntGen.sample.get),
    headers = headers.toColl,
    preHeader = preHeader,
    dataBoxes = dataBoxes.toIndexedSeq,
    boxesToSpend = boxesToSpend.toIndexedSeq,
    spendingTransaction = spendingTransaction,
    selfIndex = 0,
    extension = extension,
    validationSettings = ValidationRules.currentSettings,
    costLimit = costLimit,
    initCost = initCost,
    activatedScriptVersion = activatedVersionInTests
  ).withErgoTreeVersion(ergoTreeVersionInTests)

}
