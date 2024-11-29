package org.ergoplatform.sdk

import org.ergoplatform.ErgoAddressEncoder.NetworkPrefix
import org.ergoplatform.ErgoBox.{Token, TokenId}
import org.ergoplatform._
import org.ergoplatform.sdk.Extensions.{CollBuilderOps, PairCollOps}
import org.ergoplatform.sdk.JavaHelpers.{TokenColl, TokenIdRType}
import org.ergoplatform.sdk.wallet.secrets.{DerivationPath, ExtendedSecretKey}
import org.ergoplatform.sdk.wallet.{Constants, TokensMap}
import org.ergoplatform.settings.ErgoAlgos
import scorex.crypto.authds.ADKey
import scorex.util.encode.Base16
import scorex.util.{ModifierId, bytesToId, idToBytes}
import sigma.ast.{ErgoTree, SType}
import sigma.ast.syntax.SValue
import sigma.crypto.{CryptoFacade, EcPointType}
import sigma.data.ExactIntegral.LongIsExactIntegral
import sigma.data.{CSigmaDslBuilder, Digest32Coll, Iso, ProveDHTuple, ProveDlog, RType, SigmaBoolean, SigmaConstants}
import sigma.serialization.GroupElementSerializer
import sigma.util.StringUtil.StringUtilExtensions
import sigma.{AnyValue, AvlTree, Coll, Colls, Evaluation, GroupElement, Header}
import sigma.ast.{Constant, EvaluatedValue, SigmaPropConstant}
import sigmastate.crypto.DiffieHellmanTupleProverInput
import sigma.serialization.{ErgoTreeSerializer, SigmaSerializer, ValueSerializer}
import sigmastate.utils.Helpers._  // required for Scala 2.11

import java.lang.{Boolean => JBoolean, Byte => JByte, Integer => JInt, Long => JLong, Short => JShort, String => JString}
import java.math.BigInteger
import java.util
import java.util.{List => JList, Map => JMap}
import scala.collection.{JavaConverters, mutable}

trait LowPriorityIsos {
}

object SdkIsos extends LowPriorityIsos {
  implicit val jbyteToByte: Iso[JByte, Byte] = new Iso[JByte, Byte] {
    override def to(b: JByte): Byte = b
    override def from(a: Byte): JByte = a
  }

  implicit val jshortToShort: Iso[JShort, Short] = new Iso[JShort, Short] {
    override def to(b: JShort): Short = b
    override def from(a: Short): JShort = a
  }

  implicit val jintToInt: Iso[JInt, Int] = new Iso[JInt, Int] {
    override def to(b: JInt): Int = b
    override def from(a: Int): JInt = a
  }

  implicit val jlongToLong: Iso[JLong, Long] = new Iso[JLong, Long] {
    override def to(b: JLong): Long = b
    override def from(a: Long): JLong = a
  }

  implicit val jboolToBool: Iso[JBoolean, Boolean] = new Iso[JBoolean, Boolean] {
    override def to(b: JBoolean): Boolean = b
    override def from(a: Boolean): JBoolean = a
  }

  implicit def collToColl[A: RType, B: RType](implicit iso: Iso[A, B]): Iso[Coll[A], Coll[B]] = new Iso[Coll[A], Coll[B]] {
    override def to(as: Coll[A]): Coll[B] = as.map(iso.to)
    override def from(bs: Coll[B]): Coll[A] = bs.map(iso.from)
  }

  implicit val isoErgoTokenToPair: Iso[ErgoToken, Token] = new Iso[ErgoToken, Token] {
    override def to(a: ErgoToken): Token =
      (Digest32Coll @@ Colls.fromArray(a.getId.getBytes), a.getValue)
    override def from(t: Token): ErgoToken = new ErgoToken(t._1.toArray, t._2)
  }

  implicit val isoErgoTokenSeqToLinkedMap: Iso[IndexedSeq[ErgoToken], mutable.LinkedHashMap[ModifierId, Long]] =
    new Iso[IndexedSeq[ErgoToken], mutable.LinkedHashMap[ModifierId, Long]] {
      override def to(a: IndexedSeq[ErgoToken]): mutable.LinkedHashMap[ModifierId, Long] = {
        val lhm = new mutable.LinkedHashMap[ModifierId, Long]()
        a.foreach { et =>
          val t = isoErgoTokenToPair.to(et)
          lhm += bytesToId(t._1.toArray) -> t._2
        }
        lhm
      }

      override def from(t: mutable.LinkedHashMap[ModifierId, Long]): IndexedSeq[ErgoToken] = {
        val pairs = t.toIndexedSeq
          .map { t =>
            val id = Digest32Coll @@ Colls.fromArray(idToBytes(t._1))
            val value = t._2
            isoErgoTokenToPair.from((id, value))
          }
        pairs
      }
    }

  implicit def isoJMapToMap[K,V1,V2](iso: Iso[V1, V2]): Iso[JMap[K, V1], scala.collection.Map[K,V2]] = new Iso[JMap[K, V1], scala.collection.Map[K,V2]] {
    import JavaConverters._
    override def to(a: JMap[K, V1]): scala.collection.Map[K, V2] = {
      a.asScala.mapValues(iso.to).toMap
    }
    override def from(b: scala.collection.Map[K, V2]): JMap[K, V1] = {
      b.mapValues(iso.from).toMap.asJava
    }
  }

  val isoEvaluatedValueToSConstant: Iso[EvaluatedValue[SType], Constant[SType]] = new Iso[EvaluatedValue[SType], Constant[SType]] {
    override def to(x: EvaluatedValue[SType]): Constant[SType] = x.asInstanceOf[Constant[SType]]
    override def from(x: Constant[SType]): EvaluatedValue[SType] = x
  }

  val isoTokensListToPairsColl: Iso[JList[ErgoToken], Coll[Token]] = {
    JListToColl(isoErgoTokenToPair, RType[Token])
  }

  val isoTokensListToTokenColl: Iso[JList[ErgoToken], TokenColl] = new Iso[JList[ErgoToken], TokenColl] {
    override def to(ts: JList[ErgoToken]): TokenColl =
      isoTokensListToPairsColl.to(ts)

    override def from(ts: TokenColl): JList[ErgoToken] = {
      isoTokensListToPairsColl.from(ts)
    }
  }

  val isoSigmaBooleanToByteArray: Iso[SigmaBoolean, Array[Byte]] = new Iso[SigmaBoolean, Array[Byte]] {
    override def to(a: SigmaBoolean): Array[Byte] = {
      val w = SigmaSerializer.startWriter()
      SigmaBoolean.serializer.serialize(a, w)
      w.toBytes
    }
    override def from(b: Array[Byte]): SigmaBoolean ={
      val r = SigmaSerializer.startReader(b, 0)
      SigmaBoolean.serializer.parse(r)
    }
  }

  implicit val jstringToOptionString: Iso[JString, Option[String]] = new Iso[JString, Option[String]] {
    override def to(a: JString): Option[String] = if (a.isNullOrEmpty) None else Some(a)
    override def from(b: Option[String]): JString = if (b.isEmpty) "" else b.get
  }

  implicit val arrayCharToOptionString: Iso[SecretString, Option[String]] = new Iso[SecretString, Option[String]] {
    override def to(ss: SecretString): Option[String] = {
      if (ss == null || ss.isEmpty) None else Some(ss.toStringUnsecure)
    }
    override def from(b: Option[String]): SecretString =
      if (b.isEmpty) SecretString.empty() else SecretString.create(b.get)
  }

  implicit def JListToIndexedSeq[A, B](implicit itemIso: Iso[A, B]): Iso[JList[A], IndexedSeq[B]] =
    new Iso[JList[A], IndexedSeq[B]] {
      import JavaConverters._

      override def to(as: JList[A]): IndexedSeq[B] = {
        as.iterator().asScala.map(itemIso.to).toIndexedSeq
      }

      override def from(bs: IndexedSeq[B]): JList[A] = {
        val res = new util.ArrayList[A](bs.length)
        for ( a <- bs.map(itemIso.from) ) res.add(a)
        res
      }
    }

  implicit def JListToColl[A, B](implicit itemIso: Iso[A, B], tB: RType[B]): Iso[JList[A], Coll[B]] =
    new Iso[JList[A], Coll[B]] {
      import JavaConverters._

      override def to(as: JList[A]): Coll[B] = {
        val bsIter = as.iterator.asScala.map { a =>
          itemIso.to(a)
        }
        Colls.fromArray(bsIter.toArray(tB.classTag))
      }

      override def from(bs: Coll[B]): JList[A] = {
        val res = new util.ArrayList[A](bs.length)
        bs.toArray.foreach { b =>
          res.add(itemIso.from(b))
        }
        res
      }
    }

}

object JavaHelpers {
  implicit class UniversalConverter[A](val x: A) extends AnyVal {
    def convertTo[B](implicit iso: Iso[A,B]): B = iso.to(x)
  }

  implicit class StringExtensions(val base16: String) extends AnyVal {
    /** Decodes this base16 string to byte array. */
    def toBytes: Array[Byte] = decodeStringToBytes(base16)

    /** Decodes this base16 string to byte array and wrap it as Coll[Byte]. */
    def toColl: Coll[Byte] = decodeStringToColl(base16)

    /** Decodes this base16 string to byte array and parse it using [[GroupElementSerializer]]. */
    def toGroupElement: GroupElement = decodeStringToGE(base16)

    /** Decodes this base16 string to byte array and parse it using
      * [[ErgoTreeSerializer.deserializeErgoTree()]].
      */
    def toErgoTree: ErgoTree = decodeStringToErgoTree(base16)
  }

  implicit val TokenIdRType: RType[TokenId] = collRType(sigma.ByteType).asInstanceOf[RType[TokenId]]
  implicit val JByteRType: RType[JByte] = sigma.ByteType.asInstanceOf[RType[JByte]]
  implicit val JShortRType: RType[JShort] = sigma.ShortType.asInstanceOf[RType[JShort]]
  implicit val JIntRType: RType[JInt] = sigma.IntType.asInstanceOf[RType[JInt]]
  implicit val JLongRType: RType[JLong] = sigma.LongType.asInstanceOf[RType[JLong]]
  implicit val JBooleanRType: RType[JBoolean] = sigma.BooleanType.asInstanceOf[RType[JBoolean]]
  implicit val JUnitRType: RType[Unit] = sigma.UnitType

  val HeaderRType: RType[Header] = sigma.HeaderRType
  val PreHeaderRType: RType[sigma.PreHeader] = sigma.PreHeaderRType

  def Algos: ErgoAlgos = org.ergoplatform.settings.ErgoAlgos

  def deserializeValue[T <: SValue](bytes: Array[Byte]): T = {
    ValueSerializer.deserialize(bytes).asInstanceOf[T]
  }

  /** Decodes this base16 string to byte array. */
  def decodeStringToBytes(base16: String): Array[Byte] = {
    val bytes = ErgoAlgos.decode(base16).fold(t => throw t, identity)
    bytes
  }

  /** Decodes this base16 string to byte array and wrap it as Coll[Byte]. */
  def decodeStringToColl(base16: String): Coll[Byte] = {
    val bytes = ErgoAlgos.decode(base16).fold(t => throw t, identity)
    Colls.fromArray(bytes)
  }

  /** Decodes this base16 string to byte array and parse it using [[GroupElementSerializer]]. */
  def decodeStringToGE(base16: String): GroupElement = {
    val bytes = ErgoAlgos.decode(base16).fold(t => throw t, identity)
    val pe = GroupElementSerializer.parse(SigmaSerializer.startReader(bytes))
    SigmaDsl.GroupElement(pe)
  }

  /** Decodes this base16 string to byte array and parse it using
   * [[ErgoTreeSerializer.deserializeErgoTree()]].
   */
  def decodeStringToErgoTree(base16: String): ErgoTree = {
    ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(Base16.decode(base16).get)
  }

  def createP2PKAddress(pk: ProveDlog, networkPrefix: NetworkPrefix): P2PKAddress = {
    implicit val ergoAddressEncoder: ErgoAddressEncoder = ErgoAddressEncoder(networkPrefix)
    P2PKAddress(pk)
  }

  def createP2SAddress(ergoTree: ErgoTree, networkPrefix: NetworkPrefix): Pay2SAddress = {
    implicit val ergoAddressEncoder: ErgoAddressEncoder = ErgoAddressEncoder(networkPrefix)
    Pay2SAddress(ergoTree)
  }

  def hash(s: String): String = {
    ErgoAlgos.encode(ErgoAlgos.hash(s))
  }

  def toErgoTree(sigmaBoolean: SigmaBoolean): ErgoTree = ErgoTree.fromSigmaBoolean(sigmaBoolean)

  def getStateDigest(tree: AvlTree): Array[Byte] = {
    tree.digest.toArray
  }

  def toIndexedSeq[T](xs: util.List[T]): IndexedSeq[T] = {
    import JavaConverters._
    xs.iterator().asScala.toIndexedSeq
  }

  private def anyValueToConstant(v: AnyValue): Constant[_ <: SType] = {
    val tpe = Evaluation.rtypeToSType(v.tVal)
    Constant(v.value.asInstanceOf[SType#WrappedType], tpe)
  }

  /** Converts mnemonic phrase to seed it was derived from.
    * This method should be equivalent to Mnemonic.toSeed().
    */
  def mnemonicToSeed(mnemonic: String, passOpt: Option[String] = None): Array[Byte] = {
    val normalizedMnemonic = CryptoFacade.normalizeChars(mnemonic.toCharArray)
    val normalizedPass = CryptoFacade.normalizeChars(s"mnemonic${passOpt.getOrElse("")}".toCharArray)
    CryptoFacade.generatePbkdf2Key(normalizedMnemonic, normalizedPass)
  }

  /**
   * Create an extended secret key from mnemonic
   *
   * @param seedPhrase secret seed phrase to be used in prover for generating proofs.
   * @param pass   password to protect secret seed phrase.
   * @param usePre1627KeyDerivation use incorrect(previous) BIP32 derivation, expected to be false for new 
   * wallets, and true for old pre-1627 wallets (see https://github.com/ergoplatform/ergo/issues/1627 for details)
   */
  def seedToMasterKey(seedPhrase: SecretString, pass: SecretString = null, usePre1627KeyDerivation: java.lang.Boolean): ExtendedSecretKey = {
    val passOpt = if (pass == null || pass.isEmpty()) None else Some(pass.toStringUnsecure)
    val seed = mnemonicToSeed(seedPhrase.toStringUnsecure, passOpt)
    val masterKey = ExtendedSecretKey.deriveMasterKey(seed, usePre1627KeyDerivation)
    masterKey
  }

  def createUnsignedInput(boxId: String): UnsignedInput = {
    val idBytes = decodeStringToBytes(boxId)
    createUnsignedInput(idBytes)
  }

  def createUnsignedInput(boxIdBytes: Array[Byte]): UnsignedInput = {
    new UnsignedInput(ADKey @@ boxIdBytes)
  }

  def createDataInput(boxIdBytes: Array[Byte]): DataInput = {
    DataInput(ADKey @@ boxIdBytes)
  }

  def collRType[T](tItem: RType[T]): RType[Coll[T]] = sigma.collRType(tItem)

  def BigIntRType: RType[sigma.BigInt] = sigma.BigIntRType

  def GroupElementRType: RType[sigma.GroupElement] = sigma.GroupElementRType

  def SigmaPropRType: RType[sigma.SigmaProp] = sigma.SigmaPropRType

  def AvlTreeRType: RType[sigma.AvlTree] = sigma.AvlTreeRType

  def BoxRType: RType[sigma.Box] = sigma.BoxRType

  def SigmaDsl: CSigmaDslBuilder = sigma.eval.SigmaDsl

  def collFrom(arr: Array[Byte]): Coll[Byte] = {
    Colls.fromArray(arr)
  }

  def collToByteArray(in: Coll[Byte]): Array[Byte] = {
    in.toArray
  }

  def collFrom(arr: Array[Long]): Coll[Long] = Colls.fromArray(arr)

  def collFrom(arr: Array[Int]): Coll[Int] = Colls.fromArray(arr)

  def collFrom(arr: Array[Boolean]): Coll[Boolean] = Colls.fromArray(arr)

  def collFrom(arr: Array[Short]): Coll[Short] = Colls.fromArray(arr)

  def ergoTreeTemplateBytes(ergoTree: ErgoTree): Array[Byte] = {
    val r = SigmaSerializer.startReader(ergoTree.bytes)
    ErgoTreeSerializer.DefaultSerializer.deserializeHeaderWithTreeBytes(r)._4
  }

  def createTokensMap(linkedMap: mutable.LinkedHashMap[ModifierId, Long]): TokensMap = {
    linkedMap.toMap
  }

  // TODO the method below is copied from ErgoTransaction. Both of them should be moved to ergo-wallet.

  /**
   * Extracts a mapping (assets -> total amount) from a set of boxes passed as a parameter.
   * That is, the method is checking amounts of assets in the boxes(i.e. that a box contains positive
   * amount for an asset) and then summarize and group their corresponding amounts.
   *
   * @param boxes - boxes to check and extract assets from
   * @return a mapping from asset id to to balance and total assets number
   */
  def extractAssets(boxes: IndexedSeq[ErgoBoxCandidate]): (Map[Digest32Coll, Long], Int) = {
    import sigma.Extensions.CollOps
    val map = mutable.Map[Digest32Coll, Long]()
    val assetsNum = boxes.foldLeft(0) { case (acc, box) =>
      require(box.additionalTokens.length <= SigmaConstants.MaxTokens.value, "too many assets in one box")
      box.additionalTokens.foreach { case (assetId, amount) =>
        val total = map.getOrElse(assetId, 0L)
        map.put(assetId, java7.compat.Math.addExact(total, amount))
      }
      acc + box.additionalTokens.size
    }
    map.toMap -> assetsNum
  }

  /** Creates a new EIP-3 derivation path with the given last index.
    * The resulting path corresponds to `m/44'/429'/0'/0/index` path.
    */
  def eip3DerivationWithLastIndex(index: Int) = {
    val firstPath = Constants.eip3DerivationPath
    DerivationPath(firstPath.decodedPath.dropRight(1) :+ index, firstPath.publicBranch)
  }

  /** Creates a new EIP-3 derivation parent path.
    * The resulting path is the `m/44'/429'/0'/0` path.
    */
  def eip3DerivationParent() = {
    val firstPath = Constants.eip3DerivationPath
    DerivationPath(firstPath.decodedPath.dropRight(1), firstPath.publicBranch)
  }

  /** Type synonym for a collection of tokens represented using Coll */
  type TokenColl = Coll[Token]

  /** Ensures that all tokens have strictly positive value.
    * @throws IllegalArgumentException when any token have value <= 0
    * @return the original `tokens` collection (passes the argument through to the result)
    */
  def checkAllTokensPositive(tokens: TokenColl): TokenColl = {
    val invalidTokens = tokens.filter(_._2 <= 0)
    require(invalidTokens.isEmpty, s"All token values should be > 0: ")
    tokens
  }

  /** Compute the difference between `reducedTokens` collection and `subtractedTokens` collection.
    * It can be thought as `reducedTokens - subtractedTokens` operation.
    *
    * Each collection can have many `(tokenId, amount)` pairs with the same `tokenId`.
    * The method works by first combining all the pairs with the same tokenId and then
    * computing the difference.
    * The resulting collection contain a single pair for each tokenId and those token ids
    * form a subset of tokens from the argument collections.
    *
    * One concrete use case to think of is `subtractTokenColls(outputTokens, inputTokens)`.
    * In this case the resulting collection of (tokenId, amount) pairs can be interpreted as:
    * - when `amount < 0` then it is to be burnt
    * - when `amount > 0` then it is to be minted
    *
    * @param reducedTokens    the tokens to be subracted from
    * @param subtractedTokens the tokens which amounts will be subtracted from the
    *                         corresponding tokens from `reducedTokens`
    * @return the differences between token amounts (matched by token ids)
    */
  def subtractTokenColls(
    reducedTokens: TokenColl,
    subtractedTokens: TokenColl
  ): TokenColl = {
    val exactNumeric: Numeric[Long] = new Utils.IntegralFromExactIntegral(LongIsExactIntegral)
    val b = reducedTokens.builder // any Coll has builder, which is suitable
    val reduced = checkAllTokensPositive(reducedTokens).sumByKey(exactNumeric) // summation with overflow checking
    val subtracted = checkAllTokensPositive(subtractedTokens).sumByKey(exactNumeric) // summation with overflow checking
    val tokensDiff = b.outerJoin(subtracted, reduced)(
      { case (_, sV) => -sV }, // for each token missing in reduced: amount to burn
      { case (_, rV) => rV }, // for each token missing in subtracted: amount to mint
      { case (_, (sV, rV)) => rV - sV } // for tokens both in subtracted and reduced: balance change
    )
    tokensDiff.filter(_._2 != 0)  // return only unbalanced tokens
  }

  /** Compute the difference between `reducedTokens` collection and `subtractedTokens`
    * collection.
    * @see subtractTokenColls for details
    */
  def subtractTokens(
    reducedTokens: IndexedSeq[Token],
    subtractedTokens: IndexedSeq[Token]
  ): TokenColl = {
    subtractTokenColls(
      reducedTokens = Colls.fromItems(reducedTokens:_*),
      subtractedTokens = Colls.fromItems(subtractedTokens:_*)
    )
  }

  def createDiffieHellmanTupleProverInput(
      g: GroupElement,
      h: GroupElement,
      u: GroupElement,
      v: GroupElement,
      x: BigInteger): DiffieHellmanTupleProverInput = {
    createDiffieHellmanTupleProverInput(
      g = sdk.JavaHelpers.SigmaDsl.toECPoint(g),
      h = sdk.JavaHelpers.SigmaDsl.toECPoint(h),
      u = sdk.JavaHelpers.SigmaDsl.toECPoint(u),
      v = sdk.JavaHelpers.SigmaDsl.toECPoint(v),
      x
    )
  }

  def createDiffieHellmanTupleProverInput(
      g: EcPointType,
      h: EcPointType,
      u: EcPointType,
      v: EcPointType,
      x: BigInteger): DiffieHellmanTupleProverInput = {
    val dht = ProveDHTuple(g, h, u, v)
    DiffieHellmanTupleProverInput(x, dht)
  }
}



