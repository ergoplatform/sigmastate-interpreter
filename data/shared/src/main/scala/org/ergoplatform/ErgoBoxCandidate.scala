package org.ergoplatform

import debox.cfor
import org.ergoplatform.ErgoBox._
import scorex.util.encode.Base16
import scorex.util.{ModifierId, bytesToId}
import sigma.Extensions.{ArrayOps, CollOps}
import sigma.ast.{ErgoTree, SType}
import sigma.ast.SType.AnyOps
import sigma.data.Digest32Coll
import sigma.util.safeNewArray
import sigma.{Coll, Colls}
import sigma.ast._
import sigma.ast.syntax._
import sigma.serialization.ErgoTreeSerializer.DefaultSerializer
import sigma.serialization.{SigmaByteReader, SigmaByteWriter, SigmaSerializer}

import scala.collection.{immutable, mutable}
import scala.runtime.ScalaRunTime

/**
  * Contains the same fields as `org.ergoplatform.ErgoBox`, except if transaction id and index,
  * that will be calculated after full transaction formation.
  *
  * @see org.ergoplatform.ErgoBox for more details
  *
  * @param value               - amount of money associated with the box
  * @param ergoTree            - guarding script, which should be evaluated to true in order to open this box
  * @param creationHeight      - height when a transaction containing the box was created.
  *                            This height is declared by user and should not exceed height of the block,
  *                            containing the transaction with this box.
  * @param additionalTokens    - secondary tokens the box contains
  * @param additionalRegisters - additional registers the box can carry over
  */
class ErgoBoxCandidate(val value: Long,
                       val ergoTree: ErgoTree,
                       val creationHeight: Int,
                       val additionalTokens: Coll[Token] = Colls.emptyColl,
                       val additionalRegisters: AdditionalRegisters = Map())
                       extends ErgoBoxAssets {

  /** Transforms this tree to a proposition, substituting the constants if the constant
    * segregation flag is set.
    * @see [[SigmaPropValue]]
    */
  def proposition: SigmaPropValue = ergoTree.toProposition(ergoTree.isConstantSegregation)

  /** Returns the serialized bytes of the guarding [[ErgoTree]]. */
  lazy val propositionBytes: Array[Byte] = ergoTree.bytes

  /** Serialized bytes of this Box without transaction reference data (transactionId and boxIndex). */
  // TODO optimize: re-implement extracting directly from `ErgoBox.bytes` array
  lazy val bytesWithNoRef: Array[Byte] = ErgoBoxCandidate.serializer.toBytes(this)

  /** Creates a new [[ErgoBox]] based on this candidate using the given transaction reference data.
    *
    * @param txId id of transaction which created the box
    * @param boxIndex  index of the box in the transaction's OUTPUTS
    */
  def toBox(txId: ModifierId, boxIndex: Short) =
    new ErgoBox(value, ergoTree, additionalTokens, additionalRegisters, txId, boxIndex, creationHeight)

  /** Extracts register by id.
    *
    * @param identifier id of the register to return.
    * @return Some(value) if the register is present, None otherwise
    */
  def get(identifier: RegisterId): Option[Value[SType]] = {
    identifier match {
      case ValueRegId => Some(LongConstant(value))
      case ScriptRegId => Some(ByteArrayConstant(propositionBytes))
      case TokensRegId =>
        // TODO optimize using mapFirst
        //  However this requires fixing Coll equality (see property("ErgoBox test vectors"))
        Some(Constant(additionalTokens.map(identity).asWrappedType, STokensRegType))
      case ReferenceRegId =>
        val tupleVal = (creationHeight, ErgoBoxCandidate.UndefinedBoxRef)
        Some(Constant(tupleVal.asWrappedType, SReferenceRegType))
      case n: NonMandatoryRegisterId =>
        additionalRegisters.get(n)
    }
  }

  override def equals(arg: Any): Boolean = {
    arg match {
      case x: ErgoBoxCandidate => java.util.Arrays.equals(bytesWithNoRef, x.bytesWithNoRef)
      case _ => false
    }
  }

  // TODO refactor: fix hashCode, it should be consistent with [[equals]] and use [[bytesWithNoRef]]
  //  Since all five properties are also serialized as part of bytesWithNoRef, then this
  //  hashCode is safe, but less efficient and simple than it can be.
  override def hashCode(): Int = {
    ScalaRunTime._hashCode((value, ergoTree, additionalTokens, additionalRegisters, creationHeight))
  }

  override def toString: String = s"ErgoBoxCandidate($value, $ergoTree," +
    s"tokens: (${additionalTokens.map(t => Base16.encode(t._1.toArray) + ":" + t._2).toArray.mkString(", ")}), " +
    s"$additionalRegisters, creationHeight: $creationHeight)"

  /** Additional tokens stored in the box, merged into a Map.
    * This method is not used in ErgoTree and serialization, not part of consensus.
    */
  lazy val tokens: Map[ModifierId, Long] = {
    val merged = new mutable.HashMap[ModifierId, Long]
    additionalTokens.foreach { case (id, amount) =>
      val mId = bytesToId(id.toArray)
      merged.put(mId, java7.compat.Math.addExact(merged.getOrElse(mId, 0L), amount))
    }
    merged.toMap
  }
}

object ErgoBoxCandidate {
  /** Default value of encoded (txId, boxIndex) pair encoded as Coll[Byte]
    * and returned as part of Reference Register (R3) of ErgoBoxCandidate.
    *
    * In contrast [[ErgoBox]] extends [[ErgoBoxCandidate]] by adding `transactionId` and
    * `index` properties which are returned as part of R3.
    */
  val UndefinedBoxRef: Coll[Byte] = Array.fill(34)(0: Byte).toColl

  /** HOTSPOT: don't beautify the code */
  object serializer extends SigmaSerializer[ErgoBoxCandidate, ErgoBoxCandidate] {

    /** Helper method for [[ErgoBoxCandidate]] serialization.
      * If `tokensInTx` is defined, then token ids are not serialized, instead they are
      * looked up in this collection and the corresponding index is serialized.
      * This saves at least (32 - 4) bytes for each token. The ids themselves are
      * serialized elsewhere, e.g. as part of transaction.
      *
      * @param box        candidate box to be serialized
      * @param tokensInTx optional collection of token ids that should be replaced by indexes.
      * @param w          writer of serialized bytes
      */
    def serializeBodyWithIndexedDigests(box: ErgoBoxCandidate,
                                        tokensInTx: Option[Coll[Digest32Coll]],
                                        w: SigmaByteWriter): Unit = {
      w.putULong(box.value)
      w.putBytes(DefaultSerializer.serializeErgoTree(box.ergoTree))
      w.putUInt(box.creationHeight)
      w.putUByte(box.additionalTokens.size)

      val unzipped = Colls.unzip(box.additionalTokens)
      val ids = unzipped._1
      val amounts = unzipped._2
      val limit = ids.length
      cfor(0)(_ < limit, _ + 1) { i =>
        val id = ids(i)
        val amount = amounts(i)
        if (tokensInTx.isDefined) {
          val tokenIndex = tokensInTx.get.indexWhere(_ == id, 0) // using equality on Coll
          if (tokenIndex == -1) sys.error(s"failed to find token id ($id) in tx's digest index")
          w.putUInt(tokenIndex)
        } else {
          w.putBytes(id.toArray)
        }
        w.putULong(amount)
      }

      val nRegs = box.additionalRegisters.keys.size
      if (nRegs + ErgoBox.startingNonMandatoryIndex > 255)
        sys.error(s"The number of non-mandatory indexes $nRegs exceeds ${255 - ErgoBox.startingNonMandatoryIndex} limit.")
      w.putUByte(nRegs)
      // we assume non-mandatory indexes are densely packed from startingNonMandatoryIndex
      // this convention allows to save 1 bite for each register
      val startReg = ErgoBox.startingNonMandatoryIndex
      val endReg = ErgoBox.startingNonMandatoryIndex + nRegs - 1
      cfor(startReg: Int)(_ <= endReg, _ + 1) { regId =>
        val reg = ErgoBox.findRegisterByIndex(regId.toByte).get
        box.get(reg) match {
          case Some(v) =>
            w.putValue(v)
          case None =>
            sys.error(s"Set of non-mandatory indexes is not densely packed: " +
              s"register R$regId is missing in the range [$startReg .. $endReg]")
        }
      }
    }

    override def serialize(obj: ErgoBoxCandidate, w: SigmaByteWriter): Unit = {
      serializeBodyWithIndexedDigests(obj, None, w)
    }

    /** Helper method to parse [[ErgoBoxCandidate]] previously serialized by
      * [[serializeBodyWithIndexedDigests()]].
      */
    def parseBodyWithIndexedDigests(digestsInTx: Array[TokenId], r: SigmaByteReader): ErgoBoxCandidate = {
      val previousPositionLimit = r.positionLimit
      r.positionLimit = r.position + ErgoBox.MaxBoxSize
      val value = r.getULong()                  // READ
      val tree = DefaultSerializer.deserializeErgoTree(r, SigmaSerializer.MaxPropositionSize)  // READ
      val creationHeight = r.getUIntExact       // READ
      // NO-FORK: ^ in v5.x getUIntExact may throw Int overflow exception
      // in v4.x r.getUInt().toInt is used and may return negative Int instead of the overflow
      // and ErgoBoxCandidate with negative creation height is created, which is then invalidated
      // during transaction validation. See validation rule # 122 in the Ergo node (ValidationRules.scala)
      val nTokens = r.getUByte()                // READ
      val tokenIds = safeNewArray[TokenId](nTokens)
      val tokenAmounts = safeNewArray[Long](nTokens)
      if (digestsInTx != null) {
        val nDigests = digestsInTx.length
        cfor(0)(_ < nTokens, _ + 1) { i =>
          val digestIndex = r.getUIntExact    // READ
          // NO-FORK: in v5.x getUIntExact throws Int overflow exception
          // in v4.x r.getUInt().toInt is used and may return negative Int in which case
          // the error below is thrown
          if (digestIndex < 0 || digestIndex >= nDigests)
            sys.error(s"failed to find token id with index $digestIndex")
          val amount = r.getULong()           // READ
          tokenIds(i) = digestsInTx(digestIndex)
          tokenAmounts(i) = amount
        }
      } else {
        val tokenIdSize = TokenId.size  // optimization: access the value once
        cfor(0)(_ < nTokens, _ + 1) { i =>
          tokenIds(i) = Digest32Coll @@ Colls.fromArray(r.getBytes(tokenIdSize)) // READ
          tokenAmounts(i) = r.getULong()         // READ
        }
      }
      val tokens = Colls.pairCollFromArrays(tokenIds, tokenAmounts)

      // TODO optimize: hotspot: replace Map with much faster Coll
      val nRegs = r.getUByte()              // READ
      val b = immutable.Map.newBuilder[NonMandatoryRegisterId, EvaluatedValue[_ <: SType]]
      b.sizeHint(nRegs)
      cfor(0)(_ < nRegs, _ + 1) { iReg =>
        val reg = ErgoBox.nonMandatoryRegisters(iReg)
        val v = r.getValue().asInstanceOf[EvaluatedValue[SType]]  // READ
        b += ((reg, v))  // don't use `->` since it incur additional wrapper overhead
      }
      r.positionLimit = previousPositionLimit
      new ErgoBoxCandidate(
        value, tree, creationHeight,
        tokens, b.result())
    }

    override def parse(r: SigmaByteReader): ErgoBoxCandidate = {
      parseBodyWithIndexedDigests(digestsInTx = null, r)
    }
  }

}
