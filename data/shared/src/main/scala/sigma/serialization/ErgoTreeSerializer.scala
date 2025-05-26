package sigma.serialization

import org.ergoplatform.validation.ValidationRules.{CheckDeserializedScriptIsSigmaProp, CheckHeaderSizeBit}
import sigma.ast.{Constant, DeserializationSigmaBuilder, ErgoTree, SType, UnparsedErgoTree}
import sigma.ast.syntax.ValueOps
import sigma.ast.ErgoTree.{EmptyConstants, HeaderType}
import sigma.util.safeNewArray
import debox.cfor
import sigma.VersionContext
import sigma.validation.ValidationException
import sigma.validation.ValidationRules.CheckPositionLimit

/**
  * Rationale for soft-forkable ErgoTree serialization.
  * There are 2 points:
  *
  * 1) we can make size bit obligatory, i.e. always save total size of script body
  * (in this case we don't need size bit in the header). This will allow to
  * always skip right number of bytes in case of any exception (including
  * ValidationException) thrown during deserialization and produce
  * UnparsedErgoTree. The decision about soft-fork can be done later.
  * But is looks like this is not necessary if we do as described below.
  *
  * 2) HeaderVersionCheck:
  * we can also strictly check during deserialization the content of the script
  * against version number in the header. Thus if the header have vS, then
  * script is allowed to have instructions from versions from v1 to vS. On a node vN, N > S,
  * this should also be enforced, i.e. vN node will reject scripts as invalid
  * if the script has vS in header and vS+1 instruction in body.
  *
  * Keeping this in mind, if we have a vN node and a script with vS in its header then:
  * During script deserialization:
  * 1) if vN >= vS then
  *   the node knows all the instructions and should check that only instructions
  *   up to vS are used in the script.
  *   It either parses successfully or throws MalformedScriptException.
  *   If during the process some unknown instruction is encountered (i.e. ValidationException is thrown),
  *   this cannot be a soft-fork, because vN >= vS guarantees that all instructions are known,
  *   thus the script is malformed.
  *
  * 2) if vN < vS then
  *   the vN node is expecting unknown instructions.
  *   If the script is parsed successfully, then
  *     vN subset of the language is used and script is accepted for execution
  *   else if ValidationException is thrown then
  *     UnparsedErgoTree is created, delaying decision about soft-fork until stateful validation.
  *     if bodySize is stored then
  *       script body is skipped and whole TX deserialization continues.
  *     otherwise
  *       we cannot skip the body which leads to whole TX to be rejected (CannotSkipScriptException)
  *   else if some other exception is thrown then
  *     the whole TX is rejected due to said exception.
  *
  * In the stateful context:
  *   if vN >= vS then
  *     we can execute script, but we do additional check
  *     if vS > the current version of protocol (vP) then
  *       the script is rejected as invalid because its version exceeds
  *       the current consensus version of the protocol
  *     else
  *       the script can be executed
  *   if vN < vS then
  *     if we have Right(tree)
  *       the script is executed
  *     if Left(UnparsedErgoTree()) then check soft fork and either execute or throw
  *
  * Proposition:
  *   CannotSkipScriptException can only happen on < 10% of the nodes, which is safe for consensus.
  * Proof.
  *   If follows from the fact that vN >= vS nodes will reject the script
  *   until new vP is upgraded to vS, which means the majority has upgraded to at least vS
  *   Thus, before vP is upgraded to vS, majority reject (either because they cannot parse, or because vP is not actualized)
  *   after that majority accept (however old nodes still reject but they are < 10%)
  * End of proof.
  *
  */
class ErgoTreeSerializer {

  /** Serialize header and constants section only.*/
  private def serializeHeader(ergoTree: ErgoTree, w: SigmaByteWriter): Unit = {
    w.put(ergoTree.header)
    if (ergoTree.isConstantSegregation) {
      val constantSerializer = ConstantSerializer(DeserializationSigmaBuilder)
      val constants = ergoTree.constants
      val nConstants = constants.length
      w.putUInt(nConstants)
      cfor(0)(_ < nConstants, _ + 1) { i =>
        val c = constants(i)
        constantSerializer.serialize(c, w)
      }
    }
  }

  private def serializeWithoutSize(ergoTree: ErgoTree): Array[Byte] = {
    val w = SigmaSerializer.startWriter()
    serializeHeader(ergoTree, w)
    assert(ergoTree.isRightParsed, s"Right parsed ErgoTree expected: $ergoTree")
    ValueSerializer.serialize(ergoTree.root.right.get, w)
    w.toBytes
  }

  /** Default serialization of ErgoTree.
    * Doesn't apply any transformations and guarantee to preserve original
    * structure after deserialization. */
  def serializeErgoTree(ergoTree: ErgoTree): Array[Byte] = {
    val res = ergoTree.root match {
      case Left(UnparsedErgoTree(bytes, _)) => bytes.array.asInstanceOf[Array[Byte]]
      case _ =>
        val bytes = serializeWithoutSize(ergoTree)
        if (ergoTree.hasSize) {
          val w = SigmaSerializer.startWriter()
          val header = bytes(0)
          val contentLength = bytes.length - 1
          w.put(header)
          w.putUInt(contentLength)
          w.putBytes(bytes, 1, contentLength)
          w.toBytes
        }
        else bytes
    }
    res
  }

  /** Default deserialization of ErgoTree (should be inverse to `serializeErgoTree`).
    * Doesn't apply any transformations to the parsed tree. */
  def deserializeErgoTree(bytes: Array[Byte]): ErgoTree  = {
    val r = SigmaSerializer.startReader(bytes)
    deserializeErgoTree(r, SigmaSerializer.MaxPropositionSize)
  }

  def deserializeErgoTree(r: SigmaByteReader, maxTreeSizeBytes: Int): ErgoTree = {
    deserializeErgoTree(r, maxTreeSizeBytes, true)
  }

  private[sigma] def deserializeErgoTree(r: SigmaByteReader, maxTreeSizeBytes: Int, checkType: Boolean): ErgoTree = {
    val startPos = r.position
    val previousPositionLimit = r.positionLimit
    r.positionLimit = r.position + maxTreeSizeBytes
    val (h, sizeOpt) = deserializeHeaderAndSize(r)
    val bodyPos = r.position
    val tree = try {
      try { // nested try-catch to intercept size limit exceptions and rethrow them as ValidationExceptions
        val cs = deserializeConstants(h, r)
        val previousConstantStore = r.constantStore
        // reader with constant store attached is required (to get tpe for a constant placeholder)
        r.constantStore = new ConstantStore(cs)

        val wasDeserialize_saved = r.wasDeserialize
        r.wasDeserialize = false

        val wasUsingBlockchainContext_saved = r.wasUsingBlockchainContext
        r.wasUsingBlockchainContext = false

        val root = ValueSerializer.deserialize(r)
        val hasDeserialize = r.wasDeserialize  // == true if there was deserialization node
        r.wasDeserialize = wasDeserialize_saved

        val isUsingBlockchainContext = r.wasUsingBlockchainContext // == true if there was a node using the blockchain context
        r.wasUsingBlockchainContext = wasUsingBlockchainContext_saved

        if (checkType) {
          CheckDeserializedScriptIsSigmaProp(root)
        }

        r.constantStore = previousConstantStore
        // now we know the end position of propositionBytes, read them all at once into array
        val treeSize = r.position - startPos
        r.position = startPos
        val propositionBytes = r.getBytes(treeSize)

        new ErgoTree(
          h, cs, Right(root.asSigmaProp),
          propositionBytes, Some(hasDeserialize), Some(isUsingBlockchainContext))
      }
      catch {
        case e: ReaderPositionLimitExceeded =>
          CheckPositionLimit.throwValidationException(e)
      }
    }
    catch {
      case ve: ValidationException =>
        sizeOpt match {
          case Some(treeSize) =>
            val numBytes = bodyPos - startPos + treeSize
            r.position = startPos
            val bytes = r.getBytes(numBytes)
            new ErgoTree(ErgoTree.DefaultHeader, EmptyConstants, Left(UnparsedErgoTree(bytes, ve)), bytes, None, None)
          case None =>
            throw new SerializerException(
              s"Cannot handle ValidationException, ErgoTree serialized without size bit.", Some(ve))
        }
    }
    finally {
      r.positionLimit = previousPositionLimit
    }
    tree
  }

  /** Deserialize `header` and optional `size` slots only. */
  private def deserializeHeaderAndSize(r: SigmaByteReader): (HeaderType, Option[Int]) = {
    val header = HeaderType @@ r.getByte()
    CheckHeaderSizeBit(header)
    val sizeOpt = if (ErgoTree.hasSize(header)) {
      val size = r.getUInt().toInt
      // Note, when size < 0 as a result of Int overflow nothing happens here and later
      // when deserialization proceeds normally as sizeOpt is not used on this pass.
      // However, when ValidationException is thrown in deserializeErgoTree this negative
      // tree size value will be used in
      // val val numBytes = bodyPos - startPos + treeSize
      //            r.position = startPos
      //            val bytes = r.getBytes(numBytes) = bodyPos - startPos + treeSize
      // val bytes = r.getBytes(numBytes)
      // If numBytes < 0 then it throws on getBytes and the whole deserialization fails
      // On the other hand if numBytes >= 0 then UnparsedErgoTree will be created.
      // The Reader however will be in some unpredictable state, as not all ErgoTree bytes
      // are consumed.
      Some(size)
    } else
      None
    (header, sizeOpt)
  }

  private val constantSerializer = ConstantSerializer(DeserializationSigmaBuilder)

  /** Deserialize constants section only.
    * HOTSPOT: don't beautify this code
    */
  private def deserializeConstants(header: HeaderType, r: SigmaByteReader): IndexedSeq[Constant[SType]] = {
    val constants: IndexedSeq[Constant[SType]] =
      if (ErgoTree.isConstantSegregation(header)) {
        val nConsts = r.getUInt().toInt
        // Note, when nConsts < 0 as a result of Int overflow, the empty seq is returned
        // deserialization will succeed

        if (nConsts > 0) {
          // HOTSPOT:: allocate new array only if it is not empty
          val res = safeNewArray[Constant[SType]](nConsts)
          cfor(0)(_ < nConsts, _ + 1) { i =>
            res(i) = constantSerializer.deserialize(r)
          }
          res
        }
        else
          Constant.EmptySeq
      }
      else
        Constant.EmptySeq
    constants
  }

  /** Deserialize header and constant sections, but output the rest of the bytes as separate array. */
  def deserializeHeaderWithTreeBytes(r: SigmaByteReader): (HeaderType, Option[Int], IndexedSeq[Constant[SType]], Array[Byte]) = {
    val (header, sizeOpt) = deserializeHeaderAndSize(r)
    val constants = deserializeConstants(header, r)
    val treeBytes = r.getBytes(r.remaining)
    (header, sizeOpt, constants, treeBytes)
  }

  /** Computes back references from constants to positions.
    * This method helps to implement substituteConstants efficiently
    * (i.e. O(n + m) time, where n - number of positions and m - number of constants)
    *
    * @param positions indexes in the range [0..positionsRange)
    * @param positionsRange upper bound on values in `positions`
    * @return array `r` of back references, i.e. indices in `positions` such that
    *         positions(r(i)) == i whenever r(i) != -1. When r(i) == -1 then backreference
    *         is not defined (which means the constant with the index `i` is not substituted.
    */
  private[sigma] def getPositionsBackref(positions: Array[Int], positionsRange: Int): Array[Int] = {
    // allocate array of back references: forall i: positionsBackref(i) is index in `positions`
    val positionsBackref = safeNewArray[Int](positionsRange)
    // mark all positions are not assigned
    java.util.Arrays.fill(positionsBackref, -1)

    cfor(0)(_ < positions.length, _ + 1) { iPos =>
      val pos = positions(iPos)
      if (0 <= pos && pos < positionsBackref.length && positionsBackref(pos) == -1) {
        // back reference is not yet assigned, assign in now
        positionsBackref(pos) = iPos
      }
    }
    positionsBackref
  }

  /** Transforms serialized bytes of ErgoTree with segregated constants by
    * replacing constants at given positions with new values. This operation
    * allow to use serialized scripts as pre-defined templates.
    * See [[SubstConstants]] for details.
    *
    * @param scriptBytes serialized ErgoTree with ConstantSegregationFlag set to 1.
    * @param positions   zero based indexes in ErgoTree.constants array which
    *                    should be replaced with new values
    * @param newVals     new values to be injected into the corresponding
    *                    positions in ErgoTree.constants array
    * @return a pair (newBytes, len), where:
    *         newBytes - the original array scriptBytes such that only specified constants
    *                    are replaced and all other bytes remain exactly the same
    *         len      - length of the `constants` array of the given ErgoTree bytes
    */
  def substituteConstants(scriptBytes: Array[Byte],
                          positions: Array[Int],
                          newVals: Array[Constant[SType]]): (Array[Byte], Int) = {
    require(positions.length == newVals.length,
      s"expected positions and newVals to have the same length, got: positions: ${positions.toSeq},\n newVals: ${newVals.toSeq}")
    val r = SigmaSerializer.startReader(scriptBytes)
    val (header, _, constants, treeBytes) = deserializeHeaderWithTreeBytes(r)
    val w = SigmaSerializer.startWriter()
    w.put(header)

    if (VersionContext.current.isJitActivated) {
      // The following `constants.length` should not be serialized when segregation is off
      // in the `header`, because in this case there is no `constants` section in the
      // ErgoTree serialization format. Thus, applying this `substituteConstants` for
      // non-segregated trees will return non-parsable ErgoTree bytes (when
      // `constants.length` is put in `w`).
      if (ErgoTree.isConstantSegregation(header)) {
        w.putUInt(constants.length)
      }

      // The following is optimized O(nConstants + position.length) implementation
      val nConstants = constants.length
      if (nConstants > 0) {
        val backrefs = getPositionsBackref(positions, nConstants)
        cfor(0)(_ < nConstants, _ + 1) { i =>
          val c = constants(i)
          val iPos = backrefs(i) // index to `positions`
          if (iPos == -1) {
            // no position => no substitution, serialize original constant
            constantSerializer.serialize(c, w)
          } else {
            assert(positions(iPos) == i) // INV: backrefs and positions are mutually inverse
            val newConst = newVals(iPos)
            require(c.tpe == newConst.tpe,
              s"expected new constant to have the same ${c.tpe} tpe, got ${newConst.tpe}")
            constantSerializer.serialize(newConst, w)
          }
        }
      }
    } else {
      // for v4.x compatibility we save constants.length here (see the above comment to
      // understand the consequences)
      w.putUInt(constants.length)

      // the following is v4.x O(nConstants * positions.length) inefficient implementation
      constants.zipWithIndex.foreach {
        case (c, i) if positions.contains(i) =>
          val newVal = newVals(positions.indexOf(i))
          // we need to get newVal's serialized constant value (see ProveDlogSerializer for example)
          val constantStore = new ConstantStore()
          val valW = SigmaSerializer.startWriter(constantStore)
          valW.putValue(newVal)
          val newConsts = constantStore.getAll
          require(newConsts.length == 1)
          val newConst = newConsts.head
          require(c.tpe == newConst.tpe, s"expected new constant to have the same ${c.tpe} tpe, got ${newConst.tpe}")
          constantSerializer.serialize(newConst, w)
        case (c, _) =>
          constantSerializer.serialize(c, w)
      }
    }

    w.putBytes(treeBytes)
    (w.toBytes, constants.length)
  }

}

object ErgoTreeSerializer {
  val DefaultSerializer = new ErgoTreeSerializer
}
