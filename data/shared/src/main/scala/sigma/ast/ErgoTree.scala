package sigma.ast

import scorex.util.encode.Base16
import sigma.VersionContext
import sigma.ast.ErgoTree.{HeaderType, substConstants}
import sigma.ast.syntax._
import sigma.data.{CSigmaProp, SigmaBoolean}
import sigma.kiama.rewriting.Rewriter.{everywherebu, strategy}
import sigma.validation.ValidationException
import sigma.ast.syntax.ValueOps
import sigma.eval.Extensions.SigmaBooleanOps
import sigma.serialization.ErgoTreeSerializer.DefaultSerializer
import sigma.serialization.{ConstantStore, ErgoTreeSerializer, SigmaSerializer, ValueSerializer}
import supertagged.TaggedType

import scala.collection.mutable

/** This is alternative representation of ErgoTree expression when it cannot be parsed
  * due to `error`. This is used by the nodes running old versions of code to recognize
  * soft-fork conditions and skip validation of box propositions which are unparsable. */
case class UnparsedErgoTree(bytes: mutable.WrappedArray[Byte], error: ValidationException)

/** The root of ErgoScript IR. Serialized instances of this class are self sufficient and can be passed around.
  * ErgoTreeSerializer defines top-level serialization format of the scripts.
  * The interpretation of the byte array depend on the first `header` byte, which uses VLQ encoding up to 30 bits.
  * Currently we define meaning for only first byte, which may be extended in future versions.
  *   7  6  5  4  3  2  1  0
  * -------------------------
  * |  |  |  |  |  |  |  |  |
  * -------------------------
  * Bit 7 == 1 if the header contains more than 1 byte (default == 0)
  * Bit 6 - reserved for GZIP compression (should be 0)
  * Bit 5 == 1 - reserved (should be = 0)
  * Bit 4 == 1 if constant segregation is used for this ErgoTree (default = 0)
  * (see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/264)
  * Bit 3 == 1 if size of the whole tree is serialized after the header byte (default = 0)
  * Bits 2-0 - language version (current version == 0)
  *
  * Currently we don't specify interpretation for the second and other bytes of the header.
  * We reserve the possibility to extend header by using Bit 7 == 1 and chain additional bytes as in VLQ.
  * Once the new bytes are required, a new version of the language should be created and implemented.
  * That new language will give an interpretation for the new bytes.
  *
  * Consistency between fields is ensured by private constructor and factory methods in `ErgoTree` object.
  * For performance reasons, ErgoTreeSerializer can be configured to perform additional constant segregation.
  * In such a case after deserialization there may be more constants segregated. This is done for example to
  * support caching optimization described in #264 mentioned above.
  *
  * The default behavior of ErgoTreeSerializer is to preserve original structure of ErgoTree and check
  * consistency. In case of any inconsistency the serializer throws exception.
  *
  * Note: prior to issue #694, this was a `case class` with eager `root`. It is now a plain class so the
  * `root` field can be backed by a `lazy val`, allowing the deserializer to defer parsing of the
  * tree body until the root is actually accessed. See `docs/avoid-ergotree-duplicates.md`.
  *
  * @param header           the first byte of serialized byte array which determines interpretation of the rest of the array
  * @param constants        If isConstantSegregation == true contains the constants for which there may be
  *                         ConstantPlaceholders in the tree.
  *                         If isConstantSegregation == false this array should be empty and any placeholder in
  *                         the tree will lead to exception.
  * @param rootInit         Thunk producing `Either[UnparsedErgoTree, SigmaPropValue]`. For trees constructed
  *                         eagerly this is just `() => providedRoot`; for trees deserialized with the
  *                         size bit set, the thunk parses the body bytes on first access.
  *                         On the right side it has a valid expression of `SigmaProp` type. Alternatively,
  *                         on the left side, it has unparsed bytes along with the ValidationException
  *                         which caused the deserializer to fail.
  * @param propositionBytes original bytes of this tree from which it has been deserialized.
  *                         If null then the bytes are not provided, and will be lazily generated when `bytes`
  *                         method is called.
  *                         These bytes are obtained in two ways:
  *                         1) in the ErgoTreeSerializer from Reader
  *                         2) in the alternative constructor using ErgoTreeSerializer.serializeErgoTree
  * @param givenDeserialize optional flag, which contains information about presence of
  *                         deserialization operations in the tree. If it is None, the information is not
  *                         available. If Some(true) then there are deserialization operations, otherwise
  *                         the tree doesn't contain deserialization and is eligible
  *                         for optimized execution.
  *                         ErgoTreeSerializer parsing method computes the value of
  *                         this flag and provides it to the constructor.
  * @param givenIsUsingBlockchainContext optional flag which indicates that blockchain context related operations
  *                                      are used in the tree
  */
class ErgoTree private[sigma](
    val header: HeaderType,
    val constants: IndexedSeq[Constant[SType]],
    rootInit: () => Either[UnparsedErgoTree, SigmaPropValue],
    private val propositionBytes: Array[Byte],
    private val givenDeserialize: Option[Boolean],
    private val givenIsUsingBlockchainContext: Option[Boolean]
) extends Serializable {

  /** Eager 6-arg constructor preserving the previous case-class signature. */
  def this(
      header: HeaderType,
      constants: IndexedSeq[Constant[SType]],
      root: Either[UnparsedErgoTree, SigmaPropValue],
      propositionBytes: Array[Byte],
      givenDeserialize: Option[Boolean],
      givenIsUsingBlockchainContext: Option[Boolean]) =
    this(header, constants, () => root, propositionBytes, givenDeserialize, givenIsUsingBlockchainContext)

  /** Eager 3-arg constructor preserving the previous secondary constructor. */
  def this(
      header: HeaderType,
      constants: IndexedSeq[Constant[SType]],
      root: Either[UnparsedErgoTree, SigmaPropValue]) =
    this(
      header, constants, root,
      propositionBytes = DefaultSerializer.serializeErgoTree(
        new ErgoTree(header, constants, () => root, null, None, None)
      ),
      givenDeserialize = None,
      givenIsUsingBlockchainContext = None
    )

  // Manual lazy backing for `root` so the init thunk (and any state it captured, most
  // notably the body-bytes slice for the lazy deserialize path) can be released after
  // the first force. Scala's auto `lazy val` would retain `rootInit` as a synthetic
  // field forever, defeating part of the memory win for trees that are eventually
  // evaluated. `Either[L, R]` is never `null`, so a null `_rootValue` is a safe
  // "not yet computed" sentinel. Double-checked locking on @volatile fields is the
  // standard Java/Scala recipe for one-shot lazy initialization across threads.
  @volatile private[this] var _rootValue: Either[UnparsedErgoTree, SigmaPropValue] = _
  @volatile private[this] var _rootInit: () => Either[UnparsedErgoTree, SigmaPropValue] = rootInit

  /** Root value of this tree.
    *
    * For eagerly constructed trees this returns the value supplied to the constructor.
    * For trees built by the deserializer from a size-bit ErgoTree, the first access
    * triggers parsing of the captured body bytes; subsequent accesses return the
    * cached result and the underlying parse closure is released for GC.
    */
  def root: Either[UnparsedErgoTree, SigmaPropValue] = {
    val cached = _rootValue
    if (cached != null) cached
    else this.synchronized {
      val cached2 = _rootValue
      if (cached2 != null) cached2
      else {
        val computed = _rootInit()
        _rootValue = computed
        _rootInit = null // release the closure and its captured body bytes
        computed
      }
    }
  }

  /** Test-only / package-private: whether `root` has been computed yet. Lets the
    * test suite verify that operations like `bytes`, `template`, equality, etc.
    * don't accidentally force the lazy root. */
  private[sigma] def isRootForced: Boolean = _rootValue != null

  require(isConstantSegregation || constants.isEmpty)

  require(version == 0 || hasSize, s"For newer version the size bit is required: header=$header")

  /** Then it throws the error from UnparsedErgoTree.
    * It does so on every usage of `proposition` because the lazy value remains uninitialized.
    */
  @deprecated("Use toProposition instead", "v2.1")
  lazy val proposition: SigmaPropValue = toProposition(isConstantSegregation)

  /** Version of this tree (== BlockVersion - 1). */
  @inline final def version: Byte = ErgoTree.getVersion(header)

  @inline final def isRightParsed: Boolean = root.isRight

  /** @return true, if constant segregation bit is set in the header. */
  @inline final def isConstantSegregation: Boolean = ErgoTree.isConstantSegregation(header)

  /** @return true, if size bit is set in the header. */
  @inline final def hasSize: Boolean = ErgoTree.hasSize(header)

  private[sigma] var _bytes: Array[Byte] = propositionBytes

  /** Serialized bytes of this tree. */
  final def bytes: Array[Byte] = {
    if (_bytes == null) {
      _bytes = DefaultSerializer.serializeErgoTree(this)
    }
    _bytes
  }

  /** Hexadecimal encoded string of ErgoTree.bytes. */
  final def bytesHex: String = Base16.encode(bytes)

  private[sigma] var _hasDeserialize: Option[Boolean] = givenDeserialize

  /** Returns true if the tree contains at least one deserialization operation,
    * false otherwise.
    */
  lazy val hasDeserialize: Boolean = {
    if (_hasDeserialize.isEmpty) {
      _hasDeserialize = Some(root match {
        case Right(p) => Value.hasDeserialize(p)
        case _ => false
      })
    }
    _hasDeserialize.get
  }

  private[sigma] var _isUsingBlockchainContext: Option[Boolean] = givenIsUsingBlockchainContext

  /** Returns true if the tree depends on the blockchain context.
   */
  lazy val isUsingBlockchainContext: Boolean = {
    if (_isUsingBlockchainContext.isEmpty) {
      _isUsingBlockchainContext = Some(root match {
        case Right(p) => Value.isUsingBlockchainContext(p)
        case _ => false
      })
    }
    _isUsingBlockchainContext.get
  }

  /** Serialized proposition expression of SigmaProp type with
    * ConstantPlaceholder nodes not replaced by Constant nodes.
    */
  lazy val template: Array[Byte] = {
    val r = SigmaSerializer.startReader(bytes)
    DefaultSerializer.deserializeHeaderWithTreeBytes(r)._4
  }

  /** Base16 encoding of `template` bytes. */
  def templateHex: String = Base16.encode(template)

  /** Get proposition expression from this contract.
    * When root.isRight then
    * if replaceConstants == false this is the same as `root.right.get`.
    * Otherwise, it is equivalent to `root.right.get` where all placeholders are replaced by Constants.
    * When root.isLeft then
    * throws the error from UnparsedErgoTree.
    * It does so on every usage of `proposition` because the lazy value remains uninitialized.
    */
  def toProposition(replaceConstants: Boolean): SigmaPropValue = root match {
    case Right(tree) =>
      val prop = if (replaceConstants)
        substConstants(tree, constants).asSigmaProp
      else
        tree
      prop
    case Left(UnparsedErgoTree(_, error)) =>
      throw error
  }

  /** This method attempts to convert the current instance into a `SigmaBoolean`.
    * It does so by first converting the instance to a `SigmaPropValue` using the
    * `toProposition` method, and then checks if the resulting expression is a
    * `SigmaPropConstant`. If it is, the method extracts the `SigmaBoolean` from the
    * `SigmaPropConstant`. Otherwise, the method returns `None`.
    *
    * @note This method relies on constant segregation flag in the header to determine the
    *       behavior of `toProposition` method.
    *
    * @return `Some(SigmaBoolean)` if conversion is successful, `None` otherwise.
    */
  def toSigmaBooleanOpt: Option[SigmaBoolean] = {
    val prop = this.toProposition(this.isConstantSegregation)
    prop match {
      case SigmaPropConstant(p) => Some(p.asInstanceOf[CSigmaProp].wrappedValue)
      case _ => None
    }
  }

  /** Replicates the synthetic `copy` method of the previous case class, with one
    * semantic change required by bytes-based equality (issue #694):
    *
    * `propositionBytes` and the cached `givenDeserialize` / `givenIsUsingBlockchainContext`
    * flags are NOT preserved from `this`. They are reset and will be lazily recomputed
    * from the (possibly changed) field values on first access. This guarantees that
    * `ergoTree.bytes` and the cached flags stay consistent with `header`, `constants`,
    * and `root` after any override.
    *
    * The previous case-class `copy` blindly preserved `propositionBytes`, which was
    * masked by structural equality but would surface as `bytes` inconsistency under
    * bytes-based equality.
    */
  def copy(
      header: HeaderType = this.header,
      constants: IndexedSeq[Constant[SType]] = this.constants,
      root: Either[UnparsedErgoTree, SigmaPropValue] = this.root
  ): ErgoTree =
    new ErgoTree(
      header, constants, root,
      propositionBytes = null,
      givenDeserialize = None,
      givenIsUsingBlockchainContext = None)

  def canEqual(that: Any): Boolean = that.isInstanceOf[ErgoTree]

  /* NO HF PROOF:
     Changed: ErgoTree.equals / hashCode switched from structural over (header, constants, root)
     to bytes-based (`java.util.Arrays.equals(this.bytes, other.bytes)`).
     Motivation: avoids forcing the lazy `root` solely to test equality; enables byte-level
     deduplication of identical deserialized trees (issue #694).
     Safety:
       - ErgoTree equality is NOT used inside ErgoTransaction.validateStateful or any code path
         it reaches (verified by grep across the repository — only references are in
         `ErgoTreeSpecification` tests and structural comments).
       - For any tree obtained via the serializer, `bytes` is deterministically computed from
         `(header, constants, root)`; therefore bytes-equality is structurally equivalent to
         the previous definition. The serializer is the only producer of `propositionBytes`,
         and `bytes` lazily recomputes from `(header, constants, root)` for programmatically
         constructed trees.
       - hashCode collision profile changes (from Objects.hash to Arrays.hashCode), but the
         hashCode contract is preserved.
  */
  override def hashCode(): Int = java.util.Arrays.hashCode(bytes)

  override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) ||
      ((obj.asInstanceOf[AnyRef] != null) && (obj match {
        case other: ErgoTree => java.util.Arrays.equals(bytes, other.bytes)
        case _ => false
      }))

  override def toString: String =
    s"ErgoTree(header=$header, constants=${constants.length}, bytes=${bytes.length}B)"
}

object ErgoTree {
  /** Represents information in ErgoTree header. */
  object HeaderType extends TaggedType[Byte]

  type HeaderType = HeaderType.Type

  /** Current version of ErgoTree serialization format (aka bite-code language version) */
  val VersionFlag: Byte = VersionContext.MaxSupportedScriptVersion

  /** Header flag to indicate that constant segregation should be applied. */
  val ConstantSegregationFlag: Byte = 0x10

  /** Header flag to indicate that whole size of ErgoTree should be saved before tree content. */
  val SizeFlag: Byte = 0x08

  /** Header mask to extract version bits. */
  val VersionMask: Byte = 0x07

  /** Header with all the flags set to 0 and version 0. */
  val ZeroHeader: HeaderType = HeaderType @@ 0.toByte

  /** Default value of ErgoTree.header byte */
  val DefaultHeader: HeaderType = ZeroHeader //HeaderType @@ (VersionFlag | SizeFlag).toByte

  /** Default header with constant segregation enabled. */
  val ConstantSegregationHeader: HeaderType = HeaderType @@ (DefaultHeader | ConstantSegregationFlag).toByte

  /** @return true if the constant segregation flag is set to 1 in the given header byte. */
  @inline final def isConstantSegregation(header: HeaderType): Boolean = (header & ConstantSegregationFlag) != 0

  /** @return true if the size flag is set to 1 in the given header byte. */
  @inline final def hasSize(header: HeaderType): Boolean = (header & SizeFlag) != 0

  /** @return a value of the version bits from the given header byte. */
  @inline final def getVersion(header: HeaderType): Byte = (header & VersionMask).toByte

  /** Update the version bits of the given header byte with the given version value,
    * leaving all other bits unchanged.
    */
  @inline final def setVersionBits(header: HeaderType, version: Byte): HeaderType = {
    require(version < 8, s"ErgoTree.version should be < 8: $version")
    val h = header & (~VersionMask) // clear version bits
    HeaderType @@ (h | version).toByte
  }

  /** Sets the required bit in the given header:
    * - The SizeFlag is set if version > 0
    */
  @inline final def setRequiredBits(header: HeaderType): HeaderType = {
    if (getVersion(header) > 0) {
      // set SizeFlag if version is greater then 0 (see require() in ErgoTree constructor)
      HeaderType @@ (header | ErgoTree.SizeFlag).toByte
    } else
      header
  }

  /** Sets the ConstantSegregationFlag in the given header */
  @inline final def setConstantSegregation(header: HeaderType): HeaderType = {
    HeaderType @@ (header | ConstantSegregationFlag).toByte
  }

  /** Sets the ConstantSegregationFlag in the given header */
  @inline final def setSizeBit(header: HeaderType): HeaderType = {
    HeaderType @@ (header | SizeFlag).toByte
  }

  /** Creates valid header byte with the given version.
    * The SizeFlag is set if version > 0 */
  @inline final def defaultHeaderWithVersion(version: Byte): HeaderType = {
    headerWithVersion(DefaultHeader, version)
  }

  /** Creates valid header byte with the given version.
    * The SizeFlag is set if version > 0 */
  @inline final def headerWithVersion(header: HeaderType, version: Byte): HeaderType = {
    // take the header and embedd the given version in it
    val h = setVersionBits(header, version)
    setRequiredBits(h)
  }

  /** Substitute [[ConstantPlaceholder]] nodes in the given expression with the constants
    * taken from the given collection.
    *
    * @param root      expression to transform
    * @param constants collection of constants to replace placeholders
    * @return new expression without placeholders
    */
  def substConstants(root: SValue, constants: IndexedSeq[Constant[SType]]): SValue = {
    val store     = new ConstantStore(constants)
    val substRule = strategy[Any] {
      case ph: ConstantPlaceholder[_] =>
        Some(store.get(ph.id))
      case _ => None
    }
    everywherebu(substRule)(root).fold(root)(_.asInstanceOf[SValue])
  }

  /** Create an ErgoTree with the given parameters. */
  def apply(
      header: HeaderType,
      constants: IndexedSeq[Constant[SType]],
      root: SigmaPropValue): ErgoTree = {
    new ErgoTree(setRequiredBits(header), constants, Right(root))
  }

  /** Pattern-match extractor preserving the previous case-class arity.
    *
    * Returns the 6 fields from the original case class definition. Pattern matchers
    * typically only inspect the first three (header, constants, root); the remaining
    * three are exposed to keep existing match patterns binary-compatible.
    *
    * Forcing `t.root` is unavoidable here because the pattern surfaces the root value.
    */
  def unapply(t: ErgoTree): Option[(HeaderType, IndexedSeq[Constant[SType]], Either[UnparsedErgoTree, SigmaPropValue], Array[Byte], Option[Boolean], Option[Boolean])] =
    Some((t.header, t.constants, t.root, t.bytes, t._hasDeserialize, t._isUsingBlockchainContext))

  val EmptyConstants: IndexedSeq[Constant[SType]] = Array[Constant[SType]]()

  /** Create new ErgoTree for the given proposition using default header.
    * If the property is not a simple constant, then constant segregation is performed.
    */
  def fromProposition(prop: SigmaPropValue): ErgoTree = {
    fromProposition(ErgoTree.ZeroHeader, prop)
  }

  /** Create new ErgoTree for the given proposition using the given header flags.
    * If the property is not a simple constant, then constant segregation is performed.
    */
  def fromProposition(header: HeaderType, prop: SigmaPropValue): ErgoTree = {
    prop match {
      case SigmaPropConstant(_) => withoutSegregation(header, prop)
      case _ => withSegregation(header, prop)
    }
  }

  /** Create new ErgoTree for the given sigma proposition using default header and
    * without performing constant segregation.
    */
  def fromSigmaBoolean(pk: SigmaBoolean): ErgoTree = {
    withoutSegregation(ZeroHeader, pk.toSigmaPropValue)
  }

  /** Create new ErgoTree for the given sigma proposition using the given header flags
    * and without performing constant segregation.
    */
  def fromSigmaBoolean(header: HeaderType, pk: SigmaBoolean): ErgoTree = {
    withoutSegregation(header, pk.toSigmaPropValue)
  }

  /** Create new ErgoTree for the given proposition using the given header flags and
    * without performing constant segregation.
    */
  def withoutSegregation(header: HeaderType, root: SigmaPropValue): ErgoTree =
    ErgoTree(setRequiredBits(header), EmptyConstants, root)

  /** Build ErgoTree via serialization of the value with ConstantSegregationHeader, constants segregated
    * from the tree and ConstantPlaceholders referring to the segregated constants.
    *
    * This method uses single traverse of the tree to:
    * 1) find and segregate all constants;
    * 2) replace constants with ConstantPlaceholders in the `tree`;
    * 3) write the `tree` to the Writer's buffer obtaining `treeBytes`;
    * 4) deserialize `tree` with ConstantPlaceholders.
    *
    * @param header      additional header flags to combine with
    *                    ConstantSegregationHeader flag.
    * @param prop        expression to be transformed into ErgoTree
    * */
  def withSegregation(header: HeaderType, prop: SigmaPropValue): ErgoTree = {
    val constantStore = new ConstantStore()
    val w             = SigmaSerializer.startWriter(Some(constantStore))
    // serialize value and segregate constants into constantStore
    ValueSerializer.serialize(prop, w)
    val extractedConstants = constantStore.getAll
    val r                  = SigmaSerializer.startReader(w.toBytes)
    r.constantStore = new ConstantStore(extractedConstants)
    // deserialize value with placeholders
    val valueWithPlaceholders = ValueSerializer.deserialize(r).asSigmaProp
    new ErgoTree(
      header = setRequiredBits(setConstantSegregation(header)),
      constants = extractedConstants,
      root = Right(valueWithPlaceholders))
  }

  /** Deserializes an ErgoTree instance from a hexadecimal string.
    *
    * @param hex a hexadecimal string representing the serialized ErgoTree
    */
  def fromHex(hex: String): ErgoTree = {
    val bytes = Base16.decode(hex).get
    fromBytes(bytes)
  }

  /** Deserializes an ErgoTree instance from an array of bytes.
    *
    * @param bytes an array of bytes representing the serialized ErgoTree
    */
  def fromBytes(bytes: Array[Byte]): ErgoTree = {
    ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(bytes)
  }

}
