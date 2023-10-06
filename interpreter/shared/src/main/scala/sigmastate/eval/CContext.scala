package sigmastate.eval

import debox.cfor
import org.ergoplatform.ErgoBox
import org.ergoplatform.validation.ValidationRules
import scorex.crypto.hash.{Blake2b256, Sha256}
import scorex.utils.Longs
import sigma.Extensions.ArrayOps
import sigma.ast.{AtLeast, SubstConstants}
import sigma.crypto.{EcPointType, Ecp}
import sigma.data._
import sigma.serialization.GroupElementSerializer
import sigma.util.Extensions.BigIntegerOps
import sigma.validation.SigmaValidationSettings
import sigma.{VersionContext, _}
import sigmastate.crypto.CryptoConstants
import sigmastate.eval.Extensions._
import sigma.serialization.SigmaSerializer

import java.math.BigInteger
import scala.annotation.unused
import scala.reflect.ClassTag


/** This class represents context variable and register value of a functional type A => B.
  * When variable or register is accessed using `getVar[A => B](id).get` or
  * `box.getReg[A => B].get an instance of this class is returned.
  *
  * It internally transforms a given `tree` into executable function.
  * This it similar to what happens during validation of propositions in the input boxes:
  * - size check of underlying ErgoTree against limits
  * - construction of `calcF` and `costF` graphs, both are stored together with resulting function.
  * - check the types of `calcF` graph to be compatible with expected types A and B
  * If anything goes wrong, this operation fails and if it is used in the script, the script also fails.
  *
  * When f is obtained as `val f = getVar[Int => Int](id).get` then any application `f(x)` involves size estimation
  * using underlying `costF(x)`.
  * */
//case class CFunc[A,B](context: sigmastate.interpreter.Context, tree: SValue)
//    (implicit tDom: RType[A], tRange: RType[B], IR: IRContext) extends (A => B) {
//  import CFunc._
//
//  private val compiled = {
//    import IR._
//    val IR.Pair(calcF, costF) = IR.doCosting(emptyEnv, tree)
//
//    val eDom = asElem[Any](IR.rtypeToElem(tDom))
//    val eRange = asElem[Any](IR.rtypeToElem(tRange))
//
//    IR.verifyCalcFunc[Any => Any](asRep[Context => (Any => Any)](calcF), IR.funcElement(eDom, eRange))
////    IR.verifyCostFunc(costF).getOrThrow
////    IR.verifyIsProven(calcF).getOrThrow
//
//    // check cost
////    val costingCtx = context.toSigmaContext(IR, isCost = true)
////    val costFun = IR.compile[SInt.type](IR.getDataEnv, costF)
////    val IntConstant(estimatedCost) = costFun(costingCtx)
////    if (estimatedCost > maxCost) {
////      throw new Error(s"Estimated execution cost $estimatedCost exceeds the limit $maxCost in $tree")
////    }
//    // check calc
//    val calcCtx = context.toSigmaContext(IR, isCost = false)
//    val valueFun = IR.compile[SFunc](IR.getDataEnv, asRep[Context => SFunc#WrappedType](calcF))
//    val res = valueFun(calcCtx) match {
//      case Constant(f, fTpe: SFunc) => f
//      case v => v
//    }
//    res.asInstanceOf[A => B]
//  }
//
//  override def apply(x: A): B = compiled(x)
//}
object CFunc {
  /** The cost of creating resulting function but not its execution.
    * Thus it is expected to be small. It can be increased if useful cases are found
    * such that `tree` should contains heavy operations. */
  val maxCost = 1000
}

/** A default implementation of [[PreHeader]] interface.
  * @see [[PreHeader]] for detailed descriptions
  */
case class CPreHeader(
                       version: Byte,
                       parentId: Coll[Byte],
                       timestamp: Long,
                       nBits: Long,
                       height: Int,
                       minerPk: GroupElement,
                       votes: Coll[Byte]
                     ) extends PreHeader {}

/** A default implementation of [[Header]] interface.
  * @see [[Header]] for detailed descriptions
  */
case class CHeader(
                    id: Coll[Byte],
                    version: Byte,
                    parentId: Coll[Byte],
                    ADProofsRoot: Coll[Byte],
                    stateRoot: AvlTree,
                    transactionsRoot: Coll[Byte],
                    timestamp: Long,
                    nBits: Long,
                    height: Int,
                    extensionRoot: Coll[Byte],
                    minerPk: GroupElement,
                    powOnetimePk: GroupElement,
                    powNonce: Coll[Byte],
                    powDistance: BigInt,
                    votes: Coll[Byte]
                  ) extends Header {
}

object CHeader {
  val VotesSize: Int = SigmaConstants.VotesArraySize.value
  val NonceSize: Int = SigmaConstants.AutolykosPowSolutionNonceArraySize.value
}

/** A default implementation of [[SigmaDslBuilder]] interface.
  * @see [[SigmaDslBuilder]] for detailed descriptions
  */
class CSigmaDslBuilder extends SigmaDslBuilder { dsl =>
  implicit val validationSettings: SigmaValidationSettings = ValidationRules.currentSettings

  override val Colls: CollBuilder = sigma.Colls

  override def BigInt(n: BigInteger): BigInt = CBigInt(n)

  override def toBigInteger(n: BigInt): BigInteger = n.asInstanceOf[CBigInt].wrappedValue

  /** Wraps the given elliptic curve point into GroupElement type. */
  def GroupElement(p: Ecp): GroupElement = p match {
    case ept: EcPointType => CGroupElement(ept)
    case m => sys.error(s"Point of type ${m.getClass} is not supported")
  }

  /** Wraps the given sigma proposition into SigmaDsl value of type SigmaProp. */
  def SigmaProp(sigmaTree: SigmaBoolean): SigmaProp = new CSigmaProp(sigmaTree)

  /** Extract `sigma.data.SigmaBoolean` from DSL's `SigmaProp` type. */
  @inline def toSigmaBoolean(p: SigmaProp): SigmaBoolean = p.asInstanceOf[CSigmaProp].sigmaTree

  /** Extract `sigmastate.AvlTreeData` from DSL's `AvlTree` type. */
  def toAvlTreeData(p: AvlTree): AvlTreeData = p.asInstanceOf[CAvlTree].treeData

  /** Extract `sigmastate.crypto.Ecp` from DSL's `GroupElement` type. */
  def toECPoint(ge: GroupElement): Ecp = ge.asInstanceOf[CGroupElement].wrappedValue

  /** Creates a new AvlTree instance with the given parameters.
    * @see AvlTreeData for details
    */
  override def avlTree(operationFlags: Byte, digest: Coll[Byte], keyLength: Int, valueLengthOpt: Option[Int]): CAvlTree = {
    val treeData = AvlTreeData(digest, AvlTreeFlags(operationFlags), keyLength, valueLengthOpt)
    CAvlTree(treeData)
  }

  /** Wraps the given tree data into SigmaDsl value of type [[AvlTree]]. */
  def avlTree(treeData: AvlTreeData): AvlTree = {
    CAvlTree(treeData)
  }

  /** Wraps the given [[ErgoBox]] into SigmaDsl value of type [[Box]].
    * @param ebox  the value to be wrapped
    * @see [[sigmastate.SBox]], [[sigma.Box]]
    */
  def Box(ebox: ErgoBox): Box = CBox(ebox)

  /** Extracts [[ErgoBox]] from the given [[Box]] instance. This is inverse to the Box method. */
  def toErgoBox(b: Box): ErgoBox = b.asInstanceOf[CBox].ebox

  /** HOTSPOT: don't beautify this code */
  private def toSigmaTrees(props: Array[SigmaProp]): Array[SigmaBoolean] = {
    val len = props.length
    val res = new Array[SigmaBoolean](len)
    cfor(0)(_ < len, _ + 1) { i =>
      res(i) = toSigmaBoolean(props(i))
    }
    res
  }

  @inline private def toEcPointType(ge: GroupElement): EcPointType =
    toECPoint(ge).asInstanceOf[EcPointType]

  override def atLeast(bound: Int, props: Coll[SigmaProp]): SigmaProp = {
    if (props.length > AtLeast.MaxChildrenCount)
      throw new IllegalArgumentException(s"Expected input elements count should not exceed ${AtLeast.MaxChildrenCount}, actual: ${props.length}")
    val sigmaTrees = toSigmaTrees(props.toArray)
    val tree = AtLeast.reduce(bound, sigmaTrees)
    CSigmaProp(tree)
  }

  override def allOf(conditions: Coll[Boolean]): Boolean =
    conditions.forall(c => c)

  override def anyOf(conditions: Coll[Boolean]): Boolean =
    conditions.exists(c => c)

  override def xorOf(conditions: Coll[Boolean]): Boolean = {
    if (VersionContext.current.isJitActivated) {
      val len = conditions.length
      if (len == 0) false
      else if (len == 1) conditions(0)
      else {
        var res = conditions(0)
        cfor(1)(_ < len, _ + 1) { i =>
          res ^= conditions(i)
        }
        res
      }
    } else {
      // This is buggy version used in v4.x interpreter (for ErgoTrees v0, v1)
      conditions.toArray.distinct.length == 2
    }
  }

  override def allZK(props: Coll[SigmaProp]): SigmaProp = {
    val sigmaTrees = toSigmaTrees(props.toArray)
    val tree = CAND.normalized(sigmaTrees)
    CSigmaProp(tree)
  }

  override def anyZK(props: Coll[SigmaProp]): SigmaProp = {
    val sigmaTrees = toSigmaTrees(props.toArray)
    val tree = COR.normalized(sigmaTrees)
    CSigmaProp(tree)
  }

  override def xor(l: Coll[Byte], r: Coll[Byte]): Coll[Byte] =
    Colls.xor(l, r)

  override def sigmaProp(b: Boolean): SigmaProp = {
    CSigmaProp(TrivialProp(b))
  }

  override def blake2b256(bytes: Coll[Byte]): Coll[Byte] = {
    val h = Blake2b256.hash(bytes.toArray)
    Colls.fromArray(h)
  }

  override def sha256(bytes: Coll[Byte]): Coll[Byte] = {
    val h = Sha256.hash(bytes.toArray)
    Colls.fromArray(h)
  }

  override def byteArrayToBigInt(bytes: Coll[Byte]): BigInt = {
    val bi = new BigInteger(bytes.toArray).to256BitValueExact
    this.BigInt(bi)
  }

  override def longToByteArray(l: Long): Coll[Byte] =
    Colls.fromArray(Longs.toByteArray(l))

  override def byteArrayToLong(bytes: Coll[Byte]): Long =
    Longs.fromByteArray(bytes.toArray)

  override def proveDlog(ge: GroupElement): SigmaProp =
    CSigmaProp(ProveDlog(toECPoint(ge).asInstanceOf[EcPointType]))

  override def proveDHTuple(g: GroupElement, h: GroupElement, u: GroupElement, v: GroupElement): SigmaProp = {
    val dht = ProveDHTuple(toEcPointType(g), toEcPointType(h), toEcPointType(u), toEcPointType(v))
    CSigmaProp(dht)
  }

  private lazy val _generatorElement = this.GroupElement(CryptoConstants.dlogGroup.generator)
  override def groupGenerator: GroupElement = _generatorElement

  /**
    * @return the identity of the Dlog group used in ErgoTree
    */
  def groupIdentity: GroupElement = {
    this.GroupElement(CryptoConstants.dlogGroup.identity)
  }

  override def substConstants[T](scriptBytes: Coll[Byte],
                                 positions: Coll[Int],
                                 newValues: Coll[T]): Coll[Byte] = {
    val constants = try newValues.toArrayOfConstants
    catch {
      case e: Throwable =>
        throw new RuntimeException(s"Cannot evaluate substConstants($scriptBytes, $positions, $newValues)", e)
    }
    val (res, _) = SubstConstants.eval(scriptBytes.toArray, positions.toArray, constants)(validationSettings)
    Colls.fromArray(res)
  }

  override def decodePoint(encoded: Coll[Byte]): GroupElement = {
    val r = SigmaSerializer.startReader(encoded.toArray)
    val p = GroupElementSerializer.parse(r)
    this.GroupElement(p)
  }

}

/** Default singleton instance of Global object, which implements global ErgoTree functions. */
object CSigmaDslBuilder extends CSigmaDslBuilder

/** A default implementation of [[Context]] interface.
  * @see [[Context]] for detailed descriptions
  */
case class CContext(
    _dataInputs: Coll[Box],
    override val headers: Coll[Header],
    override val preHeader: PreHeader,
    inputs: Coll[Box],
    outputs: Coll[Box],
    height: Int,
    selfBox: Box,
    private val selfIndex: Int,
    lastBlockUtxoRootHash: AvlTree,
    _minerPubKey: Coll[Byte],
    vars: Coll[AnyValue],
    override val activatedScriptVersion: Byte,
    override val currentErgoTreeVersion: Byte
) extends Context {
  @inline override def builder: SigmaDslBuilder = CSigmaDslBuilder

  @inline override def HEIGHT: Int = height

  @inline override def SELF: Box = selfBox

  @inline override def dataInputs: Coll[Box] = _dataInputs

  @inline override def INPUTS = inputs

  @inline override def OUTPUTS = outputs

  @inline override def LastBlockUtxoRootHash = lastBlockUtxoRootHash

  @inline override def minerPubKey = _minerPubKey

  override def selfBoxIndex: Int = {
    if (VersionContext.current.isJitActivated) {
      // starting from v5.0 this is fixed
      selfIndex
    } else {
      // this used to be a bug in v4.x (https://github.com/ScorexFoundation/sigmastate-interpreter/issues/603)
      -1
    }
  }

  override def getVar[T](id: Byte)(implicit tT: RType[T]): Option[T] = {
    @unused // avoid warning about unused ctA
    implicit val tag: ClassTag[T] = tT.classTag
    if (id < 0 || id >= vars.length) return None
    val value = vars(id)
    if (value != null) {
      // once the value is not null it should be of the right type
      value match {
        case value: CAnyValue[_] if value.value != null && value.tA == tT =>
          Some(value.value.asInstanceOf[T])
        case _ =>
          throw new InvalidType(s"Cannot getVar[${tT.name}]($id): invalid type of value $value at id=$id")
      }
    } else None
  }

  /** Return a new context instance with variables collection updated.
    * @param bindings  a new binding of the context variables with new values.
    * @return a new instance (if `bindings` non-empty) with the specified bindings.
    *         other existing bindings are copied to the new instance
    */
  def withUpdatedVars(bindings: (Int, AnyValue)*): CContext = {
    if (bindings.isEmpty) return this

    val ids = bindings.map(_._1).toArray
    val values = bindings.map(_._2).toArray
    val maxVarId = ids.max  // INV: ids is not empty
    val requiredNewLength = maxVarId + 1

    val newVars = if (vars.length < requiredNewLength) {
      // grow vars collection
      val currVars = vars.toArray
      val buf = new Array[AnyValue](requiredNewLength)
      Array.copy(currVars, 0, buf, 0, currVars.length)
      cfor(0)(_ < ids.length, _ + 1) { i =>
        buf(ids(i)) = values(i)
      }
      buf.toColl
    } else {
      vars.updateMany(ids.toColl, values.toColl)
    }

    this.copy(vars = newVars)
  }
}

