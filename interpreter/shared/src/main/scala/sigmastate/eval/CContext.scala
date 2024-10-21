package sigmastate.eval

import debox.cfor
import org.ergoplatform.{ErgoLikeTransactionTemplate, UnsignedInput}
import sigma.Evaluation.stypeToRType
import sigma.Extensions.ArrayOps
import sigma._
import sigma.ast.SType
import sigma.data._
import sigma.exceptions.InvalidType

import scala.annotation.unused
import scala.reflect.ClassTag

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
    spendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput],
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

  override def getVarFromInput[T](inputId: Short, id: Byte)(implicit tT: RType[T]): Option[T] = {
    spendingTransaction.inputs.lift(inputId).flatMap(_.extension.get(id)) match {
      case Some(v) if stypeToRType[SType](v.tpe) == tT => Some(v.value.asInstanceOf[T])
      case _ =>
        None
    }
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

