package sigmastate.eval

import debox.cfor
import sigma.Extensions.ArrayOps
import sigma._
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
                     vars: ContextVarsMap,
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
    if (id < 0 || id > vars.maxKey) return None
    val value = vars.getNullable(id)
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

}

