package sigma.data

import org.ergoplatform.ErgoBox
import scorex.utils.Ints
import sigma.Evaluation.stypeToRType
import sigma.ast.SCollection.SByteArray
import sigma.ast.syntax._
import sigma.ast.{ConstantNode, EvaluatedValue, SInt, STuple, SType}
import sigma.data.CBox.regs
import sigma.eval.Extensions.toAnyValue
import sigma.exceptions.InvalidType
import sigma.{AnyValue, Box, Coll, Colls}

import java.util.Arrays

/** A default implementation of [[Box]] interface.
  *
  * @see [[Box]] for detailed descriptions
  */
case class CBox(ebox: ErgoBox) extends Box with WrapperOf[ErgoBox] {
  val builder = CSigmaDslBuilder

  val value = ebox.value
  lazy val id              : Coll[Byte]     = Colls.fromArray(ebox.id)
  lazy val bytes           : Coll[Byte]     = Colls.fromArray(ebox.bytes)
  lazy val bytesWithoutRef : Coll[Byte]     = Colls.fromArray(ebox.bytesWithNoRef)
  lazy val propositionBytes: Coll[Byte]     = Colls.fromArray(ebox.propositionBytes)
  lazy val registers       : Coll[AnyValue] = regs(ebox)

  override def wrappedValue: ErgoBox = ebox

  override def getReg[T](i: Int)(implicit tT: RType[T]): Option[T] = {
    if (i < 0 || i >= registers.length) return None
    val value = registers(i)
    if (value != null) {
      // once the value is not null it should be of the right type
      value match {
        case value: CAnyValue[_] if value.value != null && value.tA == tT =>
          Some(value.value.asInstanceOf[T])
        case _ =>
          throw new InvalidType(s"Cannot getReg[${tT.name}]($i): invalid type of value $value at id=$i")
      }
    } else None
  }

  override def creationInfo: (Int, Coll[Byte]) = {
    this.getReg[(Int, Coll[Byte])](3).get.asInstanceOf[Any] match {
      case info: Tuple2[Int, Coll[Byte]]@unchecked => info
      case ConstantNode(arr: Array[Any], STuple(IndexedSeq(SInt, SByteArray))) if arr.length == 2 =>
        (arr(0).asInstanceOf[Int], builder.Colls.fromArray(arr(1).asInstanceOf[Array[Byte]]))
      case v =>
        sys.error(s"Invalid value $v of creationInfo register R3")
    }
  }

  override def tokens: Coll[(Coll[Byte], Long)] = {
    this.getReg[Coll[(Coll[Byte], Long)]](ErgoBox.R2.asIndex).get
  }

  override def executeFromRegister[T](regId: Byte)
      (implicit cT: RType[T]): T = ??? // TODO implement

  override def hashCode(): Int = Ints.fromByteArray(id.toArray)

  override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) || (obj != null && {
    obj match {
      case obj: Box => Arrays.equals(id.toArray, obj.id.toArray)
      case _ =>
        // this case was missing in v4.x, however has never been a problem
        // Thus, v5.0 interpreter will not fail (while v4.x would fail here)
        false
    }
  })
}

object CBox {
  def regs(ebox: ErgoBox): Coll[AnyValue] = {
    val res = new Array[AnyValue](ErgoBox.maxRegisters)

    def checkNotYetDefined(id: Int, newValue: SValue) =
      require(res(id) == null, s"register $id is defined more then once: previous value ${res(id)}, new value $newValue")

    for ( (k, v: EvaluatedValue[t]) <- ebox.additionalRegisters ) {
      checkNotYetDefined(k.number, v)
      res(k.number) = toAnyValue(v.value)(stypeToRType(v.tpe))
    }
    for ( r <- ErgoBox.mandatoryRegisters ) {
      val regId = r.number
      val v     = ebox.get(r).get.asInstanceOf[EvaluatedValue[SType]]
      checkNotYetDefined(regId, v)
      res(regId) = toAnyValue(v.value)(stypeToRType(v.tpe))
    }
    Colls.fromArray(res)
  }
}