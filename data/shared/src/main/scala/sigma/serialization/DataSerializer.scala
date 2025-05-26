package sigma.serialization

import org.ergoplatform.ErgoBox
import sigma.ast._
import sigma.data.CBox

/** This works in tandem with ConstantSerializer, if you change one make sure to check the other.*/
object DataSerializer extends CoreDataSerializer {

  /** Use type descriptor `tpe` to deconstruct type structure and recursively serialize subcomponents.
    * Primitive types are leaves of the type tree, and they are served as basis of recursion.
    * The data value `v` is expected to conform to the type described by `tpe`.
    */
  override def serialize[T <: SType](v: T#WrappedType, tpe: T, w: CoreByteWriter): Unit = tpe match {
    case SBox =>
      val b = v.asInstanceOf[CBox]
      ErgoBox.sigmaSerializer.serialize(b.ebox, w.asInstanceOf[SigmaByteWriter])
    case _ =>
      super.serialize(v, tpe, w)
  }

  /** Reads a data value from Reader. The data value bytes is expected to confirm
    * to the type descriptor `tpe`.
    * The data structure depth is limited by r.maxTreeDepth which is
    * SigmaSerializer.MaxTreeDepth by default.
    */
  override def deserialize[T <: SType](tpe: T, r: CoreByteReader): T#WrappedType = {
    val res = (tpe match {
      case SBox =>
        val depth = r.level
        r.level = depth + 1
        val res = CBox(ErgoBox.sigmaSerializer.parse(r.asInstanceOf[SigmaByteReader]))
        r.level = r.level - 1
        res
      case t =>
        super.deserialize(t, r)
    }).asInstanceOf[T#WrappedType]
    res
  }

}
