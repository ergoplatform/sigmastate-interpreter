package sigma.serialization

import debox.cfor
import scorex.util.serialization.Reader
import sigma.ast._
import sigma.ast.syntax._
import sigma.util.safeNewArray

/** Reader used in the concrete implementations of [[SigmaSerializer]].
  * It decorates the given reader, delegates most of the methods to it, but also adds new
  * methods.
  *
  * @param r                              the underlying reader this reader reads from
  * @param constantStore                  the store of constants which is used to resolve
  *                                       [[sigma.ast.ConstantPlaceholder]]
  * @param resolvePlaceholdersToConstants if true then resolved constants will be
  *                                       substituted in the tree instead of the placeholder.
  * @param maxTreeDepth                   limit on the tree depth (recursive invocations)
  *                                       of the deserializer
  */
class SigmaByteReader(override val r: Reader,
                      var constantStore: ConstantStore,
                      var resolvePlaceholdersToConstants: Boolean,
                      override val maxTreeDepth: Int = SigmaSerializer.MaxTreeDepth)
  extends CoreByteReader(r, maxTreeDepth) {

  /** The reader should be lightweight to create. In most cases ErgoTrees don't have
    * ValDef nodes hence the store is not necessary and it's initialization dominates the
    * reader instantiation time. Hence it's lazy.
    * HOTSPOT:
    */
  lazy val valDefTypeStore: ValDefTypeStore = new ValDefTypeStore()

  override type CH = r.CH


  /** Returns all bytes of the underlying ByteBuffer. */
  private[sigma] def getAllBufferBytes: Array[Byte] = {
    val savedPos = position
    position = 0
    val res = getBytesUnsafe(remaining)
    position = savedPos
    res
  }

  @inline def getValue(): SValue = ValueSerializer.deserialize(this)

  /** Read sequence of values from this reader.
    * It first reads the number of values and then reads each value using `getValue` method.
    *
    * @return a sequence of zero of more values read
    */
  @inline def getValues(): IndexedSeq[SValue] = {
    val size = getUIntExact
    if (size == 0) Value.EmptySeq // quick short-cut when there is nothing to read
    else {
      val xs = safeNewArray[SValue](size)
      cfor(0)(_ < size, _ + 1) { i =>
        xs(i) = getValue()
      }
      xs
    }
  }

}
