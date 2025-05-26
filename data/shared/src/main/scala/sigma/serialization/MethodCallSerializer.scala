package sigma.serialization

import sigma.ast.syntax._
import sigma.ast.{MethodCall, SContextMethods, SMethod, SType, STypeSubst, Value, ValueCompanion}
import sigma.util.safeNewArray
import SigmaByteWriter._
import debox.cfor
import sigma.ast.SContextMethods.BlockchainContextMethodNames
import sigma.serialization.CoreByteWriter.{ArgInfo, DataInfo}

case class MethodCallSerializer(cons: (Value[SType], SMethod, IndexedSeq[Value[SType]], STypeSubst) => Value[SType])
  extends ValueSerializer[MethodCall] {
  override def opDesc: ValueCompanion = MethodCall
  val typeCodeInfo: DataInfo[Byte] = ArgInfo("typeCode", "type of the method (see Table~\\ref{table:predeftypes})")
  val methodCodeInfo: DataInfo[Byte] = ArgInfo("methodCode", "a code of the method")
  val objInfo: DataInfo[SValue] = ArgInfo("obj", "receiver object of this method call")
  val argsInfo: DataInfo[Seq[SValue]] = ArgInfo("args", "arguments of the method call")
  val argsItemInfo = valuesItemInfo(argsInfo)

  override def serialize(mc: MethodCall, w: SigmaByteWriter): Unit = {
    w.put(mc.method.objType.typeId, typeCodeInfo)
    w.put(mc.method.methodId, methodCodeInfo)
    w.putValue(mc.obj, objInfo)
    assert(mc.args.nonEmpty)
    w.putValues(mc.args, argsInfo, argsItemInfo)
  }

  /** The SMethod instances in STypeCompanions may have type STypeIdent in methods types,
    * but a valid ErgoTree should have SMethod instances specialized for specific types
    * of `obj` and `args` using `specializeFor`.
    * This means, if we save typeId, methodId, and we save all the arguments,
    * we can restore the specialized SMethod instance.
    * This work by induction, if we assume all arguments are monomorphic,
    * then we can make MethodCall monomorphic. Thus, all ErgoTree is monomorphic by construction.
    * This is limitation of MethodCall, because we cannot use it to represent for example
    * def Box.getReg[T](id: Int): Option[T], which require serialization of expected type `T`
    * However it can be implemented using separate node type (new type code) and can be added via soft-fork.
    * HOTSPOT: don't beautify this code
    */
  override def parse(r: SigmaByteReader): Value[SType] = {
    val typeId = r.getByte()
    val methodId = r.getByte()
    val obj = r.getValue()
    val args = r.getValues()
    val method = SMethod.fromIds(typeId, methodId)
    val nArgs = args.length

    val types: Seq[SType] =
      if (nArgs == 0) SType.EmptySeq
      else {
        val types = safeNewArray[SType](nArgs)
        cfor(0)(_ < nArgs, _ + 1) { i =>
          types(i) = args(i).tpe
        }
        types
      }

    val specMethod = method.specializeFor(obj.tpe, types)

    var isUsingBlockchainContext = specMethod.objType == SContextMethods &&
      BlockchainContextMethodNames.contains(method.name)
    r.wasUsingBlockchainContext ||= isUsingBlockchainContext

    cons(obj, specMethod, args, Map.empty)
  }
}
