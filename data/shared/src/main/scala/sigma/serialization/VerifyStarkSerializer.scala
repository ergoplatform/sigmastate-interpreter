package sigma.serialization

import sigma.ast.SCollection.SByteArray
import sigma.ast._
import sigma.serialization.ValueCodes.OpCode

/**
 * P2P Serializer for the VerifyStark AST node (EIP-0045).
 *
 * Serializes and deserializes the 5 child values in strict order:
 * 1. proofChunks:  Value[SCollection[SCollection[SByte.type]]]
 * 2. publicInputs: Value[SByteArray]
 * 3. imageId:      Value[SByteArray]
 * 4. vmType:       Value[SInt.type]
 * 5. costParams:   Value[SCollection[SInt.type]]
 *
 * The order is consensus-critical: all nodes must serialize/deserialize
 * in exactly this sequence for binary compatibility.
 */
object VerifyStarkSerializer extends ValueSerializer[VerifyStark] {
  override def opDesc: ValueCompanion = VerifyStark

  override def serialize(obj: VerifyStark, w: SigmaByteWriter): Unit = {
    w.putValue(obj.proofChunks)
    w.putValue(obj.publicInputs)
    w.putValue(obj.imageId)
    w.putValue(obj.vmType)
    w.putValue(obj.costParams)
  }

  override def parse(r: SigmaByteReader): Value[SBoolean.type] = {
    val proofChunks = r.getValue().asInstanceOf[Value[SCollection[SCollection[SByte.type]]]]
    val publicInputs = r.getValue().asInstanceOf[Value[SByteArray]]
    val imageId = r.getValue().asInstanceOf[Value[SByteArray]]
    val vmType = r.getValue().asInstanceOf[Value[SInt.type]]
    val costParams = r.getValue().asInstanceOf[Value[SCollection[SInt.type]]]

    VerifyStark(proofChunks, publicInputs, imageId, vmType, costParams)
  }
}
