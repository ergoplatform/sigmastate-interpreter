package org.ergoplatform.dsl

import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.TokenId
import sigma.data.{AvlTreeData, CSigmaDslBuilder, RType, SigmaBoolean}
import org.ergoplatform.dsl.ContractSyntax.{ErgoScript, Proposition}
import org.ergoplatform.sdk.JavaHelpers.collRType
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigma._
import sigma.ast.{SType, syntax}
import sigma.ast.SType.AnyOps

import scala.reflect.ClassTag
import scala.util.Try

/** Defines methods to be used in contract implementations based on [[SigmaContract]]. */
trait ContractSyntax { contract: SigmaContract =>
  override def builder: SigmaDslBuilder = CSigmaDslBuilder

  /** Instance of contract specification DSL, which can be imported in the body of
   * [[SigmaContract]] implementations. */
  val spec: ContractSpec

  /** A contract environment which defines named constants used in the contract.
    * Should be defined in [[SigmaContract]] implementations.
    */
  def contractEnv: ScriptEnv

  /** The default verifier which represents miner's role in verification of transactions.
    * It can be overriden in derived classes. */
  lazy val verifier: spec.VerifyingParty = spec.VerifyingParty("Miner")

  /** Helper method to support Scala <-> ErgoScript equivalence. */
  def Coll[T](items: T*)(implicit cT: RType[T]) = builder.Colls.fromItems(items:_*)

  /** Tries to reconstruct RType of the given value.
    * If not successfull returns failure. */
  def rtypeOf(value: Any): Try[RType[_]] = Try {
    value match {
      case arr if arr.getClass.isArray =>
        val itemClass = arr.getClass.getComponentType
        if (itemClass.isPrimitive) {
          val itemTag = ClassTag[Any](itemClass)
          RType.fromClassTag(itemTag)
        } else
          sys.error(s"Cannot compute rtypeOf($value): non-primitive type of array items")
      case coll: Coll[_] => collRType(coll.tItem)

      // all primitive types
      case _: Boolean => BooleanType
      case _: Byte => ByteType
      case _: Short => ShortType
      case _: Int => IntType
      case _: Long => LongType
      case _: String => StringType
      case _: Unit => UnitType
      case _: sigma.BigInt => BigIntRType
      case _: GroupElement => GroupElementRType
      case _: ErgoBox => syntax.ErgoBoxRType // TODO remove this RType
      case _: Box => BoxRType
      case _: AvlTreeData => syntax.AvlTreeDataRType // TODO remove this RType
      case _: AvlTree => AvlTreeRType
      case _: SigmaBoolean => SigmaBooleanRType // TODO remove this RType
      case _: SigmaProp => SigmaPropRType
      case _: Context => ContextRType
      case _ =>
        sys.error(s"Don't know how to compute typeOf($value)")
    }
  }

  /** Call this function in [[SigmaContract]] implementations to define propositions.
    *
    * @param name    name of the proposition (aka contract name)
    * @param dslSpec Scala lambda of type [[Proposition]] which defines contract semantics
    *                and can be executed directly.
    * @param scriptCode ErgoScript representation of the contract.
    * @param scriptVersion optional script version to be used in ErgoTree.
    *                      If None then ergoTreeVersionInTests is used.
    * @return proposition specification with compiled ErgoTree.
    */
  def proposition(name: String,
                  dslSpec: Proposition,
                  scriptCode: String,
                  scriptVersion: Option[Byte] = None): spec.PropositionSpec = {
    val env = contractEnv.map { case (k, v) =>
      val tV = rtypeOf(v).get
      val elemTpe = Evaluation.rtypeToSType(tV)
      k -> spec.IR.builder.mkConstant[SType](v.asWrappedType, elemTpe)
    }.toMap
    spec.mkPropositionSpec(name, dslSpec, ErgoScript(env, scriptCode, scriptVersion))
  }

  /** Creates new environment with the given named constants. */
  def Env(entries: (String, Any)*): ScriptEnv = Map(entries:_*)
}
object ContractSyntax {
  /** Type of proposition as Scala lambda. */
  type Proposition = Context => SigmaProp
  /** Represents ErgoScript contract to be compiled. */
  case class ErgoScript(env: ScriptEnv, code: String, scriptVersion: Option[Byte])
  /** Typed representation of token id and amount. */
  case class Token(id: TokenId, value: Long)
}

trait SigmaContractSyntax extends SigmaContract with ContractSyntax {

  implicit class BooleanSyntax(source: Boolean) {
    /** Logical AND between Boolean on the left and SigmaProp value on the right. */
    def &&(prop: SigmaProp) = builder.sigmaProp(source) && prop

    /** Logical AND between Boolean on the left and SigmaProp value on the right. */
    def ||(prop: SigmaProp) = builder.sigmaProp(source) || prop
  }

}






