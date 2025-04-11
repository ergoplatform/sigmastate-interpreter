package sigmastate

import sigma.SigmaDataReflection
import sigma.data.{CAND, COR, CTHRESHOLD}
import sigma.reflection.mkConstructor
import sigma.reflection.ReflectionData.registerClassEntry
import sigmastate.crypto.GF2_192_Poly
import sigmastate.crypto.VerifierMessage.Challenge

/** Reflection metadata for `interpreter` module.
  * Such metadata is only used on JS platform to support reflection-like interfaces of
  * RClass, RMethod, RConstructor. These interfaces implemented on JVM using Java
  * reflection.
  *
  * For each class of this module that needs reflection metadata,
  * we register a class entry with the necessary information.
  * Only information that is needed at runtime is registered.
  */
object InterpreterReflection {
  val reflection = SigmaDataReflection

  registerClassEntry(classOf[CAndUncheckedNode],
    constructors = Array(
      mkConstructor(Array(classOf[Array[Byte]], classOf[Seq[_]])) { args =>
        new CAndUncheckedNode(args(0).asInstanceOf[Challenge], args(1).asInstanceOf[Seq[UncheckedSigmaTree]])
      }
    )
  )

  registerClassEntry(classOf[CAndUnproven],
    constructors = Array(
      mkConstructor(Array(classOf[CAND], classOf[Option[_]], classOf[Boolean], classOf[Seq[_]], classOf[NodePosition])) { args =>
        new CAndUnproven(args(0).asInstanceOf[CAND],
          args(1).asInstanceOf[Option[Challenge]],
          args(2).asInstanceOf[Boolean],
          args(3).asInstanceOf[Seq[ProofTree]],
          args(4).asInstanceOf[NodePosition])
      }
    )
  )

  registerClassEntry(classOf[COrUncheckedNode],
    constructors = Array(
      mkConstructor(Array(classOf[Array[Byte]], classOf[Seq[_]])) { args =>
        new COrUncheckedNode(args(0).asInstanceOf[Challenge], args(1).asInstanceOf[Seq[UncheckedSigmaTree]])
      }
    )
  )

  registerClassEntry(classOf[COrUnproven],
    constructors = Array(
      mkConstructor(Array(classOf[COR], classOf[Option[_]], classOf[Boolean], classOf[Seq[_]], classOf[NodePosition])) { args =>
        new COrUnproven(args(0).asInstanceOf[COR],
          args(1).asInstanceOf[Option[Challenge]],
          args(2).asInstanceOf[Boolean],
          args(3).asInstanceOf[Seq[ProofTree]],
          args(4).asInstanceOf[NodePosition])
      }
    )
  )

  registerClassEntry(classOf[CThresholdUncheckedNode],
    constructors = Array(
      mkConstructor(Array(classOf[Array[Byte]], classOf[Seq[_]], classOf[java.lang.Integer], classOf[Option[_]])) { args =>
        new CThresholdUncheckedNode(args(0).asInstanceOf[Challenge],
          args(1).asInstanceOf[Seq[UncheckedSigmaTree]],
          args(2).asInstanceOf[java.lang.Integer],
          args(3).asInstanceOf[Option[GF2_192_Poly]])
      }
    )
  )

  registerClassEntry(classOf[CThresholdUnproven],
    constructors = Array(
      mkConstructor(Array(classOf[CTHRESHOLD], classOf[Option[_]], classOf[Boolean], classOf[java.lang.Integer], classOf[Seq[_]], classOf[Option[_]], classOf[NodePosition])) { args =>
        new CThresholdUnproven(args(0).asInstanceOf[CTHRESHOLD],
          args(1).asInstanceOf[Option[Challenge]],
          args(2).asInstanceOf[Boolean],
          args(3).asInstanceOf[java.lang.Integer],
          args(4).asInstanceOf[Seq[ProofTree]],
          args(5).asInstanceOf[Option[GF2_192_Poly]],
          args(6).asInstanceOf[NodePosition])
      }
    )
  )
}
