package sigma.compiler.ir

import sigma.compiler.ir.primitives.Thunks
import sigma.reflection.ReflectionData.registerClassEntry
import sigma.reflection.{ReflectionData, mkConstructor}

/** Registrations of reflection metadata for graph-ir module (see README.md).
  * Such metadata is only used on JS platform to support reflection-like interfaces of
  * RClass, RMethod, RConstructor. These interfaces implemented on JVM using Java
  * reflection.
  *
  * For each class of this module that needs reflection metadata,
  * we register a class entry with the necessary information.
  * Only information that is needed at runtime is registered.
  */
object GraphIRReflection {
  /** Forces initialization of reflection data. */
  val reflection = ReflectionData

  registerClassEntry(classOf[TypeDescs#FuncElem[_,_]],
    constructors = Array(
      mkConstructor(Array(classOf[IRContext], classOf[TypeDescs#Elem[_]], classOf[TypeDescs#Elem[_]])) { args =>
        val ctx = args(0).asInstanceOf[IRContext]
        new ctx.FuncElem(args(1).asInstanceOf[ctx.Elem[_]], args(2).asInstanceOf[ctx.Elem[_]])
      }
    )
  )

  registerClassEntry(classOf[TypeDescs#PairElem[_,_]],
    constructors = Array(
      mkConstructor(Array(classOf[IRContext], classOf[TypeDescs#Elem[_]], classOf[TypeDescs#Elem[_]])) { args =>
        val ctx = args(0).asInstanceOf[IRContext]
        new ctx.PairElem(args(1).asInstanceOf[ctx.Elem[_]], args(2).asInstanceOf[ctx.Elem[_]])
      }
    )
  )

  registerClassEntry(classOf[Thunks#ThunkElem[_]],
    constructors = Array(
      mkConstructor(Array(classOf[IRContext], classOf[TypeDescs#Elem[_]])) { args =>
        val ctx = args(0).asInstanceOf[IRContext]
        new ctx.ThunkElem(args(1).asInstanceOf[ctx.Elem[_]])
      }
    )
  )




}
