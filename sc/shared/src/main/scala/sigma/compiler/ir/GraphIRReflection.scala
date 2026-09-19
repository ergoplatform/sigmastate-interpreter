package sigma.compiler.ir

import sigma.Coll
import sigma.{BigInt, SigmaDslBuilder}
import sigma.ast.SType
import sigma.compiler.ir.primitives.Thunks
import sigma.data.RType
import sigma.reflection.ReflectionData.registerClassEntry
import sigma.reflection.{ReflectionData, mkConstructor, mkMethod}
import sigma.compiler.ir.wrappers.sigma.SigmaDsl

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


  { val clazz = classOf[SigmaDsl#SigmaDslBuilder]
    val ctx = null.asInstanceOf[IRContext] // ok! type level only
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "byteArrayToLong", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].byteArrayToLong(args(0).asInstanceOf[ctx.Ref[sigma.Coll[Byte]]])
        },
        mkMethod(clazz, "anyOf", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].anyOf(args(0).asInstanceOf[ctx.Ref[sigma.Coll[Boolean]]])
        },
        mkMethod(clazz, "allZK", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].allZK(args(0).asInstanceOf[ctx.Ref[sigma.Coll[sigma.SigmaProp]]])
        },
        mkMethod(clazz, "xorOf", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].xorOf(args(0).asInstanceOf[ctx.Ref[sigma.Coll[Boolean]]])
        },
        mkMethod(clazz, "substConstants", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].substConstants(args(0).asInstanceOf[ctx.Ref[sigma.Coll[Byte]]],
            args(1).asInstanceOf[ctx.Ref[sigma.Coll[Int]]], args(2).asInstanceOf[ctx.Ref[sigma.Coll[Any]]])
        },
        mkMethod(clazz, "sigmaProp", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].sigmaProp(args(0).asInstanceOf[ctx.Ref[Boolean]])
        },
        mkMethod(clazz, "groupGenerator", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].groupGenerator
        },
        mkMethod(clazz, "proveDlog", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].proveDlog(args(0).asInstanceOf[ctx.Ref[sigma.GroupElement]])
        },
        mkMethod(clazz, "blake2b256", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].blake2b256(args(0).asInstanceOf[ctx.Ref[sigma.Coll[Byte]]])
        },
        mkMethod(clazz, "longToByteArray", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].longToByteArray(args(0).asInstanceOf[ctx.Ref[Long]])
        },
        mkMethod(clazz, "xor", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].xor(args(0).asInstanceOf[ctx.Ref[sigma.Coll[Byte]]],
            args(1).asInstanceOf[ctx.Ref[sigma.Coll[Byte]]])
        },
        mkMethod(clazz, "atLeast", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].atLeast(args(0).asInstanceOf[ctx.Ref[Int]],
            args(1).asInstanceOf[ctx.Ref[sigma.Coll[sigma.SigmaProp]]])
        },
        mkMethod(clazz, "byteArrayToBigInt", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].byteArrayToBigInt(args(0).asInstanceOf[ctx.Ref[sigma.Coll[Byte]]])
        },
        mkMethod(clazz, "allOf", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].allOf(args(0).asInstanceOf[ctx.Ref[sigma.Coll[Boolean]]])
        },
        mkMethod(clazz, "proveDHTuple", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]], classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].proveDHTuple(args(0).asInstanceOf[ctx.Ref[sigma.GroupElement]],
            args(1).asInstanceOf[ctx.Ref[sigma.GroupElement]],
            args(2).asInstanceOf[ctx.Ref[sigma.GroupElement]],
            args(3).asInstanceOf[ctx.Ref[sigma.GroupElement]])
        },
        mkMethod(clazz, "anyZK", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].anyZK(args(0).asInstanceOf[ctx.Ref[sigma.Coll[sigma.SigmaProp]]])
        },
        mkMethod(clazz, "sha256", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].sha256(args(0).asInstanceOf[ctx.Ref[sigma.Coll[Byte]]])
        },
        mkMethod(clazz, "decodePoint", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].decodePoint(args(0).asInstanceOf[ctx.Ref[sigma.Coll[Byte]]])
        },
        mkMethod(clazz, "serialize", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].serialize(args(0).asInstanceOf[ctx.Ref[Any]])
        },
        mkMethod(clazz, "powHit", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]],
          classOf[Base#Ref[_]], classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].powHit(args(0).asInstanceOf[ctx.Ref[Int]],
            args(1).asInstanceOf[ctx.Ref[sigma.Coll[Byte]]], args(2).asInstanceOf[ctx.Ref[sigma.Coll[Byte]]],
            args(3).asInstanceOf[ctx.Ref[sigma.Coll[Byte]]], args(4).asInstanceOf[ctx.Ref[Int]])
        },
        mkMethod(clazz, "encodeNbits", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].encodeNbits(args(0).asInstanceOf[ctx.Ref[sigma.BigInt]])
        },
        mkMethod(clazz, "decodeNbits", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].decodeNbits(args(0).asInstanceOf[ctx.Ref[Long]])
        },
        mkMethod(clazz, "fromBigEndianBytes", Array[Class[_]](classOf[Base#Ref[_]], classOf[TypeDescs#Elem[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].fromBigEndianBytes(args(0).asInstanceOf[ctx.Ref[sigma.Coll[Byte]]])(args(1).asInstanceOf[ctx.Elem[SType]])
        },
        mkMethod(clazz, "deserializeTo", Array[Class[_]](classOf[Base#Ref[_]], classOf[TypeDescs#Elem[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].deserializeTo(args(0).asInstanceOf[ctx.Ref[sigma.Coll[Byte]]])(args(1).asInstanceOf[ctx.Elem[SType]])
        },
        mkMethod(clazz, "some", Array[Class[_]](classOf[Base#Ref[_]], classOf[TypeDescs#Elem[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].some(args(0).asInstanceOf[ctx.Ref[Any]])(args(1).asInstanceOf[ctx.Elem[Any]])
        },
        mkMethod(clazz, "none", Array[Class[_]](classOf[TypeDescs#Elem[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].none()(args(0).asInstanceOf[ctx.Elem[SType]])
        }
      )
    )
  }


}
