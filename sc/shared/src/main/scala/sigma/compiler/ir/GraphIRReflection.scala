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

  { val clazz = classOf[sigma.compiler.ir.wrappers.sigma.SigmaDsl#SigmaProp]
    val ctx = null.asInstanceOf[SigmaDsl] // ok! type level only
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "$bar$bar", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaProp].$bar$bar(args(0).asInstanceOf[ctx.Ref[ctx.SigmaProp]])
        },
        mkMethod(clazz, "isValid", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[ctx.SigmaProp].isValid
        },
        mkMethod(clazz, "propBytes", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[ctx.SigmaProp].propBytes
        },
        mkMethod(clazz, "$amp$amp", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaProp].$amp$amp(args(0).asInstanceOf[ctx.Ref[ctx.SigmaProp]])
        }
      )
    )
  }

  { val clazz = classOf[sigma.compiler.ir.wrappers.sigma.Colls#CollBuilder]
    val ctx = null.asInstanceOf[IRContext] // ok! type level only
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "xor", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.CollBuilder].xor(
            args(0).asInstanceOf[ctx.Ref[ctx.Coll[Byte]]],
            args(1).asInstanceOf[ctx.Ref[ctx.Coll[Byte]]])
        },
        mkMethod(clazz, "fromItems", Array[Class[_]](classOf[Seq[_]], classOf[TypeDescs#Elem[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.CollBuilder].fromItems[Any](
            args(0).asInstanceOf[Seq[ctx.Ref[Any]]]:_*)(args(1).asInstanceOf[ctx.Elem[Any]])
        },
        mkMethod(clazz, "replicate", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.CollBuilder].replicate(args(0).asInstanceOf[ctx.Ref[Int]],
            args(1).asInstanceOf[ctx.Ref[Any]])
        }
      )
    )
  }

  {
    val clazz = classOf[sigma.compiler.ir.wrappers.sigma.Colls#Coll[_]]
    val ctx = null.asInstanceOf[IRContext] // ok! type level only
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "append", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[Any]].append(args(0).asInstanceOf[ctx.Ref[ctx.Coll[Any]]])
        },
        mkMethod(clazz, "forall", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[Any]].forall(args(0).asInstanceOf[ctx.Ref[Any => Boolean]])
        },
        mkMethod(clazz, "length", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[_]].length
        },
        mkMethod(clazz, "updateMany", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[Any]].updateMany(
            args(0).asInstanceOf[ctx.Ref[ctx.Coll[Int]]],
            args(1).asInstanceOf[ctx.Ref[ctx.Coll[Any]]])
        },
        mkMethod(clazz, "updated", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[Any]].updated(
            args(0).asInstanceOf[ctx.Ref[Int]], args(1).asInstanceOf[ctx.Ref[Any]])
        },
        mkMethod(clazz, "patch", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[Any]].patch(args(0).asInstanceOf[ctx.Ref[Int]],
            args(1).asInstanceOf[ctx.Ref[ctx.Coll[Any]]],
            args(2).asInstanceOf[ctx.Ref[Int]])
        },
        mkMethod(clazz, "foldLeft", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[Any]].foldLeft(args(0).asInstanceOf[ctx.Ref[Any]],
            args(1).asInstanceOf[ctx.Ref[((Any, Any)) => Any]])
        },
        mkMethod(clazz, "flatMap", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[Any]].flatMap(args(0).asInstanceOf[ctx.Ref[Any => ctx.Coll[Any]]])
        },
        mkMethod(clazz, "filter", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[Any]].filter(args(0).asInstanceOf[ctx.Ref[Any => Boolean]])
        },
        mkMethod(clazz, "slice", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[Any]].slice(args(0).asInstanceOf[ctx.Ref[Int]],
            args(1).asInstanceOf[ctx.Ref[Int]])
        },
        mkMethod(clazz, "indices", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[_]].indices
        },
        mkMethod(clazz, "zip", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[Any]].zip(args(0).asInstanceOf[ctx.Ref[ctx.Coll[Any]]])
        },
        mkMethod(clazz, "map", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[Any]].map(args(0).asInstanceOf[ctx.Ref[Any => Any]])
        },
        mkMethod(clazz, "getOrElse", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[Any]].getOrElse(args(0).asInstanceOf[ctx.Ref[Int]], args(1).asInstanceOf[ctx.Ref[Any]])
        },
        mkMethod(clazz, "indexOf", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[Any]].indexOf(args(0).asInstanceOf[ctx.Ref[Any]], args(1).asInstanceOf[ctx.Ref[Int]])
        },
        mkMethod(clazz, "apply", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[_]].apply(args(0).asInstanceOf[ctx.Ref[Int]])
        },
        mkMethod(clazz, "exists", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[Any]].exists(args(0).asInstanceOf[ctx.Ref[Any => Boolean]])
        },
        // V6 methods
        mkMethod(clazz, "reverse", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[ctx.Coll[Any]].reverse
        },
        mkMethod(clazz, "startsWith", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[Any]].startsWith(args(0).asInstanceOf[ctx.Ref[ctx.Coll[Any]]])
        },
        mkMethod(clazz, "endsWith", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[Any]].endsWith(args(0).asInstanceOf[ctx.Ref[ctx.Coll[Any]]])
        },
        mkMethod(clazz, "get", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Coll[_]].apply(args(0).asInstanceOf[ctx.Ref[Int]])
        }
      )
    )
  }
  { val clazz = classOf[SigmaDsl#SigmaDslBuilder]
    val ctx = null.asInstanceOf[IRContext] // ok! type level only
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "byteArrayToLong", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].byteArrayToLong(args(0).asInstanceOf[ctx.Ref[ctx.Coll[Byte]]])
        },
        mkMethod(clazz, "anyOf", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].anyOf(args(0).asInstanceOf[ctx.Ref[ctx.Coll[Boolean]]])
        },
        mkMethod(clazz, "allZK", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].allZK(args(0).asInstanceOf[ctx.Ref[ctx.Coll[ctx.SigmaProp]]])
        },
        mkMethod(clazz, "xorOf", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].xorOf(args(0).asInstanceOf[ctx.Ref[ctx.Coll[Boolean]]])
        },
        mkMethod(clazz, "substConstants", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].substConstants(args(0).asInstanceOf[ctx.Ref[ctx.Coll[Byte]]],
            args(1).asInstanceOf[ctx.Ref[ctx.Coll[Int]]], args(2).asInstanceOf[ctx.Ref[ctx.Coll[Any]]])
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
          obj.asInstanceOf[ctx.SigmaDslBuilder].blake2b256(args(0).asInstanceOf[ctx.Ref[ctx.Coll[Byte]]])
        },
        mkMethod(clazz, "longToByteArray", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].longToByteArray(args(0).asInstanceOf[ctx.Ref[Long]])
        },
        mkMethod(clazz, "xor", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].xor(args(0).asInstanceOf[ctx.Ref[ctx.Coll[Byte]]],
            args(1).asInstanceOf[ctx.Ref[ctx.Coll[Byte]]])
        },
        mkMethod(clazz, "atLeast", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].atLeast(args(0).asInstanceOf[ctx.Ref[Int]],
            args(1).asInstanceOf[ctx.Ref[ctx.Coll[ctx.SigmaProp]]])
        },
        mkMethod(clazz, "byteArrayToBigInt", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].byteArrayToBigInt(args(0).asInstanceOf[ctx.Ref[ctx.Coll[Byte]]])
        },
        mkMethod(clazz, "allOf", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].allOf(args(0).asInstanceOf[ctx.Ref[ctx.Coll[Boolean]]])
        },
        mkMethod(clazz, "proveDHTuple", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]], classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].proveDHTuple(args(0).asInstanceOf[ctx.Ref[sigma.GroupElement]],
            args(1).asInstanceOf[ctx.Ref[sigma.GroupElement]],
            args(2).asInstanceOf[ctx.Ref[sigma.GroupElement]],
            args(3).asInstanceOf[ctx.Ref[sigma.GroupElement]])
        },
        mkMethod(clazz, "anyZK", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].anyZK(args(0).asInstanceOf[ctx.Ref[ctx.Coll[ctx.SigmaProp]]])
        },
        mkMethod(clazz, "sha256", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].sha256(args(0).asInstanceOf[ctx.Ref[ctx.Coll[Byte]]])
        },
        mkMethod(clazz, "decodePoint", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].decodePoint(args(0).asInstanceOf[ctx.Ref[ctx.Coll[Byte]]])
        },
        mkMethod(clazz, "serialize", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].serialize(args(0).asInstanceOf[ctx.Ref[Any]])
        },
        mkMethod(clazz, "powHit", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]],
          classOf[Base#Ref[_]], classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].powHit(args(0).asInstanceOf[ctx.Ref[Int]],
            args(1).asInstanceOf[ctx.Ref[ctx.Coll[Byte]]], args(2).asInstanceOf[ctx.Ref[ctx.Coll[Byte]]],
            args(3).asInstanceOf[ctx.Ref[ctx.Coll[Byte]]], args(4).asInstanceOf[ctx.Ref[Int]])
        },
        mkMethod(clazz, "encodeNbits", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].encodeNbits(args(0).asInstanceOf[ctx.Ref[sigma.BigInt]])
        },
        mkMethod(clazz, "decodeNbits", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].decodeNbits(args(0).asInstanceOf[ctx.Ref[Long]])
        },
        mkMethod(clazz, "fromBigEndianBytes", Array[Class[_]](classOf[Base#Ref[_]], classOf[TypeDescs#Elem[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].fromBigEndianBytes(args(0).asInstanceOf[ctx.Ref[ctx.Coll[Byte]]])(args(1).asInstanceOf[ctx.Elem[SType]])
        },
        mkMethod(clazz, "deserializeTo", Array[Class[_]](classOf[Base#Ref[_]], classOf[TypeDescs#Elem[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].deserializeTo(args(0).asInstanceOf[ctx.Ref[ctx.Coll[Byte]]])(args(1).asInstanceOf[ctx.Elem[SType]])
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


  { val ctx = null.asInstanceOf[IRContext] // ok! type level only
    val clazz = classOf[ctx.Coll.CollElem[_, _]]
    registerClassEntry(clazz,
      constructors = Array(
        mkConstructor(Array(classOf[ctx.CollCls], classOf[TypeDescs#Elem[_]])) { args =>
          val entityObj = args(0).asInstanceOf[ctx.CollCls]
          new entityObj.CollElem()(args(1).asInstanceOf[ctx.Elem[_]])
        }
      )
    )
  }
}
