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

  { val clazz = classOf[sigma.compiler.ir.wrappers.scala.WOptions#WOption[_]]
    val ctx = null.asInstanceOf[IRContext] // ok! type level only
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "filter", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.WOption[Any]].filter(args(0).asInstanceOf[ctx.Ref[Any => Boolean]])
        },
        mkMethod(clazz, "get", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[ctx.WOption[_]].get
        },
        mkMethod(clazz, "isDefined", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[ctx.WOption[_]].isDefined
        },
        mkMethod(clazz, "getOrElse", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.WOption[Any]].getOrElse(args(0).asInstanceOf[ctx.Ref[ctx.Thunk[Any]]])
        },
        mkMethod(clazz, "map", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.WOption[Any]].map(args(0).asInstanceOf[ctx.Ref[Any => Any]])
        }
      )
    )
  }

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

  { val clazz = classOf[SigmaDsl#BigInt]
    val ctx = null.asInstanceOf[SigmaDsl] // ok! type level only
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "add", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.BigInt].add(args(0).asInstanceOf[ctx.Ref[ctx.BigInt]])
        },
        mkMethod(clazz, "max", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.BigInt].max(args(0).asInstanceOf[ctx.Ref[ctx.BigInt]])
        },
        mkMethod(clazz, "min", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.BigInt].min(args(0).asInstanceOf[ctx.Ref[ctx.BigInt]])
        },
        mkMethod(clazz, "subtract", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.BigInt].subtract(args(0).asInstanceOf[ctx.Ref[ctx.BigInt]])
        },
        mkMethod(clazz, "multiply", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.BigInt].multiply(args(0).asInstanceOf[ctx.Ref[ctx.BigInt]])
        },
        mkMethod(clazz, "mod", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.BigInt].mod(args(0).asInstanceOf[ctx.Ref[ctx.BigInt]])
        },
        mkMethod(clazz, "divide", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.BigInt].divide(args(0).asInstanceOf[ctx.Ref[ctx.BigInt]])
        },
        mkMethod(clazz, "toUnsigned", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[ctx.BigInt].toUnsigned()
        },
        mkMethod(clazz, "toUnsignedMod", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.BigInt].toUnsignedMod(args(0).asInstanceOf[ctx.Ref[ctx.UnsignedBigInt]])
        }
      )
    )
  }

  { val clazz = classOf[SigmaDsl#UnsignedBigInt]
    val ctx = null.asInstanceOf[SigmaDsl] // ok! type level only
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "add", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.UnsignedBigInt].add(args(0).asInstanceOf[ctx.Ref[ctx.UnsignedBigInt]])
        },
        mkMethod(clazz, "max", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.UnsignedBigInt].max(args(0).asInstanceOf[ctx.Ref[ctx.UnsignedBigInt]])
        },
        mkMethod(clazz, "min", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.UnsignedBigInt].min(args(0).asInstanceOf[ctx.Ref[ctx.UnsignedBigInt]])
        },
        mkMethod(clazz, "subtract", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.UnsignedBigInt].subtract(args(0).asInstanceOf[ctx.Ref[ctx.UnsignedBigInt]])
        },
        mkMethod(clazz, "multiply", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.UnsignedBigInt].multiply(args(0).asInstanceOf[ctx.Ref[ctx.UnsignedBigInt]])
        },
        mkMethod(clazz, "mod", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.UnsignedBigInt].mod(args(0).asInstanceOf[ctx.Ref[ctx.UnsignedBigInt]])
        },
        mkMethod(clazz, "divide", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.UnsignedBigInt].divide(args(0).asInstanceOf[ctx.Ref[ctx.UnsignedBigInt]])
        },
        mkMethod(clazz, "plusMod", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.UnsignedBigInt].plusMod(args(0).asInstanceOf[ctx.Ref[ctx.UnsignedBigInt]], args(1).asInstanceOf[ctx.Ref[ctx.UnsignedBigInt]])
        },
        mkMethod(clazz, "subtractMod", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.UnsignedBigInt].subtractMod(args(0).asInstanceOf[ctx.Ref[ctx.UnsignedBigInt]], args(1).asInstanceOf[ctx.Ref[ctx.UnsignedBigInt]])
        },
        mkMethod(clazz, "multiplyMod", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.UnsignedBigInt].multiplyMod(args(0).asInstanceOf[ctx.Ref[ctx.UnsignedBigInt]], args(1).asInstanceOf[ctx.Ref[ctx.UnsignedBigInt]])
        },
        mkMethod(clazz, "mod", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.UnsignedBigInt].mod(args(0).asInstanceOf[ctx.Ref[ctx.UnsignedBigInt]])
        },
        mkMethod(clazz, "modInverse", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.UnsignedBigInt].modInverse(args(0).asInstanceOf[ctx.Ref[ctx.UnsignedBigInt]])
        },
        mkMethod(clazz, "toSigned", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[ctx.UnsignedBigInt].toSigned
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
  {
    val clazz = classOf[SigmaDsl#Context]
    val ctx = null.asInstanceOf[IRContext] // ok! type level only
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "LastBlockUtxoRootHash", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.Context].LastBlockUtxoRootHash
        },
        mkMethod(clazz, "dataInputs", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.Context].dataInputs
        },
        mkMethod(clazz, "selfBoxIndex", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.Context].selfBoxIndex
        },
        mkMethod(clazz, "INPUTS", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.Context].INPUTS
        },
        mkMethod(clazz, "minerPubKey", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.Context].minerPubKey
        },
        mkMethod(clazz, "HEIGHT", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.Context].HEIGHT
        },
        mkMethod(clazz, "OUTPUTS", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.Context].OUTPUTS
        },
        mkMethod(clazz, "SELF", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.Context].SELF
        },
        mkMethod(clazz, "preHeader", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.Context].preHeader
        },
        mkMethod(clazz, "getVar", Array[Class[_]](classOf[Base#Ref[_]], classOf[TypeDescs#Elem[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Context].getVar(args(0).asInstanceOf[ctx.Ref[Byte]])(args(1).asInstanceOf[ctx.Elem[_]])
        },
        mkMethod(clazz, "getVarFromInput", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]], classOf[TypeDescs#Elem[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Context].getVarFromInput(args(0).asInstanceOf[ctx.Ref[Short]], args(1).asInstanceOf[ctx.Ref[Byte]])(args(2).asInstanceOf[ctx.Elem[_]])
        },
        mkMethod(clazz, "headers", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.Context].headers
        }
      )
    )
  }
  
  { val clazz = classOf[SigmaDsl#GroupElement]
    val ctx = null.asInstanceOf[IRContext] // ok! type level only
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "exp", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.GroupElement].exp(args(0).asInstanceOf[ctx.Ref[ctx.BigInt]])
        },
        mkMethod(clazz, "expUnsigned", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.GroupElement].expUnsigned(args(0).asInstanceOf[ctx.Ref[ctx.UnsignedBigInt]])
        },
        mkMethod(clazz, "multiply", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.GroupElement].multiply(args(0).asInstanceOf[ctx.Ref[ctx.GroupElement]])
        },
        mkMethod(clazz, "getEncoded", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.GroupElement].getEncoded
        },
        mkMethod(clazz, "negate", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.GroupElement].negate
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
          obj.asInstanceOf[ctx.SigmaDslBuilder].proveDlog(args(0).asInstanceOf[ctx.Ref[ctx.GroupElement]])
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
          obj.asInstanceOf[ctx.SigmaDslBuilder].proveDHTuple(args(0).asInstanceOf[ctx.Ref[ctx.GroupElement]],
            args(1).asInstanceOf[ctx.Ref[ctx.GroupElement]],
            args(2).asInstanceOf[ctx.Ref[ctx.GroupElement]],
            args(3).asInstanceOf[ctx.Ref[ctx.GroupElement]])
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
          obj.asInstanceOf[ctx.SigmaDslBuilder].encodeNbits(args(0).asInstanceOf[ctx.Ref[ctx.BigInt]])
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
    val clazz = classOf[ctx.WOption.WOptionElem[_, _]]
    registerClassEntry(clazz,
      constructors = Array(
        mkConstructor(Array(classOf[ctx.WOptionCls], classOf[TypeDescs#Elem[_]])) { args =>
          val entityObj = args(0).asInstanceOf[ctx.WOptionCls]
          new entityObj.WOptionElem()(args(1).asInstanceOf[ctx.Elem[_]])
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
