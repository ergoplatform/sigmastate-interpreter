package sigma.compiler.ir

import sigma.ast.SType
import sigma.compiler.ir.primitives.Thunks
import sigma.data.RType
import sigma.reflection.ReflectionData.registerClassEntry
import sigma.reflection.{ReflectionData, mkConstructor, mkMethod}
import sigma.compiler.ir.wrappers.{OptionWrapSpec, RTypeWrapSpec}
import sigma.compiler.ir.wrappers.sigma.{SigmaDsl, WRTypes}

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
        mkMethod(clazz, "distinct", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[ctx.Coll[Any]].distinct
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
    val clazz = classOf[SigmaDsl#AvlTree]
    val ctx = null.asInstanceOf[IRContext] // ok! type level only
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "updateOperations", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.AvlTree].updateOperations(args(0).asInstanceOf[ctx.Ref[Byte]])
        },
        mkMethod(clazz, "getMany", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.AvlTree].getMany(args(0).asInstanceOf[ctx.Ref[ctx.Coll[ctx.Coll[Byte]]]],
            args(1).asInstanceOf[ctx.Ref[ctx.Coll[Byte]]])
        },
        mkMethod(clazz, "update", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.AvlTree].update(args(0).asInstanceOf[ctx.Ref[ctx.Coll[(ctx.Coll[Byte], ctx.Coll[Byte])]]],
            args(1).asInstanceOf[ctx.Ref[ctx.Coll[Byte]]])
        },
        mkMethod(clazz, "keyLength", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[ctx.AvlTree].keyLength
        },
        mkMethod(clazz, "enabledOperations", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[ctx.AvlTree].enabledOperations
        },
        mkMethod(clazz, "updateDigest", Array[Class[_]](classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.AvlTree].updateDigest(args(0).asInstanceOf[ctx.Ref[ctx.Coll[Byte]]])
        },
        mkMethod(clazz, "digest", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[ctx.AvlTree].digest
        },
        mkMethod(clazz, "insert", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.AvlTree].insert(args(0).asInstanceOf[ctx.Ref[ctx.Coll[(ctx.Coll[Byte], ctx.Coll[Byte])]]],
            args(1).asInstanceOf[ctx.Ref[ctx.Coll[Byte]]])
        },
        mkMethod(clazz, "isRemoveAllowed", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[ctx.AvlTree].isRemoveAllowed
        },
        mkMethod(clazz, "valueLengthOpt", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[ctx.AvlTree].valueLengthOpt
        },
        mkMethod(clazz, "get", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.AvlTree].get(args(0).asInstanceOf[ctx.Ref[ctx.Coll[Byte]]],
            args(1).asInstanceOf[ctx.Ref[ctx.Coll[Byte]]])
        },
        mkMethod(clazz, "remove", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.AvlTree].remove(args(0).asInstanceOf[ctx.Ref[ctx.Coll[ctx.Coll[Byte]]]], args(1).asInstanceOf[ctx.Ref[ctx.Coll[Byte]]])
        },
        mkMethod(clazz, "contains", Array[Class[_]](classOf[Base#Ref[_]], classOf[Base#Ref[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.AvlTree].contains(args(0).asInstanceOf[ctx.Ref[ctx.Coll[Byte]]],
            args(1).asInstanceOf[ctx.Ref[ctx.Coll[Byte]]])
        },
        mkMethod(clazz, "isUpdateAllowed", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[ctx.AvlTree].isUpdateAllowed
        },
        mkMethod(clazz, "isInsertAllowed", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[ctx.AvlTree].isInsertAllowed
        }
      )
    )
  }

  { val clazz = classOf[SigmaDsl#Box]
    val ctx = null.asInstanceOf[IRContext] // ok! type level only
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "value", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[ctx.Box].value
        },
        mkMethod(clazz, "id", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[ctx.Box].id
        },
        mkMethod(clazz, "creationInfo", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[ctx.Box].creationInfo
        },
        mkMethod(clazz, "bytes", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[ctx.Box].bytes
        },
        mkMethod(clazz, "getReg", Array[Class[_]](classOf[Base#Ref[_]], classOf[TypeDescs#Elem[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.Box].getReg(args(0).asInstanceOf[ctx.Ref[Int]])(args(1).asInstanceOf[ctx.Elem[_]])
        },
        mkMethod(clazz, "tokens", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[ctx.Box].tokens
        },
        mkMethod(clazz, "bytesWithoutRef", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.Box].bytesWithoutRef
        },
        mkMethod(clazz, "propositionBytes", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.Box].propositionBytes
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
  
  { val clazz = classOf[SigmaDsl#Header]
    val ctx = null.asInstanceOf[IRContext] // ok! type level only
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "minerPk", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.Header].minerPk
        },
        mkMethod(clazz, "powNonce", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.Header].powNonce
        },
        mkMethod(clazz, "transactionsRoot", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.Header].transactionsRoot
        },
        mkMethod(clazz, "powOnetimePk", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.Header].powOnetimePk
        },
        mkMethod(clazz, "nBits", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.Header].nBits
        },
        mkMethod(clazz, "votes", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.Header].votes
        },
        mkMethod(clazz, "id", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.Header].id
        },
        mkMethod(clazz, "version", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.Header].version
        },
        mkMethod(clazz, "parentId", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.Header].parentId
        },
        mkMethod(clazz, "timestamp", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.Header].timestamp
        },
        mkMethod(clazz, "height", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.Header].height
        },
        mkMethod(clazz, "extensionRoot", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.Header].extensionRoot
        },
        mkMethod(clazz, "ADProofsRoot", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.Header].ADProofsRoot
        },
        mkMethod(clazz, "stateRoot", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.Header].stateRoot
        },
        mkMethod(clazz, "powDistance", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.Header].powDistance
        },
        mkMethod(clazz, "checkPow", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[ctx.Header].checkPow
        }
      )
    )
  }
  
  { val clazz = classOf[SigmaDsl#PreHeader]
    val ctx = null.asInstanceOf[IRContext] // ok! type level only
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "minerPk", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[ctx.PreHeader].minerPk
        },
        mkMethod(clazz, "votes", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[ctx.PreHeader].votes
        },
        mkMethod(clazz, "nBits", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[ctx.PreHeader].nBits
        },
        mkMethod(clazz, "version", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[ctx.PreHeader].version
        },
        mkMethod(clazz, "timestamp", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[ctx.PreHeader].timestamp
        },
        mkMethod(clazz, "parentId", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[ctx.PreHeader].parentId
        },
        mkMethod(clazz, "height", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[ctx.PreHeader].height
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
        mkMethod(clazz, "fromBigEndianBytes", Array[Class[_]](classOf[Base#Ref[_]], classOf[TypeDescs#Elem[_]])) { (obj, args) =>
          obj.asInstanceOf[ctx.SigmaDslBuilder].fromBigEndianBytes(args(0).asInstanceOf[ctx.Ref[ctx.Coll[Byte]]])(args(1).asInstanceOf[ctx.Elem[SType]])
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

  { val clazz = classOf[WRTypes#WRType[_]]
    val ctx = null.asInstanceOf[IRContext] // ok! type level only
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "name", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[ctx.WRType[_]].name
        }
      )
    )
  }

  { val ctx = null.asInstanceOf[IRContext] // ok! type level only
    val clazz = classOf[ctx.WRType.WRTypeElem[_, _]]
    registerClassEntry(clazz,
      constructors = Array(
        mkConstructor(Array(classOf[ctx.WRTypeCls], classOf[TypeDescs#Elem[_]])) { args =>
          val entityObj = args(0).asInstanceOf[ctx.WRTypeCls]
          new entityObj.WRTypeElem()(args(1).asInstanceOf[ctx.Elem[_]])
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

  {
    val clazz = classOf[OptionWrapSpec]
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "getOrElse", Array[Class[_]](classOf[Option[_]], classOf[Function0[_]])) { (obj, args) =>
          val opt = args(0).asInstanceOf[Option[Any]]
          val defaultFunc = args(1).asInstanceOf[Function0[Any]]
          obj.asInstanceOf[OptionWrapSpec].getOrElse(opt, defaultFunc())
        },
        mkMethod(clazz, "isDefined", Array[Class[_]](classOf[Option[_]])) { (obj, args) =>
          obj.asInstanceOf[OptionWrapSpec].isDefined(args(0).asInstanceOf[Option[_]])
        },
        mkMethod(clazz, "filter", Array[Class[_]](classOf[Option[_]], classOf[Function1[_, _]])) { (obj, args) =>
          obj.asInstanceOf[OptionWrapSpec].filter(
            args(0).asInstanceOf[Option[Any]], args(1).asInstanceOf[Any => Boolean])
        },
        mkMethod(clazz, "map", Array[Class[_]](classOf[Option[_]], classOf[Function1[_, _]])) { (obj, args) =>
          obj.asInstanceOf[OptionWrapSpec].map(
            args(0).asInstanceOf[Option[Any]], args(1).asInstanceOf[Any => Any])
        },
        mkMethod(clazz, "get", Array[Class[_]](classOf[Option[_]])) { (obj, args) =>
          obj.asInstanceOf[OptionWrapSpec].get(args(0).asInstanceOf[Option[_]])
        }
      )
    )
  }
  {
    val clazz = classOf[RTypeWrapSpec]
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "name", Array[Class[_]](classOf[RType[_]])) { (obj, args) =>
          obj.asInstanceOf[RTypeWrapSpec].name(args(0).asInstanceOf[RType[_]])
        }
      )
    )
  }
}
