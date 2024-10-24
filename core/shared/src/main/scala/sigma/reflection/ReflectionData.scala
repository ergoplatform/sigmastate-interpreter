package sigma.reflection

import sigma._
import sigma.ast.{SCollectionType, SOption, STuple, SType}
import sigma.data.RType

import scala.collection.compat.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.immutable

/** Reflection metadata and global dictionaries to access it.
  * Such metadata is only used on JS platform to support reflection-like interfaces of
  * RClass, RMethod, RConstructor. These interfaces implemented on JVM using Java
  * reflection.
  *
  * For each class that needs reflection metadata, we register a class entry using
  * `registerClassEntry` method with the necessary information such as constructors and
  * methods.
  * Only information that is needed at runtime is registered.
  * @see mkConstructor, mkMethod
  */
object ReflectionData {
  /** Descriptors of classes. */
  val classes = mutable.HashMap.empty[Class[_], SRClass[_]]

  /** Registers a class entry in the map of classes.
    *
    * @tparam T the type of the class to be registered
    * @param clazz        the class to be registered
    * @param constructors the constructors of the class
    * @param fields       the fields of the class
    * @param methods      the methods of the class (represented as a map of method names and argument types to the corresponding RMethod instances)
    */
  def registerClassEntry[T](clazz: Class[T],
                            constructors: Seq[SRConstructor[_]] = ArraySeq.empty,
                            fields: Map[String, SRField] = Map.empty,
                            methods: Map[(String, Seq[Class[_]]), RMethod] = Map.empty): Unit = classes.synchronized {
    classes.put(clazz, new SRClass(clazz, constructors, fields, methods))
  }

  registerClassEntry(classOf[Boolean])

  registerClassEntry(classOf[Byte])

  registerClassEntry(classOf[Short])

  registerClassEntry(classOf[Int])

  registerClassEntry(classOf[Long])

  registerClassEntry(classOf[Product2[_, _]])

  registerClassEntry(classOf[immutable.$colon$colon[_]],
    constructors = Array(
      mkConstructor(Array(classOf[java.lang.Object], classOf[immutable.List[_]])) { args =>
        new immutable.$colon$colon(args(0).asInstanceOf[java.lang.Object], args(1).asInstanceOf[immutable.List[_]])
      }
    )
  )

  { val clazz = classOf[scala.Option[_]]
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "filter", Array(classOf[scala.Function1[_,_]])) { (obj, args) =>
          obj.asInstanceOf[Option[Any]].filter(args(0).asInstanceOf[Any => Boolean])
        },
        mkMethod(clazz, "map", Array(classOf[scala.Function1[_,_]])) { (obj, args) =>
          obj.asInstanceOf[Option[Any]].map(args(0).asInstanceOf[Any => Any])
        }
      )
    )
  }

  { val clazz = classOf[scala.Some[_]]
    registerClassEntry(clazz,
      constructors = Array(
        mkConstructor(Array(classOf[java.lang.Object])) { args =>
          new scala.Some(args(0).asInstanceOf[java.lang.Object])
        }
      )
    )
  }
  {
    val clazz = classOf[SigmaProp]
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "$bar$bar", Array[Class[_]](classOf[SigmaProp])) { (obj, args) =>
          obj.asInstanceOf[SigmaProp].$bar$bar(args(0).asInstanceOf[SigmaProp])
        },
        mkMethod(clazz, "isValid", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[SigmaProp].isValid
        },
        mkMethod(clazz, "propBytes", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[SigmaProp].propBytes
        },
        mkMethod(clazz, "$amp$amp", Array[Class[_]](classOf[SigmaProp])) { (obj, args) =>
          obj.asInstanceOf[SigmaProp].$amp$amp(args(0).asInstanceOf[SigmaProp])
        }
      )
    )
  }
  {
    val clazz      = classOf[sigma.BigInt]
    val paramTypes = Array[Class[_]](clazz)
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "add", paramTypes) { (obj, args) =>
          obj.asInstanceOf[BigInt].add(args(0).asInstanceOf[BigInt])
        },
        mkMethod(clazz, "max", paramTypes) { (obj, args) =>
          obj.asInstanceOf[BigInt].max(args(0).asInstanceOf[BigInt])
        },
        mkMethod(clazz, "min", paramTypes) { (obj, args) =>
          obj.asInstanceOf[BigInt].min(args(0).asInstanceOf[BigInt])
        },
        mkMethod(clazz, "subtract", paramTypes) { (obj, args) =>
          obj.asInstanceOf[BigInt].subtract(args(0).asInstanceOf[BigInt])
        },
        mkMethod(clazz, "multiply", paramTypes) { (obj, args) =>
          obj.asInstanceOf[BigInt].multiply(args(0).asInstanceOf[BigInt])
        },
        mkMethod(clazz, "mod", paramTypes) { (obj, args) =>
          obj.asInstanceOf[BigInt].mod(args(0).asInstanceOf[BigInt])
        },
        mkMethod(clazz, "divide", paramTypes) { (obj, args) =>
          obj.asInstanceOf[BigInt].divide(args(0).asInstanceOf[BigInt])
        }
      )
    )
  }
  {
    val clazz = classOf[CollBuilder]
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "xor", Array[Class[_]](classOf[Coll[_]], classOf[Coll[_]])) { (obj, args) =>
          obj.asInstanceOf[CollBuilder].xor(
            args(0).asInstanceOf[Coll[Byte]],
            args(1).asInstanceOf[Coll[Byte]])
        },
        mkMethod(clazz, "fromItems", Array[Class[_]](classOf[Seq[_]], classOf[RType[_]])) { (obj, args) =>
          obj.asInstanceOf[CollBuilder].fromItems[Any](
            args(0).asInstanceOf[Seq[Any]]: _*)(args(1).asInstanceOf[RType[Any]])
        },
        mkMethod(clazz, "replicate", Array[Class[_]](classOf[Int], classOf[Object], classOf[RType[_]])) { (obj, args) =>
          obj.asInstanceOf[CollBuilder].replicate(args(0).asInstanceOf[Int],
            args(1).asInstanceOf[Any])(args(2).asInstanceOf[RType[Any]])
        }
      )
    )
  }
  {
    val clazz = classOf[Coll[_]]
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "updated", Array[Class[_]](classOf[Int], classOf[java.lang.Object])) { (obj, args) =>
          obj.asInstanceOf[Coll[Any]].updated(args(0).asInstanceOf[Int], args(1).asInstanceOf[Any])
        },
        mkMethod(clazz, "flatMap", Array[Class[_]](classOf[Function1[_, _]], classOf[RType[_]])) { (obj, args) =>
          obj.asInstanceOf[Coll[Any]].flatMap(
            args(0).asInstanceOf[Any => Coll[Any]])(args(1).asInstanceOf[RType[Any]])
        },
        mkMethod(clazz, "apply", Array[Class[_]](classOf[Int])) { (obj, args) =>
          obj.asInstanceOf[Coll[_]].apply(args(0).asInstanceOf[Int])
        },
        mkMethod(clazz, "get", Array[Class[_]](classOf[Int])) { (obj, args) =>
          obj.asInstanceOf[Coll[_]].get(args(0).asInstanceOf[Int])
        },
        mkMethod(clazz, "append", Array[Class[_]](classOf[Coll[_]])) { (obj, args) =>
          obj.asInstanceOf[Coll[Any]].append(args(0).asInstanceOf[Coll[Any]])
        },
        mkMethod(clazz, "indexOf", Array[Class[_]](classOf[java.lang.Object], classOf[Int])) { (obj, args) =>
          obj.asInstanceOf[Coll[Any]].indexOf(args(0).asInstanceOf[Any], args(1).asInstanceOf[Int])
        },
        mkMethod(clazz, "map", Array[Class[_]](classOf[Function1[_, _]], classOf[RType[_]])) { (obj, args) =>
          obj.asInstanceOf[Coll[Any]].map(args(0).asInstanceOf[Any => Any])(args(1).asInstanceOf[RType[Any]])
        },
        mkMethod(clazz, "reverse", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[Coll[Any]].reverse
        },
        mkMethod(clazz, "distinct", Array[Class[_]]()) { (obj, args) =>
          obj.asInstanceOf[Coll[Any]].distinct
        },
        mkMethod(clazz, "startsWith", Array[Class[_]](classOf[Coll[_]])) { (obj, args) =>
          obj.asInstanceOf[Coll[Any]].startsWith(args(0).asInstanceOf[Coll[Any]])
        },
        mkMethod(clazz, "endsWith", Array[Class[_]](classOf[Coll[_]])) { (obj, args) =>
          obj.asInstanceOf[Coll[Any]].endsWith(args(0).asInstanceOf[Coll[Any]])
        }
      )
    )
  }
  {
    val clazz = classOf[AvlTree]
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "updateOperations", Array[Class[_]](classOf[Byte])) { (obj, args) =>
          obj.asInstanceOf[AvlTree].updateOperations(args(0).asInstanceOf[Byte])
        },
        mkMethod(clazz, "keyLength", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[AvlTree].keyLength
        },
        mkMethod(clazz, "enabledOperations", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[AvlTree].enabledOperations
        },
        mkMethod(clazz, "updateDigest", Array[Class[_]](classOf[Coll[_]])) { (obj, args) =>
          obj.asInstanceOf[AvlTree].updateDigest(args(0).asInstanceOf[Coll[Byte]])
        },
        mkMethod(clazz, "digest", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[AvlTree].digest
        },
        mkMethod(clazz, "isRemoveAllowed", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[AvlTree].isRemoveAllowed
        },
        mkMethod(clazz, "valueLengthOpt", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[AvlTree].valueLengthOpt
        },
        mkMethod(clazz, "isUpdateAllowed", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[AvlTree].isUpdateAllowed
        },
        mkMethod(clazz, "isInsertAllowed", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[AvlTree].isInsertAllowed
        }
      )
    )
  }
  {
    val clazz = classOf[Box]
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "value", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Box].value
        },
        mkMethod(clazz, "id", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Box].id
        },
        mkMethod(clazz, "creationInfo", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Box].creationInfo
        },
        mkMethod(clazz, "bytes", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Box].bytes
        },
        mkMethod(clazz, "tokens", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Box].tokens
        },
        mkMethod(clazz, "getReg", Array[Class[_]](classOf[Int], classOf[RType[_]])) { (obj, args) =>
          obj.asInstanceOf[Box].getReg(args(0).asInstanceOf[Int])(args(1).asInstanceOf[RType[_]])
        },
        mkMethod(clazz, "bytesWithoutRef", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Box].bytesWithoutRef
        },
        mkMethod(clazz, "propositionBytes", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Box].propositionBytes
        }
      )
    )
  }
  {
    val clazz = classOf[Context]
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "LastBlockUtxoRootHash", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Context].LastBlockUtxoRootHash
        },
        mkMethod(clazz, "dataInputs", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Context].dataInputs
        },
        mkMethod(clazz, "selfBoxIndex", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Context].selfBoxIndex
        },
        mkMethod(clazz, "INPUTS", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Context].INPUTS
        },
        mkMethod(clazz, "minerPubKey", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Context].minerPubKey
        },
        mkMethod(clazz, "HEIGHT", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Context].HEIGHT
        },
        mkMethod(clazz, "OUTPUTS", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Context].OUTPUTS
        },
        mkMethod(clazz, "SELF", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Context].SELF
        },
        mkMethod(clazz, "preHeader", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Context].preHeader
        },
        mkMethod(clazz, "getVar", Array[Class[_]](classOf[Byte], classOf[RType[_]])) { (obj, args) =>
          obj.asInstanceOf[Context].getVar(args(0).asInstanceOf[Byte])(args(1).asInstanceOf[RType[_]])
        },
        mkMethod(clazz, "getVarFromInput", Array[Class[_]](classOf[Short], classOf[Byte], classOf[RType[_]])) { (obj, args) =>
          obj.asInstanceOf[Context].getVarFromInput(args(0).asInstanceOf[Short], args(1).asInstanceOf[Byte])(args(2).asInstanceOf[RType[_]])
        },
        mkMethod(clazz, "headers", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Context].headers
        }
      )
    )
  }
  {
    val clazz = classOf[GroupElement]
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "exp", Array[Class[_]](classOf[BigInt])) { (obj, args) =>
          obj.asInstanceOf[GroupElement].exp(args(0).asInstanceOf[BigInt])
        },
        mkMethod(clazz, "multiply", Array[Class[_]](classOf[GroupElement])) { (obj, args) =>
          obj.asInstanceOf[GroupElement].multiply(args(0).asInstanceOf[GroupElement])
        },
        mkMethod(clazz, "getEncoded", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[GroupElement].getEncoded
        },
        mkMethod(clazz, "negate", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[GroupElement].negate
        }
      )
    )
  }
  {
    val clazz = classOf[Header]
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "minerPk", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Header].minerPk
        },
        mkMethod(clazz, "powNonce", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Header].powNonce
        },
        mkMethod(clazz, "transactionsRoot", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Header].transactionsRoot
        },
        mkMethod(clazz, "powOnetimePk", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Header].powOnetimePk
        },
        mkMethod(clazz, "nBits", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Header].nBits
        },
        mkMethod(clazz, "votes", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Header].votes
        },
        mkMethod(clazz, "id", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Header].id
        },
        mkMethod(clazz, "version", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Header].version
        },
        mkMethod(clazz, "parentId", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Header].parentId
        },
        mkMethod(clazz, "timestamp", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Header].timestamp
        },
        mkMethod(clazz, "height", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Header].height
        },
        mkMethod(clazz, "extensionRoot", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Header].extensionRoot
        },
        mkMethod(clazz, "ADProofsRoot", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Header].ADProofsRoot
        },
        mkMethod(clazz, "stateRoot", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Header].stateRoot
        },
        mkMethod(clazz, "powDistance", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Header].powDistance
        },
        mkMethod(clazz, "checkPow", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[Header].checkPow
        }
      )
    )
  }
  {
    val clazz = classOf[PreHeader]
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "minerPk", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[PreHeader].minerPk
        },
        mkMethod(clazz, "votes", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[PreHeader].votes
        },
        mkMethod(clazz, "nBits", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[PreHeader].nBits
        },
        mkMethod(clazz, "version", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[PreHeader].version
        },
        mkMethod(clazz, "timestamp", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[PreHeader].timestamp
        },
        mkMethod(clazz, "parentId", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[PreHeader].parentId
        },
        mkMethod(clazz, "height", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[PreHeader].height
        }
      )
    )
  }
  {
    val clazz         = classOf[SigmaDslBuilder]
    val cColl         = classOf[Coll[_]]
    val cGroupElement = classOf[GroupElement]
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "byteArrayToLong", Array[Class[_]](cColl)) { (obj, args) =>
          obj.asInstanceOf[SigmaDslBuilder].byteArrayToLong(args(0).asInstanceOf[Coll[Byte]])
        },
        mkMethod(clazz, "anyOf", Array[Class[_]](cColl)) { (obj, args) =>
          obj.asInstanceOf[SigmaDslBuilder].anyOf(args(0).asInstanceOf[Coll[Boolean]])
        },
        mkMethod(clazz, "allZK", Array[Class[_]](cColl)) { (obj, args) =>
          obj.asInstanceOf[SigmaDslBuilder].allZK(args(0).asInstanceOf[Coll[SigmaProp]])
        },
        mkMethod(clazz, "xorOf", Array[Class[_]](cColl)) { (obj, args) =>
          obj.asInstanceOf[SigmaDslBuilder].xorOf(args(0).asInstanceOf[Coll[Boolean]])
        },
        mkMethod(clazz, "substConstants", Array[Class[_]](cColl, cColl, cColl)) { (obj, args) =>
          obj.asInstanceOf[SigmaDslBuilder].substConstants(args(0).asInstanceOf[Coll[Byte]],
            args(1).asInstanceOf[Coll[Int]], args(2).asInstanceOf[Coll[Any]])
        },
        mkMethod(clazz, "groupGenerator", Array[Class[_]]()) { (obj, _) =>
          obj.asInstanceOf[SigmaDslBuilder].groupGenerator
        },
        mkMethod(clazz, "sigmaProp", Array[Class[_]](classOf[Boolean])) { (obj, args) =>
          obj.asInstanceOf[SigmaDslBuilder].sigmaProp(args(0).asInstanceOf[Boolean])
        },
        mkMethod(clazz, "proveDlog", Array[Class[_]](cGroupElement)) { (obj, args) =>
          obj.asInstanceOf[SigmaDslBuilder].proveDlog(args(0).asInstanceOf[GroupElement])
        },
        mkMethod(clazz, "blake2b256", Array[Class[_]](cColl)) { (obj, args) =>
          obj.asInstanceOf[SigmaDslBuilder].blake2b256(args(0).asInstanceOf[Coll[Byte]])
        },
        mkMethod(clazz, "longToByteArray", Array[Class[_]](classOf[Long])) { (obj, args) =>
          obj.asInstanceOf[SigmaDslBuilder].longToByteArray(args(0).asInstanceOf[Long])
        },
        mkMethod(clazz, "xor", Array[Class[_]](cColl, cColl)) { (obj, args) =>
          obj.asInstanceOf[SigmaDslBuilder].xor(args(0).asInstanceOf[Coll[Byte]],
            args(1).asInstanceOf[Coll[Byte]])
        },
        mkMethod(clazz, "atLeast", Array[Class[_]](classOf[Int], cColl)) { (obj, args) =>
          obj.asInstanceOf[SigmaDslBuilder].atLeast(args(0).asInstanceOf[Int],
            args(1).asInstanceOf[Coll[SigmaProp]])
        },
        mkMethod(clazz, "byteArrayToBigInt", Array[Class[_]](cColl)) { (obj, args) =>
          obj.asInstanceOf[SigmaDslBuilder].byteArrayToBigInt(args(0).asInstanceOf[Coll[Byte]])
        },
        mkMethod(clazz, "allOf", Array[Class[_]](cColl)) { (obj, args) =>
          obj.asInstanceOf[SigmaDslBuilder].allOf(args(0).asInstanceOf[Coll[Boolean]])
        },
        mkMethod(clazz, "proveDHTuple", Array[Class[_]](cGroupElement, cGroupElement, cGroupElement, cGroupElement)) { (obj, args) =>
          obj.asInstanceOf[SigmaDslBuilder].proveDHTuple(args(0).asInstanceOf[GroupElement],
            args(1).asInstanceOf[GroupElement],
            args(2).asInstanceOf[GroupElement],
            args(3).asInstanceOf[GroupElement])
        },
        mkMethod(clazz, "anyZK", Array[Class[_]](cColl)) { (obj, args) =>
          obj.asInstanceOf[SigmaDslBuilder].anyZK(args(0).asInstanceOf[Coll[SigmaProp]])
        },
        mkMethod(clazz, "sha256", Array[Class[_]](cColl)) { (obj, args) =>
          obj.asInstanceOf[SigmaDslBuilder].sha256(args(0).asInstanceOf[Coll[Byte]])
        },
        mkMethod(clazz, "serialize", Array[Class[_]](classOf[Object], classOf[RType[_]])) { (obj, args) =>
          obj.asInstanceOf[SigmaDslBuilder].serialize[Any](
            args(0).asInstanceOf[Any])(args(1).asInstanceOf[RType[Any]])
        },
        mkMethod(clazz, "decodePoint", Array[Class[_]](cColl)) { (obj, args) =>
          obj.asInstanceOf[SigmaDslBuilder].decodePoint(args(0).asInstanceOf[Coll[Byte]])
        },
        mkMethod(clazz, "fromBigEndianBytes", Array[Class[_]](cColl, classOf[RType[_]])) { (obj, args) =>
          obj.asInstanceOf[SigmaDslBuilder].fromBigEndianBytes(args(0).asInstanceOf[Coll[Byte]])(args(1).asInstanceOf[RType[_]])
        },
        mkMethod(clazz, "some", Array[Class[_]](classOf[Object], classOf[RType[_]])) { (obj, args) =>
          obj.asInstanceOf[SigmaDslBuilder].some(args(0).asInstanceOf[Any])(args(1).asInstanceOf[RType[Any]])
        },
        mkMethod(clazz, "none", Array[Class[_]](classOf[RType[_]])) { (obj, args) =>
          obj.asInstanceOf[SigmaDslBuilder].none()(args(0).asInstanceOf[RType[_]])
        }
      )
    )
  }

  registerClassEntry(classOf[SCollectionType[_]],
    constructors = Array(
      mkConstructor(Array(classOf[SType])) { args =>
        new SCollectionType(args(0).asInstanceOf[SType])
      }
    )
  )

  registerClassEntry(classOf[SOption[_]],
    constructors = Array(
      mkConstructor(Array(classOf[SType])) { args =>
        new SOption(args(0).asInstanceOf[SType])
      }
    )
  )

  registerClassEntry(classOf[STuple],
    constructors = Array(
      mkConstructor(Array(classOf[IndexedSeq[_]])) { args =>
        new STuple(args(0).asInstanceOf[IndexedSeq[SType]])
      }
    )
  )
}
