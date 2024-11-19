package sigma

import org.ergoplatform.ErgoBox.RegisterId
import sigma.ast.SCollection.{SBooleanArray, SByteArray, SIntArray}
import sigma.ast._
import sigma.ast.syntax._
import sigma.data.KeyValueColl
import sigma.eval.ErgoTreeEvaluator
import sigma.reflection.ReflectionData.registerClassEntry
import sigma.reflection.{ReflectionData, mkConstructor, mkMethod}
import sigma.serialization.ValueCodes.OpCode

/** Reflection metadata for `interpreter` module.
  * Such metadata is only used on JS platform to support reflection-like interfaces of
  * RClass, RMethod, RConstructor. These interfaces implemented on JVM using Java
  * reflection.
  *
  * For each class of this module that needs reflection metadata,
  * we register a class entry with the necessary information.
  * Only information that is needed at runtime is registered.
  */
object SigmaDataReflection {
  val reflection = ReflectionData

  registerClassEntry(classOf[AND],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]])) { args =>
        new AND(args(0).asInstanceOf[Value[SBooleanArray]])
      }
    )
  )

  registerClassEntry(classOf[ArithOp[_]],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[Value[_]], classOf[Byte])) { args =>
        new ArithOp(args(0).asInstanceOf[SValue], args(1).asInstanceOf[SValue], args(2).asInstanceOf[OpCode])
      }
    )
  )

  registerClassEntry(classOf[AtLeast],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[Value[_]])) { args =>
        new AtLeast(args(0).asInstanceOf[IntValue], args(1).asInstanceOf[CollectionValue[SSigmaProp.type]])
      }
    )
  )

  registerClassEntry(classOf[BinAnd],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[Value[_]])) { args =>
        new BinAnd(args(0).asInstanceOf[BoolValue], args(1).asInstanceOf[BoolValue])
      }
    )
  )

  registerClassEntry(classOf[BinOr],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[Value[_]])) { args =>
        new BinOr(args(0).asInstanceOf[BoolValue], args(1).asInstanceOf[BoolValue])
      }
    )
  )

  registerClassEntry(classOf[BinXor],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[Value[_]])) { args =>
        new BinXor(args(0).asInstanceOf[BoolValue], args(1).asInstanceOf[BoolValue])
      }
    )
  )

  registerClassEntry(classOf[BoolToSigmaProp],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]])) { args =>
        new BoolToSigmaProp(args(0).asInstanceOf[BoolValue])
      }
    )
  )

  registerClassEntry(classOf[ByteArrayToBigInt],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]])) { args =>
        new ByteArrayToBigInt(args(0).asInstanceOf[Value[SByteArray]])
      }
    )
  )

  registerClassEntry(classOf[ByteArrayToLong],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]])) { args =>
        new ByteArrayToLong(args(0).asInstanceOf[Value[SByteArray]])
      }
    )
  )

  registerClassEntry(classOf[LongToByteArray],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]])) { args =>
        new LongToByteArray(args(0).asInstanceOf[Value[SLong.type]])
      }
    )
  )

  registerClassEntry(classOf[CalcBlake2b256],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]])) { args =>
        new CalcBlake2b256(args(0).asInstanceOf[Value[SByteArray]])
      }
    )
  )

  registerClassEntry(classOf[CalcSha256],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]])) { args =>
        new CalcSha256(args(0).asInstanceOf[Value[SByteArray]])
      }
    )
  )

  registerClassEntry(classOf[CreateProveDHTuple],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[Value[_]], classOf[Value[_]], classOf[Value[_]])) { args =>
        new CreateProveDHTuple(args(0).asInstanceOf[GroupElementValue],
          args(1).asInstanceOf[GroupElementValue],
          args(2).asInstanceOf[GroupElementValue],
          args(3).asInstanceOf[GroupElementValue])
      }
    )
  )

  registerClassEntry(classOf[Downcast[_,_]],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[SNumericType])) { args =>
        new Downcast(args(0).asInstanceOf[Value[SNumericType]], args(1).asInstanceOf[SNumericType])
      }
    )
  )

  registerClassEntry(classOf[EQ[_]],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[Value[_]])) { args =>
        new EQ(args(0).asInstanceOf[SAnyValue], args(1).asInstanceOf[SAnyValue])
      }
    )
  )

  registerClassEntry(classOf[Exponentiate],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[Value[_]])) { args =>
        new Exponentiate(args(0).asInstanceOf[GroupElementValue], args(1).asInstanceOf[BigIntValue])
      }
    )
  )

  registerClassEntry(classOf[GE[_]],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[Value[_]])) { args =>
        new GE(args(0).asInstanceOf[SAnyValue], args(1).asInstanceOf[SAnyValue])
      }
    )
  )

  registerClassEntry(classOf[GT[_]],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[Value[_]])) { args =>
        new GT(args(0).asInstanceOf[SAnyValue], args(1).asInstanceOf[SAnyValue])
      }
    )
  )

  registerClassEntry(classOf[If[_]],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[Value[_]], classOf[Value[_]])) { args =>
        new If(args(0).asInstanceOf[BoolValue], args(1).asInstanceOf[SAnyValue], args(2).asInstanceOf[SAnyValue])
      }
    )
  )

  registerClassEntry(classOf[LE[_]],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[Value[_]])) { args =>
        new LE(args(0).asInstanceOf[SAnyValue], args(1).asInstanceOf[SAnyValue])
      }
    )
  )

  registerClassEntry(classOf[LT[_]],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[Value[_]])) { args =>
        new LT(args(0).asInstanceOf[SAnyValue], args(1).asInstanceOf[SAnyValue])
      }
    )
  )

  registerClassEntry(classOf[LogicalNot],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]])) { args =>
        new LogicalNot(args(0).asInstanceOf[BoolValue])
      }
    )
  )

  registerClassEntry(classOf[MultiplyGroup],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[Value[_]])) { args =>
        new MultiplyGroup(args(0).asInstanceOf[GroupElementValue], args(1).asInstanceOf[GroupElementValue])
      }
    )
  )

  registerClassEntry(classOf[NEQ[_]],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[Value[_]])) { args =>
        new NEQ(args(0).asInstanceOf[SAnyValue], args(1).asInstanceOf[SAnyValue])
      }
    )
  )

  registerClassEntry(classOf[Negation[_]],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]])) { args =>
        new Negation(args(0).asInstanceOf[SAnyValue])
      }
    )
  )

  registerClassEntry(classOf[OR],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]])) { args =>
        new OR(args(0).asInstanceOf[Value[SBooleanArray]])
      }
    )
  )

  { val clazz = SAvlTreeMethods.getClass
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "update_eval", Array[Class[_]](classOf[MethodCall], classOf[AvlTree], classOf[Coll[_]], classOf[Coll[_]], classOf[ErgoTreeEvaluator])) { (obj, args) =>
          obj.asInstanceOf[SAvlTreeMethods.type].update_eval(args(0).asInstanceOf[MethodCall],
            args(1).asInstanceOf[AvlTree],
            args(2).asInstanceOf[KeyValueColl],
            args(3).asInstanceOf[Coll[Byte]])(args(4).asInstanceOf[ErgoTreeEvaluator])
        },
        mkMethod(clazz, "contains_eval", Array[Class[_]](classOf[MethodCall], classOf[AvlTree], classOf[Coll[_]], classOf[Coll[_]], classOf[ErgoTreeEvaluator])) { (obj, args) =>
          obj.asInstanceOf[SAvlTreeMethods.type].contains_eval(args(0).asInstanceOf[MethodCall],
            args(1).asInstanceOf[AvlTree],
            args(2).asInstanceOf[Coll[Byte]],
            args(3).asInstanceOf[Coll[Byte]])(args(4).asInstanceOf[ErgoTreeEvaluator])
        },
        mkMethod(clazz, "get_eval", Array[Class[_]](classOf[MethodCall], classOf[AvlTree], classOf[Coll[_]], classOf[Coll[_]], classOf[ErgoTreeEvaluator])) { (obj, args) =>
          obj.asInstanceOf[SAvlTreeMethods.type].get_eval(args(0).asInstanceOf[MethodCall],
            args(1).asInstanceOf[AvlTree],
            args(2).asInstanceOf[Coll[Byte]],
            args(3).asInstanceOf[Coll[Byte]])(args(4).asInstanceOf[ErgoTreeEvaluator])
        },
        mkMethod(clazz, "getMany_eval", Array[Class[_]](classOf[MethodCall], classOf[AvlTree], classOf[Coll[_]], classOf[Coll[_]], classOf[ErgoTreeEvaluator])) { (obj, args) =>
          obj.asInstanceOf[SAvlTreeMethods.type].getMany_eval(args(0).asInstanceOf[MethodCall],
            args(1).asInstanceOf[AvlTree],
            args(2).asInstanceOf[Coll[Coll[Byte]]],
            args(3).asInstanceOf[Coll[Byte]])(args(4).asInstanceOf[ErgoTreeEvaluator])
        },
        mkMethod(clazz, "remove_eval", Array[Class[_]](classOf[MethodCall], classOf[AvlTree], classOf[Coll[_]], classOf[Coll[_]], classOf[ErgoTreeEvaluator])) { (obj, args) =>
          obj.asInstanceOf[SAvlTreeMethods.type].remove_eval(args(0).asInstanceOf[MethodCall],
            args(1).asInstanceOf[AvlTree],
            args(2).asInstanceOf[Coll[Coll[Byte]]],
            args(3).asInstanceOf[Coll[Byte]])(args(4).asInstanceOf[ErgoTreeEvaluator])
        },
        mkMethod(clazz, "insert_eval", Array[Class[_]](classOf[MethodCall], classOf[AvlTree], classOf[Coll[_]], classOf[Coll[_]], classOf[ErgoTreeEvaluator])) { (obj, args) =>
          obj.asInstanceOf[SAvlTreeMethods.type].insert_eval(args(0).asInstanceOf[MethodCall],
            args(1).asInstanceOf[AvlTree],
            args(2).asInstanceOf[KeyValueColl],
            args(3).asInstanceOf[Coll[Byte]])(args(4).asInstanceOf[ErgoTreeEvaluator])
        }
      )
    )
  }

  { val clazz = SCollectionMethods.getClass
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "zip_eval", Array[Class[_]](classOf[MethodCall], classOf[Coll[_]], classOf[Coll[_]], classOf[ErgoTreeEvaluator])) { (obj, args) =>
          obj.asInstanceOf[SCollectionMethods.type].zip_eval(args(0).asInstanceOf[MethodCall],
            args(1).asInstanceOf[Coll[Any]],
            args(2).asInstanceOf[Coll[Any]])(args(3).asInstanceOf[ErgoTreeEvaluator])
        },
        mkMethod(clazz, "getOrElse_eval", Array[Class[_]](classOf[MethodCall], classOf[Coll[_]], classOf[Int], classOf[java.lang.Object], classOf[ErgoTreeEvaluator])) { (obj, args) =>
          obj.asInstanceOf[SCollectionMethods.type].getOrElse_eval(args(0).asInstanceOf[MethodCall],
            args(1).asInstanceOf[Coll[Any]],
            args(2).asInstanceOf[Int],
            args(3).asInstanceOf[Any])(args(4).asInstanceOf[ErgoTreeEvaluator])
        },
        mkMethod(clazz, "patch_eval", Array[Class[_]](classOf[MethodCall], classOf[Coll[_]], classOf[Int], classOf[Coll[_]], classOf[Int], classOf[ErgoTreeEvaluator])) { (obj, args) =>
          obj.asInstanceOf[SCollectionMethods.type].patch_eval(args(0).asInstanceOf[MethodCall],
            args(1).asInstanceOf[Coll[Any]],
            args(2).asInstanceOf[Int],
            args(3).asInstanceOf[Coll[Any]],
            args(4).asInstanceOf[Int])(args(5).asInstanceOf[ErgoTreeEvaluator])
        },
        mkMethod(clazz, "map_eval", Array[Class[_]](classOf[MethodCall], classOf[Coll[_]], classOf[Function1[_,_]], classOf[ErgoTreeEvaluator])) { (obj, args) =>
          obj.asInstanceOf[SCollectionMethods.type].map_eval(args(0).asInstanceOf[MethodCall],
            args(1).asInstanceOf[Coll[Any]],
            args(2).asInstanceOf[Any => Any])(args(3).asInstanceOf[ErgoTreeEvaluator])
        },
        mkMethod(clazz, "updated_eval", Array[Class[_]](classOf[MethodCall], classOf[Coll[_]], classOf[Int], classOf[java.lang.Object], classOf[ErgoTreeEvaluator])) { (obj, args) =>
          obj.asInstanceOf[SCollectionMethods.type].updated_eval(args(0).asInstanceOf[MethodCall],
            args(1).asInstanceOf[Coll[Any]],
            args(2).asInstanceOf[Int],
            args(3))(args(4).asInstanceOf[ErgoTreeEvaluator])
        },
        mkMethod(clazz, "indexOf_eval", Array[Class[_]](classOf[MethodCall], classOf[Coll[_]], classOf[java.lang.Object], classOf[Int], classOf[ErgoTreeEvaluator])) { (obj, args) =>
          obj.asInstanceOf[SCollectionMethods.type].indexOf_eval(args(0).asInstanceOf[MethodCall],
            args(1).asInstanceOf[Coll[Any]], args(2), args(3).asInstanceOf[Int])(args(4).asInstanceOf[ErgoTreeEvaluator])
        },
        mkMethod(clazz, "updateMany_eval", Array[Class[_]](classOf[MethodCall], classOf[Coll[_]], classOf[Coll[_]], classOf[Coll[_]], classOf[ErgoTreeEvaluator])) { (obj, args) =>
          obj.asInstanceOf[SCollectionMethods.type].updateMany_eval(args(0).asInstanceOf[MethodCall],
            args(1).asInstanceOf[Coll[Any]],
            args(2).asInstanceOf[Coll[Int]],
            args(3).asInstanceOf[Coll[Any]])(args(4).asInstanceOf[ErgoTreeEvaluator])
        },
        mkMethod(clazz, "indices_eval", Array[Class[_]](classOf[MethodCall], classOf[Coll[_]], classOf[ErgoTreeEvaluator])) { (obj, args) =>
          obj.asInstanceOf[SCollectionMethods.type].indices_eval(args(0).asInstanceOf[MethodCall],
            args(1).asInstanceOf[Coll[Any]])(args(2).asInstanceOf[ErgoTreeEvaluator])
        },
        mkMethod(clazz, "flatMap_eval", Array[Class[_]](classOf[MethodCall], classOf[Coll[_]], classOf[Function1[_,_]], classOf[ErgoTreeEvaluator])) { (obj, args) =>
          obj.asInstanceOf[SCollectionMethods.type].flatMap_eval(args(0).asInstanceOf[MethodCall],
            args(1).asInstanceOf[Coll[Any]], args(2).asInstanceOf[Any => Coll[Any]])(args(3).asInstanceOf[ErgoTreeEvaluator])
        },
        mkMethod(clazz, "reverse_eval", Array[Class[_]](classOf[MethodCall], classOf[Coll[_]], classOf[ErgoTreeEvaluator])) { (obj, args) =>
          obj.asInstanceOf[SCollectionMethods.type].reverse_eval(args(0).asInstanceOf[MethodCall],
            args(1).asInstanceOf[Coll[Any]])(args(2).asInstanceOf[ErgoTreeEvaluator])
        },
        mkMethod(clazz, "distinct_eval", Array[Class[_]](classOf[MethodCall], classOf[Coll[_]], classOf[ErgoTreeEvaluator])) { (obj, args) =>
          obj.asInstanceOf[SCollectionMethods.type].distinct_eval(args(0).asInstanceOf[MethodCall],
            args(1).asInstanceOf[Coll[Any]])(args(2).asInstanceOf[ErgoTreeEvaluator])
        },
        mkMethod(clazz, "startsWith_eval", Array[Class[_]](classOf[MethodCall], classOf[Coll[_]], classOf[Coll[_]], classOf[ErgoTreeEvaluator])) { (obj, args) =>
          obj.asInstanceOf[SCollectionMethods.type].startsWith_eval(args(0).asInstanceOf[MethodCall],
            args(1).asInstanceOf[Coll[Any]], args(2).asInstanceOf[Coll[Any]])(args(3).asInstanceOf[ErgoTreeEvaluator])
        },
        mkMethod(clazz, "endsWith_eval", Array[Class[_]](classOf[MethodCall], classOf[Coll[_]], classOf[Coll[_]], classOf[ErgoTreeEvaluator])) { (obj, args) =>
          obj.asInstanceOf[SCollectionMethods.type].endsWith_eval(args(0).asInstanceOf[MethodCall],
            args(1).asInstanceOf[Coll[Any]], args(2).asInstanceOf[Coll[Any]])(args(3).asInstanceOf[ErgoTreeEvaluator])
        }
      )
    )
  }

  { val clazz = SGlobalMethods.getClass
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "xor_eval", Array[Class[_]](classOf[MethodCall], classOf[SigmaDslBuilder], classOf[Coll[_]], classOf[Coll[_]], classOf[ErgoTreeEvaluator])) { (obj, args) =>
          obj.asInstanceOf[SGlobalMethods.type].xor_eval(args(0).asInstanceOf[MethodCall],
            args(1).asInstanceOf[SigmaDslBuilder],
            args(2).asInstanceOf[Coll[Byte]],
            args(3).asInstanceOf[Coll[Byte]])(args(4).asInstanceOf[ErgoTreeEvaluator])
        },
        mkMethod(clazz, "serialize_eval", Array[Class[_]](classOf[MethodCall], classOf[SigmaDslBuilder], classOf[Object], classOf[ErgoTreeEvaluator])) { (obj, args) =>
          obj.asInstanceOf[SGlobalMethods.type].serialize_eval(args(0).asInstanceOf[MethodCall],
            args(1).asInstanceOf[SigmaDslBuilder],
            args(2).asInstanceOf[SType#WrappedType])(args(3).asInstanceOf[ErgoTreeEvaluator])
        }
      )
    )
  }

  registerClassEntry(classOf[SigmaAnd],
    constructors = Array(
      mkConstructor(Array(classOf[Seq[_]])) { args =>
        new SigmaAnd(args(0).asInstanceOf[Seq[SigmaPropValue]])
      }
    )
  )

  registerClassEntry(classOf[SigmaOr],
    constructors = Array(
      mkConstructor(Array(classOf[Seq[_]])) { args =>
        new SigmaOr(args(0).asInstanceOf[Seq[SigmaPropValue]])
      }
    )
  )

  registerClassEntry(classOf[SubstConstants[_]],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[Value[_]], classOf[Value[_]])) { args =>
        new SubstConstants(args(0).asInstanceOf[Value[SByteArray]],
          args(1).asInstanceOf[Value[SIntArray]],
          args(2).asInstanceOf[CollectionValue[SType]])
      }
    )
  )

  registerClassEntry(classOf[Upcast[_,_]],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[SNumericType])) { args =>
        new Upcast(args(0).asInstanceOf[Value[SNumericType]], args(1).asInstanceOf[SNumericType])
      }
    )
  )

  registerClassEntry(classOf[BlockValue],
    constructors = Array(
      mkConstructor(Array(classOf[IndexedSeq[_]], classOf[Value[_]])) { args =>
        new BlockValue(args(0).asInstanceOf[IndexedSeq[BlockItem]], args(1).asInstanceOf[SValue])
      }
    )
  )

  registerClassEntry(classOf[ConcreteCollection[_]],
    constructors = Array(
      mkConstructor(Array(classOf[Seq[_]], classOf[SType])) { args =>
        new ConcreteCollection(args(0).asInstanceOf[Seq[SValue]], args(1).asInstanceOf[SType])
      }
    )
  )

  registerClassEntry(classOf[FuncValue],
    constructors = Array(
      mkConstructor(Array(classOf[IndexedSeq[_]], classOf[Value[_]])) { args =>
        new FuncValue(args(0).asInstanceOf[IndexedSeq[(Int, SType)]], args(1).asInstanceOf[SValue])
      }
    )
  )

  registerClassEntry(classOf[Tuple],
    constructors = Array(
      mkConstructor(Array(classOf[IndexedSeq[_]])) { args =>
        new Tuple(args(0).asInstanceOf[IndexedSeq[SValue]])
      }
    )
  )

  registerClassEntry(classOf[ValDef],
    constructors = Array(
      mkConstructor(Array(classOf[Int], classOf[Seq[_]], classOf[Value[_]])) { args =>
        new ValDef(args(0).asInstanceOf[Int], args(1).asInstanceOf[Seq[STypeVar]], args(2).asInstanceOf[SValue])
      }
    )
  )

  registerClassEntry(classOf[Apply],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[IndexedSeq[_]])) { args =>
        new Apply(args(0).asInstanceOf[SValue], args(1).asInstanceOf[IndexedSeq[SValue]])
      }
    )
  )

  registerClassEntry(classOf[ApplyTypes],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[Seq[_]])) { args =>
        new ApplyTypes(args(0).asInstanceOf[SValue], args(1).asInstanceOf[Seq[SType]])
      }
    )
  )

  registerClassEntry(classOf[Block],
    constructors = Array(
      mkConstructor(Array(classOf[Seq[_]], classOf[Value[_]])) { args =>
        new Block(args(0).asInstanceOf[Seq[Val]], args(1).asInstanceOf[SValue])
      }
    )
  )

  registerClassEntry(classOf[Lambda],
    constructors = Array(
      mkConstructor(Array(classOf[Seq[_]], classOf[IndexedSeq[_]], classOf[SType], classOf[Option[_]])) { args =>
        new Lambda(args(0).asInstanceOf[Seq[STypeParam]],
          args(1).asInstanceOf[IndexedSeq[(String, SType)]],
          args(2).asInstanceOf[SType],
          args(3).asInstanceOf[Option[SValue]])
      }
    )
  )

  registerClassEntry(classOf[MethodCall],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[SMethod], classOf[IndexedSeq[_]], classOf[scala.collection.immutable.Map[_,_]])) { args =>
        new MethodCall(args(0).asInstanceOf[SValue],
          args(1).asInstanceOf[SMethod],
          args(2).asInstanceOf[IndexedSeq[SValue]],
          args(3).asInstanceOf[Map[STypeVar,SType]])
      }
    )
  )

  registerClassEntry(classOf[MethodCallLike],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[java.lang.String], classOf[IndexedSeq[_]], classOf[SType])) { args =>
        new MethodCallLike(args(0).asInstanceOf[SValue],
          args(1).asInstanceOf[String],
          args(2).asInstanceOf[IndexedSeq[SValue]],
          args(3).asInstanceOf[SType])
      }
    )
  )

  registerClassEntry(classOf[Select],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[java.lang.String], classOf[Option[_]])) { args =>
        new Select(args(0).asInstanceOf[SValue], args(1).asInstanceOf[String], args(2).asInstanceOf[Option[SType]])
      }
    )
  )

  registerClassEntry(classOf[ValNode],
    constructors = Array(
      mkConstructor(Array(classOf[java.lang.String], classOf[SType], classOf[Value[_]])) { args =>
        new ValNode(args(0).asInstanceOf[String], args(1).asInstanceOf[SType], args(2).asInstanceOf[SValue])
      }
    )
  )

  registerClassEntry(classOf[Append[_]],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[Value[_]])) { args =>
        new Append(args(0).asInstanceOf[CollectionValue[SType]], args(1).asInstanceOf[CollectionValue[SType]])
      }
    )
  )

  registerClassEntry(classOf[ByIndex[_]],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[Value[_]], classOf[Option[_]])) { args =>
        new ByIndex(args(0).asInstanceOf[CollectionValue[SType]],
          args(1).asInstanceOf[IntValue], args(2).asInstanceOf[Option[SValue]])
      }
    )
  )

  registerClassEntry(classOf[Exists[_]],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[Value[_]])) { args =>
        new Exists(args(0).asInstanceOf[CollectionValue[SType]], args(1).asInstanceOf[Value[SFunc]])
      }
    )
  )

  registerClassEntry(classOf[ExtractAmount],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]])) { args =>
        new ExtractAmount(args(0).asInstanceOf[BoxValue])
      }
    )
  )

  registerClassEntry(classOf[ExtractBytesWithNoRef],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]])) { args =>
        new ExtractBytesWithNoRef(args(0).asInstanceOf[BoxValue])
      }
    )
  )

  registerClassEntry(classOf[ExtractCreationInfo],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]])) { args =>
        new ExtractCreationInfo(args(0).asInstanceOf[BoxValue])
      }
    )
  )

  registerClassEntry(classOf[ExtractId],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]])) { args =>
        new ExtractId(args(0).asInstanceOf[BoxValue])
      }
    )
  )

  registerClassEntry(classOf[ExtractRegisterAs[_]],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[org.ergoplatform.ErgoBox.RegisterId], classOf[SOption[_]])) { args =>
        new ExtractRegisterAs(args(0).asInstanceOf[BoxValue], args(1).asInstanceOf[RegisterId], args(2).asInstanceOf[SOption[SAny.type]])
      }
    )
  )

  registerClassEntry(classOf[ExtractScriptBytes],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]])) { args =>
        new ExtractScriptBytes(args(0).asInstanceOf[BoxValue])
      }
    )
  )

  registerClassEntry(classOf[Filter[_]],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[Value[_]])) { args =>
        new Filter(args(0).asInstanceOf[CollectionValue[SType]], args(1).asInstanceOf[Value[SFunc]])
      }
    )
  )

  registerClassEntry(classOf[Fold[_,_]],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[Value[_]], classOf[Value[_]])) { args =>
        new Fold(args(0).asInstanceOf[CollectionValue[SType]],
          args(1).asInstanceOf[SValue], args(2).asInstanceOf[Value[SFunc]])
      }
    )
  )

  registerClassEntry(classOf[ForAll[_]],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[Value[_]])) { args =>
        new ForAll(args(0).asInstanceOf[CollectionValue[SType]], args(1).asInstanceOf[Value[SFunc]])
      }
    )
  )

  registerClassEntry(classOf[MapCollection[_,_]],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[Value[_]])) { args =>
        new MapCollection(args(0).asInstanceOf[CollectionValue[SType]], args(1).asInstanceOf[Value[SFunc]])
      }
    )
  )

  registerClassEntry(classOf[OptionGet[_]],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]])) { args =>
        new OptionGet(args(0).asInstanceOf[Value[SOption[SType]]])
      }
    )
  )

  registerClassEntry(classOf[OptionGetOrElse[_]],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[Value[_]])) { args =>
        new OptionGetOrElse(args(0).asInstanceOf[Value[SOption[SType]]], args(1).asInstanceOf[SValue])
      }
    )
  )

  registerClassEntry(classOf[OptionIsDefined[_]],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]])) { args =>
        new OptionIsDefined(args(0).asInstanceOf[Value[SOption[SType]]])
      }
    )
  )

  registerClassEntry(classOf[SelectField],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[Byte])) { args =>
        new SelectField(args(0).asInstanceOf[Value[STuple]], args(1).asInstanceOf[Byte])
      }
    )
  )

  registerClassEntry(classOf[SigmaPropBytes],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]])) { args =>
        new SigmaPropBytes(args(0).asInstanceOf[SigmaPropValue])
      }
    )
  )

  registerClassEntry(classOf[SizeOf[_]],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]])) { args =>
        new SizeOf(args(0).asInstanceOf[CollectionValue[SType]])
      }
    )
  )

  registerClassEntry(classOf[Slice[_]],
    constructors = Array(
      mkConstructor(Array(classOf[Value[_]], classOf[Value[_]], classOf[Value[_]])) { args =>
        new Slice(args(0).asInstanceOf[CollectionValue[SType]],
          args(1).asInstanceOf[IntValue], args(2).asInstanceOf[IntValue])
      }
    )
  )
}
