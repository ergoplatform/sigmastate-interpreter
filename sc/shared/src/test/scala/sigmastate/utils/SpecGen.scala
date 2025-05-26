package sigmastate.utils

import sigma.Evaluation._
import sigma.ast.TypeCodes.LastConstantCode
import sigma.ast._
import sigma.serialization.CoreByteWriter.ArgInfo
import sigma.util.CollectionUtil
import sigma.util.Extensions.ByteOps
import sigma.util.PrintExtensions._
import SigmaPredef.{PredefinedFunc, PredefinedFuncRegistry}
import sigma.ast.{MethodCall, PropertyCall}
import sigma.serialization.ValueCodes.OpCode
import sigma.serialization.ValueSerializer

object SpecGenUtils {
  val types = SType.allPredefTypes.diff(Seq(SString))
  val companions: Seq[STypeCompanion] = types.collect { case tc: STypeCompanion => tc }
  val typesWithMethods = companions ++ Seq(SCollection, SOption)
}

trait SpecGen {
  import SpecGenUtils._
  import SType.tT

  case class OpInfo(
      opDesc: ValueCompanion,
      description: String,
      args: Seq[ArgInfo], op: Either[PredefinedFunc, SMethod])

  def collectSerializers(): Seq[ValueSerializer[_ <: Value[SType]]] = {
    ((LastConstantCode + 1) to 255).collect {
      case i if ValueSerializer.serializers(i.toByte) != null =>
        val ser = ValueSerializer.serializers(i.toByte)
        assert(i == ser.opDesc.opCode.toUByte)
        ser
    }
  }

  def collectFreeCodes(): Seq[Int] = {
    ((LastConstantCode + 1) to 255).collect {
      case i if ValueSerializer.serializers(i.toByte) == null => i
    }
  }

  def collectMethods(): Seq[SMethod] = {
    for {
      tc <- typesWithMethods.sortBy(_.typeId)
      mc = MethodsContainer(tc.typeId)
      m <- mc.methods.sortBy(_.methodId)
    } yield m
  }

  def collectSerializableOperations(): Seq[(OpCode, ValueCompanion)] = {
    val sers = collectSerializers()
    sers.map(s => (s.opCode, s.opDesc))
  }

  protected val predefFuncRegistry = new PredefinedFuncRegistry(StdSigmaBuilder)
  val noFuncs: Set[ValueCompanion] = Set(Constant, MethodCall, PropertyCall)
  val predefFuncs: Seq[PredefinedFunc] = predefFuncRegistry.funcs.values
      .filterNot { f => f.docInfo.opDesc.exists(noFuncs.contains) }.toSeq
  val specialFuncs: Seq[PredefinedFunc] = predefFuncRegistry.specialFuncs.values.toSeq

  def collectOpsTable() = {
    val ops = collectSerializableOperations().filterNot { case (_, opDesc) => noFuncs.contains(opDesc) }
    val methods = collectMethods()
    val funcs = predefFuncs ++ specialFuncs

    val methodsByOpCode = methods
        .groupBy(_.docInfo.flatMap(i => i.opDesc.map(_.opCode)))
    //      .map { case p @ (k, xs) => p.ensuring({ k.isEmpty || xs.length == 1}, p) }

    val funcsByOpCode = funcs
        .groupBy(_.docInfo.opDesc.map(_.opCode))
        .ensuring(g => g.forall{ case (k, xs) => xs.length <= 1})

    val table = ops.map { case (opCode, opDesc) =>
      val methodOpt = methodsByOpCode.get(Some(opCode)).map(_.head)
      val funcOpt = funcsByOpCode.get(Some(opCode)).map(_.head)
      (opCode, opDesc, methodOpt, funcOpt)
    }
    val rowsWithInfo =
      for ((opCode, opDesc, optM, optF) <- table if optM.nonEmpty || optF.nonEmpty)
        yield (opDesc, optM, optF)
    rowsWithInfo
  }

  def getOpInfo(opDesc: ValueCompanion, optM: Option[SMethod], optF: Option[PredefinedFunc]): OpInfo = {
    (optM, optF) match {
      case (Some(m), _) =>
        val description = m.docInfo.map(i => i.description).opt()
        val args = m.docInfo.map(i => i.args).getOrElse(Seq())
        OpInfo(opDesc, description, args, Right(m))
      case (_, Some(f)) =>
        val description = f.docInfo.description
        val args = f.docInfo.args
        OpInfo(opDesc, description, args, Left(f))
      case p => sys.error(s"Unexpected $opDesc with $p")
    }
  }

  def toTexName(name: String) = name.replace("%", "\\%")

  def toDisplayCode(opCode: OpCode): Int = opCode.toUByte

  def printTypes(companions: Seq[STypeCompanion]) = {
    val lines = for { tc <- companions.sortBy(_.typeId) } yield {
      val t = tc match {
        case t: SType => t
        case SCollection => SCollection(tT)
        case SOption => SOption(tT)
      }
      val rtype = stypeToRType(t)
      val name = t match { case SGlobal => "Global" case _ => rtype.name }
      val isPrim = t.isInstanceOf[SPrimType]
      val isEmbed = t.isInstanceOf[SEmbeddable]
      val isNum = t.isInstanceOf[SNumericType]
      val valRange = t match {
        case SBoolean => s"$$\\Set{\\lst{true}, \\lst{false}}$$"
        case n: SNumericType =>
          val s = if (n.isInstanceOf[SBigInt.type]) 32 else 1 << n.numericTypeIndex
          val bits = s * 8 - 1 // non-sign bits
          s"$$\\Set{-2^{$bits} \\dots 2^{$bits}-1}$$~\\ref{sec:type:${name}}"
        case SGroupElement => s"$$\\Set{p \\in \\lst{SecP256K1Point}}$$"
        case _ => s"Sec.~\\ref{sec:type:${name}}"
      }
      val line =
        s"""\\lst{$name}	&	$$${tc.typeId}$$	&	\\lst{isConst}	& \\lst{$isPrim}	&	\\lst{$isEmbed} &	\\lst{$isNum}	& $valRange \\\\"""
      line
    }
    val table = lines.mkString("\n\\hline\n")
    table
  }
  
  def methodSubsection(typeName: String, m: SMethod) = {
    val argTypes = m.stype.tDom
    val resTpe = m.stype.tRange.toTermString
    val types = argTypes.map(_.toTermString)
    val argInfos = m.docInfo.fold(
      Range(0, types.length).map(i => ArgInfo("arg" + i, "")))(info => info.args.toIndexedSeq)

    val serializedAs = m.docInfo.flatMap(_.opDesc).opt { d =>
      val opName = d.typeName
      val opCode = d.opCode.toUByte
      s"""
        |  \\bf{Serialized as} & \\hyperref[sec:serialization:operation:$opName]{\\lst{$opName}} \\\\
        |  \\hline
       """.stripMargin
    }
    subsectionTempl(
      opName = s"$typeName.${m.name}",
      opCode = s"${m.objType.typeId}.${m.methodId}",
      label  = s"sec:type:$typeName:${m.name}",
      desc = m.docInfo.opt(i => i.description + i.isFrontendOnly.opt(" (FRONTEND ONLY)")),
      types = types,
      argInfos = argInfos,
      resTpe = resTpe,
      serializedAs = serializedAs
      )
  }

  def funcSubsection(f: PredefinedFunc) = {
    val argTypes = f.declaration.tpe.tDom
    val resTpe = f.declaration.tpe.tRange.toTermString
    val types = argTypes.map(_.toTermString)
    val argInfos = f.docInfo.args
    val opDesc = f.docInfo.opDesc
    val serializedAs = {
      val nodeName = f.docInfo.opTypeName
      s"""
        |  \\bf{Serialized as} & \\hyperref[sec:serialization:operation:$nodeName]{\\lst{$nodeName}} \\\\
        |  \\hline
       """.stripMargin
    }
    subsectionTempl(
      opName = toTexName(f.name),
      opCode = opDesc.map(_.opCode.toUByte.toString).getOrElse("NA"),
      label  = s"sec:appendix:primops:${f.docInfo.opTypeName}",
      desc = f.docInfo.description + f.docInfo.isFrontendOnly.opt(" (FRONTEND ONLY)"),
      types = types,
      argInfos = argInfos,
      resTpe = resTpe,
      serializedAs = serializedAs
    )
  }

  def subsectionTempl(opName: String, opCode: String, label: String, desc: String, types: Seq[String], argInfos: Seq[ArgInfo], resTpe: String, serializedAs: String) = {
    val params = types.opt { ts =>
      val args = argInfos.zip(ts).filter { case (a, _) => a.name != "this" }
      s"""
        |  \\hline
        |  \\bf{Parameters} &
        |      \\(\\begin{array}{l l l}
        |         ${args.rep({ case (info, t) =>
        s"\\lst{${info.name}} & \\lst{: $t} & \\text{// ${info.description}} \\\\"
      }, "\n")}
        |      \\end{array}\\) \\\\
       """.stripMargin
    }
    s"""
      |\\subsubsection{\\lst{$opName} method (Code $opCode)}
      |\\label{$label}
      |\\noindent
      |\\begin{tabularx}{\\textwidth}{| l | X |}
      |   \\hline
      |   \\bf{Description} & $desc \\\\
      |  $params
      |  \\hline
      |  \\bf{Result} & \\lst{${resTpe}} \\\\
      |  \\hline
      |  $serializedAs
      |\\end{tabularx}
      |""".stripMargin
  }

  def printMethods(tc: STypeCompanion) = {
    val mc = MethodsContainer(tc.typeId)
    val methodSubsections = for { m <- mc.methods.sortBy(_.methodId) } yield {
      methodSubsection(tc.typeName, m)
    }
    val res = methodSubsections.mkString("\n\n")
    res
  }
}

object PrintSerializersInfoApp extends SpecGen {
  def main(args: Array[String]) = {
    val sers = collectSerializers()

    val serTypes =  sers.groupBy(s => s.getClass.getSimpleName)
    for ((name, group) <- serTypes) {
      println(name + ":")
      for (ser <- group) {
        val line = s"\t${ser.opCode.toUByte} -> serializer: ${ser.getClass.getSimpleName}; opDesc: ${ser.opDesc}"
        println(line)
      }
    }
    println(s"Total Serializable Ops: ${sers.length}")
    println(s"Total Serializer Types: ${serTypes.keys.size}")

    val ops = ValueCompanion.allOperations
    for ((k,v) <- ops.toArray.sortBy(_._1.toUByte)) {
      println(s"${k.toUByte} -> $v")
    }
    println(s"Total ops: ${ops.size}")
  }
}

object GenPrimOpsApp extends SpecGen {

  def main(args: Array[String]) = {
    val methods = collectMethods()
    val ops = collectSerializableOperations()
    val noOps = Set(
      ValUse, ConstantPlaceholder, TrueLeaf, FalseLeaf,
      ConcreteCollection, ConcreteCollectionBooleanConstant, Tuple, SelectField, SigmaPropIsProven, ValDef, FunDef, BlockValue
    )

    // join collection of all operations with all methods by optional opCode
    val primOps = CollectionUtil.outerJoinSeqs(ops, methods)(
      o => Some(o._1), m => m.docInfo.map(info => if (info.isFrontendOnly) {
        System.err.println(s"WARNING: Operation is frontend only: $info")
        None
      } else info.opDesc.map(_.opCode))
    )(
      (k, o) => Some(o), // left without right
      (k,i) => None,     // right without left
      (k,i,o) => None    // left and right
    ).map(_._2).collect { case Some(op) if !noOps.contains(op._2) => op }

    // primOps is collection of operations which are not referenced by any method m.irInfo.opDesc
    for (p <- primOps) {
      println(s"$p")
    }

    println(s"Total ops: ${primOps.size}\n")


    // join collection of operations with all predef functions by opCode
    val danglingOps = CollectionUtil.outerJoinSeqs(primOps, predefFuncs)(
      o => Some(o._1), f => Some(f.docInfo.opDesc.map(_.opCode))
    )(
      (k, o) => Some(o), // left without right
      (k,i) => None,     // right without left
      (k,i,o) => None    // left and right
    ).map(_._2).collect { case Some(op) => op }

    // danglingOps are the operations which are not referenced by any predef funcs
    for (p <- danglingOps) {
      println(s"$p")
    }

    println(s"Total dangling: ${danglingOps.size}")
  }
}



