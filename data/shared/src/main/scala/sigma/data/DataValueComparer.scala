package sigma.data

import debox.{cfor, sp}
import sigma._
import sigma.ast.{FixedCost, JitCost, NamedDesc, OperationCostInfo, PerItemCost}
import sigma.crypto.EcPointType
import sigma.eval.{ErgoTreeEvaluator, SigmaDsl}

/** Implementation of data equality for two arbitrary ErgoTree data types.
  * @see [[DataValueComparer.equalDataValues]]
  */
object DataValueComparer {

  /** NOTE: The cost of most equality operations depends on the position in `match` statement.
    * Thus the full cost to compare x and y equals DispatchCost * OperationCost, where
    * DispatchCost = CasePosition * CostOf_MatchType,
    * OperationCost is the type specific cost.
    * For this reason reordering of cases may lead to divergence between an estimated and
    * the actual execution cost (time).
    * The constants are part of the consensus protocol and cannot be changed without forking.
    */
  final val CostOf_MatchType = 1
  final val CostKind_MatchType = FixedCost(JitCost(CostOf_MatchType))
  final val OpDesc_MatchType = NamedDesc("MatchType")
  final val MatchType = OperationCostInfo(CostKind_MatchType, OpDesc_MatchType)

  final val CostKind_EQ_Prim = FixedCost(JitCost(3))         // case 1
  final val OpDesc_EQ_Prim = NamedDesc("EQ_Prim")
  final val EQ_Prim = OperationCostInfo(CostKind_EQ_Prim, OpDesc_EQ_Prim)


  /** Equals two Colls of non-primitive (boxed) types.
    */
  final val CostKind_EQ_Coll = PerItemCost(
    baseCost = JitCost(10), perChunkCost = JitCost(2), chunkSize = 1) // case 2
  final val OpDesc_EQ_Coll = NamedDesc("EQ_Coll")
  final val EQ_Coll = OperationCostInfo(CostKind_EQ_Coll, OpDesc_EQ_Coll)

  final val CostKind_EQ_Tuple = FixedCost(JitCost(4))  // case 3
  final val OpDesc_EQ_Tuple = NamedDesc("EQ_Tuple")
  final val EQ_Tuple = OperationCostInfo(CostKind_EQ_Tuple, OpDesc_EQ_Tuple)

  /** NOTE: the value is set based on benchmarking of SigmaDslSpecification. */
  final val CostKind_EQ_GroupElement = FixedCost(JitCost(172)) // case 4
  final val OpDesc_EQ_GroupElement = NamedDesc("EQ_GroupElement")
  final val EQ_GroupElement = OperationCostInfo(CostKind_EQ_GroupElement, OpDesc_EQ_GroupElement)

  final val CostKind_EQ_BigInt = FixedCost(JitCost(5))       // case 5
  final val OpDesc_EQ_BigInt = NamedDesc("EQ_BigInt")
  final val EQ_BigInt = OperationCostInfo(CostKind_EQ_BigInt, OpDesc_EQ_BigInt)

  final val CostKind_EQ_AvlTree = FixedCost(JitCost(3 + (6 * CostOf_MatchType) / 2))      // case 6
  final val OpDesc_EQ_AvlTree = NamedDesc("EQ_AvlTree")
  final val EQ_AvlTree = OperationCostInfo(CostKind_EQ_AvlTree, OpDesc_EQ_AvlTree)

  final val CostKind_EQ_Box = FixedCost(JitCost(6))          // case 7
  final val OpDesc_EQ_Box = NamedDesc("EQ_Box")
  final val EQ_Box = OperationCostInfo(CostKind_EQ_Box, OpDesc_EQ_Box)

  /** NOTE: In the formula `(7 + 1)` the 1 corresponds to the second type match. */
  final val CostKind_EQ_Option = FixedCost(JitCost(1 + (7 + 1) * CostOf_MatchType / 2 - 1)) // case 8
  final val OpDesc_EQ_Option = NamedDesc("EQ_Option")
  final val EQ_Option = OperationCostInfo(CostKind_EQ_Option, OpDesc_EQ_Option)

  final val CostKind_EQ_PreHeader = FixedCost(JitCost(4)) // case 9
  final val OpDesc_EQ_PreHeader = NamedDesc("EQ_PreHeader")
  final val EQ_PreHeader = OperationCostInfo(CostKind_EQ_PreHeader, OpDesc_EQ_PreHeader)

  final val CostKind_EQ_Header = FixedCost(JitCost(6)) // case 10
  final val OpDesc_EQ_Header = NamedDesc("EQ_Header")
  final val EQ_Header = OperationCostInfo(CostKind_EQ_Header, OpDesc_EQ_Header)

  /** Equals two CollOverArray of Boolean type. */
  final val CostKind_EQ_COA_Boolean = PerItemCost(
    baseCost = JitCost(15), perChunkCost = JitCost(2), chunkSize = 128)
  final val OpDesc_EQ_COA_Boolean = NamedDesc("EQ_COA_Boolean")
  final val EQ_COA_Boolean = OperationCostInfo(CostKind_EQ_COA_Boolean, OpDesc_EQ_COA_Boolean)

  /** Equals two CollOverArray of Byte type. */
  final val CostKind_EQ_COA_Byte = PerItemCost(
    baseCost = JitCost(15), perChunkCost = JitCost(2), chunkSize = 128)
  final val OpDesc_EQ_COA_Byte = NamedDesc("EQ_COA_Byte")
  final val EQ_COA_Byte = OperationCostInfo(CostKind_EQ_COA_Byte, OpDesc_EQ_COA_Byte)

  /** Equals two CollOverArray of Short type. */
  final val CostKind_EQ_COA_Short = PerItemCost(
    baseCost = JitCost(15), perChunkCost = JitCost(2), chunkSize = 96)
  final val OpDesc_EQ_COA_Short = NamedDesc("EQ_COA_Short")
  final val EQ_COA_Short = OperationCostInfo(CostKind_EQ_COA_Short, OpDesc_EQ_COA_Short)

  /** Equals two CollOverArray of Int type. */
  final val CostKind_EQ_COA_Int = PerItemCost(
    baseCost = JitCost(15), perChunkCost = JitCost(2), chunkSize = 64)
  final val OpDesc_EQ_COA_Int = NamedDesc("EQ_COA_Int")
  final val EQ_COA_Int = OperationCostInfo(CostKind_EQ_COA_Int, OpDesc_EQ_COA_Int)

  /** Equals two CollOverArray of Long type. */
  final val CostKind_EQ_COA_Long = PerItemCost(
    baseCost = JitCost(15), perChunkCost = JitCost(2), chunkSize = 48)
  final val OpDesc_EQ_COA_Long = NamedDesc("EQ_COA_Long")
  final val EQ_COA_Long = OperationCostInfo(CostKind_EQ_COA_Long, OpDesc_EQ_COA_Long)

  /** Equals two CollOverArray of GroupElement type. */
  final val CostKind_EQ_COA_GroupElement = PerItemCost(
    baseCost = JitCost(15), perChunkCost = JitCost(5), chunkSize = 1)
  final val OpDesc_EQ_COA_GroupElement = NamedDesc("EQ_COA_GroupElement")
  final val EQ_COA_GroupElement = OperationCostInfo(CostKind_EQ_COA_GroupElement, OpDesc_EQ_COA_GroupElement)

  /** Equals two CollOverArray of BigInt type. */
  final val CostKind_EQ_COA_BigInt = PerItemCost(
    baseCost = JitCost(15), perChunkCost = JitCost(7), chunkSize = 5)
  final val OpDesc_EQ_COA_BigInt = NamedDesc("EQ_COA_BigInt")
  final val EQ_COA_BigInt = OperationCostInfo(CostKind_EQ_COA_BigInt, OpDesc_EQ_COA_BigInt)

  /** Equals two CollOverArray of AvlTree type. */
  final val CostKind_EQ_COA_AvlTree = PerItemCost(
    baseCost = JitCost(15), perChunkCost = JitCost(5), chunkSize = 2)
  final val OpDesc_EQ_COA_AvlTree = NamedDesc("EQ_COA_AvlTree")
  final val EQ_COA_AvlTree = OperationCostInfo(CostKind_EQ_COA_AvlTree, OpDesc_EQ_COA_AvlTree)

  /** Equals two CollOverArray of Box type. */
  final val CostKind_EQ_COA_Box = PerItemCost(
    baseCost = JitCost(15), perChunkCost = JitCost(5), chunkSize = 1)
  final val OpDesc_EQ_COA_Box = NamedDesc("EQ_COA_Box")
  final val EQ_COA_Box = OperationCostInfo(CostKind_EQ_COA_Box, OpDesc_EQ_COA_Box)

  /** Equals two CollOverArray of PreHeader type. */
  final val CostKind_EQ_COA_PreHeader = PerItemCost(
    baseCost = JitCost(15), perChunkCost = JitCost(3), chunkSize = 1)
  final val OpDesc_EQ_COA_PreHeader = NamedDesc("EQ_COA_PreHeader")
  final val EQ_COA_PreHeader = OperationCostInfo(CostKind_EQ_COA_PreHeader, OpDesc_EQ_COA_PreHeader)

  /** Equals two CollOverArray of Header type. */
  final val CostKind_EQ_COA_Header = PerItemCost(
    baseCost = JitCost(15), perChunkCost = JitCost(5), chunkSize = 1)
  final val OpDesc_EQ_COA_Header = NamedDesc("EQ_COA_Header")
  final val EQ_COA_Header = OperationCostInfo(CostKind_EQ_COA_Header, OpDesc_EQ_COA_Header)

  val descriptors: AVHashMap[RType[_], (OperationCostInfo[FixedCost], OperationCostInfo[PerItemCost])] =
    AVHashMap.fromSeq(Array[(RType[_], (OperationCostInfo[FixedCost], OperationCostInfo[PerItemCost]))](
      (BigIntRType, (EQ_BigInt, EQ_COA_BigInt)),
      (GroupElementRType, (EQ_GroupElement, EQ_COA_GroupElement)),
      (AvlTreeRType, (EQ_AvlTree, EQ_COA_AvlTree)),
      (BoxRType, (EQ_Box, EQ_COA_Box)),
      (PreHeaderRType, (EQ_PreHeader, EQ_COA_PreHeader)),
      (HeaderRType, (EQ_Header, EQ_COA_Header))
    ))

  type COA[A] = CollOverArray[A]
  type POC[A,B] = PairOfCols[A, B]

  /** This method is specialized for numeric types and thus Scala generates four
    * specialized methods (one for each type) which implement unboxed comparison or arrays
    * in a most efficient way. This efficient implementation is reflected in the cost
    * parameters, which are part of the protocol. Thus any alternative protocol
    * implementation should implement comparison is the same way.
    */
  private def equalCOA_Prim[@sp(Boolean, Byte, Short, Int, Long) A]
                   (c1: COA[A], c2: COA[A], costInfo: OperationCostInfo[PerItemCost])
                   (implicit E: ErgoTreeEvaluator): Boolean = {
    var okEqual = true
    E.addSeqCost(costInfo.costKind, costInfo.opDesc) { () =>
      // this loop is bounded because MaxArrayLength limit is enforced
      val len = c1.length
      var i = 0
      val a1 = c1.toArray
      val a2 = c2.toArray
      while (i < len && okEqual) {
        okEqual = a1(i) == a2(i)
        i += 1
      }
      i  // return the number of actually compared items
    }
    okEqual
  }

  /** Compare two collections for equality. Used when the element type A is NOT known
    * statically. When the type A is scalar, each collection item is boxed before
    * comparison, which have significant performace overhead.
    * For this reason, this method is used as a fallback case.
    */
  def equalColls[A](c1: Coll[A], c2: Coll[A])(implicit E: ErgoTreeEvaluator): Boolean = {
    var okEqual = true
    E.addSeqCost(CostKind_EQ_Coll, OpDesc_EQ_Coll) { () =>
      // this loop is bounded because MaxArrayLength limit is enforced
      val len = c1.length
      var i = 0
      while(i < len && okEqual) {
        okEqual = equalDataValues(c1(i), c2(i))
        i += 1
      }
      i
    }
    okEqual
  }

  /** Compares two collections by dispatching to the most efficient implementation
    * depending on the actual type A.
    * */
  def equalColls_Dispatch[A](coll1: Coll[A], coll2: Coll[A])(implicit E: ErgoTreeEvaluator): Boolean = {
    coll1.tItem match {
      case BooleanType =>
        equalCOA_Prim(
          coll1.asInstanceOf[COA[Boolean]],
          coll2.asInstanceOf[COA[Boolean]], EQ_COA_Boolean)

      case ByteType =>
        equalCOA_Prim(
          coll1.asInstanceOf[COA[Byte]],
          coll2.asInstanceOf[COA[Byte]], EQ_COA_Byte)

      case ShortType =>
        equalCOA_Prim(
          coll1.asInstanceOf[COA[Short]],
          coll2.asInstanceOf[COA[Short]], EQ_COA_Short)

      case IntType =>
        equalCOA_Prim(
          coll1.asInstanceOf[COA[Int]],
          coll2.asInstanceOf[COA[Int]], EQ_COA_Int)

      case LongType =>
        equalCOA_Prim(
          coll1.asInstanceOf[COA[Long]],
          coll2.asInstanceOf[COA[Long]], EQ_COA_Long)

      case t =>
        descriptors.get(t) match {
          case Nullable((_, info)) =>
            equalCOA_Prim(
              coll1.asInstanceOf[COA[A]],
              coll2.asInstanceOf[COA[A]], info)
          case _ =>
            equalColls(coll1, coll2)
        }
    }
  }

  /** Compare equality of two sequences of SigmaBoolean trees. */
  def equalSigmaBooleans(xs: Seq[SigmaBoolean], ys: Seq[SigmaBoolean])
                        (implicit E: ErgoTreeEvaluator): Boolean = {
    val len = xs.length
    if (len != ys.length) return false
    var okEqual = true
    cfor(0)(_ < len && okEqual, _ + 1) { i =>
      okEqual = equalSigmaBoolean(xs(i), ys(i))
    }
    okEqual
  }

  /** Compare equality of two SigmaBoolean trees. */
  def equalSigmaBoolean(l: SigmaBoolean, r: SigmaBoolean)
                        (implicit E: ErgoTreeEvaluator): Boolean = {
    E.addCost(MatchType) // once for every node of the SigmaBoolean tree
    l match {
      case ProveDlog(x) => r match {
        case ProveDlog(y) => equalECPoint(x, y)
        case _ => false
      }
      case x: ProveDHTuple => r match {
        case y: ProveDHTuple =>
          equalECPoint(x.gv, y.gv) && equalECPoint(x.hv, y.hv) &&
              equalECPoint(x.uv, y.uv) && equalECPoint(x.vv, y.vv)
        case _ => false
      }
      case x: TrivialProp => r match {
        case y: TrivialProp => x.condition == y.condition
        case _ => false
      }
      case CAND(children) if r.isInstanceOf[CAND] =>
        equalSigmaBooleans(children, r.asInstanceOf[CAND].children)
      case COR(children) if r.isInstanceOf[COR] =>
        equalSigmaBooleans(children, r.asInstanceOf[COR].children)
      case CTHRESHOLD(k, children) if r.isInstanceOf[CTHRESHOLD] =>
        val sb2 = r.asInstanceOf[CTHRESHOLD]
        k == sb2.k && equalSigmaBooleans(children, sb2.children)
      case _ =>
        sys.error(
          s"Cannot compare SigmaBoolean values $l and $r: unknown type")
    }
  }

  /** Returns true if the given GroupElement is equal to the given object. */
  def equalGroupElement(ge1: GroupElement, r: Any)(implicit E: ErgoTreeEvaluator): Boolean = {
    var okEqual = true
    E.addFixedCost(EQ_GroupElement) {
      okEqual = ge1 == r
    }
    okEqual
  }

  /** Returns true if the given EcPointType is equal to the given object. */
  def equalECPoint(p1: EcPointType, r: Any)(implicit E: ErgoTreeEvaluator): Boolean = {
    var okEqual = true
    E.addFixedCost(EQ_GroupElement) {
      okEqual = p1 == r
    }
    okEqual
  }

  /** Generic comparison of any two data values. The method dispatches on a type of the
    * left value and then performs the specific comparison.
    * The comparison recursively descends on the value structure regardless of the depth.
    * However, every step costs are accrued to `E.coster` and the defined limit
    * `E.coster.costLimit` is checked. Thus, the execution of this method is limited and
    * always finishes at least as fast as the costLimit prescribes.
    * No limit is structural depth is necessary.
    */
  def equalDataValues(l: Any, r: Any)(implicit E: ErgoTreeEvaluator): Boolean = {
    var okEqual: Boolean = false
    l match {
      case _: java.lang.Number | _: Boolean => /** case 1 (see [[EQ_Prim]]) */
        E.addFixedCost(EQ_Prim) {
          okEqual = l == r
        }

      case coll1: Coll[a] =>  /** case 2 (see [[EQ_Coll]]) */
        E.addCost(MatchType) // for second match below
        okEqual = r match {
          case coll2: Coll[_] =>
            val len = coll1.length
            if (len != coll2.length || coll1.tItem != coll2.tItem)
              return false

            equalColls_Dispatch(coll1, coll2.asInstanceOf[Coll[a]])

          case _ => false
        }

      case tup1: Tuple2[_,_] => /** case 3 (see [[EQ_Tuple]]) */
        E.addFixedCost(EQ_Tuple) {
          okEqual = r match {
            case tup2: Tuple2[_,_] =>
              equalDataValues(tup1._1, tup2._1) && equalDataValues(tup1._2, tup2._2)
            case _ => false
          }
        }

      case ge1: GroupElement => /** case 4 (see [[EQ_GroupElement]]) */
        okEqual = equalGroupElement(ge1, r)

      case bi: BigInt => /** case 5 (see [[EQ_BigInt]]) */
        E.addFixedCost(EQ_BigInt) {
          okEqual = bi == r
        }

      case sp1: SigmaProp =>
        E.addCost(MatchType) // for second match below
        okEqual = r match {
          case sp2: SigmaProp =>
            equalSigmaBoolean(
              SigmaDsl.toSigmaBoolean(sp1),
              SigmaDsl.toSigmaBoolean(sp2))
          case _ => false
        }

      case bi: AvlTree =>  /** case 6 (see [[EQ_AvlTree]]) */
        E.addFixedCost(EQ_AvlTree) {
          okEqual = bi == r
        }

      case opt1: Option[_] => /** case 7 (see [[EQ_Option]]) */
        E.addFixedCost(EQ_Option) {
          okEqual = r match {
            case opt2: Option[_] =>
              if (opt1.isDefined) {
                if (opt2.isDefined) {
                  equalDataValues(opt1.get, opt2.get)
                } else
                  false // right is not Some
              } else {
                // here left in None
                opt2.isEmpty  // return if the right is also None
              }
            case _ =>
              false // right is not an Option
          }
        }
      case ph: PreHeader =>  /** case 8 (see [[EQ_PreHeader]]) */
        E.addFixedCost(EQ_PreHeader) {
          okEqual = ph == r
        }
      case h: Header =>  /** case 9 (see [[EQ_Header]]) */
        E.addFixedCost(EQ_Header) {
          okEqual = h == r
        }
      case box: Box => /** case 10 (see [[EQ_Box]]) */
        E.addFixedCost(EQ_Box) {
          okEqual = box == r
        }
      case s1: String =>
        E.addCost(MatchType) // for second match below
        okEqual = r match {
          case s2: String =>
            val len = s1.length
            if (len != s2.length)
              return false
            E.addSeqCost(EQ_COA_Short, len) { () =>
              s1 == s2
            }
          case _ => false
        }
      case _: Unit =>
        okEqual = r.isInstanceOf[Unit]

      case _ =>
        sys.error(s"Cannot compare $l and $r: unknown type")
    }
    okEqual
  }

}
