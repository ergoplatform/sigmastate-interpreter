package sigma.serialization.generators

import org.scalacheck.{Arbitrary, Gen}
import sigma.ast.{If, SInt, TreeLookup}
import sigma.ast.{FalseLeaf, TrueLeaf}

trait RelationGenerators {
  this: ObjectGenerators with ConcreteCollectionGenerators =>

  val treeLookupGen: Gen[TreeLookup] = for {
    t <- arbGetVarAvlTree.arbitrary
    b1 <- arbByteArrayConstant.arbitrary
    b2 <- arbByteArrayConstant.arbitrary
  } yield TreeLookup(t, b1, b2)

  val ifGen: Gen[If[SInt.type]] = for {
    c <- Gen.oneOf(TrueLeaf, FalseLeaf)
    tb <- arbIntConstants.arbitrary
    fb <- arbIntConstants.arbitrary
  } yield If(c, tb, fb)

  implicit val arbTreeLookup: Arbitrary[TreeLookup] = Arbitrary(treeLookupGen)
  implicit val arbIf: Arbitrary[If[SInt.type]] = Arbitrary(ifGen)

}
