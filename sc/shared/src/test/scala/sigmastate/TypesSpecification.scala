package sigmastate

import sigma.Environment
import sigma.ast.SType.isValueOfType
import sigma.SigmaTestingData
import sigma.ast._
import sigma.data.{CSigmaDslBuilder, CSigmaProp}

class TypesSpecification extends SigmaTestingData {

  property("SType.isValueOfType") {
    forAll { t: SPredefType =>
      implicit val tWrapped = wrappedTypeGen(t)
      forAll { x: SPredefType#WrappedType =>
        isValueOfType(x, t) shouldBe true
        // since forall t. SHeader != t
        isValueOfType(x, SHeader) shouldBe false
      }
    }
  }

  import TestData._

  property("SType.isValueOfType test vectors") {
    def assertValidType(x: Any, t: SType) = {
      withClue(s"x = $x, t = $t: ") {
        isValueOfType(x, t) shouldBe true
      }
    }
    def assertInvalidType(x: Any, t: SType) = {
      withClue(s"x = $x, t = $t: ") {
        isValueOfType(x, t) shouldBe false
      }
    }
    
    assertValidType(true, SBoolean)
    assertValidType(false, SBoolean)
    assertInvalidType(true, SByte)
    
    assertValidType(0.toByte, SByte)

    if (Environment.current.isJVM) {
      assertInvalidType(0.toByte, SShort)
    } else {
      // Note difference in JS behavior
      assertValidType(0.toByte, SShort)
    }

    assertValidType(0.toShort, SShort)

    if (Environment.current.isJVM) {
      assertInvalidType(0.toShort, SInt)
    } else {
      // Note difference in JS behavior
      assertValidType(0.toShort, SInt)
    }

    assertValidType(0, SInt)
    
    if (Environment.current.isJVM) {
      assertInvalidType(0, SShort)
    } else {
      // Note difference in JS behavior
      assertValidType(0, SShort)
    }

    assertValidType(0L, SLong)
    assertInvalidType(0L, SShort)

    assertValidType(BigIntZero, SBigInt)
    assertInvalidType(BigIntZero, SShort)

    assertValidType(ge1, SGroupElement)
    assertInvalidType(ge1, SShort)

    assertValidType(CSigmaProp(create_dlog()), SSigmaProp)
    assertInvalidType(CSigmaProp(create_dlog()), SShort)

    assertValidType(b1, SBox)
    assertInvalidType(b1, SShort)

    val coll = Coll[Int](1, 2)
    assertValidType(coll, SCollection(SInt))
    assertInvalidType(coll, SShort)
    assertValidType(Coll[Long](1L), SCollection(SInt)) // because type check is shallow

    assertValidType(None, SOption(SInt))
    assertValidType(Some(10), SOption(SInt))
    assertValidType(Some(10), SOption(SLong)) // because type check is shallow
    assertInvalidType(None, SShort)
    assertInvalidType(Some(10), SShort)

    val ctx = fakeContext.toSigmaContext()
    assertValidType(ctx, SContext)
    assertInvalidType(ctx, SShort)

    assertValidType(t1, SAvlTree)
    assertInvalidType(t1, SShort)

    assertValidType(CSigmaDslBuilder, SGlobal)
    assertInvalidType(CSigmaDslBuilder, SShort)

    assertValidType(h1, SHeader)
    assertInvalidType(h1, SShort)

    assertValidType(preH1, SPreHeader)
    assertInvalidType(preH1, SShort)

    assertValidType((1, 1L), STuple(SInt, SLong))
    assertValidType((1, 1L), STuple(SInt, SInt)) // because type check is shallow
    assertValidType((1, Some(1)), STuple(SInt, SOption(SLong))) // because type check is shallow
    assertExceptionThrown(
      assertValidType((1, 1L, Some(1)), STuple(SInt, SLong, SOption(SInt))),
      exceptionLike[RuntimeException](" is not supported.")
    )

    assertValidType((x: Any) => x, SFunc(SInt, SLong))  // note, arg and result types not checked
    assertInvalidType((x: Any) => x, STuple(SInt, SLong))
    assertInvalidType(1, SFunc(SInt, SLong))
    assertExceptionThrown(
      assertValidType((x: Any) => x, SFunc(Array(SInt, SLong), SOption(SInt))),
      exceptionLike[RuntimeException](" is not supported.")
    )

    assertExceptionThrown(
      assertValidType("", SString),
      exceptionLike[RuntimeException]("We don't recognize the type")
    )
  }
}
