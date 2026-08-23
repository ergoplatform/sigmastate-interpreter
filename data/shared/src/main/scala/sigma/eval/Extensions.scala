package sigma.eval

import sigma.ast.syntax.SigmaPropValue
import sigma.data.{CAnyValue, CSigmaDslBuilder, Nullable, RType, SigmaBoolean}
import sigma.{BigInt, Coll, Colls, Evaluation, Platform, UnsignedBigInt}
import sigma.ast.{Constant, ConstantNode, SBoolean, SCollection, SCollectionType, SType, SigmaPropConstant, SigmaPropIsProven, TransformingSigmaBuilder, Value}

import java.math.BigInteger

object Extensions {
  implicit class ByteExt(val b: Byte) extends AnyVal {
    @inline def toBigInt: BigInt = CSigmaDslBuilder.BigInt(BigInteger.valueOf(b.toLong))
  }

  implicit class ShortExt(val b: Short) extends AnyVal {
    @inline def toBigInt: BigInt = CSigmaDslBuilder.BigInt(BigInteger.valueOf(b.toLong))
  }

  implicit class IntExt(val x: Int) extends AnyVal {
    /** Convert this value to BigInt. */
    @inline def toBigInt: BigInt = CSigmaDslBuilder.BigInt(BigInteger.valueOf(x.toLong))
    @inline def toUnsignedBigInt: UnsignedBigInt = CSigmaDslBuilder.UnsignedBigInt(BigInteger.valueOf(x.toLong))
  }

  implicit class LongExt(val x: Long) extends AnyVal {
    /** Convert this value to BigInt. */
    @inline def toBigInt: BigInt = CSigmaDslBuilder.BigInt(BigInteger.valueOf(x))
  }

  def toAnyValue[A: RType](x: A) = new CAnyValue(x, RType[A].asInstanceOf[RType[Any]])

  implicit class EvalCollOps[T](val coll: Coll[T]) extends AnyVal {
    /** Helper type synonym. */
    type ElemTpe = SType {type WrappedType = T}

    /** Wraps the collection into ConstantNode using collection's element type. */
    def toConstant: Constant[SCollection[ElemTpe]] = {
      val elemTpe = Evaluation.rtypeToSType(coll.tItem).asInstanceOf[ElemTpe]
      ConstantNode[SCollection[ElemTpe]](coll, SCollectionType(elemTpe))
    }

    /** Transforms this collection into array of constants.
      *
      * This method have exactly the same semantics on JS and JVM IF `coll.tItem`
      * precisely describes the type of elements in `call`. (Which is the case for all
      * collections created by ErgoTree interpreter).
      *
      * However, if it is not the case, then JVM and JS will have different semantics for Byte and Short.
      *
      * The JVM version preserves v5.0 consensus protocol semantics.
      * The JS version is a reasonable approximation of the JVM version.
      */
    def toArrayOfConstants: Array[Constant[SType]] = {
      val constants = coll.toArray.map { v =>
        // see ScalaDoc for ensureTypeCarringValue
        val valToLift = ensureTypeCarringValue(v, coll.tItem.asInstanceOf[RType[Any]])
        // call platform-specific method to transform the value to a Constant
        Platform.liftToConstant(valToLift, TransformingSigmaBuilder) match {
          case Nullable(c) => c
          case _ => sys.error(s"Cannot liftToConstant($valToLift)")
        }
      }
      constants
    }
  }

  implicit class SigmaBooleanOps(val sb: SigmaBoolean) extends AnyVal {
    def toSigmaPropValue: SigmaPropValue = SigmaPropConstant(sb)

    def isProven: Value[SBoolean.type] = SigmaPropIsProven(SigmaPropConstant(sb))
  }

  implicit class EvalIterableOps[T: RType](seq: Iterable[T]) {
    @inline def toColl: Coll[T] = Colls.fromArray[T](seq.toArray(RType[T].classTag))
  }
}
