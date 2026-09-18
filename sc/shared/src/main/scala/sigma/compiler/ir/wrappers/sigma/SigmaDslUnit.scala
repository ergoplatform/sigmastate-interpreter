package sigma.compiler.ir.wrappers.sigma

import scalan._
  import sigma.compiler.ir.{Base, IRContext}

  /** IR representation of ErgoScript (Sigma) language types and methods. */
  trait SigmaDsl extends Base { self: IRContext =>
    trait BigInt extends Def[BigInt] {
      def add(that: Ref[BigInt]): Ref[BigInt];
      def subtract(that: Ref[BigInt]): Ref[BigInt];
      def multiply(that: Ref[BigInt]): Ref[BigInt];
      def divide(that: Ref[BigInt]): Ref[BigInt];
      def mod(m: Ref[BigInt]): Ref[BigInt];
      def min(that: Ref[BigInt]): Ref[BigInt];
      def max(that: Ref[BigInt]): Ref[BigInt];
      def toUnsigned(): Ref[UnsignedBigInt];
      def toUnsignedMod(m: Ref[UnsignedBigInt]): Ref[UnsignedBigInt]
    };
    trait UnsignedBigInt extends Def[UnsignedBigInt] {
      def add(that: Ref[UnsignedBigInt]): Ref[UnsignedBigInt];
      def subtract(that: Ref[UnsignedBigInt]): Ref[UnsignedBigInt];
      def multiply(that: Ref[UnsignedBigInt]): Ref[UnsignedBigInt];
      def divide(that: Ref[UnsignedBigInt]): Ref[UnsignedBigInt];
      def mod(m: Ref[UnsignedBigInt]): Ref[UnsignedBigInt];
      def min(that: Ref[UnsignedBigInt]): Ref[UnsignedBigInt];
      def max(that: Ref[UnsignedBigInt]): Ref[UnsignedBigInt];
      def modInverse(m: Ref[UnsignedBigInt]): Ref[UnsignedBigInt]
      def plusMod(that: Ref[UnsignedBigInt], m: Ref[UnsignedBigInt]): Ref[UnsignedBigInt]
      def subtractMod(that: Ref[UnsignedBigInt], m: Ref[UnsignedBigInt]): Ref[UnsignedBigInt]
      def multiplyMod(that: Ref[UnsignedBigInt], m: Ref[UnsignedBigInt]): Ref[UnsignedBigInt]
      def toSigned(): Ref[BigInt]
    };
    trait SigmaProp extends Def[SigmaProp] {
      def isValid: Ref[Boolean];
      def propBytes: Ref[Coll[Byte]];
      def &&(other: Ref[SigmaProp]): Ref[SigmaProp];
      def ||(other: Ref[SigmaProp]): Ref[SigmaProp];
    };
    trait SigmaDslBuilder extends Def[SigmaDslBuilder] {
      def Colls: Ref[CollBuilder];
      def atLeast(bound: Ref[Int], props: Ref[Coll[SigmaProp]]): Ref[SigmaProp];
      def allOf(conditions: Ref[Coll[Boolean]]): Ref[Boolean];
      def allZK(conditions: Ref[Coll[SigmaProp]]): Ref[SigmaProp];
      def anyOf(conditions: Ref[Coll[Boolean]]): Ref[Boolean];
      def anyZK(conditions: Ref[Coll[SigmaProp]]): Ref[SigmaProp];
      def xorOf(conditions: Ref[Coll[Boolean]]): Ref[Boolean];
      def sigmaProp(b: Ref[Boolean]): Ref[SigmaProp];
      def blake2b256(bytes: Ref[Coll[Byte]]): Ref[Coll[Byte]];
      def sha256(bytes: Ref[Coll[Byte]]): Ref[Coll[Byte]];
      def byteArrayToBigInt(bytes: Ref[Coll[Byte]]): Ref[BigInt];
      def longToByteArray(l: Ref[Long]): Ref[Coll[Byte]];
      def byteArrayToLong(bytes: Ref[Coll[Byte]]): Ref[Long];
      def proveDlog(g: Ref[sigma.GroupElement]): Ref[SigmaProp];
      def proveDHTuple(g: Ref[sigma.GroupElement], h: Ref[sigma.GroupElement], u: Ref[sigma.GroupElement], v: Ref[sigma.GroupElement]): Ref[SigmaProp];
      def groupGenerator: Ref[sigma.GroupElement];
      def substConstants[T](scriptBytes: Ref[Coll[Byte]], positions: Ref[Coll[Int]], newValues: Ref[Coll[T]]): Ref[Coll[Byte]];
      def decodePoint(encoded: Ref[Coll[Byte]]): Ref[sigma.GroupElement];
      /** This method will be used in v6.0 to handle CreateAvlTree operation in GraphBuilding */
      def avlTree(operationFlags: Ref[Byte], digest: Ref[Coll[Byte]], keyLength: Ref[Int], valueLengthOpt: Ref[Option[Int]]): Ref[sigma.AvlTree];
      def xor(l: Ref[Coll[Byte]], r: Ref[Coll[Byte]]): Ref[Coll[Byte]]
      def encodeNbits(bi: Ref[BigInt]): Ref[Long]
      def decodeNbits(l: Ref[Long]): Ref[BigInt]
      def powHit(k: Ref[Int], msg: Ref[Coll[Byte]], nonce: Ref[Coll[Byte]], h: Ref[Coll[Byte]], N: Ref[Int]): Ref[UnsignedBigInt];
      def serialize[T](value: Ref[T]): Ref[Coll[Byte]]
      def fromBigEndianBytes[T](bytes: Ref[Coll[Byte]])(implicit cT: Elem[T]): Ref[T]
      def deserializeTo[T](bytes: Ref[Coll[Byte]])(implicit cT: Elem[T]): Ref[T]
      def some[T](value: Ref[T])(implicit cT: Elem[T]): Ref[Option[T]]
      def none[T]()(implicit cT: Elem[T]): Ref[Option[T]]
    };
    trait CostModelCompanion;
    trait BigIntCompanion;
    trait SigmaPropCompanion;
    trait SigmaContractCompanion;
    trait SigmaDslBuilderCompanion
  }