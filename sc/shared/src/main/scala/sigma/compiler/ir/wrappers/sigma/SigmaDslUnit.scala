package sigma.compiler.ir.wrappers.sigma

import scalan._
  import sigma.compiler.ir.{Base, IRContext}

  /** IR representation of ErgoScript (Sigma) language types and methods. */
  trait SigmaDsl extends Base { self: IRContext =>
    trait SigmaDslBuilder extends Def[SigmaDslBuilder] {
      def Colls: Ref[sigma.CollBuilder];
      def atLeast(bound: Ref[Int], props: Ref[sigma.Coll[sigma.SigmaProp]]): Ref[sigma.SigmaProp];
      def allOf(conditions: Ref[sigma.Coll[Boolean]]): Ref[Boolean];
      def allZK(conditions: Ref[sigma.Coll[sigma.SigmaProp]]): Ref[sigma.SigmaProp];
      def anyOf(conditions: Ref[sigma.Coll[Boolean]]): Ref[Boolean];
      def anyZK(conditions: Ref[sigma.Coll[sigma.SigmaProp]]): Ref[sigma.SigmaProp];
      def xorOf(conditions: Ref[sigma.Coll[Boolean]]): Ref[Boolean];
      def sigmaProp(b: Ref[Boolean]): Ref[sigma.SigmaProp];
      def blake2b256(bytes: Ref[sigma.Coll[Byte]]): Ref[sigma.Coll[Byte]];
      def sha256(bytes: Ref[sigma.Coll[Byte]]): Ref[sigma.Coll[Byte]];
      def byteArrayToBigInt(bytes: Ref[sigma.Coll[Byte]]): Ref[sigma.BigInt];
      def longToByteArray(l: Ref[Long]): Ref[sigma.Coll[Byte]];
      def byteArrayToLong(bytes: Ref[sigma.Coll[Byte]]): Ref[Long];
      def proveDlog(g: Ref[sigma.GroupElement]): Ref[sigma.SigmaProp];
      def proveDHTuple(g: Ref[sigma.GroupElement], h: Ref[sigma.GroupElement], u: Ref[sigma.GroupElement], v: Ref[sigma.GroupElement]): Ref[sigma.SigmaProp];
      def groupGenerator: Ref[sigma.GroupElement];
      def substConstants[T](scriptBytes: Ref[sigma.Coll[Byte]], positions: Ref[sigma.Coll[Int]], newValues: Ref[sigma.Coll[T]]): Ref[sigma.Coll[Byte]];
      def decodePoint(encoded: Ref[sigma.Coll[Byte]]): Ref[sigma.GroupElement];
      /** This method will be used in v6.0 to handle CreateAvlTree operation in GraphBuilding */
      def avlTree(operationFlags: Ref[Byte], digest: Ref[sigma.Coll[Byte]], keyLength: Ref[Int], valueLengthOpt: Ref[Option[Int]]): Ref[sigma.AvlTree];
      def xor(l: Ref[sigma.Coll[Byte]], r: Ref[sigma.Coll[Byte]]): Ref[sigma.Coll[Byte]]
      def encodeNbits(bi: Ref[sigma.BigInt]): Ref[Long]
      def decodeNbits(l: Ref[Long]): Ref[sigma.BigInt]
      def powHit(k: Ref[Int], msg: Ref[sigma.Coll[Byte]], nonce: Ref[sigma.Coll[Byte]], h: Ref[sigma.Coll[Byte]], N: Ref[Int]): Ref[sigma.UnsignedBigInt];
      def serialize[T](value: Ref[T]): Ref[sigma.Coll[Byte]]
      def fromBigEndianBytes[T](bytes: Ref[sigma.Coll[Byte]])(implicit cT: Elem[T]): Ref[T]
      def deserializeTo[T](bytes: Ref[sigma.Coll[Byte]])(implicit cT: Elem[T]): Ref[T]
      def some[T](value: Ref[T])(implicit cT: Elem[T]): Ref[Option[T]]
      def none[T]()(implicit cT: Elem[T]): Ref[Option[T]]
    };
    trait CostModelCompanion;
    trait SigmaContractCompanion;
    trait SigmaDslBuilderCompanion
  }