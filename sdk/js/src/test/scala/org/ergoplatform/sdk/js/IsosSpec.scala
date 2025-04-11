package org.ergoplatform.sdk.js

import org.ergoplatform.ErgoBox.{AdditionalRegisters, BoxId, TokenId}
import org.ergoplatform._
import org.ergoplatform.sdk.ExtendedInputBox
import org.ergoplatform.sdk.wallet.protocol.context.BlockchainStateContext
import org.scalacheck.Arbitrary
import sigma.ast.{Constant, SType}
import sigma.data.Iso
import sigma.interpreter.{ContextExtension, ProverResult}
import sigma.js.AvlTree
import sigma.{Coll, GroupElement}
import sigma.data.js.{Isos => DataIsos}
import scala.scalajs.js

class IsosSpec extends IsosSpecBase with sdk.generators.ObjectGenerators {

  override implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 30)

  property("Iso.isoStringToArray") {
    forAll() { (bytes: Array[Byte]) =>
      roundtrip(Iso.isoStringToArray)(bytes)
    }
  }

  property("Iso.isoStringToColl") {
    forAll() { (bytes: Coll[Byte]) =>
      roundtrip(Iso.isoStringToColl)(bytes)
    }
  }

  property("Iso.isoStringToGroupElement") {
    forAll() { (bytes: GroupElement) =>
      roundtrip(DataIsos.isoStringToGroupElement)(bytes)
    }
  }

  property("Iso.isoBoxId") {
    forAll(boxIdGen) { (id: BoxId) =>
      roundtrip(DataIsos.isoBoxId)(id)
    }
  }

  property("Iso.isoHexStringToConstant") {
    forAll(constantGen, MinSuccessful(100)) { (c: Constant[SType]) =>
      roundtrip(DataIsos.isoHexStringToConstant)(c)
    }
  }

  property("Iso.avlTree") {
    forAll { (c: sigma.AvlTree) =>
      roundtrip(AvlTree.isoAvlTree)(c)
    }
  }

  property("Iso.isoHeader") {
    forAll { (c: sigma.Header) =>
      roundtrip(Isos.isoHeader)(c)
    }
  }

  property("Iso.isoPreHeader") {
    forAll { (c: sigma.PreHeader) =>
      roundtrip(Isos.isoPreHeader)(c)
    }
  }

  property("Iso.isoBlockchainStateContext") {
    forAll(blockchainStateContextGen) { (c: BlockchainStateContext) =>
      roundtrip(Isos.isoBlockchainStateContext)(c)
    }
  }

  property("Iso.isoContextExtension") {
    forAll { (c: ContextExtension) =>
      roundtrip(Isos.isoContextExtension)(c)
    }
  }

  property("Iso.isoProverResult") {
    forAll { (c: ProverResult) =>
      roundtrip(Isos.isoProverResult)(c)
    }
  }

  property("Iso.isoUnsignedInput") {
    forAll { (c: UnsignedInput) =>
      roundtrip(Isos.isoUnsignedInput)(c)
    }
  }

  property("Iso.isoSignedInput") {
    forAll { (c: Input) =>
      roundtrip(Isos.isoSignedInput)(c)
    }
  }

  property("Iso.isoDataInput") {
    forAll { (c: DataInput) =>
      roundtrip(Isos.isoDataInput)(c)
    }
  }

  property("Iso.isoBigInt") {
    forAll { (c: sigma.BigInt) =>
      roundtrip(sigma.js.Isos.isoBigInt)(c)
    }
  }

  property("Iso.isoBigIntToLong") {
    forAll { (c: Long) =>
      roundtrip(sigma.js.Isos.isoBigIntToLong)(c)
    }
  }

  property("Iso.isoAmount") {
    forAll { (c: Long) =>
      roundtrip(DataIsos.isoAmount)(c)
      DataIsos.isoAmount.to(js.BigInt(c.toString)) shouldBe c
    }
  }

  property("Iso.isoToken") {
    forAll(tokenIdGen, Arbitrary.arbLong.arbitrary) { (id: TokenId, amount: Long) =>
      roundtrip(DataIsos.isoToken)((id, amount))
    }
  }

  property("Iso.isoTokenArray") {
    forAll(ergoBoxTokens(tokensGen.sample.get)) { tokens =>
      roundtrip(DataIsos.isoTokenArray)(tokens)
    }
  }

  property("Iso.isoUndefOr") {
    forAll { opt: Option[Long] =>
      roundtrip(sigma.js.Isos.isoUndefOr(Iso.identityIso[Long]))(opt)
    }
  }

  property("Iso.isoNonMandatoryRegisters") {
    forAll(additionalRegistersGen) { (x: AdditionalRegisters) =>
      roundtrip(DataIsos.isoNonMandatoryRegisters)(x)
    }
  }

  property("Iso.isoBoxCandidate") {
    forAll { (box: ErgoBoxCandidate) =>
      roundtrip(DataIsos.isoBoxCandidate)(box)
    }
  }

  property("Iso.isoBox") {
    forAll { (b: ErgoBox) =>
      roundtrip(sigma.js.Box.isoBox)(b)
    }
  }

  property("Iso.isoEIP12UnsignedInput") {
    forAll(extendedInputBoxGen) { (b: ExtendedInputBox) =>
      roundtrip(Isos.isoEIP12UnsignedInput)(b)
    }
  }

  property("Iso.isoUnsignedTransaction") {
    forAll { (tx: UnsignedErgoLikeTransaction) =>
      roundtrip(Isos.isoUnsignedTransaction)(tx)
    }
  }

  property("Iso.isoSignedTransaction") {
    forAll { (tx: ErgoLikeTransaction) =>
      roundtrip(Isos.isoSignedTransaction)(tx)
    }
  }

  property("Iso.isoContractTemplate") {
    forAll(contractTemplateGen) { (tx: sdk.ContractTemplate) =>
      roundtrip(ContractTemplate.isoToSdk)(tx)
    }
  }

}
