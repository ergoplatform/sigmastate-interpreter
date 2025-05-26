package org.ergoplatform.sdk.js

import org.ergoplatform.sdk.ExtendedInputBox
import org.ergoplatform.sdk.wallet.protocol.context.{BlockchainStateContext, CBlockchainStateContext}
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sigma.Colls
import sigma.data.Iso
import sigma.serialization.generators.ObjectGenerators

class IsosSpecBase extends AnyPropSpec with Matchers with ObjectGenerators with ScalaCheckPropertyChecks {
  lazy val extendedInputBoxGen: Gen[ExtendedInputBox] = for {
    box <- ergoBoxGen
    extension <- contextExtensionGen
  } yield ExtendedInputBox(box, extension)

  lazy val blockchainStateContextGen: Gen[BlockchainStateContext] = for {
    stateRoot <- avlTreeGen
    headers <- headersGen(stateRoot)
    preHeader <- preHeaderGen(headers.headOption.map(_.id).getOrElse(modifierIdBytesGen.sample.get))
  } yield CBlockchainStateContext(
    sigmaLastHeaders = Colls.fromItems(headers: _*),
    previousStateDigest = stateRoot.digest,
    sigmaPreHeader = preHeader
  )

  def roundtrip[A, B](iso: Iso[A, B])(b: B): Unit = {
    iso.to(iso.from(b)) shouldBe b
  }
}
