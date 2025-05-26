package org.ergoplatform.sdk

import org.ergoplatform.sdk.generators.ObjectGenerators
import org.scalatest.compatible.Assertion
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sigma.ast._
import sigmastate._
import sigmastate.helpers.NegativeTesting
import sigma.serialization.{SerializationSpecification, SigmaSerializer}
import sigma.{ContractsTestkit, VersionContext}
import sigma.ast.syntax.SigmaPropValue
import sigma.ast.{SByte, SInt, SType}
import sigma.data.CBigInt
import ErgoTree.setConstantSegregation

import java.math.BigInteger

class ContractTemplateSpecification extends SerializationSpecification 
  with ScalaCheckPropertyChecks 
  with ContractsTestkit
  with NegativeTesting
  with CrossVersionProps
  with ObjectGenerators {
  object JsonCodecs extends JsonCodecs

  private def jsonRoundTrip[T <: SType](obj: ContractTemplate) = {
    val json = ContractTemplate.jsonEncoder.encoder(obj)
    val res = ContractTemplate.jsonEncoder.decoder(json.hcursor).right.get
    res shouldBe obj
    val json2 = ContractTemplate.jsonEncoder.encoder(res)
    json shouldBe json2
  }
  
  private def serializationRoundTrip(template: ContractTemplate): Assertion = {
    val w = SigmaSerializer.startWriter()
    ContractTemplate.serializer.serialize(template, w)
    val bytes = w.toBytes
    val r = SigmaSerializer.startReader(bytes)
    val res2 = ContractTemplate.serializer.parse(r)
    res2 shouldEqual template

    val w2 = SigmaSerializer.startWriter()
    ContractTemplate.serializer.serialize(res2, w2)
    bytes shouldEqual w2.toBytes
  }

  private def createParameter(name: String, constantIndex: Int): Parameter = {
    Parameter(
      name,
      s"${name}_description",
      constantIndex
    )
  }

  private def createContractTemplate(constTypes: IndexedSeq[SType],
                                     constValues: Option[IndexedSeq[Option[SType#WrappedType]]],
                                     parameters: IndexedSeq[Parameter],
                                     expressionTree: SigmaPropValue): ContractTemplate = {
    ContractTemplate(
      contractTemplateNameInTests,
      contractTemplateDescriptionInTests,
      constTypes,
      constValues,
      parameters,
      expressionTree
    )
  }

  property("unequal length of constTypes and constValues") {
    assertExceptionThrown(
      createContractTemplate(
        IndexedSeq(SByte, SByte, SByte).asInstanceOf[IndexedSeq[SType]],
        Some(IndexedSeq(Some(10.toByte), Some(20.toByte)).asInstanceOf[IndexedSeq[Option[SType#WrappedType]]]),
        IndexedSeq(
          createParameter("p1", 0),
          createParameter("p2", 1),
          createParameter("p3", 2)),
        EQ(Plus(ConstantPlaceholder(0, SByte),
          ConstantPlaceholder(1, SByte)),
          ConstantPlaceholder(2, SByte)).toSigmaProp
      ),
      exceptionLike[IllegalArgumentException]("constValues must be empty or of same length as constTypes. Got 2, expected 3")
    )
  }

  property("more parameters than constants") {
    assertExceptionThrown(
      createContractTemplate(
        IndexedSeq(SByte, SByte, SByte).asInstanceOf[IndexedSeq[SType]],
        Some(IndexedSeq(Some(10.toByte), Some(20.toByte), Some(30.toByte)).asInstanceOf[IndexedSeq[Option[SType#WrappedType]]]),
        IndexedSeq(
          createParameter("p1", 0),
          createParameter("p2", 1),
          createParameter("p3", 2),
          createParameter("p4", 3)),
        EQ(Plus(ConstantPlaceholder(0, SByte),
          ConstantPlaceholder(1, SByte)),
          ConstantPlaceholder(2, SByte)).toSigmaProp
      ),
      exceptionLike[IllegalArgumentException]("number of parameters must be <= number of constants")
    )
  }

  property("invalid parameter constantIndex") {
    assertExceptionThrown(
      createContractTemplate(
        IndexedSeq(SByte, SByte, SByte).asInstanceOf[IndexedSeq[SType]],
        Some(IndexedSeq(Some(10.toByte), Some(20.toByte), Some(30.toByte)).asInstanceOf[IndexedSeq[Option[SType#WrappedType]]]),
        IndexedSeq(
          createParameter("p1", 0),
          createParameter("p2", 1),
          createParameter("p3", 100)),
        EQ(Plus(ConstantPlaceholder(0, SByte),
          ConstantPlaceholder(1, SByte)),
          ConstantPlaceholder(2, SByte)).toSigmaProp
      ),
      exceptionLike[IllegalArgumentException]("parameter constantIndex must be in range [0, 3)")
    )
  }

  property("duplicate parameter constantIndex") {
    assertExceptionThrown(
      createContractTemplate(
        IndexedSeq(SByte, SByte, SByte).asInstanceOf[IndexedSeq[SType]],
        Some(IndexedSeq(Some(10.toByte), Some(20.toByte), Some(30.toByte)).asInstanceOf[IndexedSeq[Option[SType#WrappedType]]]),
        IndexedSeq(
          createParameter("p1", 0),
          createParameter("p2", 1),
          createParameter("p3", 1)),
        EQ(Plus(ConstantPlaceholder(0, SByte),
          ConstantPlaceholder(1, SByte)),
          ConstantPlaceholder(2, SByte)).toSigmaProp
      ),
      exceptionLike[IllegalArgumentException]("multiple parameters point to the same constantIndex")
    )
  }

  property("duplicate parameter names") {
    assertExceptionThrown(
      createContractTemplate(
        IndexedSeq(SByte, SByte, SByte).asInstanceOf[IndexedSeq[SType]],
        Some(IndexedSeq(Some(10.toByte), Some(20.toByte), Some(30.toByte)).asInstanceOf[IndexedSeq[Option[SType#WrappedType]]]),
        IndexedSeq(
          createParameter("duplicate_name", 0),
          createParameter("p2", 1),
          createParameter("duplicate_name", 2)),
        EQ(Plus(ConstantPlaceholder(0, SByte),
          ConstantPlaceholder(1, SByte)),
          ConstantPlaceholder(2, SByte)).toSigmaProp
      ),
      exceptionLike[IllegalArgumentException]("parameter names must be unique. Found duplicate parameters with name duplicate_name")
    )
  }

  property("constantIndex without default value and parameter") {
    assertExceptionThrown(
      createContractTemplate(
        IndexedSeq(SByte, SByte, SByte).asInstanceOf[IndexedSeq[SType]],
        Some(IndexedSeq(None, Some(20.toByte), Some(30.toByte)).asInstanceOf[IndexedSeq[Option[SType#WrappedType]]]),
        IndexedSeq(
          createParameter("p2", 1),
          createParameter("p3", 2)),
        EQ(Plus(ConstantPlaceholder(0, SByte),
          ConstantPlaceholder(1, SByte)),
          ConstantPlaceholder(2, SByte)).toSigmaProp
      ),
      exceptionLike[IllegalArgumentException]("constantIndex 0 does not have a default value and absent from parameter as well")
    )
  }

  property("applyTemplate") {
    val parameters = IndexedSeq(
      createParameter("p1", 0),
      createParameter("p2", 1),
      createParameter("p3", 2))
    val expressionTrees = IndexedSeq(
      EQ(Plus(ConstantPlaceholder(0, SByte),
        ConstantPlaceholder(1, SByte)),
        ConstantPlaceholder(2, SByte)).toSigmaProp,
      EQ(Plus(ConstantPlaceholder(0, SInt),
        ConstantPlaceholder(1, SInt)),
        ConstantPlaceholder(2, SInt)).toSigmaProp,
      EQ(Plus(CBigInt(BigInteger.valueOf(10L)), BigIntConstant(20L)), BigIntConstant(30L)).toSigmaProp
    )
    val templates = Seq(
      createContractTemplate(
        IndexedSeq(SByte, SByte, SByte).asInstanceOf[IndexedSeq[SType]],
        Some(IndexedSeq(Some(10.toByte), Some(20.toByte), Some(30.toByte)).asInstanceOf[IndexedSeq[Option[SType#WrappedType]]]),
        parameters,
        expressionTrees(0)
      ),
      createContractTemplate(
        IndexedSeq(SInt, SInt, SInt).asInstanceOf[IndexedSeq[SType]],
        Some(IndexedSeq(Some(10), None, Some(30)).asInstanceOf[IndexedSeq[Option[SType#WrappedType]]]),
        parameters,
        expressionTrees(1)
      ),
      createContractTemplate(
        SType.EmptySeq,
        None,
        Parameter.EmptySeq,
        expressionTrees(2)
      )
    )
    val templateValues = Seq(
      Map("p1" -> ByteConstant(10.toByte), "p2" -> ByteConstant(40.toByte), "p3" -> ByteConstant(50.toByte)),
      Map("p1" -> IntConstant(10), "p2" -> IntConstant(20)),
      Map.empty[String, Constant[SType]]
    )
    val expectedErgoTreeHeader = setConstantSegregation(ergoTreeHeaderInTests)
    val expectedErgoTree = Seq(
      ErgoTree(
        expectedErgoTreeHeader,
        IndexedSeq(
          ByteConstant(10.toByte),
          ByteConstant(40.toByte),
          ByteConstant(50.toByte)
        ),
        expressionTrees(0)
      ),
      ErgoTree(
        expectedErgoTreeHeader,
        IndexedSeq(
          IntConstant(10),
          IntConstant(20),
          IntConstant(30)
        ),
        expressionTrees(1)
      ),
      ErgoTree(
        expectedErgoTreeHeader,
        Constant.EmptySeq,
        expressionTrees(2)
      )
    )

    templates.indices.foreach { i =>
      val template = templates(i)
      val applied = template.applyTemplate(Some(ergoTreeVersionInTests), templateValues(i))
      applied shouldEqual expectedErgoTree(i)
    }
  }

  property("applyTemplate num(parameters) < num(constants)") {
    val parameters = IndexedSeq(
      createParameter("p1", 0),
      createParameter("p2", 2))
    val expressionTree =
      EQ(Plus(ConstantPlaceholder(0, SInt),
        ConstantPlaceholder(1, SInt)),
        ConstantPlaceholder(2, SInt)).toSigmaProp
    val template = createContractTemplate(
        IndexedSeq(SInt, SInt, SInt).asInstanceOf[IndexedSeq[SType]],
        Some(IndexedSeq(None, Some(20), None).asInstanceOf[IndexedSeq[Option[SType#WrappedType]]]),
        parameters,
        expressionTree
      )
    val templateValues = Map("p1" -> IntConstant(10), "p2" -> IntConstant(30))

    val expectedErgoTreeHeader = setConstantSegregation(ergoTreeHeaderInTests)
    val expectedErgoTree = ErgoTree(
        expectedErgoTreeHeader,
        IndexedSeq(
          IntConstant(10),
          IntConstant(20),
          IntConstant(30)
        ),
        expressionTree
      )

    template.applyTemplate(Some(ergoTreeVersionInTests), templateValues) shouldEqual expectedErgoTree
  }

  property("(de)serialization round trip") {
    forAll(contractTemplateGen, minSuccessful(500)) { template =>
      serializationRoundTrip(template)
    }
  }

  property("Data Json serialization round trip") {
    forAll(contractTemplateGen, minSuccessful(500)) { template =>
      jsonRoundTrip(template)
    }
  }
}
