package sigmastate.utxo

import sigmastate.helpers.CompilerTestingCommons
import sigmastate.CompilerCrossVersionProps

class GlobalTransferPoliciesSpecification extends CompilerTestingCommons
  with CompilerCrossVersionProps {
  override val printVersions: Boolean = false
  implicit lazy val IR = new TestingIRContext {
  }

  val env = Map.empty[String, Any]

  property("script hash verification from context var and data input R4 with singleton token check") {
    // Test that the script compiles correctly
    val compiledScript = compile(env, 
      """{
        |  // Get script from context variable #1
        |  val scriptBytes = getVar[Coll[Byte]](1).get
        |  
        |  // Compute script hash
        |  val computedHash = blake2b256(scriptBytes)
        |  
        |  // Get expected hash from R4 of first data input
        |  val expectedHash = CONTEXT.dataInputs(0).R4[Coll[Byte]].get
        |  
        |  // Check that data input has exactly one token (singleton)
        |  val hasSingletonToken = CONTEXT.dataInputs(0).tokens.size == 1
        |  
        |  // Check that token #0 has specific ID (example: token ID from context variable #2)
        |  val expectedTokenId = getVar[Coll[Byte]](2).get
        |  val correctTokenId = CONTEXT.dataInputs(0).tokens(0)._1 == expectedTokenId
        |  
        |  // Verify all conditions
        |  sigmaProp(computedHash == expectedHash && hasSingletonToken && correctTokenId)
        |}""".stripMargin)
    
    // If we get here without exceptions, the script compiles correctly
    compiledScript should not be null
  }

}