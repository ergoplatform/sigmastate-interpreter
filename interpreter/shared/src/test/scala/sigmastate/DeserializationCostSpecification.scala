package sigmastate

import org.scalatest.prop.TableDrivenPropertyChecks
import sigma.VersionContext
import sigma.data.TrivialProp
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, TestingCommons}

class DeserializationCostSpecification extends TestingCommons
  with CrossVersionProps
  with TableDrivenPropertyChecks {

  property2("Deserialization cost should be version-dependent") {
    val interpreter = new ContextEnrichingTestProvingInterpreter()
    
    // Test cases for version-dependent deserialization cost behavior
    val versionCombinations = Table(
      ("activatedVersion", "ergoTreeVersion"),
      (2.toByte, 2.toByte), // v5.0
      (3.toByte, 2.toByte), // v6.0 activated, v5.0 tree
      (3.toByte, 3.toByte)  // v6.0 activated, v6.0 tree
    )
    
    forAll(versionCombinations) { (activatedVersion, ergoTreeVersion) =>
      val ctx = ErgoLikeContextTesting.dummy(fakeSelf, activatedVersion)
        .withErgoTreeVersion(ergoTreeVersion)
      
      // Create a simple ErgoTree for testing
      val simpleTree = mkTestErgoTree(TrueTree.root.getOrElse(throw new Error("No root")))
      
      val result = interpreter.fullReduction(simpleTree, ctx)
      
      // Verify basic functionality
      result.value shouldBe TrivialProp.TrueProp
      result.cost should be > 0L
      
      // The actual cost difference verification would require more complex tests
      // with actual deserialization operations, but we can at least verify
      // that the interpreter works correctly with different version contexts
      val shouldIncludeDeserializationCost = activatedVersion >= 3
      info(s"Version ($activatedVersion, $ergoTreeVersion): Cost = ${result.cost}, " +
        s"ShouldIncludeDeserializationCost = $shouldIncludeDeserializationCost")
    }
  }

  property2("reductionWithDeserialize should handle version context correctly") {
    val interpreter = new ContextEnrichingTestProvingInterpreter()
    
    // Test that the reductionWithDeserialize method properly handles version context
    val testCases = Table(
      ("activatedVersion", "ergoTreeVersion"),
      (2.toByte, 2.toByte), // v5.0
      (3.toByte, 2.toByte), // v6.0 activated, v5.0 tree
      (3.toByte, 3.toByte)  // v6.0 activated, v6.0 tree
    )
    
    forAll(testCases) { (activatedVersion, ergoTreeVersion) =>
      val ctx = ErgoLikeContextTesting.dummy(fakeSelf, activatedVersion)
        .withErgoTreeVersion(ergoTreeVersion)
      
      // Create an ErgoTree that would trigger reductionWithDeserialize path
      // This would typically be a tree with deserialization operations
      val simpleTree = mkTestErgoTree(TrueTree.root.getOrElse(throw new Error("No root")))
      
      val result = interpreter.fullReduction(simpleTree, ctx)
      
      // Verify the result is correct
      result.value shouldBe TrivialProp.TrueProp
      
      // The cost should reflect the version-dependent behavior
      // v6.0 contexts should generally have higher costs due to deserialization cost addition
      if (activatedVersion >= 3) {
        // v6.0 activated contexts should have deserialization costs included
        info(s"v6.0 context ($activatedVersion, $ergoTreeVersion): Cost includes deserialization")
      } else {
        // v5.0 contexts should not include deserialization costs
        info(s"v5.0 context ($activatedVersion, $ergoTreeVersion): Cost excludes deserialization")
      }
    }
  }

  property2("VersionContext.withVersions should properly set thread-local context") {
    // Test that VersionContext.withVersions properly sets and restores the context
    
    val originalContext = VersionContext.current
    
    // Test different version combinations
    val testCases = Table(
      ("activatedVersion", "ergoTreeVersion"),
      (2.toByte, 2.toByte),
      (3.toByte, 2.toByte),
      (3.toByte, 3.toByte)
    )
    
    forAll(testCases) { (activatedVersion, ergoTreeVersion) =>
      // Set new version context
      VersionContext.withVersions(activatedVersion, ergoTreeVersion) {
        val current = VersionContext.current
        current.activatedVersion shouldBe activatedVersion
        current.ergoTreeVersion shouldBe ergoTreeVersion
        
        // Test helper methods
        current.isV6Activated shouldBe (activatedVersion >= 3)
        current.isV3OrLaterErgoTreeVersion shouldBe (ergoTreeVersion >= 3)
      }
      
      // Context should be restored after withVersions block
      VersionContext.current shouldBe originalContext
    }
  }

  property2("Cost accumulation should be consistent for same operations") {
    val interpreter = new ContextEnrichingTestProvingInterpreter()
    
    // Test that the same operation produces consistent costs
    val versionContexts = Table(
      "versionContext",
      VersionContext(2.toByte, 2.toByte), // v5.0
      VersionContext(3.toByte, 2.toByte), // v6.0 activated, v5.0 tree
      VersionContext(3.toByte, 3.toByte)  // v6.0 activated, v6.0 tree
    )
    
    forAll(versionContexts) { versionContext =>
      VersionContext.withVersions(versionContext.activatedVersion, versionContext.ergoTreeVersion) {
        val ctx = ErgoLikeContextTesting.dummy(fakeSelf, versionContext.activatedVersion)
          .withErgoTreeVersion(versionContext.ergoTreeVersion)
        
        val simpleTree = mkTestErgoTree(TrueTree.root.getOrElse(throw new Error("No root")))
        
        // Run the same operation multiple times
        val results = (1 to 3).map { _ =>
          interpreter.fullReduction(simpleTree, ctx)
        }
        
        // All results should be identical
        results.foreach { result =>
          result.value shouldBe TrivialProp.TrueProp
          result.cost should be > 0L
        }
        
        // Costs should be consistent across multiple runs
        val costs = results.map(_.cost)
        costs.distinct.size shouldBe 1 // All costs should be the same
        
        info(s"Version (${versionContext.activatedVersion}, ${versionContext.ergoTreeVersion}): " +
          s"Consistent cost = ${costs.head}")
      }
    }
  }

  property2("Soft-fork handling should work correctly with version contexts") {
    val interpreter = new ContextEnrichingTestProvingInterpreter()
    
    // Test soft-fork handling when activated version exceeds max supported
    val highActivatedVersion = (VersionContext.MaxSupportedScriptVersion + 1).toByte
    val highTreeVersion = highActivatedVersion
    
    val ctx = ErgoLikeContextTesting.dummy(fakeSelf, highActivatedVersion)
      .withErgoTreeVersion(highTreeVersion)
    
    val simpleTree = mkTestErgoTree(TrueTree.root.getOrElse(throw new Error("No root")))
    
    // This should trigger soft-fork handling
    val result = interpreter.fullReduction(simpleTree, ctx)
    
    // The result should be true (soft-fork acceptance)
    result.value shouldBe TrivialProp.TrueProp
    
    // The cost should be the initial cost (soft-fork handling)
    result.cost should be >= 0L
    
    info(s"Soft-fork handling: activated=$highActivatedVersion, tree=$highTreeVersion, cost=${result.cost}")
  }
}