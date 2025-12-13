package sigma.lsp

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.eclipse.lsp4j.{DiagnosticSeverity, CompletionItemKind}

/**
 * Tests for ErgoScript Language Server components.
 */
class LspTests extends AnyFunSuite with Matchers {

  // ============ Diagnostics Tests ============
  
  test("DiagnosticsProvider: valid code should have no errors") {
    val code = """
      |{
      |  val x = SELF.value
      |  sigmaProp(x > 1000L)
      |}
    """.stripMargin
    
    val diagnostics = DiagnosticsProvider.getDiagnostics(code, "test.es")
    diagnostics shouldBe empty
  }

  test("DiagnosticsProvider: simple valid code") {
    val code = "sigmaProp(HEIGHT > 100)"
    
    val diagnostics = DiagnosticsProvider.getDiagnostics(code, "test.es")
    diagnostics shouldBe empty
  }

  test("DiagnosticsProvider: empty code should have no errors") {
    val code = ""
    
    val diagnostics = DiagnosticsProvider.getDiagnostics(code, "test.es")
    diagnostics shouldBe empty
  }

  test("DiagnosticsProvider: whitespace only should have no errors") {
    val code = "   \n\n   "
    
    val diagnostics = DiagnosticsProvider.getDiagnostics(code, "test.es")
    diagnostics shouldBe empty
  }

  test("DiagnosticsProvider: syntax error should be detected") {
    val code = """
      |{
      |  val x = SELF.value +
      |}
    """.stripMargin
    
    val diagnostics = DiagnosticsProvider.getDiagnostics(code, "test.es")
    diagnostics should not be empty
    diagnostics.head.getSeverity shouldBe DiagnosticSeverity.Error
  }

  test("DiagnosticsProvider: unclosed brace should be detected") {
    val code = """
      |{
      |  val x = SELF.value
    """.stripMargin
    
    val diagnostics = DiagnosticsProvider.getDiagnostics(code, "test.es")
    diagnostics should not be empty
  }

  test("DiagnosticsProvider: undefined variable should be detected") {
    val code = """
      |{
      |  sigmaProp(undefinedVar > 100)
      |}
    """.stripMargin
    
    val diagnostics = DiagnosticsProvider.getDiagnostics(code, "test.es")
    diagnostics should not be empty
  }

  // ============ Completion Tests ============
  
  test("CompletionProvider: SELF. should suggest Box methods") {
    val completions = CompletionProvider.getCompletions("SELF.", 0, 5)
    val labels = completions.map(_.getLabel)
    
    labels should contain("value")
    labels should contain("tokens")
    labels should contain("propositionBytes")
    labels should contain("R4")
    labels should contain("R5")
    labels should contain("id")
  }

  test("CompletionProvider: top-level should suggest globals") {
    val completions = CompletionProvider.getCompletions("", 0, 0)
    val labels = completions.map(_.getLabel)
    
    labels should contain("SELF")
    labels should contain("INPUTS")
    labels should contain("OUTPUTS")
    labels should contain("HEIGHT")
    labels should contain("CONTEXT")
  }

  test("CompletionProvider: top-level should suggest functions") {
    val completions = CompletionProvider.getCompletions("", 0, 0)
    val labels = completions.map(_.getLabel)
    
    labels should contain("sigmaProp")
    labels should contain("atLeast")
    labels should contain("blake2b256")
    labels should contain("sha256")
    labels should contain("proveDlog")
  }

  test("CompletionProvider: should include keywords") {
    val completions = CompletionProvider.getCompletions("", 0, 0)
    val labels = completions.map(_.getLabel)
    
    labels should contain("val")
    labels should contain("def")
    labels should contain("if")
    labels should contain("else")
  }

  test("CompletionProvider: should include types") {
    val completions = CompletionProvider.getCompletions("", 0, 0)
    val labels = completions.map(_.getLabel)
    
    labels should contain("Boolean")
    labels should contain("Long")
    labels should contain("Box")
    labels should contain("SigmaProp")
    labels should contain("Coll")
  }

  test("CompletionProvider: prefix filtering works") {
    val completions = CompletionProvider.getCompletions("sig", 0, 3)
    val labels = completions.map(_.getLabel)
    
    labels should contain("sigmaProp")
    labels should contain("SigmaProp")
    labels should not contain("HEIGHT")
  }

  test("CompletionProvider: INPUTS. suggests collection methods") {
    val completions = CompletionProvider.getCompletions("INPUTS.", 0, 7)
    val labels = completions.map(_.getLabel)
    
    labels should contain("size")
    labels should contain("map")
    labels should contain("filter")
    labels should contain("fold")
    labels should contain("exists")
    labels should contain("forall")
  }

  test("CompletionProvider: R4. suggests Option methods") {
    val completions = CompletionProvider.getCompletions("R4.", 0, 3)
    val labels = completions.map(_.getLabel)
    
    labels should contain("get")
    labels should contain("getOrElse")
    labels should contain("isDefined")
    labels should contain("isEmpty")
  }

  test("CompletionProvider: completion items have correct kinds") {
    val completions = CompletionProvider.getCompletions("", 0, 0)
    
    val selfCompletion = completions.find(_.getLabel == "SELF")
    selfCompletion shouldBe defined
    selfCompletion.get.getKind shouldBe CompletionItemKind.Variable
    
    val sigmaPropCompletion = completions.find(_.getLabel == "sigmaProp")
    sigmaPropCompletion shouldBe defined
    sigmaPropCompletion.get.getKind shouldBe CompletionItemKind.Function
    
    val valCompletion = completions.find(_.getLabel == "val")
    valCompletion shouldBe defined
    valCompletion.get.getKind shouldBe CompletionItemKind.Keyword
  }

  // ============ Hover Tests ============
  
  test("HoverProvider: SELF should return documentation") {
    val hover = HoverProvider.getHover("SELF")
    
    hover shouldBe defined
    val content = hover.get.getContents.getRight.getValue
    content should include("Box")
    content should include("input box")
  }

  test("HoverProvider: sigmaProp should return documentation") {
    val hover = HoverProvider.getHover("sigmaProp")
    
    hover shouldBe defined
    val content = hover.get.getContents.getRight.getValue
    content should include("SigmaProp")
    content should include("Boolean")
  }

  test("HoverProvider: HEIGHT should return documentation") {
    val hover = HoverProvider.getHover("HEIGHT")
    
    hover shouldBe defined
    val content = hover.get.getContents.getRight.getValue
    content should include("Int")
    content should include("height")
  }

  test("HoverProvider: atLeast should return documentation") {
    val hover = HoverProvider.getHover("atLeast")
    
    hover shouldBe defined
    val content = hover.get.getContents.getRight.getValue
    content should include("threshold")
  }

  test("HoverProvider: unknown word should return None") {
    val hover = HoverProvider.getHover("unknownIdentifier123")
    
    hover shouldBe None
  }

  test("HoverProvider: Box property 'value' should return documentation") {
    val hover = HoverProvider.getHover("value")
    
    hover shouldBe defined
    val content = hover.get.getContents.getRight.getValue
    content should include("nanoERG")
  }

  test("HoverProvider: collection method 'fold' should return documentation") {
    val hover = HoverProvider.getHover("fold")
    
    hover shouldBe defined
    val content = hover.get.getContents.getRight.getValue
    content should include("accumulator")
  }

  // ============ Definition Tests ============
  
  test("DefinitionProvider: should find val declaration") {
    val code = """
      |{
      |  val myValue = SELF.value
      |  sigmaProp(myValue > 0)
      |}
    """.stripMargin
    
    val locations = DefinitionProvider.getDefinition(code, "myValue", "test.es")
    
    locations should not be empty
    locations.head.getRange.getStart.getLine shouldBe 2
  }

  test("DefinitionProvider: should find def declaration") {
    val code = """
      |{
      |  def isLarge(box: Box): Boolean = box.value > 1000L
      |  sigmaProp(isLarge(SELF))
      |}
    """.stripMargin
    
    val locations = DefinitionProvider.getDefinition(code, "isLarge", "test.es")
    
    locations should not be empty
    locations.head.getRange.getStart.getLine shouldBe 2
  }

  test("DefinitionProvider: should not find built-ins") {
    val code = """
      |{
      |  sigmaProp(SELF.value > 0)
      |}
    """.stripMargin
    
    val locations = DefinitionProvider.getDefinition(code, "SELF", "test.es")
    
    locations shouldBe empty
  }

  test("DefinitionProvider: should find function parameter") {
    val code = """
      |{
      |  def process(amount: Long): Boolean = amount > 0
      |  sigmaProp(process(SELF.value))
      |}
    """.stripMargin
    
    val locations = DefinitionProvider.getDefinition(code, "amount", "test.es")
    
    locations should not be empty
  }

  // ============ Signature Tests ============
  
  test("SignatureProvider: atLeast should show parameters") {
    val code = "atLeast(2, "
    
    val help = SignatureProvider.getSignatureHelp(code, 0, code.length)
    
    help shouldBe defined
    val sig = help.get.getSignatures.get(0)
    sig.getLabel should include("atLeast")
    sig.getParameters.size shouldBe 2
    help.get.getActiveParameter shouldBe 1
  }

  test("SignatureProvider: sigmaProp should show parameters") {
    val code = "sigmaProp("
    
    val help = SignatureProvider.getSignatureHelp(code, 0, code.length)
    
    help shouldBe defined
    val sig = help.get.getSignatures.get(0)
    sig.getLabel should include("sigmaProp")
    help.get.getActiveParameter shouldBe 0
  }

  test("SignatureProvider: blake2b256 should show parameters") {
    val code = "blake2b256("
    
    val help = SignatureProvider.getSignatureHelp(code, 0, code.length)
    
    help shouldBe defined
    val sig = help.get.getSignatures.get(0)
    sig.getLabel should include("blake2b256")
  }

  test("SignatureProvider: unknown function should return None") {
    val code = "unknownFunc("
    
    val help = SignatureProvider.getSignatureHelp(code, 0, code.length)
    
    help shouldBe None
  }

  test("SignatureProvider: nested call should find outer function") {
    val code = "atLeast(min(1, 2), "
    
    val help = SignatureProvider.getSignatureHelp(code, 0, code.length)
    
    help shouldBe defined
    val sig = help.get.getSignatures.get(0)
    sig.getLabel should include("atLeast")
    help.get.getActiveParameter shouldBe 1
  }

  // ============ Server Tests ============
  
  test("ErgoScriptLanguageServer: initialize returns capabilities") {
    val server = new ErgoScriptLanguageServer()
    val params = new org.eclipse.lsp4j.InitializeParams()
    
    val result = server.initialize(params).get()
    
    result.getCapabilities.getCompletionProvider should not be null
    result.getCapabilities.getHoverProvider.getLeft shouldBe true
    result.getCapabilities.getDefinitionProvider.getLeft shouldBe true
  }

  test("ErgoScriptLanguageServer: document tracking works") {
    val server = new ErgoScriptLanguageServer()
    
    server.updateDocument("file:///test.es", "{ sigmaProp(true) }")
    
    server.getDocument("file:///test.es") shouldBe defined
    server.getDocument("file:///test.es").get shouldBe "{ sigmaProp(true) }"
  }

  test("ErgoScriptLanguageServer: document removal works") {
    val server = new ErgoScriptLanguageServer()
    
    server.updateDocument("file:///test.es", "{ sigmaProp(true) }")
    server.removeDocument("file:///test.es")
    
    server.getDocument("file:///test.es") shouldBe None
  }
}
