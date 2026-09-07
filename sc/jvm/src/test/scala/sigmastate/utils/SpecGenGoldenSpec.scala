package sigmastate.utils

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import sigma.util.FileUtil

/** Golden-file ("approval") tests for the spec generators (issue #496).
  *
  * For each generated file we regenerate its content from the current metadata and compare
  * it against the committed copy. Any drift fails the corresponding test and instructs the
  * developer to re-run the generator and commit the result, so the generated artifacts can
  * never silently fall out of sync with the code in a new version.
  *
  * Comparison is done on a normalized form (see [[normalize]]) which tolerates purely
  * cosmetic differences (trailing whitespace, surrounding blank lines, line endings) while
  * still catching any change to the meaningful content. This is required because the
  * generated Scala source (`Operations.scala`) is committed with trailing whitespace stripped,
  * whereas the raw generator output keeps it.
  */
class SpecGenGoldenSpec extends AnyPropSpec with Matchers {
  import SpecGenUtils._

  /** Normalize text for golden comparison: unify line endings, strip trailing whitespace
    * from every line and drop any leading/trailing blank lines. */
  private def normalize(s: String): String =
    s.replace("\r\n", "\n").linesIterator.map(_.replaceAll("\\s+$", "")).mkString("\n").trim

  /** Asserts that the committed file at `path` (relative to the repo root) matches the
    * freshly `regenerated` content, modulo cosmetic formatting. */
  private def assertUnchanged(path: String, regenerated: => String): Unit = {
    val file = FileUtil.file(path)
    withClue(s"$path is out of date - re-run the corresponding Gen* tool and commit the result.\n") {
      file.exists() shouldBe true
      normalize(regenerated) shouldBe normalize(FileUtil.read(file))
    }
  }

  property("Operations.scala is up to date (GenInfoObjects)") {
    assertUnchanged(
      "data/shared/src/main/scala/sigma/ast/Operations.scala",
      GenInfoObjects.generateOperationsFile())
  }

  property("predeftypes.tex is up to date (GenPredefTypesApp)") {
    assertUnchanged(
      "docs/spec/generated/predeftypes.tex",
      GenPredefTypesApp.printTypes(companions))
  }

  property("<Type>_methods.tex files are up to date (GenPredefTypesApp)") {
    for (tc <- typesWithMethods) {
      assertUnchanged(
        s"docs/spec/generated/${tc.typeName}_methods.tex",
        GenPredefTypesApp.printMethods(tc))
    }
  }

  property("predeffunc_rows.tex / predeffunc_sections.tex are up to date (GenPredefFuncsApp)") {
    val (rows, sections) = GenPredefFuncsApp.generatePredefFuncTables()
    assertUnchanged("docs/spec/generated/predeffunc_rows.tex", rows)
    assertUnchanged("docs/spec/generated/predeffunc_sections.tex", sections)
  }
}
