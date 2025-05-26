package sigma.util

import sigma.BaseNestedTests

class FileUtilTests extends BaseNestedTests {
  import FileUtil._

  describe("StringExtension methods") {
    it("lastComponent") {
      "a/b/c".lastComponent('/') shouldBe ("c")
      "a/b/".lastComponent('/') shouldBe ("")
      "a".lastComponent('/') shouldBe ("a")
      "".lastComponent('/') shouldBe ("")
    }
    it("prefixBefore") {
      "a/b/c".prefixBefore("/b") shouldBe ("a")
      "a/b/c".prefixBefore("/c") shouldBe ("a/b")
      "a/b/c".prefixBefore("a/b/c") shouldBe ("")
      "a/b/c".prefixBefore("") shouldBe ("")
    }
  }

  describe("File traversals") {
    val root = file("core/shared/src/test/resources/root")
    val subdir = file(root, "subdir")
    val subsubdir = file(subdir, "subsubdir")
    val empty = { val dir = file(root, "empty"); dir.mkdir(); dir }
    val A = file(root, "A.txt")
    val B = file(root, "B.txt")
    val C = file(subdir, "C.txt")
    val D = file(subsubdir, "D.txt")

    it("list all files") {
      listFiles(root).toSet shouldBe Set(A, B)
      listFiles(empty) shouldBe Array()
    }
    it("list directories") {
      listDirectories(root).toSet shouldBe Set(subdir, empty)
    }
    it("list directories recursive") {
      listDirectoriesRecursive(root).toSet shouldBe Set(root, subdir, empty, subsubdir)
    }
    it("list files recursive") {
      listFilesRecursive(root).toSet shouldBe Set(A, B, C, D)
    }
  }
  describe("file path methods") {
    it("extractModuleName") {
      extractModuleName("src/main/scala/d") shouldBe("")
      extractModuleName("/src/main/scala/d") shouldBe("")
      extractModuleName("b/src/main/scala/d") shouldBe("b")
      extractModuleName("a/b/src/main/scala/d") shouldBe("b")
    }
  }
}
