package sigma.util

import sigma.util.CollectionUtil.AnyOps
import sigma.util.StringUtil.cleanFileName

import java.io._
import java.net.{JarURLConnection, URL}
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import scala.Console
import scala.collection.JavaConverters._

object FileUtil {

  implicit class StringUtilExtensions(val str: String) extends AnyVal {
    /** The last component of the string after the given separator.
      *
      * @param sep the separator character
      */
    def lastComponent(sep: Char): String = {
      str.substring(str.lastIndexOf(sep) + 1)
    }

    /** The prefix of the string before the specified substring.
      *
      * @param substr the substring to find in the input string
      */
    def prefixBefore(substr: String): String = {
      val pos = str.indexOf(substr)
      val res = if (pos == -1) str else str.substring(0, pos)
      res
    }
  }

  /** Executes a function with a PrintWriter for the specified file.
    *
    * @param file the target file
    * @param f    a function that takes a PrintWriter
    */
  def withFile(file: File)(f: PrintWriter => Unit): Unit = {
    if (file.isDirectory && !file.delete()) {
      throw new RuntimeException(s"File $file is a non-empty directory")
    } else {
      file.getParentFile.mkdirs()
      val stream = new PrintWriter(new FileOutputStream(file))
      try {
        f(stream)
      } finally {
        stream.close()
      }
    }
  }

  /** Writes the provided text to the specified file.
    *
    * @param file the target file
    * @param text the text to write
    */
  def write(file: File, text: String): Unit = withFile(file) { _.print(text) }

  /** Executes a function with the provided PrintStream as standard output and error streams.
    *
    * @param out  the PrintStream to use as standard output and error streams
    * @param func the function to execute
    */
  def withStdOutAndErr(out: PrintStream)(func: => Unit): Unit = {
    val oldStdOut = System.out
    val oldStdErr = System.err
    try {
      System.setOut(out)
      System.setErr(out)
      Console.withOut(out)(Console.withErr(out)(func))
    } finally {
      out.flush()
      System.setOut(oldStdOut)
      System.setErr(oldStdErr)
    }
  }

  /** Captures the standard output and error streams produced by the given function.
    *
    * @param func the function to execute
    * @return the captured output as a string
    */
  def captureStdOutAndErr(func: => Unit): String = {
    val out = new ByteArrayOutputStream
    val ps = new PrintStream(out)
    try {withStdOutAndErr(ps)(func)}
    finally {ps.close() }
    out.toString
  }

  /** Returns the last modified timestamp of the specified resource on the classpath.
    *
    * @param source      the resource to find
    * @param classLoader the class loader to use for searching the resource
    */
  def classPathLastModified(source: String, classLoader: ClassLoader = getClass.getClassLoader) = {
    def urlLastModified(url: URL): Long = {
      url.getProtocol match {
        case "file" =>
          val file = urlToFile(url)
          if (file.isDirectory) {
            var result = file.lastModified()
            Files.walkFileTree(file.toPath, new SimpleFileVisitor[Path]() {
              override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
                result = math.max(result, file.toFile.lastModified())
                FileVisitResult.CONTINUE
              }

              override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
                if (exc == null) {
                  result = math.max(result, dir.toFile.lastModified())
                  FileVisitResult.CONTINUE
                } else {
                  throw exc
                }
              }
            })
            result
          } else
            file.lastModified()
        case "jar" =>
          jarUrlToJarFile(url).lastModified()
      }
    }

    val urls = classLoader.getResources(source)
    if (urls.hasMoreElements) {
      urls.asScala.map(urlLastModified).max
    } else
      throw new IllegalArgumentException(s"Resource $source not found on classpath")
  }

  def jarUrlToJarFile(url: URL) = {
    val jarFileUrl = url.openConnection().asInstanceOf[JarURLConnection].getJarFileURL
    urlToFile(jarFileUrl)
  }

  /**
    * Like fileOrDirectory.delete() but works for non-empty directories
    * and throws exceptions instead of returning false on failure
    */
  def delete(fileOrDirectory: File): Unit = {
    deleteRecursive(fileOrDirectory.toPath)
  }

  def deleteIfExist(fileOrDirectory: File): Unit = {
    if (fileOrDirectory.exists()) delete(fileOrDirectory)
  }

  def deleteRecursive(path: Path) {
    Files.walkFileTree(path, new SimpleFileVisitor[Path]() {
      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        Files.delete(file)
        FileVisitResult.CONTINUE
      }

      override def visitFileFailed(file: Path, exc: IOException): FileVisitResult = {
        Files.delete(file)
        FileVisitResult.CONTINUE
      }

      override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
        if (exc == null) {
          Files.delete(dir)
          FileVisitResult.CONTINUE
        }
        else {
          throw exc
        }
      }
    })
  }

  def currentWorkingDir = Paths.get("").toAbsolutePath.toFile

  def file(first: String, rest: String*): File =
    file(new File(first), rest: _*)

  def file(first: File, rest: String*): File =
    rest.foldLeft(first) { (file, child) => new File(file, child) }

  final val AcceptAllFiles = new FilenameFilter {
    override def accept(dir: File, name: String): Boolean = !file(dir, name).isDirectory
  }
  final val AcceptAllDirectories = new FilenameFilter {
    override def accept(dir: File, name: String): Boolean = file(dir, name).isDirectory
  }

  /**
    * Same as dir.listFiles(filter), except it returns empty array instead of null
    * if dir doesn't exist or is not a directory
    */
  def listFiles(dir: File, filter: FilenameFilter = AcceptAllFiles): Array[File] = dir.listFiles(filter) match {
    case null => Array.empty
    case array => array
  }

  def listDirectories(dir: File): Array[File] = listFiles(dir, AcceptAllDirectories)

  /** Starts from <code>dir</code> and builds an array of sub-directories including <code>dir</code> */
  def listDirectoriesRecursive(dir: File): Array[File] = {
    dir.traverseDepthFirst(f => listDirectories(f).toList).toArray
  }

  def listFilesRecursive(dir: File): Array[File] = {
    val dirs = listDirectoriesRecursive(dir)
    for {d <- dirs; f <- listFiles(d)} yield f
  }

  def stripExtension(fileName: String) =
    fileName.lastIndexOf('.') match {
      case -1 =>
        fileName
      case n =>
        fileName.substring(0, n)
    }

  def replaceOrAppendExtension(fileName: String, extension: String): String =
    s"${stripExtension(fileName) }.$extension"

  def modifyName(file: File)(f: String => String): File = {
    val parent = file.getParentFile
    val name = file.getName
    val newName = f(name)
    new File(parent, newName)
  }

  def urlToFile(url: URL) = Paths.get(url.toURI).toFile


  def isBadFileName(string: String) = cleanFileName(string) != string

  def extractModuleName(path: String, sourceDir: String = "src/main/scala"): String = {
    val moduleDir = path.prefixBefore("/" + sourceDir)
    if (moduleDir.length == path.length) return ""
    moduleDir.lastComponent('/')
  }

}

case class ExtensionFilter(extension: String) extends FilenameFilter {
  override def accept(dir: File, name: String): Boolean = name.toLowerCase.endsWith(s".$extension")
}
