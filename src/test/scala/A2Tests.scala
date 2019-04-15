import java.io.File

import scala.reflect.ClassTag
import Scanner.tokenizeJoos1w
import Scanner.LexerException
import Parser.parseJoos1w
import Parser.ParserException
import Weeding._
import Asts.{CompilationUnit, treeToAst}
import Environments._
import org.scalatest.FunSuite
import Utils._
import Environments.HierarchyCheckingException

class A2Tests extends FunSuite {

  def compile(f: File): CompilationUnit = {
    val source = io.Source.fromFile(f)
    val str = (for (line <- source.getLines()) yield line).mkString("\n")
    val tokens = tokenizeJoos1w(str)
    val root = parseJoos1w(tokens)
    weedJoos1w(root)
    treeToAst(root).head.asInstanceOf[CompilationUnit]
  }

  val stdlib: Seq[File] = allFilesInDir("resources/testcases/stdlib/2.0")
  val stdlibCompiled: Seq[CompilationUnit] = stdlib.map(compile)

  def assertPass(testName: String, files: Seq[File]): Unit = {
    print("testing " + testName + ": ")

    val asts: Seq[CompilationUnit] = files.map(compile) ++ stdlibCompiled
    // single file reject can all be resolved when linkType is enabled. For now it is still incomplete linkType(asts)
    joos1wA2(asts)
    println("passed")
  }

  def assertRejectBecauseOf[T <: AnyRef](testName: String, files: Seq[File])(implicit classTag: ClassTag[T]): Unit = {
    val e = intercept[T] {
      assertPass(testName, files)
    }

    e match {
      case _: LexerException => print("rejected by scanner")
      case _: ParserException => print("rejected by parser")
      case _: WeederException => print("rejected by weeder")
      case _: EnvironmentBuildingException => print("rejected by environment builder")
      case _: TypeLinkingException => print("rejected by type linking")
      case _: HierarchyCheckingException => print("rejected by hierarchy check")
    }
    println(" " + e.toString)
  }

  test("stdlib test") {
    println(stdlib.map(_.getAbsolutePath))
  }

  // TODO test that the order of command line argument doesn't matter

  val root = new File("resources/testcases/a2")

  test("single files pass") {
    val files = root.listFiles.filter(_.isFile).filter(_.getName.matches("J[0-9].*"))

    for (file <- files) {
      assertPass(file.getName, Seq(file))
    }
  }

  test("single files reject") {
    val files = root.listFiles.filter(_.isFile).filter(_.getName.matches("Je.*"))

    for ((file, i) <- files.zipWithIndex) {
      print((i + 1) + "/" + files.length + " ")
      assertRejectBecauseOf[Throwable](file.getName, Seq(file))
    }
  }

  test("multiple files pass") {
    val directories = root.listFiles.filter(_.isDirectory).filter(_.getName.matches("J[0-9].*"))

    for (directory <- directories) {
      assertPass(directory.getName, allFilesInDir(directory.getAbsolutePath))
    }
  }

  test("multiple files reject") {
    val directories = root.listFiles.filter(_.isDirectory).filter(_.getName.matches("Je.*"))

    for ((directory, i) <- directories.zipWithIndex) {
      print((i + 1) + "/" + directories.length + " ")
      assertRejectBecauseOf[Throwable](directory.getName, allFilesInDir(directory.getAbsolutePath))
    }
  }

  test("single folder test") {
    val directory = new File(root.getAbsolutePath + "/Je_4_ProtectedOverride_TwoVersionsFromSuperclass")
    assertRejectBecauseOf[Throwable](directory.getName, allFilesInDir(directory.getAbsolutePath))
  }

  test("single file test") {
    val file = new File(root.getAbsolutePath + "/Je_3_Resolve_SamePackageAndClassName.java")
    assertRejectBecauseOf[Throwable](file.getName, Seq(file))
  }

}