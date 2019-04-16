import java.io.File

import scala.reflect.ClassTag
import A1.Scanner.tokenizeJoos1w
import A1.Scanner.LexerException
import A1.Parser.parseJoos1w
import A1.Parser.ParserException
import A1.Weeding._
import A2.Asts.{CompilationUnit, treeToAst}
import A2.Environments._
import A3.Disambiguation.{DisambiguateException, TypeCheckingException, joos1wA3}
import org.scalatest.FunSuite
import Util.Utils._

class A3Tests extends FunSuite {

  def compile(f: File): CompilationUnit = {
    val source = io.Source.fromFile(f)
    val str = (for (line <- source.getLines()) yield line).mkString("\n")
    val tokens = tokenizeJoos1w(str)
    val root = parseJoos1w(tokens)
    weedJoos1w(root)
    treeToAst(root).head.asInstanceOf[CompilationUnit]
  }

  val stdlib: Seq[File] = allFilesInDir("resources/testcases/stdlib/3.0")
  val stdlibCompiled: Seq[CompilationUnit] = stdlib.map(compile)

  def assertPass(testName: String, files: Seq[File]): Unit = {
    print("testing " + testName + ": ")

    val asts: Seq[CompilationUnit] = files.map(compile) ++ stdlibCompiled
    // single file reject can all be resolved when linkType is enabled. For now it is still incomplete linkType(asts)
    joos1wA3(asts)
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
      case _: DisambiguateException => print("rejected by disambuation")
      case _: TypeCheckingException => print("rejected by type checking")
    }
    println(" " + e.toString)
  }

  test("stdlib test") {
    println(stdlib.map(_.getAbsolutePath))
  }

  val root = new File("resources/testcases/a3")

  test("single files pass") {
    val files = root.listFiles.filter(_.isFile).filter(_.getName.matches("J[0-9].*"))

    for ((file, i) <- files.zipWithIndex) {
      print((i + 1) + "/" + files.length + " ")
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

    for ((directory, i) <- directories.zipWithIndex) {
      print((i + 1) + "/" + directories.length + " ")
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

  test("single test") {
    val directory = new File(root.getAbsolutePath + "/Je_6_ProtectedAccess_InstanceMethod_SubDeclare_SubVar")
    assertRejectBecauseOf[Throwable](directory.getName, allFilesInDir(directory.getAbsolutePath))
  }

  test("single file test") {
    val file = new File(root.getAbsolutePath + "/Je_6_InstanceOf_Primitive_3.java")
    assertPass(file.getName, Seq(file))
  }

}
