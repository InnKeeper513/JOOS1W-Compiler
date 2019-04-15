import java.io.File

import Scanner.tokenizeJoos1w
import Scanner.LexerException
import Parser.parseJoos1w
import Parser.ParserException
import Weeding._
import Asts.{CompilationUnit, treeToAst}
import Environments._
import Disambiguation.{DisambiguateException, TypeCheckingException}
import StaticAnalysis.{ReachabilityException, DefiniteAssignmentException, ConstantEvaluationException, joos1wA4}
import org.scalatest.FunSuite
import Utils._

import scala.reflect.ClassTag

class AllTests extends FunSuite {

  def compile(f: File): CompilationUnit = {
    val source = io.Source.fromFile(f)
    val str = (for (line <- source.getLines()) yield line).mkString("\n")
    val tokens = tokenizeJoos1w(str)
    val root = parseJoos1w(tokens)
    weedJoos1w(root)
    treeToAst(root).head.asInstanceOf[CompilationUnit]
  }

  val stdlibs: Seq[String] = Seq("resources/testcases/stdlib/2.0", "resources/testcases/stdlib/2.0", "resources/testcases/stdlib/3.0", "resources/testcases/stdlib/4.0", "resources/testcases/stdlib/5.0")
  val roots: Seq[String] = Seq("resources/testcases/a1", "resources/testcases/a2", "resources/testcases/a3", "resources/testcases/a4", "resources/testcases/a5")

  def runTestForAssignment(stdlib: Seq[File], root: File, num: Int) = {
    val stdlibCompiled: Seq[CompilationUnit] = stdlib.map(compile)

    def assertPass(testName: String, files: Seq[File]): Unit = {
      print("testing " + testName + ": ")

      val asts: Seq[CompilationUnit] = files.map(compile) ++ stdlibCompiled
      // single file reject can all be resolved when linkType is enabled. For now it is still incomplete linkType(asts)
      joos1wA4(asts)
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
        case _: ReachabilityException => print("rejected by reachability analysis")
        case _: DefiniteAssignmentException => print("rejected by definite assignment analysis")
        case _: ConstantEvaluationException => print("rejected by constant evaluation")
      }
      println(" " + e.toString)
    }

    test("stdlib test Assignment" + num) {
      println(stdlib.map(_.getAbsolutePath))
    }

    test("single files pass Assignment" + num) {
      val files = root.listFiles.filter(_.isFile).filter(_.getName.matches("J[0-9].*"))

      for ((file, i) <- files.zipWithIndex) {
        print((i + 1) + "/" + files.length + " ")
        assertPass(file.getName, Seq(file))
      }
    }

    test("single files reject Assignment" + num) {
      val files = root.listFiles.filter(_.isFile).filter(_.getName.matches("Je.*"))

      for ((file, i) <- files.zipWithIndex) {
        print((i + 1) + "/" + files.length + " ")
        assertRejectBecauseOf[Throwable](file.getName, Seq(file))
      }
    }

    test("multiple files pass Assignment" + num) {
      val directories = root.listFiles.filter(_.isDirectory).filter(_.getName.matches("J[0-9].*"))

      for ((directory, i) <- directories.zipWithIndex) {
        print((i + 1) + "/" + directories.length + " ")
        assertPass(directory.getName, allFilesInDir(directory.getAbsolutePath))
      }
    }

    test("multiple files reject Assignment" + num) {
      val directories = root.listFiles.filter(_.isDirectory).filter(_.getName.matches("Je.*"))

      for ((directory, i) <- directories.zipWithIndex) {
        print((i + 1) + "/" + directories.length + " ")
        assertRejectBecauseOf[Throwable](directory.getName, allFilesInDir(directory.getAbsolutePath))
      }
    }
  }

  (stdlibs zip roots).zipWithIndex.foreach {
    x => runTestForAssignment(allFilesInDir(x._1._1), new File(x._1._2), x._2 + 1)
  }


}
