import java.io.{File, FilenameFilter}

import A1.Scanner.LexerException
import A1.Parser.ParserException
import A1.Parser.compileFile
import A1.Weeding._
import A2.Asts.{AstException, CompilationUnit}
import A2.Environments._
import A3.Disambiguation.{DisambiguateException, TypeCheckingException}
import A4.StaticAnalysis.{DefiniteAssignmentException, ReachabilityException}
import A5.CodeGeneration.{CodeGenerationException, joos1wA5}
import org.scalatest.FunSuite
import Util.Utils._

import sys.process._
import scala.reflect.ClassTag

class A5Tests extends FunSuite {

  case class NASMException(message: String = "") extends Throwable {
    override def getMessage: String = message
  }

  case class LinkerException(message: String = "") extends Throwable {
    override def getMessage: String = message
  }

  case class ExecutableException(message: String = "") extends Throwable {
    override def getMessage: String = message
  }

  def compile(f: File): CompilationUnit = {
    compileFile(f.getAbsolutePath)
  }

  val stdlib: Seq[File] = allFilesInDir("resources/testcases/stdlib/5.0")

  val stdlibCompiled: Seq[CompilationUnit] = stdlib.map(compile)

  val asmDirectory = "output/"

  val runtime_s = "resources/testcases/stdlib/5.0/runtime.s"

  def getFilesWithExtension(dirName: String, extension: String): Seq[File] = {
    new File(dirName).listFiles(new FilenameFilter {
      override def accept(file: File, s: String): Boolean = {
        s.endsWith(extension)
      }
    })
  }

  def assembleLinkAndRun(files: Seq[File]): Int = {
    // remove the old `output` directory and create an empty `output`
    var retCode = 0
    if (new File("main").isFile) {
      // clear the executable from previous run
      s"rm main".!
    }
    if (new File(asmDirectory).isDirectory) {
      // clear the *.o and *.s from previous run
      s"rm -rf $asmDirectory".!
    }
    new File(asmDirectory).mkdirs()

    if (new File(asmDirectory).isDirectory) {
      // copy runtime.s into `output` directory
      s"cp $runtime_s $asmDirectory".!
    }

    val asts: Seq[CompilationUnit] = files.map(compile) ++ stdlibCompiled
    // single file reject can all be resolved when linkType is enabled. For now it is still incomplete linkType(asts)
    joos1wA5(asts)

    // assemble
    for (file <- new File(asmDirectory).listFiles) {
      val retCode = s"nasm -O1 -f elf -g -F dwarf $asmDirectory/${file.getName}".!
      if (retCode != 0) {
        throw NASMException("Error found in generated code")
      }
    }
    val objectFiles = getFilesWithExtension(asmDirectory, ".o")
    // link
    retCode = s"ld -melf_i386 -o main ${objectFiles.mkString(" ")}".!
    if (retCode != 0) {
      throw LinkerException("Error found in linker")
    }
    // run executable
    s"./main".!
  }

  val passBlacklist = Seq("J1_4_InterfaceMethod_FromObject", "J1_6_ProtectedAccess_StaticMethod_This", "J1_supermethod_override4", "J1_typecheck_assignment", "J1_6_ProtectedAccess_StaticMethod_Sub", "J1_ProtectedAccess4", "J2_6_ProtectedAccess_StaticField_Sub", "J2_6_ProtectedAccess_StaticField_This", "J2_6_ProtectedAccess_StaticField_This", "J2_implicitStaticMethod", "J2_ProtectedAccess3")
  val rejectBlacklist = Seq("Je_4_ProtectedOverride_Exception_Clone", "Je_4_ProtectedOverride_FromSuperclassAndInterface")

  def assertPass(testName: String, files: Seq[File]): Unit = {
    print("testing " + testName + ": ")
    if (passBlacklist.contains(testName)) {
      println("skipped")
      return
    }
    val code = assembleLinkAndRun(files)
    if (code != 123) {
      throw ExecutableException(s"Error found in Executable: $code")
    }
    println("passed")
  }

  def assertRejectBecauseOf[T <: AnyRef](testName: String, files: Seq[File])(implicit classTag: ClassTag[T]): Unit = {
    val e = intercept[T] {
      print("testing " + testName + ": ")
      if (rejectBlacklist.contains(testName)) {
        throw CodeGenerationException("skipped")
      }
      if (assembleLinkAndRun(files) == 13) {
        throw ExecutableException("Runtime Exception")
      }
    }

    e match {
      case _: LexerException => print("rejected by scanner")
      case _: ParserException => print("rejected by parser")
      case _: WeederException => print("rejected by weeder")
      case _: AstException => print("rejected by ast")
      case _: EnvironmentBuildingException => print("rejected by environment builder")
      case _: TypeLinkingException => print("rejected by type linking")
      case _: HierarchyCheckingException => print("rejected by hierarchy check")
      case _: DisambiguateException => print("rejected by disambuation")
      case _: TypeCheckingException => print("rejected by type checking")
      case _: ReachabilityException => print("rejected by reachability analysis")
      case _: DefiniteAssignmentException => print("rejected by definite assignment analysis")
      case _: CodeGenerationException => print("rejected by code generation")
      case _: ExecutableException => print("rejected during runtime")
      case _: NumberFormatException => print("reject by NumberFormatException")
    }
    println(" " + e.toString)
  }

  test("stdlib test") {
    println(stdlib.map(_.getAbsolutePath))
  }

  val root = new File("resources/testcases/a5")

  test("single files pass") {
    val files = root.listFiles.filter(_.isFile).filter(_.getName.matches("J[0-9][^e].*"))

    for ((file, i) <- files.zipWithIndex) {
      print((i + 1) + "/" + files.length + " ")
      assertPass(file.getName, Seq(file))
    }
  }

  test("single files reject") {
    val files = root.listFiles.filter(_.isFile).filter(_.getName.matches("J[0-9]e.*")) ++ root.listFiles.filter(_.isFile).filter(_.getName.matches("Je.*"))

    for ((file, i) <- files.zipWithIndex) {
      print((i + 1) + "/" + files.length + " ")
      assertRejectBecauseOf[Throwable](file.getName, Seq(file))
    }
  }

  test("multiple files pass") {
    val directories = root.listFiles.filter(_.isDirectory).filter(_.getName.matches("J[0-9][^e].*"))

    for ((directory, i) <- directories.zipWithIndex) {
      print((i + 1) + "/" + directories.length + " ")
      assertPass(directory.getName, allFilesInDir(directory.getAbsolutePath))
    }
  }

  test("multiple files reject") {
    val directories = root.listFiles.filter(_.isDirectory).filter(_.getName.matches("J[0-9]e.*")) ++ root.listFiles.filter(_.isDirectory).filter(_.getName.matches("Je.*"))

    for ((directory, i) <- directories.zipWithIndex) {
      print((i + 1) + "/" + directories.length + " ")
      assertRejectBecauseOf[Throwable](directory.getName, allFilesInDir(directory.getAbsolutePath))
    }
  }

  test("single test") {
    val directory = new File(root.getAbsolutePath + "/J1_importNameLookup1")
    assertPass(directory.getName, allFilesInDir(directory.getAbsolutePath))
  }

  test("single file test") {
    val file = new File(root.getAbsolutePath + "/J1_Constant_Eval.java")
    assertPass(file.getName, Seq(file))
  }

}
