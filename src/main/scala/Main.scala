import java.io.{File, FilenameFilter}

import A2.Asts
import A2.Asts._
import A1.Weeding._
import A1.Parser._
import A1.Scanner._
import Util.Utils._
import A5.CodeGeneration._

import sys.process._

object Main {
  case class NASMException(message: String = "") extends Throwable {
    override def getMessage: String = message
  }
  case class LinkerException(message: String = "") extends Throwable {
    override def getMessage: String = message
  }

  def compile(f: File): Asts.CompilationUnit = {
    val source = io.Source.fromFile(f)
    val str = (for (line <- source.getLines()) yield line).mkString("\n")
    val tokens = tokenizeJoos1w(str)
    val root = parseJoos1w(tokens)
    weedJoos1w(root)
    treeToAst(root).head.asInstanceOf[CompilationUnit]
  }

  def getFilesWithExtension(dirName: String, extension: String): Seq[File] = {
    new File(dirName).listFiles(new FilenameFilter {
      override def accept(file: File, s: String): Boolean = {
        s.endsWith(extension)
      }
    })
  }

  val stdlib: Seq[File] = allFilesInDir("resources/testcases/stdlib/5.0")
  val stdlibCompiled: Seq[CompilationUnit] = stdlib.map(compile)
  val runtime_s: String = "resources/testcases/stdlib/5.0/runtime.s"
  val asmDirectory = "output/"

  val usage: String =
    """
      |Usage: joosc <source files directory>
    """.stripMargin

  def main(args: Array[String]): Unit = {
    val sourceDir = new File(args(0))

    if (args.isEmpty || !sourceDir.isDirectory) println(usage)

    // Get all the files
    val files = allFilesInDir(args(0))

    new File(asmDirectory).mkdirs()
    if (new File(asmDirectory).isDirectory) {
      // copy runtime.s into `output` directory
      s"cp $runtime_s $asmDirectory".!
    }

    try {
      // Collect all compilation units
      val cpUnits: Seq[CompilationUnit] = files.map(compile) ++ stdlibCompiled
      var retCode = 0

      s"cp $runtime_s $args(0)".!
      joos1wA5(cpUnits)

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

    } catch {
      case e: Throwable =>
        println(e)
        System.exit(42)
    }
    System.exit(0)
  }
}
