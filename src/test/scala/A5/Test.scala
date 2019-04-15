package A5;
import java.io.File

import org.scalatest.FunSuite
import Utils._

class Test extends FunSuite{

    def compile(f: File): Asts.CompilationUnit = {
        val source = io.Source.fromFile(f)
        val str = (for (line <- source.getLines()) yield line).mkString("\n")
        val tokens = tokenizeJoos1w(str)
        val root = parseJoos1w(tokens)
        weedJoos1w(root)
        treeToAst(root).head.asInstanceOf[CompilationUnit]
    }

    val stdlib: Seq[File] = allFilesInDir("resources/testcases/stdlib/5.0")
    val stdlibCompiled: Seq[CompilationUnit] = stdlib.map(compile)
    val root = new File("resources/testcases/a5/")

    def testAndPrint(testName: String, files: Seq[File]) = {

      val asts: Seq[CompilationUnit] = files.map(compile) ++ stdlibCompiled
//        println(asts)
        joos1wA5(asts)
    }

    test("single file test"){
        val file = new File(root.getAbsolutePath + "/mytest.java")
        testAndPrint(file.getName, Seq(file))
    }


}
