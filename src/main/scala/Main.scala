import Parser.compileFile
import StaticAnalysis.joos1wA4

object Main {

  val usage: String =
    """
      |Usage: joosc <source files>
    """.stripMargin

  def main(args: Array[String]): Unit = {

    if (args.isEmpty) println(usage)

    try {
      val cpUnits = args.map(compileFile)
      joos1wA4(cpUnits)
    } catch {
      case e: Throwable =>
        println(e)
        System.exit(42)
    }
    System.exit(0)
  }
}
