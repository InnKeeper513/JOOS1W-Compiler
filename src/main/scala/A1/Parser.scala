package A1

import scala.io.Source
import A1.Grammars._
import A1.Scanner.Token
import A2.Asts.CompilationUnit
import A1.Scanner.tokenizeJoos1w
import A1.Weeding.weedJoos1w
import A2.Asts.treeToAst

object Parser {

  case class ParserException(message: String = "") extends Throwable {
    override def getMessage: String = message
  }


  def parseLALR1(reduceRulesMap: Map[(Int, String), (String, Int)], productions: List[Production], tokens: Seq[Token]): Tree = {
    var states: Seq[Int] = 0 :: Nil
    var nodes: Seq[Tree] = Nil

    tokens.foreach(token => {

      while (reduceRulesMap.isDefinedAt((states.last, token.kind)) &&
          reduceRulesMap((states.last, token.kind))._1 == "reduce") {

        val reduceRule: (String, Int) = reduceRulesMap((states.last, token.kind))
        val nextProduction = productions(reduceRule._2)

        val n = nextProduction.rhs.length
        val children = nodes.takeRight(n)
        nodes = nodes.dropRight(n)
        states = states.dropRight(n)

        val newNode = new Tree(Token(nextProduction.lhs), children)

        nodes = nodes :+ newNode
        states = states :+ reduceRulesMap((states.last, nextProduction.lhs))._2
      }

      if (!reduceRulesMap.isDefinedAt((states.last, token.kind))) {
        throw ParserException("Cannot parse at " + nodes.map(_.lhs.kind).mkString(" ") + " " + token.kind)
      }

      val leaf = new Tree(token)
      nodes = nodes :+ leaf
      states = states :+ reduceRulesMap((states.last, token.kind))._2

    })

    new Tree(Token("S"), nodes)
  }


  val (joos1wReduceRulesMap, joos1wProductions) = readGrammar(Source.fromResource("output.lr1").getLines)

  def parseJoos1w(tokens: Seq[Token]): Tree = {
    parseLALR1(joos1wReduceRulesMap, joos1wProductions, tokens)
  }

  def compileFile(filename: String): CompilationUnit = {
    val source = Source.fromFile(filename)
    val str = (for (line <- source.getLines()) yield line).mkString("\n")

    val tokens = tokenizeJoos1w(str)
    val root = parseJoos1w(tokens)

    val classnames = collect(root, "TypeDeclaration").map(_.children.head)
    val word = filename.split("/").last
    classnames.head.children.foreach(name => {
      if (name.lhs.kind == "IDENTIFIER" && name.lhs.lexeme != word.substring(0, word.length - 5)) {
        throw ParserException("File Name Must Match With Class or Interface Name.")
      }
    })
    weedJoos1w(root)
    treeToAst(root).head.asInstanceOf[CompilationUnit]
  }

}
