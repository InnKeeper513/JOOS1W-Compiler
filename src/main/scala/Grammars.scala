import Scanner.Token
import scala.annotation.tailrec

object Grammars {
  case class Production(lhs: String, rhs: Seq[String]) {

  }

  class Tree(val lhs: Token, val children: Seq[Tree] = Seq.empty) {
    def production: String = lhs.kind + " " + children.map(_.lhs.kind).mkString(" ")
    def show(indent: Int = 0): String = " " * indent + lhs + "\n" + children.map(_.show(indent + 1)).mkString
    def original(): String = lhs.lexeme + children.map(_.original()).mkString(" ")

    override def toString: String = show(0)
    def size: Int = children.size
  }

  def collect(tree: Tree, lhsKind: String): Seq[Tree] =
    if(tree.lhs.kind == lhsKind) Seq(tree) else tree.children.flatMap((tree: Tree) => collect(tree, lhsKind))


  def readGrammar(lines: Iterator[String]): (Map[(Int, String), (String, Int)], List[Production]) = {

    def parseLine(line: String): Production = {
      val tokens = line.trim.split(" ").filterNot(_.isEmpty)
      Production(tokens.head, tokens.tail)
    }

    var productions: List[Production] = Nil
    var reduceRulesMap = Map[(Int, String), (String, Int)]()

    1 to 2 foreach {
      _ => 1 to lines.next().toInt foreach {
        _ => lines.next()
      }
    }

    lines.next() // start non-terminal

    val numProduction = lines.next().toInt

    1 to numProduction foreach {
      _ => productions = productions :+ parseLine(lines.next())
    }


    lines.next() // number of states

    val numTrans = lines.next().toInt
    1 to numTrans foreach {
      _ => {
        val trans = lines.next().split(" ")
        reduceRulesMap = reduceRulesMap + ((trans(0).toInt, trans(1)) -> ((trans(2), trans(3).toInt)))
      }
    }

    (reduceRulesMap, productions)
  }

}
