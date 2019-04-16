package A1

import A1.Scanner.Token

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

  val levelMap: Map[String, Int] = Map(
      "S" -> 1,
      "CompilationUnit" -> 2,
      "PackageDeclaration" -> 3,
      "ImportDeclarations" -> 3,
      "ImportDeclaration" -> 3,
      "TypeDeclaration" -> 3,
      "ClassDeclaration" -> 4,
      "InterfaceDeclaration" -> 4,
      "ClassBody" -> 5,
      "ClassBodyDeclaration" -> 6,
      "ClassBodyDeclarations" -> 6,
      "ClassMemberDeclaration" -> 7,
      "FieldDeclaration" -> 7,
      "MethodDeclaration" -> 7,
      "ConstructorDeclaration" -> 7,
      "InterfaceBody" -> 5,
      "InterfaceMemberDeclarations" -> 6,
      "InterfaceMemberDeclaration" -> 6,
      "AbstractMethodDeclaration" -> 6,
      "Super" -> 6,
      "Interfaces" -> 6,
  ).withDefault(d => {
    if (d.toLowerCase.contains("statement")) 10
    else 15
  })

  def collect(tree: Tree, lhsKind: String): Seq[Tree] = {
    if (levelMap(tree.lhs.kind) > levelMap(lhsKind)) return Seq()
    if (Set("Assignment", "UnaryExpression", "UnaryExpressionNotPlusMinus", "PrimaryNoNewArray", "ArrayCreationExpression", "Literal", "THIS", "ClassInstanceCreationExpression", "FieldAccess", "MethodInvocation").contains(tree.lhs.kind)) return Seq()
    if(tree.lhs.kind == lhsKind) Seq(tree) else tree.children.flatMap((child: Tree) => collect(child, lhsKind))
  }


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
