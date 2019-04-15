import Grammars.Tree
import Grammars.collect
import Environments.Environment
import scala.util.control.Breaks._

object Asts {

  val indentSize = 2

  /***** Enums
    * Make the enum name the same as the terminals
    * so that we can get the enum by calling
    * EnumClass.Value.withName("enum_name")
  *****/
  object LiteralType extends Enumeration {
    val INTEGER_LITERAL, CHAR_LITERAL, STRING_LITERAL, BOOLEAN_LITERAL, NULL_LITERAL = Value
  }

  object Modifier extends Enumeration {
    val PUBLIC, PROTECTED, STATIC, ABSTRACT, FINAL, NATIVE = Value
  }

  object PrimitiveType extends Enumeration {
    val BYTE, SHORT, CHAR, INT, BOOLEAN = Value
  }

  object BinaryOperator extends Enumeration {
    val LOR, LAND, EOR, EXOR, EAND, NEQ, EQ, INSTANCEOF, LT, GT, GREATER, LESS, MINUS, PLUS,
      PERC, DIV, MULTI = Value
  }

  // location for VariableExpression
  object VariableLocation extends Enumeration {
    val LocalVariable, LocalField, ExternalField = Value
  }
  /***** Enums *****/


  /***** ConstValue (Not treated as Ast) *****/
  sealed abstract class ConstValue

  case class ConstInt(intValue: Int) extends ConstValue {
    def getValue: Int = intValue
    override def toString: String = intValue.toString
  }

  case class ConstShort(shortValue: Short) extends ConstValue {
    def getValue: Short = shortValue
    override def toString: String = shortValue.toString
  }

  case class ConstChar(charValue: Char) extends ConstValue {
    def getValue: Char = charValue
    override def toString: String = charValue.toString
  }

  case class ConstByte(byteValue: Byte) extends ConstValue {
    def getValue: Byte = byteValue
    override def toString: String = byteValue.toString
  }

  case class ConstBoolean(booleanValue: Boolean) extends ConstValue {
    def getValue: Boolean = booleanValue
    override def toString: String = booleanValue.toString
  }

  case class ConstString(stringValue: String) extends ConstValue {
    def getValue: String = stringValue
    override def toString: String = stringValue.toString
  }

  /***** Type (Not treated as Ast) *****/
  sealed abstract class Type {
    var isParenthesizedType: Boolean = false
    var assignable: Boolean = true
    var isStatic: Boolean = false
  }
  case class VoidType() extends Type{
    override def toString: String = "VOID"
  }
  case class PrimitiveType(pType: PrimitiveType.Value) extends Type {
    override def toString: String = pType.toString
  }
  abstract class ReferenceType extends Type
  case class ArrayType(aType: Type) extends ReferenceType {
    // we're relying on type.toString to generate labels, and [] is not recognized by nasm so I changed it to "Array"
    override def toString: String = aType + "Array"
  }
  // FIXME: should `ClassOrInterfaceType` exists?
  case class ClassOrInterfaceType(name: Name) extends ReferenceType {
    override def toString: String = name.toString
  }
  case class ClassType(name: Name) extends ReferenceType {
    override def toString: String = name.toString
  }
  case class InterfaceType(name: Name) extends ReferenceType {
    override def toString: String = name.toString
  }
  case class NullType() extends Type {
    override def toString: String = getClass.getName
  }

  /** Scan a Type tree node, return the corresponding Type
    *
    * @param typeTree the tree with production rule that starts with
    *                 Type, PrimitiveType, ReferenceType,
    *                 ClassOrInterfaceType, ClassType, InterfaceType, or ArrayType
    * @return the Type
    */
  def getType(typeTree: Tree): Type = {
    assert(Set("Type", "PrimitiveType", "ReferenceType", "ClassOrInterfaceType", "InterfaceType", "ArrayType",
              "ClassType", "InterfaceType").contains(typeTree.lhs.kind))
    typeTree.lhs.kind match {
      case "PrimitiveType" => PrimitiveType(PrimitiveType.withName(typeTree.children.head.lhs.kind))
      case "ClassOrInterfaceType" => ClassOrInterfaceType(getNameAst(typeTree.children.head))
      case "ClassType" =>
        val classOrInterfaceType = getType(typeTree.children.head).asInstanceOf[ClassOrInterfaceType]
        ClassType(classOrInterfaceType.name)
      case "InterfaceType" =>
        val classOrInterfaceType = getType(typeTree.children.head).asInstanceOf[ClassOrInterfaceType]
        InterfaceType(classOrInterfaceType.name)
      case "ArrayType" =>
        if (typeTree.production == "ArrayType PrimitiveType LEFT_SQUARE RIGHT_SQUARE")
          ArrayType(getType(typeTree.children.head))
        else // ArrayType Name LEFT_SQUARE RIGHT_SQUARE
          ArrayType(ClassOrInterfaceType(getNameAst(typeTree.children.head)))
      case _ => getType(typeTree.children.head)
    }
  }
  /***** Type *****/


  /***** Literal *****/
  sealed abstract class Literal(lexeme: String) {
    def getType: LiteralType.Value
    def getLexeme: String = lexeme
    override def toString: String = "(" + getType + ", " + lexeme + ")"
  }

  case class IntegerLiteral(lexeme: String) extends Literal(lexeme) {
    override def getType: LiteralType.Value = LiteralType.INTEGER_LITERAL
  }

  case class CharLiteral(lexeme: String) extends Literal(lexeme) {
    override def getType: LiteralType.Value = LiteralType.CHAR_LITERAL
  }

  case class StringLiteral(lexeme: String) extends Literal(lexeme) {
    override def getType: LiteralType.Value = LiteralType.STRING_LITERAL
  }

  case class BooleanLiteral(lexeme: String) extends Literal(lexeme) {
    override def getType: LiteralType.Value = LiteralType.BOOLEAN_LITERAL
  }

  case class NullLiteral(lexeme: String) extends Literal(lexeme) {
    override def getType: LiteralType.Value = LiteralType.NULL_LITERAL
  }

  def getLiteralAst(tree: Tree): Literal = {
    assert(tree.lhs.kind == "Literal")
    val literalType = LiteralType.withName(tree.children.head.lhs.kind)
    val lexeme = tree.children.head.lhs.lexeme
    literalType match {
      case LiteralType.INTEGER_LITERAL => IntegerLiteral(lexeme)
      case LiteralType.CHAR_LITERAL => CharLiteral(lexeme)
      case LiteralType.STRING_LITERAL => StringLiteral(lexeme)
      case LiteralType.BOOLEAN_LITERAL => BooleanLiteral(lexeme)
      case LiteralType.NULL_LITERAL => NullLiteral(lexeme)
    }

  }
  /***** Literal *****/


  /***** Name *****/
  sealed abstract class Name {
    def getSimpleName: SimpleName
    def getFirstSimpleName: SimpleName
    def getQualifiedName: QualifiedName
    var cName: Option[QualifiedName] = None

    // only call this method after type linking is done
    def getCName: QualifiedName = {
      if (cName.isEmpty) throw new Exception() // something is wrong
      cName.get
    }
  }
  case class SimpleName(name: String) extends Name {
    override def toString: String = name
    override def getSimpleName: SimpleName = SimpleName(name)
    override def getFirstSimpleName: SimpleName = SimpleName(name)
    override def getQualifiedName: QualifiedName = QualifiedName(Seq(name))
  }
  case class QualifiedName(names: Seq[String]) extends Name {
    override def toString: String = names.mkString(".")
    override def getSimpleName: SimpleName = SimpleName(names.last)
    override def getFirstSimpleName: SimpleName = SimpleName(names.head)
    override def getQualifiedName: QualifiedName = QualifiedName(names)
  }

  /** Scan a tree node, return the corresponding Name Ast
    *
    * @param nameTree the tree with production rule that starts with Name
    * @return the Name (could be SimpleName or QualifiedName)
    */
  def getNameAst(nameTree: Tree): Name = {
    assert(nameTree.lhs.kind == "Name")
    val names: Seq[String] = collect(nameTree, "IDENTIFIER").map(_.lhs.lexeme)
    if (names.size == 1) SimpleName(names.head) else QualifiedName(names)
  }
  /***** Name *****/


  /***** Parameters *****/
  case class Parameter(
                        parameterType: Type,
                        parameterName: SimpleName,
                      ) {
    def show(indent: Int): String = {
      " " * indent + this.getClass.getSimpleName + ": " + parameterName + ": " + parameterType + "\n"
    }
  }
  /***** Parameters *****/


  sealed abstract class Ast {
    var children: Seq[Ast] = Seq.empty
    def astName: String = this.getClass.getSimpleName
    def show(indent: Int = 0): String = {
      " " * indent + astName + "\n" + children.map(_.show(indent + indentSize)).mkString
    }

    override def toString: String = show()

    // this environment value will be filled after we call buildEnvironment()
    var environment: Option[Environment] = None
    var astType: Option[Type] = None
    // constEval is filled when we perform static analysis
    var constEval: Option[ConstValue] = None
  }


  /***** Compilation Unit *****/
  case class CompilationUnit() extends Ast
  case class PackageDecl(name: Name) extends Ast {
    override def show(indent: Int): String = {
      " " * indent + this.getClass.getSimpleName + " " + name + "\n"
    }

    def qualifiedName: QualifiedName =
       name match {
         case q: QualifiedName => q
         case s: SimpleName => QualifiedName(Seq(s.name))
       }
  }
  case class ImportDecl(onDemand: Boolean, name: Name) extends Ast {
    override def show(indent: Int): String = {
      " " * indent + this.getClass.getSimpleName +
      (if (onDemand) " OnDemand " else " Single ") + name + "\n"
    }
  }

  case class ClassDecl(
                      modifiers: Seq[Modifier.Value],
                      className: SimpleName,
                      parent: Name,
                      var interfaces: Seq[Name],
                      ) extends Ast {
    override def show(indent: Int): String = {
      " " * indent + this.getClass.getSimpleName + ": " + className + "\n" +
      " " * (indent + indentSize) + "Modifiers: " + modifiers.mkString(", ") + "\n" +
      " " * (indent + indentSize) + "Parent: " + parent + "\n" +
      " " * (indent + indentSize) + "interfaces: " + (if(interfaces.isEmpty) "None" else interfaces.mkString(", ")) + "\n" +
      children.map(_.show(indent + indentSize)).mkString
    }
  }

  case class ConstructorDecl(
                            modifiers: Seq[Modifier.Value],
                            constructorName: SimpleName,
                            var parameterLists: Seq[Parameter]
                            ) extends Ast {
    override def show(indent: Int): String = {
      " " * indent + this.getClass.getSimpleName + ": " + constructorName + "\n" +
      " " * (indent + indentSize) + "Modifiers: " + modifiers.mkString(", ") + "\n" +
      " " * (indent + indentSize) + "Parameters: " + (if(parameterLists.isEmpty)"None \n" else "\n" +
        parameterLists.map(_.show(indent + indentSize * 2)).mkString) +
      children.map(_.show(indent + indentSize)).mkString
    }
  }

  case class FieldDecl(
                      modifiers: Seq[Modifier.Value],
                      var fType: Type,
                      name: SimpleName,
                      var initializer: Option[Expression]
                      ) extends Ast {
    override def show(indent: Int): String = {
      " " * indent + this.getClass.getSimpleName + ": " + name + "\n" +
      " " * (indent + indentSize) + "Modifiers: " + modifiers.mkString(", ") + "\n" +
      " " * (indent + indentSize) + "Type: " + fType + "\n" +
      " " * (indent + indentSize) + "Initializer: " +
        (if (initializer.isEmpty) "None" else initializer.get) + "\n"
    }
  }

  case class MethodDecl(
                       name: SimpleName,
                       modifiers: Seq[Modifier.Value],
                       var mType: Type,
                       var parameters: Seq[Parameter],
                       ) extends Ast {
    override def show(indent: Int): String = {
      " " * indent + this.getClass.getSimpleName + ": " + name + "\n" +
      " " * (indent + indentSize) + "Modifiers: " + modifiers.mkString(", ") + "\n" +
      " " * (indent + indentSize) + "Method Type: " + mType + "\n" +
      " " * (indent + indentSize) + "Parameters: " + (if(parameters.isEmpty) "None \n" else "\n" +
      parameters.map(_.show(indent + indentSize * 2)).mkString) +
      children.map(_.show(indent + indentSize)).mkString
    }
  }

  case class InterfaceMethodDecl(
                                  name: SimpleName,
                                  modifiers: Seq[Modifier.Value],
                                  var mType: Type,
                                  var parameters: Seq[Parameter],
                                ) extends Ast {
    override def show(indent: Int): String = {
      " " * indent + this.getClass.getSimpleName + ": " + name + "\n" +
        " " * (indent + indentSize) + "Modifiers: " + modifiers.mkString(", ") + "\n" +
        " " * (indent + indentSize) + "Method Type: " + mType + "\n" +
        " " * (indent + indentSize) + "Parameters: " + (if(parameters.isEmpty) "None \n" else "\n" +
        parameters.map(_.show(indent + indentSize * 2)).mkString)
    }
  }

  case class InterfaceDecl(
                        name: SimpleName,
                        modifiers: Seq[Modifier.Value],
                        var interfaces: Seq[Name],
                        ) extends Ast {
    override def show(indent: Int): String = {
      " " * indent + this.getClass.getSimpleName + ": " + name  + "\n" +
      " " * (indent + indentSize) + "Modifiers: " + modifiers.mkString(", ") + "\n" +
      " " * (indent + indentSize) + "Interfaces: " + (if(interfaces.isEmpty) "None" else interfaces.mkString(", ")) + "\n"
    }
  }

  /***** Compilation Unit *****/


  /***** Expression *****/
  sealed abstract class Expression extends Ast {
    override def toString: String = this.getClass.getSimpleName
  }
  /* corresponds to `AssignmentExpression Assignment`, under `Assignment` node */
  case class Assignment(lhs: Expression, rhs: Expression) extends Expression {
    override def toString: String = "(=: " + lhs + ", " + rhs + ")"
  }
  /* corresponds to every binary expression, under a `ConditionalExpression` node */
  case class BinaryExpression(op: BinaryOperator.Value, exp1: Expression, exp2: Expression) extends Expression {
    override def toString: String = "(" + op + ": " + exp1 + ", " + exp2 + ")"
  }
  // everything under `UnaryExpression` node:
  case class MinusExpression(exp: Expression) extends Expression {
    override def toString: String = "(Unary- " + exp + ")"
  }
  case class NotExpression(exp: Expression) extends Expression {
    override def toString: String = "(Unary! " + exp + ")"
  }
  case class CastExpression(var cType: Type, exp: Expression) extends Expression {
    override def toString: String = "(Cast=>" + cType + ": " + exp + ")"
  }
  case class VariableExpression(name: Name) extends Expression {
    var baseType: Type = VoidType() // the type that the basename resolves to
    var accessName: QualifiedName = QualifiedName(Seq.empty) // name that is used to access fields
    var modifiers: Seq[Modifier.Value] = Seq.empty

    var variableLocation: VariableLocation.Value = null

    override def toString: String = "(Var " + name.toString + ")"
  }
  case class ArrayCreationExpression(var aType: Type, sizeExp: Expression) extends Expression {
    override def toString: String = "(ArrayTypeCreation " + aType + "[" + sizeExp + "])"
  }
  case class LiteralExpression(literal: Literal) extends Expression {
    override def toString: String = literal.toString
  }
  case class ThisExpression() extends Expression
  case class ClassInstanceCreationExpression(var cType: ClassType, var args: Seq[Expression]) extends Expression {
    override def toString: String = "(CreateInstance " + cType + " " + args.map(_.toString).mkString(",") + ")"
  }
  case class FieldAccessExpression(exp: Expression, name: SimpleName) extends Expression {
    override def toString: String = "(FieldAccess " + exp + "," + name + ")"
  }
  case class MethodInvocationExpression(exp1: Expression, name: SimpleName, var argumentList: Seq[Expression]) extends Expression {
    override def toString: String = "(MethodInvocation " + exp1 + ", " + name + " " + argumentList.map(_.toString).mkString(",") + ")"
  }
  case class ArrayAccessExpression(exp1: Expression, exp3: Expression) extends Expression {
    override def toString: String = "(ArrayAccess " + exp1 + "[" + exp3 + "])"
  }
  case class InstanceOfExpression(exp: Expression, var iType: Type) extends Expression {
    override def toString: String = "(InstanceOf " + exp + "[" + iType + "])"
  }
  case class ParenthesesExpression(exp: Expression) extends Expression {
    override def toString: String = "(" + exp + ")"
  }

  def getExpressionAst(expTree: Tree): Expression = {
    expTree.lhs.kind match {
      case "Assignment" => Assignment(getExpressionAst(expTree.children.head), getExpressionAst(expTree.children.last))

      case "UnaryExpression" if expTree.size == 2 =>
        val childExp = getExpressionAst(expTree.children.last)
        childExp match {
          case LiteralExpression(x) if x.getType == LiteralType.INTEGER_LITERAL =>
            LiteralExpression(IntegerLiteral("-" + x.getLexeme))
          case x => MinusExpression(x)
        }

      case "UnaryExpressionNotPlusMinus" if expTree.size == 2 =>
        NotExpression(getExpressionAst(expTree.children.last))

      case "Name" => VariableExpression(getNameAst(expTree))

      case "PrimaryNoNewArray" =>
        if (expTree.size == 3)
          ParenthesesExpression(getExpressionAst(expTree.children(1)))
        else getExpressionAst(expTree.children.head)

      case "ArrayCreationExpression" =>
        ArrayCreationExpression(
          getType(expTree.children(1)),
          getExpressionAst(expTree.children.last.children(1))
        )

      case "Literal" => LiteralExpression(getLiteralAst(expTree))

      case "THIS" => ThisExpression()

      case "ClassInstanceCreationExpression" =>
        if (expTree.size == 5)
          ClassInstanceCreationExpression(
            getType(expTree.children(1)).asInstanceOf[ClassType],
            collect(expTree.children(3), "Expression").map(getExpressionAst)
          )
        else
          ClassInstanceCreationExpression(
            getType(expTree.children(1)).asInstanceOf[ClassType],
            Seq.empty
          )

      case "FieldAccess" =>
        val name: String = expTree.children(2).lhs.lexeme
        val primaryTree: Tree = expTree.children.head.children.head
        FieldAccessExpression(getExpressionAst(primaryTree), SimpleName(name))

      case "MethodInvocation" =>
        // MethodInvocationExpression(exp1: Expression, name: SimpleName, argumentList: Seq[Expression])
        val nameOrPrimaryTree: Tree = expTree.children.head
        if (nameOrPrimaryTree.lhs.kind == "Name") {
          val argumentTree: Tree = expTree.children(2)
          var arguments: Seq[Expression] = Seq()
          if(argumentTree.lhs.kind == "ArgumentList"){
            arguments = collect(argumentTree, "Expression").map(x => getExpressionAst(x))
          }

          val name = getNameAst(nameOrPrimaryTree)
          name match {
            case simpleName: SimpleName =>
              MethodInvocationExpression(ThisExpression(), simpleName, arguments)
            case qualifiedName: QualifiedName =>
              MethodInvocationExpression(
                VariableExpression(QualifiedName(qualifiedName.names.dropRight(1))),
                SimpleName(qualifiedName.names.last),
                arguments
              )
          }
        } else {
          val name: String = expTree.children(2).lhs.lexeme
          val argumentTree: Tree = expTree.children(4)
          var arguments: Seq[Expression] = Seq()
          if(argumentTree.lhs.kind == "ArgumentList") {
            arguments = collect(argumentTree, "Expression").map(x => getExpressionAst(x))
          }
          MethodInvocationExpression(getExpressionAst(expTree.children.head), SimpleName(name), arguments)
        }

      case "ArrayAccess" =>
        val exp: Tree = expTree.children(2)
        ArrayAccessExpression(getExpressionAst(expTree.children.head), getExpressionAst(exp))

      case "CastExpression" =>
        // try to figure out the type
        if (expTree.production == "CastExpression LEFT_PAREN PrimitiveType Dims RIGHT_PAREN UnaryExpression") {
          CastExpression(
            ArrayType(getType(expTree.children(1))),
            getExpressionAst(expTree.children.last)
          )
        } else if (expTree.production == "CastExpression LEFT_PAREN PrimitiveType RIGHT_PAREN UnaryExpression") {
          CastExpression(
            getType(expTree.children(1)),
            getExpressionAst(expTree.children.last)
          )
        } else if (expTree.production == "CastExpression LEFT_PAREN Expression RIGHT_PAREN UnaryExpressionNotPlusMinus") {
          /*
           The Expression between LEFT_PAREN and RIGHT_PAREN should be a Name
           We can reduce the expression to a VariableExpression, and steal the name and put the name
           into a ClassOrInterfaceType
           */
          val variableExpression: VariableExpression = {
            try {
              // TODO refactor
              assert(collect(expTree.children(1), "LEFT_PAREN").isEmpty)
              getExpressionAst(expTree.children(1)).asInstanceOf[VariableExpression]
            } catch {
              case e: Throwable => throw e
            }
          }
          val classOrInterfaceType: ClassOrInterfaceType = ClassOrInterfaceType(variableExpression.name)
          CastExpression(
            classOrInterfaceType,
            getExpressionAst(expTree.children.last)
          )
        } else { // CastExpression LEFT_PAREN Name Dims RIGHT_PAREN UnaryExpressionNotPlusMinus
          CastExpression(
            ArrayType(ClassOrInterfaceType(getNameAst(expTree.children(1)))),
            getExpressionAst(expTree.children.last)
          )
        }

      case x if x == "RelationalExpression" && expTree.size == 3 && expTree.children(1).lhs.kind == "INSTANCEOF" =>
        InstanceOfExpression(getExpressionAst(expTree.children.head), getType(expTree.children.last))

      // all binary expression
      case x if Set("ConditionalOrExpression", "ConditionalAndExpression", "InclusiveOrExpression",
                    "ExclusiveOrExpression", "AndExpression", "EqualityExpression", "RelationalExpression",
                    "AdditiveExpression", "MultiplicativeExpression").contains(x) && (expTree.size == 3) =>
        BinaryExpression(
          BinaryOperator.withName(expTree.children(1).lhs.kind),
          getExpressionAst(expTree.children.head),
          getExpressionAst(expTree.children.last)
        )

      // if we don't handle it above, then the node probably has only one non-terminal child
      case _ => getExpressionAst(expTree.children.head)
    }
  }
  /***** Expression *****/


  /***** Statement *****/
  // Handles tree nodes between the level of Block and the level of Expression
  sealed abstract class Statement extends Ast

  // It's important to keep the Block Structure to track the scope information
  case class Block() extends Statement
  case class LocalVariableDeclarationStatement(
                                              var vType: Type,
                                              name: SimpleName,
                                              var initializer: Option[Expression]
                                              ) extends Statement {
    override def show(indent: Int): String = {
      " " * indent + this.getClass.getSimpleName + " " + name + ": " + vType +
        (if (initializer.isEmpty) "" else " = " + initializer.get) + "\n"
    }
  }
  // for expression that can be a statement
  case class ExpressionStatement(
                                exp: Expression
                                ) extends Statement {
    override def show(indent: Int): String = {
      " " * indent + this.getClass.getSimpleName + ": " + exp.toString + "\n"
    }
  }
  case class IfElseStatement(
                            condition: Expression,
                            var thenStmt: Statement,
                            var elseStmt: Statement
                            ) extends Statement {
    override def show(indent: Int): String = {
      " " * indent + this.getClass.getSimpleName + "\n" +
        " " * (indent + indentSize) + "Condition: " + condition + "\n" +
        " " * (indent + indentSize) + "ThenStmt: \n" + thenStmt.show(indent + indentSize * 2) +
        " " * (indent + indentSize) + "ElseStmt: \n" + elseStmt.show(indent + indentSize * 2)
    }
  }
  case class WhileStatement(
                           condition: Expression,
                           var loopStmt: Statement,
                           ) extends Statement {
    override def show(indent: Int): String = {
      " " * indent + this.getClass.getSimpleName + "\n" +
        " " * (indent + indentSize) + "Condition: " + condition + "\n" +
        " " * (indent + indentSize) + "LoopStmt: \n" + loopStmt.show(indent + indentSize * 2)
    }
  }
  case class ForStatement(
                         /*
                         the initStmt is either ExpressionStatement or LocalVariableDeclarationStatement
                         might need to use instanceof to figure out exactly which one it is
                          */
                         var initStmt: Option[Statement],
                         var condition: Option[Expression],
                         var updateStmt: Option[ExpressionStatement],
                         var loopStmt: Statement
                         ) extends Statement {
    override def show(indent: Int): String = {
      " " * indent + this.getClass.getSimpleName + "\n" +
        " " * (indent + indentSize) + "InitStmt: " +
          (if (initStmt.isEmpty) "None" else "\n" + initStmt.get.show(indent + indentSize * 2)) +
        " " * (indent + indentSize) + "Condition: " +
          (if (condition.isEmpty) "None" else condition.get) + "\n" +
        " " * (indent + indentSize) + "UpdateStmt: " +
          (if (updateStmt.isEmpty) "None" else "\n" + updateStmt.get.show(indent + indentSize * 2)) +
        " " * (indent + indentSize) + "LoopStmt: \n" + loopStmt.show(indent + indentSize * 2)
    }
  }
  case class ReturnStatement(
                            var returnExp: Option[Expression]
                            ) extends Statement {
    override def show(indent: Int): String = {
      " " * indent + this.getClass.getSimpleName + " " +
        (if (returnExp.isEmpty) "None" else returnExp.get ) + "\n"
    }
  }
  case class EmptyStatement() extends Statement

  def getStatementAst(tree: Tree): Seq[Statement] = {
    tree.lhs.kind match {
      case "Block" =>
        val block = Block()
        block.children = defaultTreeToStmt(tree)
        Seq(block)
      case "LocalVariableDeclaration" =>
        val vType: Type = getType(tree.children.head)
        val variableDecl = tree.children.last
        var initializer: Option[Expression] = None
        val name = SimpleName(variableDecl.children.head.children.head.lhs.lexeme)
        if (variableDecl.children.size == 3) {
          /** this declaration has initializer, then if we transform the
            * `VariableInitializer` node to ast, the result must be an `Expression`
            */
          initializer = Some(treeToAst(variableDecl.children.last).head.asInstanceOf[Expression])
        }
        Seq(LocalVariableDeclarationStatement(
          vType,
          name,
          initializer
        ))
      case "IfThenStatement" =>
        Seq(IfElseStatement(
          getExpressionAst(tree.children(2)),
          getStatementAst(tree.children.last).head,
          EmptyStatement()
        ))
      case "IfThenElseStatement" | "IfThenElseStatementNoShortIf" =>
        Seq(IfElseStatement(
          getExpressionAst(tree.children(2)),
          getStatementAst(tree.children(4)).head,
          getStatementAst(tree.children.last).head
        ))
      case "WhileStatement" | "WhileStatementNoShortIf" =>
        Seq(WhileStatement(
          getExpressionAst(tree.children(2)),
          getStatementAst(tree.children.last).head
        ))
      case "ForStatement" =>
        Seq(tree.production match {
          case "ForStatement FOR LEFT_PAREN ForInit SEMI Expression SEMI ForUpdate RIGHT_PAREN Statement"
            | "ForStatementNoShortIf FOR LEFT_PAREN ForInit SEMI Expression SEMI ForUpdate RIGHT_PAREN StatementNoShortIf" =>
            ForStatement(
              Some(getStatementAst(tree.children(2)).head),
              Some(getExpressionAst(tree.children(4))),
              Some(getStatementAst(tree.children(6)).head.asInstanceOf[ExpressionStatement]),
              getStatementAst(tree.children(8)).head
            )
          case "ForStatement FOR LEFT_PAREN ForInit SEMI Expression SEMI RIGHT_PAREN Statement"
            | "ForStatementNoShortIf FOR LEFT_PAREN ForInit SEMI Expression SEMI RIGHT_PAREN StatementNoShortIf" =>
            ForStatement(
              Some(getStatementAst(tree.children(2)).head),
              Some(getExpressionAst(tree.children(4))),
              None,
              getStatementAst(tree.children(7)).head
            )
          case "ForStatement FOR LEFT_PAREN ForInit SEMI SEMI ForUpdate RIGHT_PAREN Statement"
            | "ForStatementNoShortIf FOR LEFT_PAREN ForInit SEMI SEMI ForUpdate RIGHT_PAREN StatementNoShortIf" =>
            ForStatement(
              Some(getStatementAst(tree.children(2)).head),
              None,
              Some(getStatementAst(tree.children(5)).head.asInstanceOf[ExpressionStatement]),
              getStatementAst(tree.children(7)).head
            )
          case "ForStatement FOR LEFT_PAREN ForInit SEMI SEMI RIGHT_PAREN Statement"
            | "ForStatementNoShortIf FOR LEFT_PAREN ForInit SEMI SEMI RIGHT_PAREN StatementNoShortIf" =>
            ForStatement(
              Some(getStatementAst(tree.children(2)).head),
              None,
              None,
              getStatementAst(tree.children(6)).head
            )
          case "ForStatement FOR LEFT_PAREN SEMI Expression SEMI ForUpdate RIGHT_PAREN Statement"
               | "ForStatementNoShortIf FOR LEFT_PAREN SEMI Expression SEMI ForUpdate RIGHT_PAREN StatementNoShortIf" =>
            ForStatement(
              None,
              Some(getExpressionAst(tree.children(3))),
              Some(getStatementAst(tree.children(5)).head.asInstanceOf[ExpressionStatement]),
              getStatementAst(tree.children(7)).head
            )
          case "ForStatement FOR LEFT_PAREN SEMI Expression SEMI RIGHT_PAREN Statement"
               | "ForStatementNoShortIf FOR LEFT_PAREN SEMI Expression SEMI RIGHT_PAREN StatementNoShortIf" =>
            ForStatement(
              None,
              Some(getExpressionAst(tree.children(3))),
              None,
              getStatementAst(tree.children(6)).head
            )
          case "ForStatement FOR LEFT_PAREN SEMI SEMI ForUpdate RIGHT_PAREN Statement"
               | "ForStatementNoShortIf FOR LEFT_PAREN SEMI SEMI ForUpdate RIGHT_PAREN StatementNoShortIf" =>
            ForStatement(
              None,
              None,
              Some(getStatementAst(tree.children(4)).head.asInstanceOf[ExpressionStatement]),
              getStatementAst(tree.children(6)).head
            )
          case "ForStatement FOR LEFT_PAREN SEMI SEMI RIGHT_PAREN Statement"
               | "ForStatementNoShortIf FOR LEFT_PAREN SEMI SEMI RIGHT_PAREN StatementNoShortIf" =>
            ForStatement(
              None,
              None,
              None,
              getStatementAst(tree.children(5)).head
            )
        })
      case "StatementExpression" =>
        Seq(ExpressionStatement(getExpressionAst(tree.children.head)))
      case "ReturnStatement" if tree.size == 3 =>
        Seq(ReturnStatement(Some(getExpressionAst(tree.children(1)))))
      case "ReturnStatement" =>
        Seq(ReturnStatement(None))
      case "EmptyStatement" =>
        Seq(EmptyStatement())
      case _ => defaultTreeToStmt(tree)
    }
  }
  def defaultTreeToStmt(tree: Tree): Seq[Statement] = tree.children.flatMap(getStatementAst)
  // There are some inconsistency for MethodBody ConstructorBody


  /***** Statement *****/


  def treeToAst(tree: Tree): Seq[Ast] = {

    tree.lhs.kind match {
      case "CompilationUnit" =>
        val compilationUnit = CompilationUnit()
        compilationUnit.children = defaultTreeToAst(tree)
        Seq(compilationUnit)
      case "PackageDeclaration" =>
        val nameTree: Tree = tree.children(1)
        val nameAst: Name = getNameAst(nameTree)
        Seq(PackageDecl(nameAst))
      case "SingleTypeImportDeclaration" =>
        val nameTree: Tree = tree.children(1)4
        val nameAst: Name = getNameAst(nameTree)
        Seq(ImportDecl(onDemand = false, nameAst))
      case "TypeImportOnDemandDeclaration" =>
        val nameTree: Tree = tree.children(1)
        val nameAst: Name = getNameAst(nameTree)
        Seq(ImportDecl(onDemand = true, nameAst))
      case "ClassDeclaration" =>
        val modifiers: Seq[Modifier.Value] = {
          // collect every modifier tree node
          val modifierTree = collect(tree.children.head, "Modifier")
          // get name of modifier
          val modifierName = modifierTree.map(_.children.head.lhs.kind)
          // map modifier name to enum
          modifierName.map(x => Modifier.withName(x))
        }
        val className: SimpleName = SimpleName(tree.children(2).lhs.lexeme)
        // collect parent name
        val superTrees: Seq[Tree] = collect(tree, "Super")
        val parent: Name = {
          if (superTrees.isEmpty) QualifiedName(Seq("java", "lang", "Object"))
          else getNameAst(collect(superTrees.head, "Name").head)
        }
        // collect interface names
        val interfacesTrees: Seq[Tree] = collect(tree, "Interfaces")
        val interfaces: Seq[Name] = {
          if (interfacesTrees.isEmpty) Seq.empty
          else collect(interfacesTrees.head, "Name").map(getNameAst)
        }

        val classDecl = ClassDecl(
          modifiers = modifiers,
          className = className,
          parent = parent,
          interfaces = interfaces,
        )
        classDecl.children = treeToAst(tree.children.last)
        Seq(classDecl)
      case "InterfaceDeclaration" =>
        val modifiers: Seq[Modifier.Value] = {
          collect(tree.children.head, "Modifier")
            .map(_.children.head.lhs.kind)
            .map(x => Modifier.withName(x))
        }
        val interfaceName: String = tree.children(2).lhs.lexeme
        val extendInterfaceTree = tree.children(3)
        val extendInterfaces: Seq[Name]= if(extendInterfaceTree.lhs.kind == "ExtendsInterfaces")
          collect(extendInterfaceTree, "Name").map(getNameAst)
        else Seq()
        val interfaceDecl = InterfaceDecl(
          name = SimpleName(interfaceName),
          modifiers = modifiers,
          interfaces = extendInterfaces,
        )
        interfaceDecl.children = treeToAst(tree.children.last)
        Seq(interfaceDecl)

      case "AbstractMethodDeclaration" =>
        val modifiers: Seq[Modifier.Value] = {
          collect(tree.children.head, "Modifier")
            .map(_.children.head.lhs.kind)
            .map(x => Modifier.withName(x))
        }
        val methodTypeTree: Tree = tree.children.head.children(1)
        val methodType = if(methodTypeTree.lhs.kind == "VOID") VoidType() else getType(methodTypeTree)
        val methodName = collect(tree, "MethodDeclarator").head.children.head.lhs.lexeme
        val parameterTree : Seq[Tree] = collect(tree.children.head, "FormalParameter")
        val parameterLists : Seq[Parameter] = parameterTree.map(x => {
          val parameterName = collect(x.children.last, "IDENTIFIER")
          Parameter(getType(x.children.head), SimpleName(parameterName.head.lhs.lexeme))
        })
        Seq(InterfaceMethodDecl(
          name = SimpleName(methodName),
          modifiers = modifiers,
          mType = methodType,
          parameters = parameterLists,
        ))

      case "ConstructorDeclaration" =>
        val modifiers: Seq[Modifier.Value] = {
          val modifierTree = collect(tree.children.head, "Modifier")
          val modifierName = modifierTree.map(_.children.head.lhs.kind)
          modifierName.map(x => Modifier.withName(x))
        }
        val constructorName: SimpleName = SimpleName(collect(tree.children(1).children.head, "IDENTIFIER").head.lhs.lexeme)
        // Collect parameters
        val parameterTree : Seq[Tree] = collect(tree.children(1), "FormalParameter")
        val parameterLists : Seq[Parameter] = parameterTree.map(x => {
            val parameterName = collect(x.children.last, "IDENTIFIER")
            Parameter(getType(x.children.head), SimpleName(parameterName.head.lhs.lexeme))
          }
        )

        val constructorDecl = ConstructorDecl(
          modifiers = modifiers,
          constructorName = constructorName,
          parameterLists = parameterLists,
        )
        constructorDecl.children = treeToAst(tree.children.last)
        Seq(constructorDecl)

      case "FieldDeclaration" =>
        val modifiers: Seq[Modifier.Value] = {
          collect(tree.children.head, "Modifier")
            .map(_.children.head.lhs.kind)
            .map(x => Modifier.withName(x))
        }
        val fType: Type = getType(tree.children(1))
        val variableDecl = tree.children(2)
        var initializer: Option[Expression] = None
        val name = SimpleName(variableDecl.children.head.children.head.lhs.lexeme)
        if (variableDecl.children.size == 3) {
          /** this declaration has initializer, then if we transform the
            * `VariableInitializer` node to ast, the result must be an `Expression`
            */
          initializer = Some(treeToAst(variableDecl.children.last).head.asInstanceOf[Expression])
        }

        Seq(FieldDecl(
          modifiers,
          fType,
          name,
          initializer,
        ))

      case "MethodDeclaration" =>
        val modifiers: Seq[Modifier.Value] = {
            collect(tree.children.head, "Modifier")
            .map(_.children.head.lhs.kind)
            .map(x => Modifier.withName(x))
        }
        val methodTypeTree: Tree = tree.children.head.children(1)
        val methodType = if(methodTypeTree.lhs.kind == "VOID") VoidType() else getType(methodTypeTree)
        val methodName = collect(tree, "MethodDeclarator").head.children.head.lhs.lexeme
        val parameterTree : Seq[Tree] = collect(tree.children.head, "FormalParameter")
        val parameterLists : Seq[Parameter] = parameterTree.map(x => {
          val parameterName = collect(x.children.last, "IDENTIFIER")
          Parameter(getType(x.children.head), SimpleName(parameterName.head.lhs.lexeme))
        })
        val methodDecl = MethodDecl(
          name = SimpleName(methodName),
          modifiers = modifiers,
          mType = methodType,
          parameters = parameterLists,
        )
        methodDecl.children = treeToAst(tree.children.last)
        Seq(methodDecl)

      case "Expression" => Seq(getExpressionAst(tree))

      case "Block" => getStatementAst(tree)


      /** !!!!! to reduce the number of cases we need to handle
        * if a non terminal doesn't match any Ast, don't match it,
        * let the default case handles it
        */
      case _ => defaultTreeToAst(tree)
    }
  }

  def defaultTreeToAst(tree: Tree): Seq[Ast] = tree.children.flatMap(treeToAst)

}
