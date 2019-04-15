import Asts.Ast

import Asts._
import Environments._
import Disambiguation._
import scala.collection.mutable

object StaticAnalysis {

  case class ReachabilityException(message: String = "") extends Throwable {
    override def getMessage: String = message
  }

  case class DefiniteAssignmentException(message: String = "") extends Throwable {
    override def getMessage: String = message
  }

  case class ConstantEvaluationException(message: String = "") extends Throwable {
    override def getMessage: String = message
  }

  /**
    * evalConstantExpression implements the constant expression rules in JLS 15.28
    * We evaluate a constant expression during static analysis in the following cases:
    *   - Literals of primitive type and literals of type String
    *   - Casts to primitive types and casts to type String
    *   - The unary operators -, and !
    *   - The multiplicative operators *, /, and %
    *   - The additive operators + and -
    *   - The relational operators <, <=, >, and >=
    *   - The equality operators == and !=
    *   - The conditional-and operator && and the conditional-or operator ||
    *   - Simple names that refer to final variables whose initializers are constant
    *     expressions
    *   - Qualified names of the form TypeName . Identifier that refer to final
    *     variables whose initializers are constant expression
    * This function will store the evaluated constant into the ast node's constEval
    * field, and the follwoing values are possible:
    *   - Some(ConstValue(x)) => the ast node evaluates to a type of ConstValue
    *   - None => the ast node is not a constant expression, so we don't evaluate
    **/
  def evalConstantExpression(ast: Ast): Unit = {
    if (ast.constEval.nonEmpty) return // already evaluated at this node
    val javaLangString = QualifiedName(Seq("java", "lang", "String"))

    def constantTypePromotion(value: ConstValue): ConstValue = {
      value match {
        case ConstChar(x) => ConstInt(x.toInt)
        case ConstShort(x) => ConstInt(x.toInt)
        case ConstByte(x) => ConstInt(x.toInt)
        case x => x // no promotion for ConstInt/ConstBoolean/ConstString
      }
    }
    ast match {
      case b: BinaryExpression =>
        evalConstantExpression(b.exp1)
        evalConstantExpression(b.exp2)

        if (b.exp1.constEval.nonEmpty && b.exp2.constEval.nonEmpty) {
          val exp1ConstVal = b.exp1.constEval.get
          val exp2ConstVal = b.exp2.constEval.get

          b.constEval = (b.op, constantTypePromotion(exp1ConstVal), constantTypePromotion(exp2ConstVal)) match {
            case (BinaryOperator.LOR, ConstBoolean(l), ConstBoolean(r)) => Some(ConstBoolean(l || r))
            case (BinaryOperator.LAND, ConstBoolean(l), ConstBoolean(r)) => Some(ConstBoolean(l && r))
            case (BinaryOperator.EQ, ConstBoolean(l), ConstBoolean(r)) => Some(ConstBoolean(l == r))
            case (BinaryOperator.EQ, ConstInt(l), ConstInt(r)) => Some(ConstBoolean(l == r))
            case (BinaryOperator.EQ, ConstString(l), ConstString(r)) => Some(ConstBoolean(l == r))
            case (BinaryOperator.NEQ, ConstBoolean(l), ConstBoolean(r)) => Some(ConstBoolean(l != r))
            case (BinaryOperator.NEQ, ConstInt(l), ConstInt(r)) => Some(ConstBoolean(l != r))
            case (BinaryOperator.NEQ, ConstString(l), ConstString(r)) => Some(ConstBoolean(l != r))
            case (BinaryOperator.LT, ConstInt(l), ConstInt(r)) => Some(ConstBoolean(l <= r))
            case (BinaryOperator.GT, ConstInt(l), ConstInt(r)) => Some(ConstBoolean(l >= r))
            case (BinaryOperator.LESS, ConstInt(l), ConstInt(r)) => Some(ConstBoolean(l < r))
            case (BinaryOperator.GREATER, ConstInt(l), ConstInt(r)) => Some(ConstBoolean(l > r))
            case (BinaryOperator.PLUS, ConstInt(l), ConstInt(r)) => Some(ConstInt(l + r))
            case (BinaryOperator.PLUS, ConstString(l), ConstString(r)) => Some(ConstString(l + r))
            case (BinaryOperator.MINUS, ConstInt(l), ConstInt(r)) => Some(ConstInt(l - r))
            case (BinaryOperator.MULTI, ConstInt(l), ConstInt(r)) => Some(ConstInt(l * r))
            case (BinaryOperator.DIV, ConstInt(_), ConstInt(0)) => throw ConstantEvaluationException("Division by 0 in expression: " + b)
            case (BinaryOperator.PERC, ConstInt(_), ConstInt(0)) => throw ConstantEvaluationException("Division by 0 in expression: " + b)
            case (BinaryOperator.DIV, ConstInt(l), ConstInt(r)) => Some(ConstInt(l / r))
            case (BinaryOperator.PERC, ConstInt(l), ConstInt(r)) => Some(ConstInt(l % r))
            case _ => None
          }
          // string concat with non-string types doesn't need type promotion (char gets promote to int and everything messed up)
          (b.op, exp1ConstVal, exp2ConstVal) match {
            case (BinaryOperator.PLUS, l, ConstString(r)) => b.constEval = Some(ConstString(String.valueOf(l) + r))
            case (BinaryOperator.PLUS, ConstString(l), r) => b.constEval = Some(ConstString(l + String.valueOf(r)))
            case _ =>
          }

        }

      case m: MinusExpression =>
        evalConstantExpression(m.exp)
        if (m.exp.constEval.nonEmpty) {
          val constExp = m.exp.constEval.get
          m.constEval = constantTypePromotion(constExp) match {
            case ConstInt(x) => Some(ConstInt(-x))
            case _ => None
          }
        }
      case n: NotExpression =>
        evalConstantExpression(n.exp)
        if (n.exp.constEval.nonEmpty) {
          val constExp = n.exp.constEval.get
          n.constEval = constExp match {
            case ConstBoolean(x) => Some(ConstBoolean(!x))
            case _ => None
          }
        }
      case c: CastExpression =>
        evalConstantExpression(c.exp)
        if (c.exp.constEval.nonEmpty) {
          c.constEval = (c.cType, c.exp.constEval.get) match {
            case (PrimitiveType(PrimitiveType.INT), ConstInt(x)) => Some(ConstInt(x))
            case (PrimitiveType(PrimitiveType.INT), ConstShort(x)) => Some(ConstInt(x.toInt))
            case (PrimitiveType(PrimitiveType.INT), ConstChar(x)) => Some(ConstInt(x.toInt))
            case (PrimitiveType(PrimitiveType.INT), ConstByte(x)) => Some(ConstInt(x.toInt))
            case (PrimitiveType(PrimitiveType.INT), ConstBoolean(_)) => None // not allowed
            case (PrimitiveType(PrimitiveType.INT), ConstString(_)) => None // not allowed
            case (PrimitiveType(PrimitiveType.SHORT), ConstInt(x)) => Some(ConstShort(x.toShort)) // narrowing
            case (PrimitiveType(PrimitiveType.SHORT), ConstShort(x)) => Some(ConstShort(x))
            case (PrimitiveType(PrimitiveType.SHORT), ConstChar(x)) => Some(ConstShort(x.toShort)) // narrowing
            case (PrimitiveType(PrimitiveType.SHORT), ConstByte(x)) => Some(ConstShort(x.toShort))
            case (PrimitiveType(PrimitiveType.SHORT), ConstBoolean(_)) => None // not allowed
            case (PrimitiveType(PrimitiveType.SHORT), ConstString(_)) => None // not allowed
            case (PrimitiveType(PrimitiveType.CHAR), ConstInt(x)) => Some(ConstChar(x.toChar)) // narrowing
            case (PrimitiveType(PrimitiveType.CHAR), ConstShort(x)) => Some(ConstChar(x.toChar)) // narrowing
            case (PrimitiveType(PrimitiveType.CHAR), ConstChar(x)) => Some(ConstChar(x))
            case (PrimitiveType(PrimitiveType.CHAR), ConstByte(x)) => Some(ConstChar(x.toChar)) // narrowing
            case (PrimitiveType(PrimitiveType.CHAR), ConstBoolean(_)) => None // not allowed
            case (PrimitiveType(PrimitiveType.CHAR), ConstString(_)) => None // not allowed
            case (PrimitiveType(PrimitiveType.BYTE), ConstInt(x)) => Some(ConstByte(x.toByte)) // narrowing
            case (PrimitiveType(PrimitiveType.BYTE), ConstShort(x)) => Some(ConstByte(x.toByte)) // narrowing
            case (PrimitiveType(PrimitiveType.BYTE), ConstChar(x)) => Some(ConstByte(x.toByte)) // narrowing
            case (PrimitiveType(PrimitiveType.BYTE), ConstByte(x)) => Some(ConstByte(x))
            case (PrimitiveType(PrimitiveType.BYTE), ConstBoolean(_)) => None // not allowed
            case (PrimitiveType(PrimitiveType.BYTE), ConstString(_)) => None // not allowed
            case (PrimitiveType(PrimitiveType.BOOLEAN), ConstInt(_)) =>  None // not allowed
            case (PrimitiveType(PrimitiveType.BOOLEAN), ConstShort(_)) => None // not allowed
            case (PrimitiveType(PrimitiveType.BOOLEAN), ConstChar(_)) => None // not allowed
            case (PrimitiveType(PrimitiveType.BOOLEAN), ConstByte(_)) => None // not allowed
            case (PrimitiveType(PrimitiveType.BOOLEAN), ConstBoolean(x)) => Some(ConstBoolean(x))
            case (PrimitiveType(PrimitiveType.BOOLEAN), ConstString(_)) => None // not allowed
            case (ClassType(s), ConstInt(_)) if s == javaLangString => None // not allowed
            case (ClassType(s), ConstShort(_)) if s == javaLangString => None // not allowed
            case (ClassType(s), ConstChar(_)) if s == javaLangString => None // not allowed
            case (ClassType(s), ConstByte(_)) if s == javaLangString => None // not allowed
            case (ClassType(s), ConstBoolean(_)) if s == javaLangString => None // not allowed
            case (ClassType(s), ConstString(x)) if s == javaLangString => Some(ConstString(x))
            case _ => None
          }
        }
      case l: LiteralExpression =>
        val lexeme: String = l.literal.getLexeme
        l.constEval = l.literal.getType match {
          case LiteralType.INTEGER_LITERAL => Some(ConstInt(lexeme.toInt))
          case LiteralType.CHAR_LITERAL =>
            Some(ConstChar(lexeme.charAt(1))) // assume lexeme of the format 'a'
          case LiteralType.STRING_LITERAL => Some(ConstString(lexeme.slice(1, lexeme.length - 1)))
          case LiteralType.BOOLEAN_LITERAL=> Some(ConstBoolean(lexeme.toBoolean))
          case LiteralType.NULL_LITERAL => None
        }
      case p: ParenthesesExpression =>
        evalConstantExpression(p.exp)
        p.constEval = p.exp.constEval
      case _ =>
    }
  }
  def evalBoolExpression(expression: Expression): Option[Boolean] = {
    evalConstantExpression(expression)
    // println("Evaluate " + expression + " to " + expression.constEval)
    expression.constEval match {
      case Some(ConstBoolean(true)) => Some(true)
      case Some(ConstBoolean(false)) => Some(false)
      case _ => None
    }
  }


  /**
    * definiteAssignmentAnalysis performs definite assignment analysis,
    * and throws error if there is any use before assign
    */
  def definiteAssignmentAnalysis(cpUnits: Seq[CompilationUnit]) : Unit = {
    def recur(ast: Ast, definedNames: mutable.Set[QualifiedName], isAssignment: Boolean): Unit = {
      val environment = ast.environment.get
      //println("recur on ast: " + ast + " with current defined names: " + definedNames)
      ast match {
        case cpUnit: CompilationUnit =>
          // reset the importedTypes, prepare for import declarations
          cpUnit.children.foreach(recur(_, mutable.Set.empty, false))
          // after processing every children of cpUnit, leave the package
          environment.asInstanceOf[RootEnvironment].leavePackage()

        // set current package in the root environment
        case packageDecl: PackageDecl =>
          val packageName = packageDecl.qualifiedName
          environment.asInstanceOf[RootEnvironment].enterPackage(packageName)

        // $1. add imported type information in the root environment, validate each type imported
        case _: ImportDecl =>

        case classDecl: ClassDecl =>
          val blockVariables: mutable.Set[QualifiedName] = mutable.Set.empty
          classDecl.children.foreach(recur(_, blockVariables, false))

        case interfaceDecl: InterfaceDecl =>
          interfaceDecl.children.foreach(recur(_, mutable.Set.empty, false))

        case constructorDecl: ConstructorDecl =>
          val constructorParams: mutable.Set[QualifiedName] = mutable.Set.empty
          constructorDecl.parameterLists.foreach{
            param => constructorParams += param.parameterName.getQualifiedName
          }
          constructorDecl.children.foreach(recur(_, definedNames ++ constructorParams, false))

        case fieldDecl: FieldDecl =>
          if (fieldDecl.initializer.nonEmpty)
            recur(fieldDecl.initializer.get, definedNames, false)
          //println("FieldDecl Adding " + fieldDecl.name.getQualifiedName + " to " + definedNames)
          definedNames += fieldDecl.name.getQualifiedName



        case l: LocalVariableDeclarationStatement =>
          if (l.initializer.nonEmpty) {
            // if we are going to override the outer scope defined variable name
            // remove it from the currently defined variable names
            if (definedNames.contains(l.name.getQualifiedName)) {
              definedNames -= l.name.getQualifiedName
            }
            // recur on rhs first before we make it defined
            recur(l.initializer.get, definedNames, false)
            //println("LocalVariable Adding " + l.name.getQualifiedName + " to " + definedNames)
            definedNames += l.name.getQualifiedName

          }

        case fieldAccess: FieldAccessExpression =>
          // we assume all the field access is valid (invalid cases should be all detected in A3)
          recur(fieldAccess.exp, definedNames, false)
          //if (fieldAccess.exp.isInstanceOf[ThisExpression]) {
            definedNames += fieldAccess.name.getQualifiedName
          //}

          //if (!definedNames.contains(fieldAccess.name.getQualifiedName)) {
            //throw DefiniteAssignmentException("variable " + fieldAccess.name.getQualifiedName + " might not have been initialized")
          //}

        case methodDecl: MethodDecl =>
          val methodParams: mutable.Set[QualifiedName] = mutable.Set.empty
          methodDecl.parameters.foreach{
            param => methodParams += param.parameterName.getQualifiedName
          }
          methodDecl.children.foreach(recur(_, methodParams ++ definedNames, false))

        case interfaceMethodDecl: InterfaceMethodDecl =>
          interfaceMethodDecl.children.foreach(recur(_, mutable.Set.empty, false))

        case block: Block => // TODO
          block.children.foreach(recur(_, definedNames, false))

        case f: ForStatement =>
          if (f.initStmt.nonEmpty) recur(f.initStmt.get, definedNames, false)
          if (f.condition.nonEmpty) recur(f.condition.get, definedNames, false)
          if (f.updateStmt.nonEmpty) recur(f.updateStmt.get, definedNames, false)
          if (f.condition.nonEmpty && evalBoolExpression(f.condition.get).contains(true)) recur(f.loopStmt, definedNames, false)

        case r: ReturnStatement =>
          if (r.returnExp.nonEmpty) recur(r.returnExp.get, definedNames, false)

        case v: VariableExpression =>
          if (isAssignment) { // assignment lhs
            //println("VariableExpression Adding " + v.name.getQualifiedName + " to " + definedNames)
            definedNames += v.name.getQualifiedName
          }
          //println("current defined names: " + definedNames)
          // if a variable is a field access, then we assume it is valid
          if (v.accessName.names.nonEmpty) {
            definedNames += v.name.getQualifiedName
          }
          if (!definedNames.contains(v.name.getQualifiedName)) {
            throw DefiniteAssignmentException("variable " + v.name.getQualifiedName + " might not have been initialized")
          }

        case e: ExpressionStatement =>
          recur(e.exp, definedNames, false)

        case a: Assignment =>
          // Je_8_DefiniteAssignment_UninitializedExpInLvalue
          val assignmentRhsVariables = mutable.Set.empty
          // recur on the rhs first
          recur(a.rhs, definedNames ++ assignmentRhsVariables, false)
          recur(a.lhs, definedNames, true)

        case a: ArrayAccessExpression =>
          recur(a.exp1, definedNames, false)
          recur(a.exp3, definedNames, false)

        case a: ArrayCreationExpression =>
          recur(a.sizeExp, definedNames, false)

        case c: ClassInstanceCreationExpression =>
          c.args.foreach(recur(_, definedNames, false))

        case m: MinusExpression =>
          recur(m.exp, definedNames, false)

        case n: NotExpression =>
          recur(n.exp, definedNames, false)

        case c: CastExpression =>
          recur(c.exp, definedNames, false)

        case i: InstanceOfExpression =>
          recur(i.exp, definedNames, false)

        case b: BinaryExpression =>
          b.op match {
            case BinaryOperator.LAND =>
              recur(b.exp1, definedNames, false)
              if (evalBoolExpression(b.exp1).contains(true)) {
                recur(b.exp2, definedNames, false)
              }
            case BinaryOperator.LOR =>
              recur(b.exp1, definedNames, false)
              if (evalBoolExpression(b.exp1).contains(false)) {
                recur(b.exp2, definedNames, false)
              }
            case _ =>
              recur(b.exp1, definedNames, false)
              recur(b.exp2, definedNames, false)
          }

        case p: ParenthesesExpression =>
          recur(p.exp, definedNames, false)

        case i: IfElseStatement =>
          recur(i.condition, definedNames, false)
          if (evalBoolExpression(i.condition).contains(true)) recur(i.thenStmt, definedNames, false)
          if (evalBoolExpression(i.condition).contains(false)) recur(i.elseStmt, definedNames, false)

        case w: WhileStatement =>
          recur(w.condition, definedNames, false)
          if (evalBoolExpression(w.condition).contains(true)) recur(w.loopStmt, definedNames, false)

        case _ =>
        //case x => println("missing impl for " + x)
      }
    }
    cpUnits.foreach(recur(_, mutable.Set.empty, false))
  }

  /**
   * reachabilityAnalysis performs reachability analysis, and throws error if
   * there is any unreachable code
   */
  def reachabilityAnalysis(cpUnits: Seq[CompilationUnit]) : Unit = {

    /**
     * checkBlock checks if all the statements inside a Block is reachable
     * implemented the following rule:
     *   L: { S1; S2; }, in[S1] = in[L], in[S2] = out[S1], out[L] = out[S2]
     * returns
     *   true if all the statements are reachable
     *   false if only the last return statement makes the block not reachable
     *   throws exception if we have unreachable statements in the block
     */
    def checkBlock(block: Block): Boolean = {
      var outMaybe = true
      block.children.foreach {
        stmt => outMaybe = checkBlockNode(stmt, outMaybe)
      }
      outMaybe
    }
    /*
     * checkLoopStmt checks if a ForStatement or WhileStatement is reachable
     */
    def checkLoopStmt(condition: Expression, loopStmt: Statement, inMaybe: Boolean): Boolean = {
      evalBoolExpression(condition) match {
        case Some(true) =>
          checkBlockNode(loopStmt, inMaybe)
          false
        case Some(false) =>
          checkBlockNode(loopStmt, false)
          inMaybe
        case None =>
          checkBlockNode(loopStmt, inMaybe)
          inMaybe
      }
    }

    def checkBlockNode(node: Ast, inMaybe: Boolean): Boolean = {
      if (!inMaybe) {
        // the current statement is not reachable anymore
        throw ReachabilityException(node + " is not reachable anymore")
      }
      node match {
        case _: LocalVariableDeclarationStatement => true
        case IfElseStatement(_, thenStmt, EmptyStatement()) =>
          checkBlockNode(thenStmt, inMaybe)
          inMaybe
        case IfElseStatement(_, thenStmt, elseStmt) =>
          checkBlockNode(thenStmt, inMaybe) || checkBlockNode(elseStmt, inMaybe)
        case ForStatement(_, None, _, loopStmt) =>
          checkBlockNode(loopStmt, inMaybe)
          false
        case ForStatement(_, Some(condition:Expression), _, loopStmt) =>
          checkLoopStmt(condition, loopStmt, inMaybe)
        case WhileStatement(condition, loopStmt) =>
          checkLoopStmt(condition, loopStmt, inMaybe)
        // any statements after return is not reachable
        case ReturnStatement(_) => false
        case b: Block => checkBlock(b)
        case ExpressionStatement(_) => inMaybe
        case EmptyStatement() => inMaybe
        case x => throw ReachabilityException("Unhandled case for " + x + " Please investigate")
      }
    }

    def recur(ast: Ast, methodDeclaration: Option[MethodDecl], constructorDeclaration: Option[ConstructorDecl]): Unit = {
      val environment = ast.environment.get

      ast match {
        case cpUnit: CompilationUnit =>
          // reset the importedTypes, prepare for import declarations
          cpUnit.children.foreach(recur(_, None, None))
          // after processing every children of cpUnit, leave the package
          environment.asInstanceOf[RootEnvironment].leavePackage()

        // set current package in the root environment
        case packageDecl: PackageDecl =>
          val packageName = packageDecl.qualifiedName
          // No package names or prefixes of package names of declared packages may resolve to types
          environment.asInstanceOf[RootEnvironment].checkPrefixOfOtherType(packageName, strict = false, checkDefaultPackage = false)

          environment.asInstanceOf[RootEnvironment].enterPackage(packageName)

        // $1. add imported type information in the root environment, validate each type imported
        case importDecl: ImportDecl =>

        case classDecl: ClassDecl =>
          classDecl.children.foreach(recur(_, None, None))

        case interfaceDecl: InterfaceDecl =>
          interfaceDecl.children.foreach(recur(_, None, None))

        case constructorDecl: ConstructorDecl =>
          constructorDecl.children.foreach(recur(_, None, Some(constructorDecl)))

        case fieldDecl: FieldDecl =>
          if (fieldDecl.initializer.nonEmpty) recur(fieldDecl.initializer.get, None, None)

        case methodDecl: MethodDecl =>
          methodDecl.children.foreach(recur(_, Some(methodDecl), None))

        case interfaceMethodDecl: InterfaceMethodDecl =>
          interfaceMethodDecl.children.foreach(recur(_, None, None))

        case block: Block =>
          val reachable = checkBlock(block)
          if (methodDeclaration.nonEmpty) { // handling the case for method body
            // in[L] = maybe
            // if method's return != void, out[L] = no
            if (reachable && methodDeclaration.get.mType != VoidType()) {
              throw ReachabilityException(methodDeclaration.get + " missing return statement")
            }
          }
          else if (constructorDeclaration.nonEmpty) {
            // it's okay for constructor to have a return stmt (reachable becomes false)
            // it's also okay for a constructor to not have a return stmt (reachable becomes true)
            // so we don't check the value of reachable here, but only perform a check on the ctor body
          }
          else if (!reachable) throw ReachabilityException(block + " has unreachable statements")

        case assignment: Assignment =>
          recur(assignment.lhs, None, None)
          recur(assignment.rhs, None, None)

        case binaryExpression: BinaryExpression =>
          recur(binaryExpression.exp1, None, None)
          recur(binaryExpression.exp2, None, None)

        case minusExpression: MinusExpression =>
          recur(minusExpression.exp, None, None)

        case notExpression: NotExpression =>
          recur(notExpression.exp, None, None)

        case castExpression: CastExpression =>
          recur(castExpression.exp, None, None)

        case _: VariableExpression =>

        case arrayCreationExpression: ArrayCreationExpression =>
          recur(arrayCreationExpression.sizeExp, None, None)

        case _: LiteralExpression =>

        case _: ThisExpression =>

        // $5.7
        case classInstanceCreationExpression: ClassInstanceCreationExpression =>
          classInstanceCreationExpression.args.foreach(recur(_, None, None))

        case fieldAccessExpression: FieldAccessExpression =>
          recur(fieldAccessExpression.exp, None, None)

        case methodInvocationExpression: MethodInvocationExpression =>
          recur(methodInvocationExpression.exp1, None, None)

        case arrayAccessExpression: ArrayAccessExpression =>
          recur(arrayAccessExpression.exp1, None, None)
          recur(arrayAccessExpression.exp3, None, None)
        // $5.10
        case instanceOfExpression: InstanceOfExpression =>
          recur(instanceOfExpression.exp, None, None)
        case parenthesesExpression: ParenthesesExpression =>
          recur(parenthesesExpression.exp, None, None)
        case _ =>
      }
    }

    cpUnits.foreach(recur(_, None, None))
  }

  def staticAnalysis(cpUnits: Seq[CompilationUnit]): Unit = {
    reachabilityAnalysis(cpUnits)
    definiteAssignmentAnalysis(cpUnits)
  }

  def joos1wA4(cpUnits: Seq[CompilationUnit]): Unit = {
    joos1wA3(cpUnits)
    staticAnalysis(cpUnits)
  }

}
