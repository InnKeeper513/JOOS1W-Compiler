import Asts.VariableLocation
import Asts._
import Environments._

object Disambiguation {


  case class DisambiguateException(message: String = "") extends Throwable {
    override def getMessage: String = message
  }

  case class TypeCheckingException(message: String = "") extends Throwable {
    override def getMessage: String = message
  }

  /**
    * first stage of assignment 3, disambituation and linking of names
    * @param cpUnit
    */
  def disambiguate(cpUnit: Seq[CompilationUnit]): Unit = {

    def recur(ast: Ast) : Unit = {

      val environment = ast.environment.get

      ast match {
        case cpUnit: CompilationUnit =>
          // reset the importedTypes, prepare for import declarations
          cpUnit.children.foreach(recur)
          // after processing every children of cpUnit, leave the package
          environment.asInstanceOf[RootEnvironment].leavePackage()

        // set current package in the root environment
        case packageDecl: PackageDecl =>
          val packageName = packageDecl.qualifiedName
          environment.asInstanceOf[RootEnvironment].enterPackage(packageName)

        // $1. add imported type information in the root environment, validate each type imported
        case importDecl: ImportDecl =>
          if (importDecl.onDemand) {
            environment.asInstanceOf[RootEnvironment].onDemandImport(importDecl.name)
          } else {
            environment.asInstanceOf[RootEnvironment].singleImport(importDecl.name)
          }

        case classDecl: ClassDecl =>
          classDecl.children.foreach(recur)

        case interfaceDecl: InterfaceDecl =>
          interfaceDecl.children.foreach(recur)

        // $5.3
        case constructorDecl: ConstructorDecl =>
          constructorDecl.children.foreach(recur)

        // $5.1
        case fieldDecl: FieldDecl =>
          if (fieldDecl.initializer.nonEmpty) recur(fieldDecl.initializer.get)

        case methodDecl: MethodDecl =>
          methodDecl.children.foreach(recur)

        case methodDecl: InterfaceMethodDecl => // abstract method declaration
          methodDecl.children.foreach(recur)

        case block: Block =>
          block.children.foreach(recur)

        // $5.5
        case varDeclStmt: LocalVariableDeclarationStatement =>
          if (varDeclStmt.initializer.nonEmpty) recur(varDeclStmt.initializer.get)

        case expressionStatement: ExpressionStatement =>
          recur(expressionStatement.exp)

        case ifStmt: IfElseStatement =>
          recur(ifStmt.condition)
          recur(ifStmt.thenStmt)
          recur(ifStmt.elseStmt)

        case whileStmt: WhileStatement =>
          recur(whileStmt.condition)
          recur(whileStmt.loopStmt)

        case forStmt: ForStatement =>
          if (forStmt.initStmt.nonEmpty) recur(forStmt.initStmt.get)
          if (forStmt.condition.nonEmpty) recur(forStmt.condition.get)
          if (forStmt.updateStmt.nonEmpty) recur(forStmt.updateStmt.get)
          recur(forStmt.loopStmt)

        case returnStatement: ReturnStatement =>
          if (returnStatement.returnExp.nonEmpty) recur(returnStatement.returnExp.get)

        case _: EmptyStatement =>

        case assignment: Assignment =>
          recur(assignment.lhs)
          recur(assignment.rhs)

        case binaryExpression: BinaryExpression =>
          recur(binaryExpression.exp1)
          recur(binaryExpression.exp2)

        case minusExpression: MinusExpression =>
          recur(minusExpression.exp)

        case notExpression: NotExpression =>
          recur(notExpression.exp)

        case castExpression: CastExpression =>
          recur(castExpression.exp)

        case variableExpression: VariableExpression =>
          // process variableExpression.name
          // a.b.c.d.e
          // a.b.c
          // 1. if a is a local variable
          // 2. if a is a field or parent's field
          // 3. if a is a canonical name for a class
          // 4. if a.b is a canonical name for a class
          //    1. if everything else can

          environment match {
            case statementScope: StatementScope =>
              val identifiers = variableExpression.name.getQualifiedName.names

              val localVariableType: Option[Type] = statementScope.resolveVariable(SimpleName(identifiers.head))
              val fieldType: Option[Type] = statementScope.resolveFieldName(SimpleName(identifiers.head))
              if (localVariableType.nonEmpty) {
                variableExpression.baseType = localVariableType.get // set base type to None indicating the variable is a localvariable
                variableExpression.accessName = QualifiedName(identifiers.drop(1))
                variableExpression.variableLocation = VariableLocation.LocalVariable
              } else if (fieldType.nonEmpty) {
                variableExpression.baseType = fieldType.get // set base type to the actual type of the field
                variableExpression.accessName = QualifiedName(identifiers.drop(1))
                variableExpression.modifiers = statementScope.gatherFields(SimpleName(identifiers.head))._2
                variableExpression.variableLocation = VariableLocation.LocalField
              } else {
                var success: Boolean = false

                def f(i: Int) : Boolean = {
                  var cName: QualifiedName = QualifiedName(Seq.empty)
                  try {
                    cName= statementScope.resolveTypeName(QualifiedName(identifiers.take(i)))
                  } catch {
                    case _: TypeLinkingException => return false
                  }
                  val typeScope = statementScope.getScopeForType(cName)
                  if (typeScope.isClass) {
                    val t = ClassType(cName)
                    t.isStatic = true
                    variableExpression.baseType = t
                    variableExpression.accessName = QualifiedName(identifiers.drop(i))
//                    variableExpression.modifiers = typeScope.gatherFields(SimpleName(identifiers.head))._2
//                    if (!variableExpression.modifiers.contains(Modifier.STATIC)) throw TypeCheckingException("???")
                    return true
                  }
                  false
                }

                for (i <- 1 to identifiers.length) {
                  success = success || f(i)
                }
                variableExpression.variableLocation = VariableLocation.ExternalField
                if (!success) throw DisambiguateException("name " + variableExpression.name.toString + " cannot be linked to any entity")
              }
            case typeScope: TypeScope =>
              val identifiers = variableExpression.name.getQualifiedName.names

              val fieldType: Option[Type] = typeScope.resolveFieldName(SimpleName(identifiers.head))
              if (fieldType.nonEmpty) {
                variableExpression.baseType = fieldType.get // set base type to the actual type of the field
                variableExpression.accessName = QualifiedName(identifiers.drop(1))
                variableExpression.variableLocation = VariableLocation.LocalField
              } else {
                var success: Boolean = false
                for (i <- 1 to identifiers.length) {
                  try {
                    val cName = typeScope.resolveTypeName(QualifiedName(identifiers.take(i)))
                    val nextScope = typeScope.getScopeForType(cName)
                    if (nextScope.isClass) {
                      val t = ClassType(cName)
                      t.isStatic = true
                      variableExpression.baseType = t
                      variableExpression.accessName = QualifiedName(identifiers.drop(i))
                      success = true
                    }
                  } catch {
                    case _ : Throwable =>
                  }
                }
                variableExpression.variableLocation = VariableLocation.ExternalField
                if (!success) throw DisambiguateException("name " + variableExpression.name.toString + " cannot be linked to any entity")
              }
          }

        case arrayCreationExpression: ArrayCreationExpression =>
          recur(arrayCreationExpression.sizeExp)

        case _: LiteralExpression =>

        case _: ThisExpression =>

        case classInstanceCreationExpression: ClassInstanceCreationExpression =>
          classInstanceCreationExpression.args.foreach(recur)

        case fieldAccessExpression: FieldAccessExpression =>
          recur(fieldAccessExpression.exp)

        case methodInvocationExpression: MethodInvocationExpression =>
          recur(methodInvocationExpression.exp1)
          methodInvocationExpression.argumentList.foreach(recur)

        case arrayAccessExpression: ArrayAccessExpression =>
          recur(arrayAccessExpression.exp1)
          recur(arrayAccessExpression.exp3)

        case instanceOfExpression: InstanceOfExpression =>
          recur(instanceOfExpression.exp)

        case parenthesesExpression: ParenthesesExpression =>
          parenthesesExpression.exp match {
            case variableExpression: VariableExpression =>
              try {
                parenthesesExpression.environment.get.resolveTypeName(variableExpression.name.getQualifiedName)
                throw TypeCheckingException("Type in parenthesis")
              } catch {
                case _: TypeLinkingException =>
              }
            case _ =>
          }
          recur(parenthesesExpression.exp)
      }
    }

    cpUnit.foreach(recur)
  }

  def typeChecking(cpUnits: Seq[CompilationUnit]) : Unit = {

    val stringType = ClassType(QualifiedName(Seq("java", "lang", "String")))
    val objectType = ClassType(QualifiedName(Seq("java", "lang", "Object")))
    val cloneableType = InterfaceType(QualifiedName(Seq("java", "lang", "Cloneable")))
    val serializableType = InterfaceType(QualifiedName(Seq("java", "io", "Serializable")))

    var currentReturnType: Type = VoidType()
    var inField: Boolean = false
    var inStaticContext: Boolean = false

    val rootEnvironment = cpUnits.head.environment.get.asInstanceOf[RootEnvironment]

    def assert(exp: Expression, eType: Type): Unit = {
      if (typeOf(exp) != eType) throw TypeCheckingException("aldjsadaslkdasklwlqkelkw")
    }

    def isNumType(exp: Expression, allowInstance: Boolean) : Boolean = {
      typeOf(exp, allowInstance) match {
        case primitiveType: PrimitiveType if Seq(PrimitiveType.BYTE, PrimitiveType.SHORT, PrimitiveType.INT, PrimitiveType.CHAR).contains(primitiveType.pType) => true
        case _ => false
      }
    }

    def checkValidSubType(target: ReferenceType, source: ReferenceType): Boolean = {
      source match {
        case sourceClass: ClassType => {
          // * If S is a class type:
          target match {
            case targetClass: ClassType => {
              //   - If T is a class type, then S must either be the same class as T, or S must be a subclass of T,
              //     or a compile-time error occurs.
              if (targetClass == sourceClass) true
              else rootEnvironment.getScopeForType(sourceClass.name.getQualifiedName).getParents.contains(targetClass.name.getQualifiedName) // S must be a subclass of T
            }
            case targetInterface: InterfaceType => {
              //   - If T is an interface type, then S must implement interface T, or a compile-time error occurs.
              rootEnvironment.getScopeForType(sourceClass.name.getQualifiedName).getParents.contains(targetInterface.name.getQualifiedName)
            }
            case _ => throw TypeCheckingException("[TODO]: checkValidSubType cannot resolve target type to either ClassType or InterfaceType")
          }
        }
        // * If S is an interface type:
        case sourceInterface: InterfaceType => {
          target match {
            case targetClass: ClassType => {
              //   - If T is a class type, then T must be Object, or a compile-time error occurs
              targetClass == objectType
            }
            case targetInterface: InterfaceType => {
              //   - If T is an interface type, then T must be either the same interface as S or a superinterface of S,
              //     or a compile-time error occurs.
              if (targetInterface == sourceInterface) true
              else rootEnvironment.getScopeForType(sourceInterface.name.getQualifiedName).getParents.contains(targetInterface.name.getQualifiedName)
            }
            case _ => throw TypeCheckingException("[TODO]: checkValidSubType cannot resolve target type to either ClassType or InterfaceType")
          }
        }
        case _ => throw TypeCheckingException("[TODO]: checkValidSubType cannot resolve source type to either ClassType or InterfaceType")
      }
    }

    def checkIdentityConversion(to: Type, from: Type): Boolean = to == from

    /*
     * JLS 5.1.2 Widening Primitive Conversion
     */
    def checkWideningPrimitiveConversion(target: Type, source: Type): Boolean = {
      (target, source) match {
        /*
         * byte to short
         * byte to int
         * short to int
         * char to int
         */
        case (PrimitiveType(PrimitiveType.INT), PrimitiveType(PrimitiveType.CHAR)) =>  true
        case (PrimitiveType(PrimitiveType.INT), PrimitiveType(PrimitiveType.SHORT)) => true
        case (PrimitiveType(PrimitiveType.SHORT), PrimitiveType(PrimitiveType.BYTE)) => true
        case (PrimitiveType(PrimitiveType.INT), PrimitiveType(PrimitiveType.BYTE)) => true
        // A value of type boolean can be assigned only to a variable of type boolean.
        case (PrimitiveType(PrimitiveType.BOOLEAN), PrimitiveType(PrimitiveType.BOOLEAN)) => true
        // cannot cast other primitive types
        case _ => false
      }
    }
    /*
     * JLS 5.1.3 Narrowing Primitive Conversions
     */
    def checkNarrowingPrimitiveConversion(target: Type, source: Type): Boolean = {
      (target, source) match {
        case (PrimitiveType(PrimitiveType.CHAR), PrimitiveType(PrimitiveType.BYTE)) =>  true
        case (PrimitiveType(PrimitiveType.CHAR), PrimitiveType(PrimitiveType.SHORT)) => true
        case (PrimitiveType(PrimitiveType.BYTE), PrimitiveType(PrimitiveType.SHORT)) => true
        case (PrimitiveType(PrimitiveType.SHORT), PrimitiveType(PrimitiveType.CHAR)) => true
        case (PrimitiveType(PrimitiveType.BYTE), PrimitiveType(PrimitiveType.CHAR)) => true
        case (PrimitiveType(PrimitiveType.BYTE), PrimitiveType(PrimitiveType.INT)) => true
        case (PrimitiveType(PrimitiveType.CHAR), PrimitiveType(PrimitiveType.INT)) => true
        case (PrimitiveType(PrimitiveType.SHORT), PrimitiveType(PrimitiveType.INT)) => true
        case _ => false
      }
    }
    /*
     * JLS 5.1.4 Widening Reference Conversions
     *
     * returns
     *   `true` if there's a matching rule, and it passed the type check
     *   `false` if there's no matching rule
     *   `TypeCheckingException` if there is a matching rule, but fails the type check
     */
    def checkWideningReferenceConversion(target: Type, source: Type): Boolean = {
      (target, source) match {
        // - From any class type S to any class type T, provided that S is a subclass of T.
        //   (An important special case is that there is a widening conversion to the class
        //   type Object from any other class type.)
        case (targetClass: ClassType, sourceClass: ClassType) => {
          if (targetClass == objectType) return true
          checkValidSubType(targetClass, sourceClass)
        }
        // - From any class type S to any interface type K, provided that S implements K.
        case (targetInterface: InterfaceType, sourceClass: ClassType) => {
          checkValidSubType(targetInterface, sourceClass)
        }
        // - From the null type to any class type, interface type, or array type.
        case (_: InterfaceType, NullType()) => true
        case (_: ClassType, NullType()) => true
        case (_: ArrayType, NullType()) => true
        // - From any interface type J to any interface type K, provided that J is a sub-interface of K.
        case (targetInterface: InterfaceType, sourceInterface: InterfaceType) => {
          checkValidSubType(targetInterface, sourceInterface)
        }
        // - From any interface type to type Object.
        case (targetClass: ClassType, _: InterfaceType) => {
          targetClass == objectType
        }
        // - From any array type to type Object.
        case (targetClass: ClassType, _: ArrayType) => {
          targetClass == objectType
        }
        // - From any array type to type Cloneable.
        // - From any array type to type java.io.Serializable
        case (targetInterface: InterfaceType, _: ArrayType) => {
          Set(cloneableType, serializableType).contains(targetInterface)
        }
        case _ => false
      }
    }
    /*
     * Implements JLS 5.2 Assignment Rules
     */
    def checkTypeAssignmentConvertible(to: Type, from: Type): Boolean = {
      /*
       * JLS 5.2 (p66)
       * Assignment contexts allow the use of an identity conversion (§5.1.1),
       * a widening primitive conversion (§5.1.2), or a widening reference conversion (§5.1.4).
       */
      // JLS 5.1.1 Identity Conversions
      // A conversion from a type to that same type is permitted for any type.
      if (checkIdentityConversion(to, from)) return true
      // JLS 5.1.2 Widening Primitive Conversion
      if (checkWideningPrimitiveConversion(to, from)) return true
      // JLS 5.1.4 Widening Reference Conversions
      if (checkWideningReferenceConversion(to, from)) return true
      (to, from) match {
        // void
        case (VoidType(), _) =>
          false
        case (_, VoidType()) =>
          false
        // JLS 5.2 (p67)
        // A value of primitive type must not be assigned to a variable of reference type; an attempt to do so will result in a compile-time error.
        case (_: ReferenceType, _: PrimitiveType) =>
          false
        // JLS 5.2 (p67)
        // A value of the null type (the null reference is the only such value) may be assigned to any reference type, resulting in a null reference of that type.
        case (_: ReferenceType, NullType()) => true
        case (_: PrimitiveType, _: ArrayType) =>
          false
        case (_: PrimitiveType, NullType()) =>
          false
        case (_: PrimitiveType, _: ReferenceType) =>
          false
        // JLS 5.2 (p68)
        // Assignment of a value of compile-time reference type S (source) to a variable
        // of compile-time reference type T (target) is checked as follows:
        // * If S is a class type:
        //   - If T is a class type, then S must either be the same class as T, or S must be a subclass of T,
        //     or a compile-time error occurs.
        case (target: ClassType, source: ClassType) => checkValidSubType(target, source)
        //   - If T is an interface type, then S must implement interface T, or a compile-time error occurs.
        case (target: InterfaceType, source: ClassType) => checkValidSubType(target, source)
        //   - If T is an array type, then a compile-time error occurs
        case (_: ArrayType, _: ClassType) => false
        // * If S is an interface type:
        //   - If T is a class type, then T must be Object, or a compile-time error occurs
        case (target: ClassType, source: InterfaceType) => checkValidSubType(target, source)
        //   - If T is an interface type, then T must be either the same interface as S or a superinterface of S,
        //     or a compile-time error occurs.
        case (target: InterfaceType, source: InterfaceType) => checkValidSubType(target, source)
        //   - If T is an array type, then a compile-time error occurs.
        case (_: ArrayType, _: InterfaceType) => false
        // * If S is an array type SC[], that is, an array of components of type SC:
        //   - If T is a class type, then T must be Object, or a compile-time error occurs
        case (target: ClassType, _: ArrayType) => target == objectType
        //   - If T is an interface type, then a compile-time error occurs unless
        //     T is the type java.io.Serializable or the type Cloneable, the only
        //     interfaces implemented by arrays.
        case (target: InterfaceType, _: ArrayType) => target == serializableType || target == cloneableType
        //   - If T is an array type TC[], that is, an array of components of type TC,
        //     then a compile-time error occurs unless one of the following is true:
        //     1. TC and SC are the same primitive type.
        //     2. TC and SC are both reference types and type SC is assignable to TC,
        //        as determined by a recursive application of these compile-time rules for assignability.
        case (target: ArrayType, source: ArrayType) => {
          val tc = target.aType
          val sc = source.aType
          if (checkIdentityConversion(tc, sc)) return true
          (tc, sc) match {
            case (rtc: ReferenceType, rsc: ReferenceType) =>
              checkTypeAssignmentConvertible(rtc, rsc)
            case _ => false
          }
        }
        case (_: ClassOrInterfaceType, _) => false
        case (_, _: ClassOrInterfaceType) => false
        case _ => false
      }
    }
    /*
     * JLS 5.5 Casting Conversion
     */
    def checkTypeCastable(target: Type, source: Type): Boolean = {
      /*
       * Casting contexts allow the use of an identity conversion (§5.1.1),
       * a widening primitive conversion (§5.1.2), a widening reference conversion (§5.1.4)
       */
      // JLS 5.1.1 Identity Conversion
      if (checkIdentityConversion(target, source)) return true
      // JLS 5.1.2 Widening Primitive Conversion
      if (checkWideningPrimitiveConversion(target, source)) return true
      // JLS 5.1.3 Narrowing Primitive Conversion
      if (checkNarrowingPrimitiveConversion(target, source)) return true
      // JLS 5.1.4 Widening Reference Conversion
      // TODO: check if we need this case (https://www.student.cs.uwaterloo.ca/~cs444/a3.html)
      //if (checkWideningReferenceConversion(target, source)) true
      // JLS 5.1.5 Narrowing Reference Conversion
      // TODO: check if we need this case (https://www.student.cs.uwaterloo.ca/~cs444/a3.html)
      //if (checkNarrowingReferenceConversion(target, source)) true
      (target, source) match {
        // A value of a primitive type cannot be cast to a reference type by casting conversion,
        case (_: ReferenceType, _: PrimitiveType) =>
          false
        // nor can a value of a reference type be cast to a primitive type.
        case (_: PrimitiveType, _: ReferenceType) =>
          false
        // https://www.student.cs.uwaterloo.ca/~cs444/a3.html
        // In a cast to a reference type T and in an instanceof expression comparing to reference type T,
        // the type of the subexpression S must be such that S is assignable to T or T is assignable to S.
        case (targetType: ReferenceType, sourceType) =>
          checkTypeAssignmentConvertible(targetType, sourceType) || checkTypeAssignmentConvertible(sourceType, targetType)

        case _ => true
      }
    }
    /*
     * JLS 5.3 Method Invocation Conversion
     */
    def checkMethodInvocationConversion(target: Type, source: Type): Boolean = {
      if (checkIdentityConversion(target, source)) return true
      if (target.isInstanceOf[ArrayType] && source.isInstanceOf[ArrayType]) return false
      // JLS 5.1.2 Widening Primitive Conversion
      if (checkWideningPrimitiveConversion(target, source)) return true
      // JLS 5.1.4 Widening Reference Conversions
      if (checkWideningReferenceConversion(target, source)) return true
      false
    }
    def isMethodInvocationConversion(target: Seq[Type], source: Seq[Type]): Boolean = {
      if (target.length != source.length) return false
      for ((targetType, sourceType) <- target zip source) {
        if (!checkMethodInvocationConversion(targetType, sourceType)) {
          return false
        }
      }
      true
    }

    def typeOf(exp: Expression, allowInstance: Boolean = true): Type = {
      if (exp.astType.nonEmpty) {
        return exp.astType.get
      }
      val scope = exp.environment.get.asInstanceOf[StatementScope]
      exp match {
        case literalExpression: LiteralExpression =>
          val result = literalExpression.literal.getType match {
            case LiteralType.INTEGER_LITERAL => PrimitiveType(PrimitiveType.INT)
            case LiteralType.BOOLEAN_LITERAL => PrimitiveType(PrimitiveType.BOOLEAN)
            case LiteralType.STRING_LITERAL => stringType
            case LiteralType.CHAR_LITERAL => PrimitiveType(PrimitiveType.CHAR)
            case LiteralType.NULL_LITERAL => NullType()
          }
          literalExpression.astType = Some(result)
          result
        case notExpression: NotExpression if typeOf(notExpression.exp, allowInstance) == PrimitiveType(PrimitiveType.BOOLEAN) =>
          PrimitiveType(PrimitiveType.BOOLEAN)
        case binaryExpression: BinaryExpression if binaryExpression.op == BinaryOperator.PLUS &&
            typeOf(binaryExpression.exp2, allowInstance) != VoidType() && typeOf(binaryExpression.exp1, allowInstance) == stringType  =>
          binaryExpression.astType = Some(stringType)
          stringType
        case binaryExpression: BinaryExpression if binaryExpression.op == BinaryOperator.PLUS &&
            typeOf(binaryExpression.exp2, allowInstance) == stringType && typeOf(binaryExpression.exp1, allowInstance) != VoidType() =>
          binaryExpression.astType = Some(stringType)
          stringType
        case binaryExpression: BinaryExpression if
            Set(BinaryOperator.DIV, BinaryOperator.MULTI, BinaryOperator.PERC, BinaryOperator.PLUS, BinaryOperator.MINUS).contains(binaryExpression.op) &&
            isNumType(binaryExpression.exp2, allowInstance) && isNumType(binaryExpression.exp1, allowInstance) =>
          binaryExpression.astType = Some(PrimitiveType(PrimitiveType.INT))
          PrimitiveType(PrimitiveType.INT)
        case binaryExpression: BinaryExpression if
            Set(BinaryOperator.LESS, BinaryOperator.GREATER, BinaryOperator.LT, BinaryOperator.GT).contains(binaryExpression.op) &&
            isNumType(binaryExpression.exp2, allowInstance) && isNumType(binaryExpression.exp1, allowInstance) =>
          binaryExpression.astType = Some(PrimitiveType(PrimitiveType.BOOLEAN))
          PrimitiveType(PrimitiveType.BOOLEAN)
        case instanceOfExpression: InstanceOfExpression if (typeOf(instanceOfExpression.exp, allowInstance).isInstanceOf[ReferenceType] || typeOf(instanceOfExpression.exp, allowInstance).isInstanceOf[NullType]) &&
            instanceOfExpression.iType.isInstanceOf[ReferenceType] =>
          instanceOfExpression.astType = Some(PrimitiveType(PrimitiveType.BOOLEAN))
          PrimitiveType(PrimitiveType.BOOLEAN)
        case binaryExpression: BinaryExpression if
            Set(BinaryOperator.EQ, BinaryOperator.NEQ).contains(binaryExpression.op) &&
            isNumType(binaryExpression.exp2, allowInstance) && isNumType(binaryExpression.exp1, allowInstance) =>
          binaryExpression.astType = Some(PrimitiveType(PrimitiveType.BOOLEAN))
          PrimitiveType(PrimitiveType.BOOLEAN)
        case binaryExpression: BinaryExpression if
            Set(BinaryOperator.EQ, BinaryOperator.NEQ).contains(binaryExpression.op) &&
            typeOf(binaryExpression.exp2, allowInstance) == PrimitiveType(PrimitiveType.BOOLEAN) && typeOf(binaryExpression.exp1, allowInstance) == PrimitiveType(PrimitiveType.BOOLEAN) =>
          binaryExpression.astType = Some(PrimitiveType(PrimitiveType.BOOLEAN))
          PrimitiveType(PrimitiveType.BOOLEAN)
        case binaryExpression: BinaryExpression if
            Seq(BinaryOperator.EQ, BinaryOperator.NEQ).contains(binaryExpression.op) &&
              (typeOf(binaryExpression.exp2, allowInstance).isInstanceOf[ReferenceType] || typeOf(binaryExpression.exp2, allowInstance).isInstanceOf[NullType]) &&
              (typeOf(binaryExpression.exp1, allowInstance).isInstanceOf[ReferenceType] || typeOf(binaryExpression.exp1, allowInstance).isInstanceOf[NullType]) =>
          (typeOf(binaryExpression.exp2, allowInstance), typeOf(binaryExpression.exp1, allowInstance)) match {
            case (r1: ReferenceType, r2: ReferenceType) => if (r1 != r2 && (r1 != ClassType(objectType.name) && r2 != ClassType(objectType.name))) throw TypeCheckingException("..?>?>?./")
            case _ =>
          }
          binaryExpression.astType = Some(PrimitiveType(PrimitiveType.BOOLEAN))
          PrimitiveType(PrimitiveType.BOOLEAN)
        case binaryExpression: BinaryExpression if
            Seq(BinaryOperator.LAND, BinaryOperator.LOR).contains(binaryExpression.op) &&
            typeOf(binaryExpression.exp2, allowInstance) == PrimitiveType(PrimitiveType.BOOLEAN) && typeOf(binaryExpression.exp1, allowInstance) == PrimitiveType(PrimitiveType.BOOLEAN) =>
          binaryExpression.astType = Some(PrimitiveType(PrimitiveType.BOOLEAN))
          PrimitiveType(PrimitiveType.BOOLEAN)
        case minusExpression: MinusExpression if isNumType(minusExpression.exp, allowInstance) =>
          minusExpression.astType = Some(PrimitiveType(PrimitiveType.INT))
          PrimitiveType(PrimitiveType.INT)
        case arrayCreationExpression: ArrayCreationExpression =>
          if (!checkTypeAssignmentConvertible(PrimitiveType(PrimitiveType.INT), typeOf(arrayCreationExpression.sizeExp)))
            throw TypeCheckingException("...")
          arrayCreationExpression.astType = Some(ArrayType(arrayCreationExpression.aType))
          ArrayType(arrayCreationExpression.aType)
        case t: ThisExpression =>
          if (!allowInstance) throw TypeCheckingException("...")
          if (inStaticContext) throw TypeCheckingException("no this in static context")
          // ??? check forward reference
          t.astType = Some(ClassType(scope.currentClassCName))
          ClassType(scope.currentClassCName)
        case fieldAccessExpression: FieldAccessExpression =>
          if (fieldAccessExpression.exp.isInstanceOf[ThisExpression]) {
            val classScope = rootEnvironment.getScopeForType(fieldAccessExpression.environment.get.currentClassCName)
            if (classScope.fieldMap.contains(fieldAccessExpression.name)) classScope.definedFields += fieldAccessExpression.name
          }

          val eType = typeOf(fieldAccessExpression.exp, allowInstance)
          if (eType.isInstanceOf[ClassType]) {
            val classScope = rootEnvironment.getScopeForType(eType.asInstanceOf[ClassType].name.getQualifiedName)
            if (inField && classScope.fieldMap.contains(fieldAccessExpression.name) && !classScope.definedFields.contains(fieldAccessExpression.name))
              throw TypeCheckingException("illegal forward reference")
            val fields = classScope.gatherFields
            val optionType = {
              if (fields.contains(fieldAccessExpression.name)) {
                if (!allowInstance && !fields(fieldAccessExpression.name)._2.contains(Modifier.STATIC)) throw TypeCheckingException("...")
                Some(fields(fieldAccessExpression.name)._1)
              } else {
                None
              }
            }
            if (optionType.isEmpty) throw TypeCheckingException("002020")
            fieldAccessExpression.astType = optionType
            optionType.get
          } else if (eType.isInstanceOf[ArrayType]) {
            if (fieldAccessExpression.name.name == "length") {
              val tipe = PrimitiveType(PrimitiveType.INT)
              tipe.assignable = false
              fieldAccessExpression.astType = Some(tipe)
              tipe
            }
            else throw TypeCheckingException("asdasdasd")
          } else {
            throw TypeCheckingException("qwe2031ad")
          }
        case methodInvocationExpression: MethodInvocationExpression =>
          val eType = typeOf(methodInvocationExpression.exp1, allowInstance)
          if (eType.isInstanceOf[ClassType]) {
            val classScope = rootEnvironment.getScopeForType(eType.asInstanceOf[ClassType].name.getQualifiedName)
            val methods = classScope.gatherMethods
            val key = (methodInvocationExpression.name, methodInvocationExpression.argumentList.map(x => typeOf(x, allowInstance)))
            if (!methods.contains(key)) throw TypeCheckingException("...2")
            if (methods(key)._2.contains(Modifier.STATIC)) {
              if (!methodInvocationExpression.exp1.isInstanceOf[VariableExpression])
                throw TypeCheckingException("static")
            }
            if (!eType.isStatic && methods(key)._2.contains(Modifier.STATIC))
              throw TypeCheckingException("static")
            if (eType.isStatic && !methods(key)._2.contains(Modifier.STATIC))
              throw TypeCheckingException("static")

            val currentClassScope = methodInvocationExpression.environment.get.getScopeForType(methodInvocationExpression.environment.get.currentClassCName)

            if (methodInvocationExpression.exp1.isInstanceOf[VariableExpression]) {
              val variableExpression = methodInvocationExpression.exp1.asInstanceOf[VariableExpression]
              val fieldName = SimpleName(variableExpression.name.getQualifiedName.names.head)

              if (classScope.gatherMethods(key)._2.contains(Modifier.PROTECTED))
                if (currentClassScope.packageName != classScope.packageName)
                  if (!currentClassScope.getParents.contains(classScope.cName))
                    if (!classScope.getParents.contains(currentClassScope.cName))
                      throw TypeCheckingException("no")

              if (methodInvocationExpression.environment.get.asInstanceOf[StatementScope].resolveVariable(fieldName).nonEmpty)
                if (classScope.gatherMethods.contains(key))
                  if (classScope.gatherMethods(key)._2.contains(Modifier.PROTECTED))
                    if (!classScope.gatherMethods(key)._2.contains(Modifier.STATIC))
                      if (classScope.getParents.contains(currentClassScope.cName)) {
                        var ptr = classScope
                        var found = false
                        val fname = key

                        while (ptr.cName != currentClassScope.cName && !found) {
                          if (ptr.methodMap.contains(fname)) found = true
                          else ptr = ptr.getScopeForType(ptr.classParent)
                        }

                        if (found)
                          throw TypeCheckingException("ok")
                      }

              if (classScope.gatherMethods.contains(key))
                if (classScope.gatherMethods(key)._2.contains(Modifier.PROTECTED))
                  if (classScope.gatherMethods(key)._2.contains(Modifier.STATIC))
                    if (classScope.getParents.contains(currentClassScope.cName)) {

                      var ptr = classScope
                      var found = false
                      val fname = key

                      while ( ptr.cName != currentClassScope.cName && !found ) {
                        if ( ptr.methodMap.contains(fname)) found = true
                        else ptr = ptr.getScopeForType(ptr.classParent)
                      }

                      if (found)
                        throw TypeCheckingException("ok")
                    }

              if (methodInvocationExpression.environment.get.asInstanceOf[StatementScope].resolveVariable(fieldName).nonEmpty)
                if (currentClassScope.gatherMethods.contains(key))
                  if (currentClassScope.gatherMethods(key)._2.contains(Modifier.PROTECTED))
                    if (!currentClassScope.gatherMethods(key)._2.contains(Modifier.STATIC))
                      if (currentClassScope.getParents.contains(classScope.cName))
                        throw TypeCheckingException("ok")

            }

            methodInvocationExpression.astType = Some(methods(key)._1)
            methods(key)._1
          } else {
            throw TypeCheckingException("qwe2031ad")
          }
        case arrayAccessExpression: ArrayAccessExpression =>
          if (!checkTypeAssignmentConvertible(PrimitiveType(PrimitiveType.INT), typeOf(arrayAccessExpression.exp3, allowInstance)))
            throw TypeCheckingException("...")
          val aType = typeOf(arrayAccessExpression.exp1, allowInstance)
          if (aType.isInstanceOf[ArrayType]) {
            val arrayType = aType.asInstanceOf[ArrayType]
            arrayAccessExpression.astType = Some(arrayType.aType)
            arrayType.aType
          } else {
            throw TypeCheckingException("kkkk")
          }
        case assignment: Assignment =>
          val t2 = typeOf(assignment.rhs)
          if (assignment.lhs.isInstanceOf[VariableExpression]) {
            val variableExpression = assignment.lhs.asInstanceOf[VariableExpression]
            if (variableExpression.name.getQualifiedName.names.length == 1) {
              val fieldName = variableExpression.name.getSimpleName
              val classScope = assignment.environment.get.getScopeForType(assignment.environment.get.currentClassCName)
              if (classScope.fieldMap.contains(fieldName)) classScope.definedFields += fieldName
            }
          }
          val t1 = typeOf(assignment.lhs)
          val result = checkTypeAssignmentConvertible(t1, t2)
          if (!result || !t1.assignable) {
            throw TypeCheckingException("Not assignable: " + assignment)
          }
          assignment.astType = Some(typeOf(assignment.lhs, allowInstance))
          assignment.astType.get
        case castExpression: CastExpression =>
          if (!checkTypeCastable(castExpression.cType, typeOf(castExpression.exp, allowInstance))) {
            throw TypeCheckingException("Not castable: " + castExpression)
          }
          castExpression.astType = Some(castExpression.cType)
          castExpression.cType
        case variableExpression: VariableExpression =>
          val currentClassScope = variableExpression.environment.get.getScopeForType(variableExpression.environment.get.currentClassCName)
          val fieldName = SimpleName(variableExpression.name.getQualifiedName.names.head)
          if (inField && currentClassScope.fieldMap.contains(fieldName) && !currentClassScope.definedFields.contains(fieldName))
            throw TypeCheckingException(fieldName + " not defined ")

          var currentType = variableExpression.baseType
          if (!allowInstance && variableExpression.accessName.names.isEmpty && !variableExpression.modifiers.contains(Modifier.STATIC))
            throw TypeCheckingException("????")
          for ((identifier, i) <- variableExpression.accessName.names.zipWithIndex) {
            currentType match {
              case _: ArrayType if identifier == "length" && i == variableExpression.accessName.names.length - 1 => {
                currentType = PrimitiveType(PrimitiveType.INT)
                currentType.assignable = false
              }
              case classType: ClassType =>
                val classScope = scope.getScopeForType(classType.name.getQualifiedName)
                val fields = classScope.gatherFields ++ currentClassScope.gatherFields
                val optionType = {
                  if (fields.contains(SimpleName(identifier))) {

                    if (fields(SimpleName(identifier))._2.contains(Modifier.PROTECTED))
                      if (currentClassScope.packageName != classScope.packageName)
                        if (!currentClassScope.getParents.contains(classScope.cName))
                          if (!classScope.getParents.contains(currentClassScope.cName))
                            throw TypeCheckingException("no")

                    if (variableExpression.environment.get.asInstanceOf[StatementScope].resolveVariable(fieldName).nonEmpty) {
                      if (currentClassScope.gatherFields.contains(SimpleName(identifier)))
                        if (currentClassScope.gatherFields(SimpleName(identifier))._2.contains(Modifier.PROTECTED))
                          if (currentClassScope.getParents.contains(classScope.cName))
                            throw TypeCheckingException("ok")
                    }

                    if (classScope.gatherFields.contains(SimpleName(identifier)))
                      if (classScope.gatherFields(SimpleName(identifier))._2.contains(Modifier.PROTECTED))
                        if (classScope.gatherFields(SimpleName(identifier))._2.contains(Modifier.STATIC))
                          if (classScope.getParents.contains(currentClassScope.cName)) {

                            var ptr = classScope
                            var found = false
                            val fname = SimpleName(identifier)

                            while ( ptr.cName != currentClassScope.cName && !found ) {
                              if ( ptr.fieldMap.contains(fname)) found = true
                              else ptr = ptr.getScopeForType(ptr.classParent)
                            }

                            if (found)
                              throw TypeCheckingException("ok")
                          }

                    if (variableExpression.environment.get.asInstanceOf[StatementScope].resolveVariable(fieldName).nonEmpty) {
                      if (classScope.gatherFields.contains(SimpleName(identifier)))
                        if (classScope.gatherFields(SimpleName(identifier))._2.contains(Modifier.PROTECTED))
                          if (!classScope.gatherFields(SimpleName(identifier))._2.contains(Modifier.STATIC))
                            if (classScope.getParents.contains(currentClassScope.cName)) {
                              var ptr = classScope
                              var found = false
                              val fname = SimpleName(identifier)

                              while ( ptr.cName != currentClassScope.cName && !found ) {
                                if ( ptr.fieldMap.contains(fname)) found = true
                                else ptr = ptr.getScopeForType(ptr.classParent)
                              }

                              if (found)
                                throw TypeCheckingException("ok")
                            }
                    }

                    if (!allowInstance && !fields(SimpleName(identifier))._2.contains(Modifier.STATIC)) throw TypeCheckingException("...")
                    if ((!currentType.isStatic) && fields(SimpleName(identifier))._2.contains(Modifier.STATIC)) throw TypeCheckingException("...")
                    if (currentType.isStatic && !fields(SimpleName(identifier))._2.contains(Modifier.STATIC)) throw TypeCheckingException("...")
                    Some(fields(SimpleName(identifier))._1)
                  } else {
                    None
                  }
                }
                if (optionType.nonEmpty) {
                  currentType = optionType.get
                } else {
                  throw TypeCheckingException("bad ambiguous name")
                }
//              case interfaceType: InterfaceType =>
              case _ => throw TypeCheckingException("bad ambiguous name")
            }
          }
          if (variableExpression.accessName.names.isEmpty && currentClassScope.fieldMap.contains(fieldName))
            if (!currentType.isStatic && inStaticContext)
              if (variableExpression.environment.get.asInstanceOf[StatementScope].resolveVariable(fieldName).isEmpty)
                throw TypeCheckingException(variableExpression.toString)
          variableExpression.astType = Some(currentType)
          currentType
        case classInstanceCreationExpression: ClassInstanceCreationExpression =>
          val classScope = rootEnvironment.getScopeForType(classInstanceCreationExpression.cType.name.getQualifiedName)
          if (classScope.modifiers.contains(Modifier.ABSTRACT))
            throw TypeCheckingException("cannot create instance of abstract class")
          val myClassScope = classInstanceCreationExpression.environment.get.getScopeForType(classInstanceCreationExpression.environment.get.currentClassCName)

          val types = classInstanceCreationExpression.args.map(x => typeOf(x, allowInstance))
          var found = -1
          var count = 0
          var modifiers: Seq[Modifier.Value] = Seq.empty
          for (signature <- classScope.constructorMap.keySet) {
            if (isMethodInvocationConversion(signature, types)) {
              val local_found = (signature zip types).count(x => x._1 == x._2)
              if (local_found > found) {
                found = local_found
                count = 1
                modifiers = classScope.constructorMap(signature)
              } else if (local_found == found) {
                count += 1
              }
            }
          }
          if (found == -1 || count > 1) {
            throw TypeCheckingException("Cannot find matching constructor for " + classInstanceCreationExpression)
          }

          if (modifiers.contains(Modifier.PROTECTED) && classScope.packageName != myClassScope.packageName)
            throw TypeCheckingException("INvalid access to protected constructor")
          classInstanceCreationExpression.astType = Some(classInstanceCreationExpression.cType)
          classInstanceCreationExpression.cType
        case parenthesesExpression: ParenthesesExpression =>
          val result = typeOf(parenthesesExpression.exp, allowInstance)
//          result.isParenthesizedType = true
          parenthesesExpression.astType = Some(result)
          result

        case _ => throw TypeCheckingException("cannot find type " + exp.toString)
      }
    }


    def typeCheck(ast: Ast): Unit = {

      val environment = ast.environment.get
//      println("checking ast: " + ast)
      ast match {
        case cpUnit: CompilationUnit =>
          // reset the importedTypes, prepare for import declarations
          cpUnit.children.foreach(typeCheck)
          // after processing every children of cpUnit, leave the package
          environment.asInstanceOf[RootEnvironment].leavePackage()

        // set current package in the root environment
        case packageDecl: PackageDecl =>
          val packageName = packageDecl.qualifiedName
          environment.asInstanceOf[RootEnvironment].enterPackage(packageName)

        // $1. add imported type information in the root environment, validate each type imported
        case importDecl: ImportDecl =>
          if (importDecl.onDemand) {
            environment.asInstanceOf[RootEnvironment].onDemandImport(importDecl.name)
          } else {
            environment.asInstanceOf[RootEnvironment].singleImport(importDecl.name)
          }

        case classDecl: ClassDecl =>
          classDecl.children.foreach(typeCheck)

        case interfaceDecl: InterfaceDecl =>
          interfaceDecl.children.foreach(typeCheck)

        // $5.3
        case constructorDecl: ConstructorDecl =>
          currentReturnType = VoidType()
          constructorDecl.children.foreach(typeCheck)

        // $5.1
        case fieldDecl: FieldDecl =>
          inField = true
          if (fieldDecl.initializer.nonEmpty) {
            val isStatic = fieldDecl.environment.get.asInstanceOf[StatementScope].parent.asInstanceOf[TypeScope].fieldMap(fieldDecl.name)._2.contains(Modifier.STATIC)
            if (!checkTypeAssignmentConvertible(fieldDecl.fType, typeOf(fieldDecl.initializer.get, allowInstance = !isStatic)))
              throw TypeCheckingException("")
          }
          inField = false
          fieldDecl.environment.get.asInstanceOf[StatementScope].parent.asInstanceOf[TypeScope].definedFields += fieldDecl.name

        case methodDecl: MethodDecl =>
          if (methodDecl.modifiers.contains(Modifier.STATIC)) inStaticContext = true
          currentReturnType = methodDecl.mType
          methodDecl.children.foreach(typeCheck)
          inStaticContext = false

        case methodDecl: InterfaceMethodDecl => // abstract method declaration
          currentReturnType = methodDecl.mType
          methodDecl.children.foreach(typeCheck)

        case block: Block =>
          block.children.foreach(typeCheck)

        case localVariableDeclarationStatement: LocalVariableDeclarationStatement =>
          if (localVariableDeclarationStatement.initializer.nonEmpty) {
            if (!checkTypeAssignmentConvertible(localVariableDeclarationStatement.vType, typeOf(localVariableDeclarationStatement.initializer.get)))
              throw TypeCheckingException("....")
          }

        case ifElseStatement: IfElseStatement =>
          assert(ifElseStatement.condition, PrimitiveType(PrimitiveType.BOOLEAN))
          typeCheck(ifElseStatement.thenStmt)
          typeCheck(ifElseStatement.elseStmt)
        case expressionStatement: ExpressionStatement =>
          typeOf(expressionStatement.exp)
        case returnStatement: ReturnStatement =>
          if (returnStatement.returnExp.isEmpty && currentReturnType != VoidType()) throw TypeCheckingException("...1")
          if (returnStatement.returnExp.nonEmpty) {
            if (!checkTypeAssignmentConvertible(currentReturnType, typeOf(returnStatement.returnExp.get)))
              throw TypeCheckingException("......")
            if (typeOf(returnStatement.returnExp.get).isInstanceOf[VoidType])
              throw TypeCheckingException("cannot return void")
          }
        case whileStatement: WhileStatement =>
          assert(whileStatement.condition, PrimitiveType(PrimitiveType.BOOLEAN))
          typeCheck(whileStatement.loopStmt)
        case forStatement: ForStatement =>
          if (forStatement.initStmt.nonEmpty) typeCheck(forStatement.initStmt.get)
          if (forStatement.condition.nonEmpty) assert(forStatement.condition.get, PrimitiveType(PrimitiveType.BOOLEAN))
          if (forStatement.updateStmt.nonEmpty) typeCheck(forStatement.updateStmt.get)
          typeCheck(forStatement.loopStmt)
        case x => x.children.foreach(typeCheck)
      }
    }
    cpUnits.foreach(typeCheck)
  }


  def joos1wA3(cpUnit: Seq[CompilationUnit]): Unit = {
    joos1wA2(cpUnit)
    disambiguate(cpUnit)
    typeChecking(cpUnit)
  }

}
