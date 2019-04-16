package A1

import A1.Grammars.Tree
import A1.Grammars.collect
import A1.ASTBuilding.ASTBuildJoos1w
import A2.Asts.getExpressionAst

object Weeding {

  case class WeederException(message: String = "") extends Throwable {
    override def getMessage: String = message
  }

  sealed abstract class Weeder {
    def weed(tree: Tree): Unit
  }

  object IntegerRangeWeeder extends Weeder {
    val max_int: Long = Int.MaxValue.toLong
    val min_int: Long = Int.MinValue.toLong * -1

    def weed(tree:Tree): Unit = {

      val unaryExpressions = collect(tree, "UnaryExpression")

      unaryExpressions.foreach(e => {
        if (e.production == "UnaryExpression PLUS UnaryExpression") {
          throw WeederException("Production rule UnaryExpression PLUS UnaryExpression not allowed")
        } else if (e.production == "UnaryExpression MINUS UnaryExpression") {
          val ast = ASTBuildJoos1w(e.children.last)
          if (ast.lhs.kind == "INTEGER_LITERAL") {
            if (ast.lhs.lexeme.length > 10 || ast.lhs.lexeme.toLong > min_int ) {
              throw WeederException("Integer -" + ast.lhs.lexeme + " out of range")
            }
          } else {
            weed(e.children.last)
          }
        } else if (e.production == "UnaryExpression UnaryExpressionNotPlusMinus") {
          val ast = ASTBuildJoos1w(e.children.last)
          if (ast.lhs.kind == "INTEGER_LITERAL") {
            if (ast.lhs.lexeme.length > 10 || ast.lhs.lexeme.toLong > max_int ) {
              throw WeederException("Integer " + ast.lhs.lexeme + " out of range")
            }
          } else {
            weed(e.children.last)
          }
        }
      })
    }
  }

  object AbstractFinalClassWeeder extends Weeder {
    // A class cannot be both abstract and final.
    def weed(tree: Tree): Unit = {
      val classes = collect(tree, "ClassDeclaration")

      classes.foreach(klass => {

        // modifiers for this klass will only be at the first children
        val modifiers = collect(klass.children.head, "Modifier")

        var isAbstract = false
        var isFinal = false

        modifiers.foreach(modifier => {
          if (modifier.children.head.lhs.kind == "ABSTRACT") {
            isAbstract = true
          }
          if (modifier.children.head.lhs.kind == "FINAL") {
            isFinal = true
          }
        })

        if (isAbstract && isFinal) {
          throw WeederException("A class cannot be both abstract and final.")
        }

      })
    }
  }

  object AbstractClassBodyWeeder extends Weeder {
    // A method has a body if and only if it is neither abstract nor native.
    def weed(tree: Tree): Unit = {
      val methods = collect(tree, "MethodDeclaration")

      methods.foreach(method => {
        // modifiers for methodheader will only be at the first children
        val modifiers = collect(method.children.head.children.head, "Modifier")

        val methodBody = method.children(1)
        if (methodBody.production == "MethodBody SEMI") {
          // does not have body, must be either abstract or native
          var valid = false
          modifiers.foreach(modifier => {
            if (Set("ABSTRACT", "NATIVE").contains(modifier.children.head.lhs.kind)) {
              valid = true
            }
          })
          if (!valid) {
            throw WeederException("abstract or native method can not have a body")
          }
        } else {
          var valid = true
          modifiers.foreach(modifier => {
            if (Set("ABSTRACT", "NATIVE").contains(modifier.children.head.lhs.kind)) {
              valid = false
            }
          })
          if (!valid) {
            throw WeederException("non-(abstract or native) method must have a body")
          }
        }
      })
    }
  }

  object MethodModifierWeeder extends Weeder {
    def weed(tree: Tree): Unit = {
      val methods = collect(tree, "MethodDeclaration")

      methods.foreach(method => {
        // modifiers for methodheader will only be at the first children
        val modifiers = collect(method.children.head.children.head, "Modifier")

        if (modifiers.isEmpty) {
          throw WeederException("Methods must have a modifier")
        }

        var modifierKinds: Set[String] = Set()

        modifiers.foreach(modifier => {
          modifierKinds = modifierKinds + modifier.children.head.lhs.kind
        })

        // An abstract method cannot be static or final.
        if (modifierKinds.contains("ABSTRACT")) {
          if (modifierKinds.contains("STATIC") || modifierKinds.contains("FINAL")) {
            throw WeederException("An abstract method cannot be static or final.")
          }
        }

        // A static method cannot be final.
        if (modifierKinds.contains("STATIC") && modifierKinds.contains("FINAL")) {
          throw WeederException("A static method cannot be final.")
        }

        // A native method must be static.
        if (modifierKinds.contains("NATIVE") && !modifierKinds.contains("STATIC")) {
          throw WeederException("A native method must be static.")
        }
      })
    }
  }

  object InterfaceNoFieldWeeder extends Weeder {
    def weed(tree: Tree): Unit = {
      val interfaceMembers = collect(tree, "InterfaceMemberDeclaration")

      interfaceMembers.foreach(interfaceMember => {
        if (interfaceMember.production == "InterfaceMemberDeclaration ConstantDeclaration") {
          throw WeederException("An interface cannot contain fields or constructors.")
        }
      })
    }
  }

  object InterfaceMethodModifierWeeder extends Weeder {
    def weed(tree: Tree): Unit = {
      val interfaceMethods = collect(tree, "AbstractMethodDeclaration")
      val modifiers = interfaceMethods.flatMap(i => collect(i, "Modifier"))

      modifiers.foreach(modifier => {
        if (Set("STATIC", "FINAL", "NATIVE").contains(modifier.children.head.lhs.kind)) {
          throw WeederException("An interface method cannot be static, final, or native.")
        }
      })
    }
  }

  object ExplicitConstructorWeeder extends Weeder {
    def weed(tree: Tree): Unit = {
      val classBodies = collect(tree, "ClassBody")

      classBodies.foreach(body => {
        val constructors = collect(body, "ConstructorDeclaration")
        if (constructors.isEmpty) {
          throw WeederException("Every class must contain at least one explicit constructor.")
        }
      })

    }
  }

  object NoFieldFinalWeeder extends Weeder {
    def weed(tree: Tree): Unit = {
      val fieldDecls = collect(tree, "FieldDeclaration")
      val modifiers = fieldDecls.flatMap(i => collect(i, "Modifier"))

      modifiers.foreach(modifier => {
        if (modifier.children.head.lhs.kind == "FINAL") {
          throw WeederException("No field can be final.")
        }
      })
    }
  }

  object NoMultipleDeclarationsInForInit extends Weeder{
    def weed(tree: Tree):Unit = {
      val declsInit = collect(tree, "ForInit")
      val VariableDeclarators = declsInit.flatMap(i => collect(i, "VariableDeclarators"))

      VariableDeclarators.foreach(variable => {
        if(variable.production == "VariableDeclarators VariableDeclarators COMMA VariableDeclarator"){
          throw WeederException("No Multiple Declarations In ForInit")
        }
      })
    }
  }

  object NoMultipleForUpdates extends Weeder{
    def weed(tree: Tree):Unit = {
      val declsInit = collect(tree, "ForUpdate")
      val VariableDeclarators = declsInit.flatMap(i => collect(i, "StatementExpressionList"))

      VariableDeclarators.foreach(variable => {
        if(variable.production == "StatementExpressionList StatementExpressionList COMMA StatementExpression"){
          throw WeederException("No Multiple Updates In ForUpdate")
        }
      })
    }
  }

  object NoMultiDimWeeder extends Weeder {
    def weed(tree: Tree): Unit = {
      // No multidimensional array types or multidimensional array creation expressions are allowed.
      val arrayTypes = collect(tree, "ArrayType")
      arrayTypes.foreach(arrayType => {
        if (arrayType.production == "ArrayType ArrayType LEFT_SQUARE RIGHT_SQUARE") {
          throw WeederException("No multidimensional array type")
        }
      })

      val aces = collect(tree, "ArrayCreationExpression")
      aces.foreach(ace => {
        if (ace.production == "ArrayCreationExpression NEW PrimitiveType DimExprs Dims" ||
            ace.production == "ArrayCreationExpression NEW ClassOrInterfaceType DimExprs Dims") {
          throw WeederException("No multidimensional array creation")
        }
      })
      val des = collect(tree, "DimExprs")
      des.foreach(de => {
        if (de.production == "DimExprs DimExprs DimExpr") {
          throw WeederException("No multidimensional array creation")
        }
      })
      val ds = collect(tree, "Dims")
      ds.foreach(d => {
        if (d.production == "Dims Dims LEFT_SQUARE RIGHT_SQUARE") {
          throw WeederException("No multidimensional array creation")
        }
      })

    }
  }

  object NoExplicitSuperThisWeeder extends Weeder {
    def weed(tree: Tree): Unit = {
      val explicits = collect(tree, "ExplicitConstructorInvocation")
      if (explicits.nonEmpty) {
        throw WeederException("A method or constructor must not contain explicit this() or super() calls.")
      }
    }
  }

  object CastWeeder extends Weeder {
    def weed(tree: Tree): Unit = {
      val casts = collect(tree, "CastExpression")
      casts.foreach(cast => {
        if (cast.production == "CastExpression LEFT_PAREN Expression RIGHT_PAREN UnaryExpressionNotPlusMinus") {
          try {
            getExpressionAst(cast)
          } catch {
            case _: Throwable => throw WeederException("Invalid cast")
          }
        }
      })
    }
  }

  object OneTypePerFileWeeder extends Weeder {
    def weed(tree: Tree): Unit = {
      var types = collect(tree, "TypeDeclaration")
      types = types.filter(t => t.production != "TypeDeclaration SEMI")
      if (types.length > 1) {
        throw WeederException("Multiple types pr. file not allowed.")
      }
    }
  }

  object PackagePrivateWeeder extends Weeder {
    // everything must be public
    def weed(tree: Tree): Unit = {
      val modifierss = collect(tree, "Modifiers")
      modifierss.foreach(modifiers => {
        val publicAndProtected = collect(modifiers, "PUBLIC") ++ collect(modifiers, "PROTECTED")
        if (publicAndProtected.isEmpty) {
          throw WeederException("No package private allowed")
        }
      })
    }
  }

  val joos1wWeeders: List[Weeder] = AbstractClassBodyWeeder :: AbstractFinalClassWeeder :: MethodModifierWeeder ::
      InterfaceNoFieldWeeder :: InterfaceMethodModifierWeeder :: IntegerRangeWeeder :: ExplicitConstructorWeeder :: NoFieldFinalWeeder ::
      NoMultiDimWeeder :: NoMultipleDeclarationsInForInit :: NoMultipleForUpdates :: NoExplicitSuperThisWeeder ::
      CastWeeder :: OneTypePerFileWeeder :: PackagePrivateWeeder :: Nil

  def weedJoos1w(tree: Tree): Unit = {
    joos1wWeeders.foreach(weeder => weeder.weed(tree))
  }

}
