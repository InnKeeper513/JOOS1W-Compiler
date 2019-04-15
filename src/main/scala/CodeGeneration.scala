import java.io.{BufferedWriter, File, FileWriter}
import Asts._
import  Environments._
import StaticAnalysis.joos1wA4
import Label._
import scala.collection.mutable


/**
  * Conventions:
  *   1. Use Pascal ABI
  *   2. All Types uses 4 bytes
  */

object CodeGeneration {

  val arrayVTableOffset: Int = 0
  val arrayTagVTableOffset: Int = 1
  val arrayLengthOffset: Int = 2
  val arrayStartOffset: Int = 3

  case class CodeGenerationException(message: String = "") extends Throwable {
    override def getMessage: String = message
  }

  var bw: BufferedWriter = null
  /**
    * emit emits the x86 instruction with comment
    * @param instruction
    * @param comment
    */
  def emit(instruction: String, comment: String = ""): Unit = {
    val instr = if(comment != "") instruction + "; " + comment + "\n" else instruction + "\n"
    bw.write(instr)
  }

  def emitStart: Unit = {
    val file = new File("output/output.s")
    bw = new BufferedWriter(new FileWriter(file))
  }

  def emitEnds: Unit = {
    bw.close()
    bw = null
  }

  /**
    * emitLabel emits the x86 instruction for a label
    * @param label
    */
  def emitLabel(label: Label): Unit = {
    emit(s"$label:")
  }

  /**
    * emitExpression emits the x86 assembly for an Expression, and stores the result
    * of expression into Reg.eax
    * @param exp
    */
  def emitExpression(exp: Expression): Unit = {
    exp match {
      case a: Assignment => emitAssignment(a)
      case b: BinaryExpression => emitBinaryExpression(b)
      case l: LiteralExpression => emitLiteralExpression(l)
      case m: MinusExpression => emitMinusExpression(m)
      case n: NotExpression => emitNotExpression(n)
      case c: CastExpression => emitCastExpression(c)
      case v: VariableExpression => emitVariableExpression(v)
      case a: ArrayCreationExpression => emitArrayCreationExpression(a)
      case _: ThisExpression => emitThisExpression()
      case c: ClassInstanceCreationExpression => emitClassInstanceCreationExpression(c)
      case f: FieldAccessExpression => emitFieldAccessExpression(f)
      case m: MethodInvocationExpression => emitMethodInvocationExpression(m)
      case a: ArrayAccessExpression => emitArrayAccessExpression(a)
        // TODO there is also an instanceOfExpression inside the binaryExpression
      case i: InstanceOfExpression => emitInstanceOfExpression(i)
      case p: ParenthesesExpression => emitParenthesesExpression(p)
    }
  }

  def emitInstanceOfExpression(expression: Asts.InstanceOfExpression): Unit ={
    emit("", "Begin of InstanceOfExpression")
    // TODO complete instance of expression
    emitExpression(expression.exp)
    emit("", "End of InstanceOfExpression")
  }

  def emitCastExpression(c: CastExpression): Unit = {
    emit("", "Begin of CastExpression")
    emitExpression(c.exp)
    // target, src
    (c.cType, c.exp.astType.get) match {
      case (targetType, sourceType) if targetType == sourceType => emit("", "identity cast")
      case (PrimitiveType(targetType), PrimitiveType(sourceType)) =>
        emit("", "Begin of PrimitiveType Cast")
        (targetType, sourceType) match {
          // we only consider the cases for downcast here, since all primitive types are already 4 bytes

          // primitive widening cases
          case (PrimitiveType.INT, PrimitiveType.CHAR) => emit("", "primitive widening from char to int")
          case (PrimitiveType.INT, PrimitiveType.SHORT) => emit("", "primitive widening from short to int")
          case (PrimitiveType.SHORT, PrimitiveType.BYTE) => emit("", "primitive widening from byte to short")
          case (PrimitiveType.INT, PrimitiveType.BYTE) => emit("", "primitive widening from byte to int")
          case (PrimitiveType.BOOLEAN, PrimitiveType.BOOLEAN) => emit("", "primitive widening from boolean to boolean")

          // primitive narrowing cases
          case (PrimitiveType.BYTE, PrimitiveType.SHORT) => emit(s"movsx ${Reg.eax}, ${Reg.al}", "cast from short to byte")
          case (PrimitiveType.BYTE, PrimitiveType.CHAR) => emit(s"movsx ${Reg.eax}, ${Reg.al}", "cast from char to byte")
          case (PrimitiveType.BYTE, PrimitiveType.INT) => emit(s"movsx ${Reg.eax}, ${Reg.al}", "cast from int to byte")
          case (PrimitiveType.SHORT, PrimitiveType.CHAR) => emit(s"movsx ${Reg.eax}, ${Reg.ax}", "cast from char to short")
          case (PrimitiveType.SHORT, PrimitiveType.INT) => emit(s"movsx ${Reg.eax}, ${Reg.ax}", "cast from int to short")
          case (PrimitiveType.CHAR, PrimitiveType.BYTE) => emit(s"movzx ${Reg.eax}, ${Reg.ax}", "cast from byte to char")
          case (PrimitiveType.CHAR, PrimitiveType.SHORT) => emit(s"movzx ${Reg.eax}, ${Reg.ax}", "cast from short to char")
          case (PrimitiveType.CHAR, PrimitiveType.INT) => emit(s"movzx ${Reg.eax}, ${Reg.ax}", "cast from int to char")
          // all the other cases should throw an exception directly
          case _ => CodeGenerationException("Bad Primitive Cast")
        }
        emit("", "End of PrimitiveType Cast")
        // If an array type is casted to an object type and falls into the following 2 categories,
      case (target: ClassType, _: ClassType) => ReferenceCastReference(target.name.getQualifiedName)
      case (target: InterfaceType, _: ClassType) => ReferenceCastReference(target.name.getQualifiedName)
      case (target: ClassType, _: InterfaceType) => ReferenceCastReference(target.name.getQualifiedName)
      case (target: InterfaceType, _: InterfaceType) => ReferenceCastReference(target.name.getQualifiedName)

      case (target: ClassType, source: ArrayType) =>
        if(target.name.toString != "java.lang.Object") throw CodeGenerationException("Cannot cast array into class type that is not java lang object")
      case (target: InterfaceType, source: ArrayType) => throw CodeGenerationException("Bad Cast")
      case (target: ArrayType, source: InterfaceType) => throw CodeGenerationException("Bad Cast")
        // a class type casted to an array type
      case (target: ArrayType, source: ClassType) => ArrayCastClass(target.aType)
      case (target: ArrayType, source: ArrayType) => ArrayCastArray(target.aType)

      case (_, _) =>
        // assume all the other cases are invalid
        CodeGenerationException("Bad Cast")
    }
    def ArrayCastClass(aType: Type): Unit = {
      emit(s"push ${Reg.eax}")

      // Check if the class is originally an array
      emit(s"mov ${Reg.ebx}, -1")
      emit(s"cmp [${Reg.eax} + 8], ${Reg.ebx}")
      emit(s"je __exception")
      // Denote the class array as array1
      // Denote the cast array type as array2

      // If the array2 is a primitive array
      // First check if array1 is a primitive array, if not exception
      if(aType.isInstanceOf[PrimitiveType]){
        // Primitive Index
        // BOOLEAN: 1
        // BYTE: 2
        // SHORT: 3
        // CHAR: 4
        // INT: 5
        val primitiveIndexVal: Int = aType match{
          case PrimitiveType.INT => 5
          case PrimitiveType.CHAR => 4
          case PrimitiveType.SHORT => 3
          case PrimitiveType.BYTE => 2
          case PrimitiveType.BOOLEAN => 1
        }

        // Check if the primitive type of the array matches with the wanted primitive
        emit(s"mov ${Reg.ebx}, $primitiveIndexVal")
        emit(s"cmp [${Reg.eax} + 4], ${Reg.ebx}")
        emit(s"jne __exception")

      } else {
        checkArraySubType(aType)
      }
      emit(s"pop ${Reg.eax}")
    }
    def checkArraySubType(aType: Type): Unit ={
        // Denote the class array as array1
        // Denote the cast array type as array2
        val aTypeName = aType match{
          case classType: ClassType => classType.name.getQualifiedName
          case interfaceType: InterfaceType => interfaceType.name.getQualifiedName
          case _ => throw CodeGenerationException("Should not be here")
        }
        // If the array2 is a reference type array
        // First check if array1 is a primitive array, if not exception
        emit("","Check if less than or equal to 5")
        emit(s"mov ${Reg.ebx}, 5")
        emit(s"cmp [${Reg.eax} + 4], ${Reg.ebx}")
        emit(s"setle ${Reg.al}")
        emit(s"movzx ${Reg.ebx}, ${Reg.al}")

        // Will get 1 if index1 of array1 is less than 5, then array1 is a primitive array, throw exception
        emit(s"cmp ${Reg.ebx}, 1")
        emit(s"je __exception")

        // Second check if array1's subtype table indicates that it is castable, if no exception
        val allClassAndInterface = c.environment.get.asInstanceOf[StatementScope].getAllClassAndInterface
        val classAndInterfaceNames: Seq[QualifiedName] = allClassAndInterface.values.map(_.cName).toSeq

        // Check the offset of target in the classAndInterfaceName Table
        val subtypeTableIndex = classAndInterfaceNames.indexOf(aTypeName)

        // Check the SIT of the array1
        // eax is the address of the array's source expression
        // Move into vtable
        emit(s"mov ${Reg.eax}, [${Reg.eax} + $arrayTagVTableOffset * 4]", "Move into array's type's vtable")
        // Navigate to subtype table, which is index 1 of the vtable
        emit(s"mov ${Reg.eax}, [${Reg.eax} + 4]", "Move into subtype table")
        // Extract the value of the target from the subtype table
        // Value 1 should be able to cast, Value 0 should not be able to cast
        emit(s"mov ${Reg.eax}, [${Reg.eax} + $subtypeTableIndex * 4]", s"Move extract subtype $aTypeName information")

        emit(s"cmp ${Reg.eax}, 0", "Throw exception if not able to cast")
        emit(s"je __exception")

    }
    def ArrayCastArray(aType: Type): Unit = {
      emit(s"push ${Reg.eax}")

      if(aType.isInstanceOf[PrimitiveType]){
        // Compare if the two arrays are of same primitive
        val primitiveIndexVal: Int = aType match{
          case PrimitiveType.INT => 5
          case PrimitiveType.CHAR => 4
          case PrimitiveType.SHORT => 3
          case PrimitiveType.BYTE => 2
          case PrimitiveType.BOOLEAN => 1
        }
        emit(s"cmp [${Reg.eax} + 4], ${primitiveIndexVal}")
        emit("jne __exception")
      } else {
        // Check if the source array pass the subtype cast
        checkArraySubType(aType)
      }
      emit(s"pop ${Reg.eax}")
    }
    def ReferenceCastReference(targetName: QualifiedName): Unit = {
      if(targetName.toString == "java.lang.Object") return
      emit(s"push ${Reg.eax}")
      // Gather all class and interfaces
      // Then check the offset of the target type in the map
      val allClassAndInterface = c.environment.get.asInstanceOf[StatementScope].getAllClassAndInterface
      val classAndInterfaceNames: Seq[QualifiedName] = allClassAndInterface.values.map(_.cName).toSeq

      // Check the offset of target in the classAndInterfaceName Table
      val subtypeTableIndex = classAndInterfaceNames.indexOf(targetName)

      // Add array check, if at index 2 of the object is not -1, then it means it is an array, array cannot be cast to any other non array object
      // Except java lang Object, but it is verified that the target is not java lang object

      emit(s"mov ${Reg.ebx}, -1")
      emit(s"cmp [${Reg.eax} + 8], ${Reg.ebx}")
      emit(s"jne __exception")

      // eax is the address of the source expression
      // Move into vtable
      emit(s"mov ${Reg.eax}, [${Reg.eax}]", "Move into vtable")
      // Navigate to subtype table, which is index 1 of the vtable
      emit(s"mov ${Reg.eax}, [${Reg.eax} + 4]", "Move into subtype table")
      // Extract the value of the target from the subtype table
      // Value 1 should be able to cast, Value 0 should not be able to cast
      emit(s"mov ${Reg.eax}, [${Reg.eax} + ${subtypeTableIndex}]", "Move extract subtype information")

      emit(s"cmp ${Reg.eax}, 0", "Throw exception if not able to cast")
      emit(s"je __exception")

      emit(s"pop ${Reg.eax}")
    }
    emit("", "End of CastExpression")
  }

  def emitThisExpression(): Unit = {
    emit("", "Begin of ThisExpression")
    emit(s"mov ${Reg.eax}, [${Reg.ebp} - 4]")
    emit("", "End of ThisExpression")
  }

//  def sizeOfObject(expression: Expression): Int = {
//    expression.environment.get match{
//      case typeScope: TypeScope => (typeScope.gatherNonStaticFields.size + 2) * 4
//      case statementScope: StatementScope => (statementScope.gatherNonStaticFields.size + 2) * 4
//      case _ => throw CodeGenerationException("Please investigate")
//    }
//  }

  def sizeOfClassInstance(expression: ClassInstanceCreationExpression): Int = {
    val classScope = expression.environment.get.getScopeForType(expression.astType.get.asInstanceOf[ClassType].name.getQualifiedName)
    (classScope.gatherNonStaticFields.size + 3 ) * 4
  }
  // Object Type Creation Structure
  // index 0: vtable
  // index 1: 0 (for primitive array type, index 1 is 0 as well)
  // index 2: -1 (indicating that this is not an array, for array index 2 is its array length)

  def emitClassInstanceCreationExpression(expression: ClassInstanceCreationExpression): Unit = {
    emit("", "Begin of ClassInstanceCreationExpression")
    // create object
    emit(s"mov ${Reg.eax}, ${sizeOfClassInstance(expression)}", s"size of object of type ${expression.cType} is ${sizeOfClassInstance(expression)}")
    emit("call __malloc")
    emit(s"mov ${Reg.ebx}, ${createVtableLabel(expression.cType.name.toString)}")
    // Index 0: Vtable
    emit(s"mov [${Reg.eax}], ${Reg.ebx}")
    // Index 1: 0
    emit(s"mov ${Reg.ebx}, 0")
    emit(s"mov [${Reg.eax} + 4], ${Reg.ebx}")
    // Index 2: -1
    emit(s"mov ${Reg.ebx}, -1")
    emit(s"mov [${Reg.eax} + 8], ${Reg.ebx}")
    // call constructor of the object
    emit(s"push ${Reg.eax}", "push the address of the allocated class onto stack")
    val arguments: Seq[Type] = expression.args.map(x => if(x.astType.isEmpty) throw CodeGenerationException("Check here typeof bug GG") else x.astType.get)
    expression.args.foreach {
      arg => {
        emitExpression(arg)
        emit(s"push ${Reg.eax}", "push the evaluated argument of ClassInstanceCreationExpression onto stack")
      }
    }
    emit(s"lea ${Reg.esi}, [4 * ${expression.args.size} + esp]", "save parameter ptr")
    emit(s"call ${createConstructorLabel(expression.environment.get.asInstanceOf[StatementScope].getTypeFullName.toString, arguments)}")
    emit(s"add ${Reg.esp}, ${4 * expression.args.size}", "pop all of the arguments of ClassInstanceCreationExpression")
    emit(s"pop ${Reg.eax}", "pop the address of the allocated class off the stack")
    emit("", "End of ClassInstanceCreationExpression")
  }

  def emitParenthesesExpression(expression: ParenthesesExpression): Unit = {
    // this case should be trivial
    emit("", "Begin of ParenthesesExpression")
    emitExpression(expression.exp)
    emit("", "End of ParenthesesExpression")
  }

  def emitMethodInvocationExpression(expression: MethodInvocationExpression): Unit = {
    // Get all non static methods
    val invokeTypeName : QualifiedName = expression.exp1.astType.get match{
      case classType: ClassType => classType.name.getQualifiedName
      case interfaceType: InterfaceType => interfaceType.name.getQualifiedName
      case _ => throw CodeGenerationException("Should not be here")
    }
    val invokeScope = expression.environment.get.asInstanceOf[StatementScope].getScopeForType(invokeTypeName)
    val nonStaticMethods = invokeScope.gatherNonStaticMethods
    val arguments: Seq[Type] = expression.argumentList.map(x => if(x.astType.isEmpty) throw CodeGenerationException("Check here typeof bug GG") else x.astType.get)
    val isStaticMethod = !nonStaticMethods.contains((expression.name, arguments))

    emit("", "Begin of MethodInvocationExpression")
    if (isStaticMethod) {

    } else {
      emitExpression(expression.exp1)
      nullCheck(Reg.eax)
    }
    emit(s"push ${Reg.eax}", "push eax to stack")

    // Pascal Convention
    expression.argumentList.zipWithIndex.foreach {
      arg =>
        emitExpression(arg._1)
        emit(s"push ${Reg.eax}", s"push argument ${arg._2} onto stack")
    }

    // Get the full method name to check if it is a non static method
    val invokeMethodTypeName: QualifiedName =
      if (expression.exp1.isInstanceOf[ThisExpression])
        expression.environment.get.asInstanceOf[StatementScope].getTypeFullName
      else
        expression.exp1.astType.get match {
          case classType: ClassType => classType.name.getQualifiedName
          case interfaceType: InterfaceType => interfaceType.name.getQualifiedName
          case _ => throw CodeGenerationException("Should not be here 1")
        }

    emit(s"lea ${Reg.esi}, [4 * ${expression.argumentList.size} + ${Reg.esp}]", "save parameter ptr")

    // Check if the method is a static method
    if (isStaticMethod) {
      emit("", s"method ${expression.name}(${arguments.mkString(",")}) is a static method")
      emit(s"mov ${Reg.eax}, ${createStaticMethodLabel(invokeMethodTypeName.toString, expression.name.toString, arguments)}")
    } else {
      emitNonStaticMethodInvocationExpression(expression, invokeMethodTypeName)
    }

    // call the method
    emit(s"call ${Reg.eax}")

    emit(s"add ${Reg.esp}, ${4 * expression.argumentList.size + 4}", "pop all of the arguments of ClassInstanceCreationExpression and this")

    // pop the arguments off the stack, do nothing here since it's on the callee side

    emit("", "End of MethodInvocationExpression")

    def emitNonStaticMethodInvocationExpression(expression: MethodInvocationExpression, invokeMethodTypeName: QualifiedName): Unit = {
      emit("", s"Begin of NonStaticMethodInvocationExpression $expression")
      // Get the TypeScope of the invoking type[${Reg.esp} + 4 * ${expression.argumentList.size + 1
      val invokeMethodType: TypeScope = expression.environment.get.asInstanceOf[StatementScope].getScopeForType(invokeMethodTypeName)
      val interfaces = invokeMethodType.parent.asInstanceOf[RootEnvironment].canonicalNameToInterfaceMap

      // need to get the address of object to eax first
      emit(s"mov ${Reg.eax}, [${Reg.esi}]", "restore the address of object to eax") // Now the eax points to the address of the vtable
      // Get the corresponding method implementation
      // If an interface invokes the method, the address of the method implementation will be in the SIT table
      if (invokeMethodType.isInterface) {

        // Compute the offset of the method in the SIT
        var offset: Int = 0
        var isFound: Boolean = false
        for(interface <- interfaces){
          if(interface._1 != invokeMethodType.cName && !isFound){
            offset += interface._2.methodMap.size
          } else {
            for(method <- interface._2.methodMap){
              // Found the method
              if(method._1 == (expression.name, arguments)){
                isFound = true
              } else if(!isFound)
                offset += 1
            }
          }
        }

        // SIT is at the first index of the vtable
        // move into the vtable
        emit(s"mov ${Reg.eax}, [${Reg.eax}]", "address of vtable") // address of vtable
        emit(s"mov ${Reg.eax}, [${Reg.eax}]", "address of SIT") // address of SIT

        // get the implementation inside the SIT
        emit(s"mov ${Reg.eax}, [${Reg.eax} + ${offset * 4}]", "get method address from SIT")
      } else {
        emit(s"mov ${Reg.eax}, [${Reg.eax}]", "address of vtable") // address of vtable
        val invokedMethodOffset: Int = invokeMethodType.gatherNonStaticMethods((expression.name, arguments))._1
        emit(s"mov ${Reg.eax}, [${Reg.eax} + 8 + ${invokedMethodOffset * 4}]", s"get method address from vtable with offset $invokedMethodOffset")
      }
      emit("", "End of NonStaticMethodInvocationExpression")
    }
  }

  def emitVariableExpression(expression: VariableExpression): Unit = {
    emit("", s"Begin of VariableExpression")
    expression.variableLocation match {
      case VariableLocation.LocalVariable =>
        val offset: Int = 1 + expression.environment.get.asInstanceOf[StatementScope].findVariableOffset(expression.name.getFirstSimpleName)
        //val offset = stack.variableIndex(expression.name)
        //    emit(s"mov [${Reg.ebp} + 4 * $offset], ${Reg.eax}", "variable assignment")
        emit(s"mov ${Reg.eax}, [${Reg.ebp} - ${4 * offset} - 4]", s"move local variable ${expression.name} value with offset ${offset} to ${Reg.eax}")

        var currentType = expression.baseType

        for (fieldStr <- expression.accessName.names) {
          currentType match {
            case arrayType: ArrayType =>
              // accessing field of array type
              // this must be the last fieldStr and the field must be length
              // which is checked by disambiguation
              emit(s"mov ${Reg.eax}, [${Reg.eax} + 4]", "access array length")
            case classType: ClassType =>
              val classScope = expression.environment.get.getScopeForType(classType.name.getQualifiedName)
              val fieldInfo = classScope.gatherNonStaticFields(SimpleName(fieldStr))
              val fieldOffset = 3 + fieldInfo._3
              currentType = fieldInfo._1

              emit(s"mov ${Reg.eax}, [${Reg.eax} + ${fieldOffset * 4}]", "access field")
            case _ => throw CodeGenerationException("bad ambiguous name")
          }
        }

      case VariableLocation.LocalField =>
        emitThisExpression()
        val className = expression.environment.get.currentClassCName
        var classScope = expression.environment.get.getScopeForType(className)

        var isStatic = classScope.gatherFields(expression.name.getFirstSimpleName)._2.contains(Modifier.STATIC)
        var fieldInfo : (Type, Seq[Modifier.Value], Int) = null
        var fieldOffset = 0

        if (isStatic) {
          fieldInfo = (classScope.gatherFields(expression.name.getFirstSimpleName)._1, Seq(), 0)
          val staticFieldLabel = createStaticFieldLabel(classScope.cName.toString, expression.name.getFirstSimpleName.toString)
          emit(s"mov ${Reg.eax}, [$staticFieldLabel]", "read static field")
        } else {
          fieldInfo = classScope.gatherNonStaticFields(expression.name.getFirstSimpleName)
          fieldOffset = 3 + fieldInfo._3

          emit(s"mov ${Reg.eax}, [${Reg.eax} + ${fieldOffset * 4}]", "access field")
        }

        for (fieldStr <- expression.accessName.names) {
          fieldInfo._1 match {
            case arrayType: ArrayType =>
              // accessing field of array type
              // this must be the last fieldStr and the field must be length
              // which is checked by disambiguation
              emit(s"mov ${Reg.eax}, [${Reg.eax} + 4]", "access array length")
            case classType: ClassType =>
              classScope = expression.environment.get.getScopeForType(classType.name.getQualifiedName)
              fieldInfo = classScope.gatherNonStaticFields(SimpleName(fieldStr))
              fieldOffset = 3 + fieldInfo._3

              emit(s"mov ${Reg.eax}, [${Reg.eax} + ${fieldOffset * 4}]", "access field")
            case _ => throw CodeGenerationException("bad ambiguous name")
          }
        }
      case VariableLocation.ExternalField =>
        val environmentClassName = expression.environment.get.currentClassCName

        var currentType = expression.baseType

        for ((fieldStr, index) <- expression.accessName.names.zipWithIndex) {
          currentType match {
            case arrayType: ArrayType =>
              emit(s"mov ${Reg.eax}, [${Reg.eax} + 4]", "shouldn't appear")
            case classType: ClassType =>
              val classScope = expression.environment.get.getScopeForType(classType.name.getQualifiedName)
              if (index == 0) {
                val staticFieldLabel = createStaticFieldLabel(classScope.cName.toString, fieldStr)
                emit(s"mov ${Reg.eax}, [$staticFieldLabel]", "read static field")
              } else {
                val fieldInfo = classScope.gatherNonStaticFields(SimpleName(fieldStr))
                currentType = fieldInfo._1
                val fieldOffset = 3 + fieldInfo._3

                emit(s"mov ${Reg.eax}, [${Reg.eax} + ${fieldOffset * 4}]", "access field")
              }
            case _ => throw CodeGenerationException("bad ambiguous name")
          }
        }
    }
    emit("", "End of VariableExpression")
  }

  // TODO need to change
  def emitStringArrayCreationExpression(stringLiteral: String): Unit ={
    emit("", s"Begin of ArrayCreationExpression of string literal $stringLiteral")
    emit(s"mov eax, ${stringLiteral.length()}")
    emit(s"push ${Reg.eax}", "save the array length onto stack")
    // allocate the array, reserve three extra fields for array
    emit(s"lea ${Reg.eax}, [4 * ${Reg.eax} + ${arrayStartOffset * 4}]")
    emit("call __malloc", "malloc for array creation expression")
    // eax now holds the pointer to the allocated memory

    emit(s"mov ${Reg.ebx}, ${createVtableLabel("java.lang.String")}")
    emit(s"mov [${Reg.eax}], ${Reg.ebx}")
    emit(s"mov ${Reg.ebx}, 0")
    emit(s"mov [${Reg.eax} + ${arrayTagVTableOffset * 4}], ${Reg.ebx}")
    emit(s"pop ${Reg.ebx}", "pop the array length to ebx")
    // array's second offset holds the value of array length
    emit(s"mov [${Reg.eax} + ${arrayLengthOffset * 4}], ${Reg.ebx}")

    // make all the allocated values default to zero, please double check
    //val startLabel = createLabel()
    //val endLabel = createLabel()
    emit(s"add ${Reg.eax}, ${arrayStartOffset * 4}", "move eax to the actual start of the array")
    //emitLabel(startLabel)
    //emit(s"cmp ${Reg.ebx}, 0")
    //emit(s"je $endLabel")
    //emit(s"mov [${Reg.eax}], 0")
    //emit(s"add ${Reg.eax}, 4")
    //emit(s"sub ${Reg.ebx}, 1")
    //emit(s"jmp $startLabel")
    //emitLabel(endLabel)
    for(character <- stringLiteral){
      emit(s"mov dword [${Reg.eax}], 0x${character.toInt.toHexString}", s"mov character '$character' into eax")
      emit(s"add ${Reg.eax}, 4")
    }
    emit("", s"End of ArrayCreationExpression of string literal $stringLiteral")
  }

  def emitArrayCreationExpression(expression: ArrayCreationExpression): Unit = {
    emit("", s"Begin of ArrayCreationExpression of type ${expression.aType}")
    emitExpression(expression.sizeExp)
    emit(s"push ${Reg.eax}", "save the array length onto stack")
    // allocate the array, reserve three extra fields for array
    emit(s"lea ${Reg.eax}, [4 * ${Reg.eax} + ${arrayStartOffset * 4}]")
    emit("call __malloc", "malloc for array creation expression")
    // eax now holds the pointer to the allocated memory

    // Index 0: A general Array Vtable
    emit(s"mov ${Reg.ebx}, $createPrimitiveArrayLabel")
    emit(s"mov [${Reg.eax}], ${Reg.ebx}")

    // Index 1: Type Vtable
    if(expression.aType.isInstanceOf[PrimitiveType]){
      // Place a Primitive Index into index 1 if the array is primitive array
      // Primitive Index
      // BOOLEAN: 1
      // BYTE: 2
      // SHORT: 3
      // CHAR: 4
      // INT: 5
      val primitiveIndex = expression.aType.asInstanceOf[PrimitiveType] match{
        case PrimitiveType(PrimitiveType.INT) => 5
        case PrimitiveType(PrimitiveType.CHAR) => 4
        case PrimitiveType(PrimitiveType.SHORT) => 3
        case PrimitiveType(PrimitiveType.BYTE) => 2
        case PrimitiveType(PrimitiveType.BOOLEAN) => 1
      }
      emit(s"mov ${Reg.ebx}, ${primitiveIndex}")
      emit(s"mov [${Reg.eax} + 4], ${Reg.ebx}")
    } else {
      // Place the type vtable into index 1
      val aTypeName = expression.aType match{
        case classType: ClassType => classType.name.getQualifiedName
        case interfaceType: InterfaceType => interfaceType.name.getQualifiedName
        case _ => throw CodeGenerationException("Should not come here")
      }
      emit(s"mov ${Reg.ebx}, ${createVtableLabel(aTypeName.toString)}")
      emit(s"mov [${Reg.eax} + ${4 * arrayTagVTableOffset}], ${Reg.ebx}")
    }

    // Index 2: Array Length
    emit(s"pop ${Reg.ebx}", "pop the array length to ebx")
    // array's second offset holds the value of array length
    emit(s"mov [${Reg.eax} + ${4 * arrayLengthOffset}], ${Reg.ebx}")
    emit("", s"End of ArrayCreationExpression of type ${expression.aType}")
  }

  def emitLiteralExpression(expression: LiteralExpression): Unit = {
    emit("", "Begin of LiteralExpression")
    val lexeme = expression.literal.getLexeme
    expression.literal.getType match {
      case LiteralType.BOOLEAN_LITERAL =>
        if (lexeme.toBoolean) {
          emit(s"mov ${Reg.eax}, 1", "boolean true literal")
        } else {
          emit(s"mov ${Reg.eax}, 0", "boolean false literal")
        }
      case LiteralType.CHAR_LITERAL =>
        val character = lexeme.charAt(1).toInt
        emit(s"mov ${Reg.eax}, $character", s"character literal with value $character")
      case LiteralType.INTEGER_LITERAL =>
        val num = lexeme.toInt
        emit(s"mov ${Reg.eax}, $num", s"integer literal with value $num")
      case LiteralType.NULL_LITERAL => emit(s"mov ${Reg.eax}, 0", "null literal")
      case LiteralType.STRING_LITERAL =>
        emitStringArrayCreationExpression(lexeme.slice(1, lexeme.length - 1))
    }
    emit("", "End of LiteralExpression")
  }

  def emitMinusExpression(expression: MinusExpression): Unit = {
    emit("", "Begin of MinusExpression")
    emitExpression(expression.exp)
    emit(s"neg ${Reg.eax}")
    emit("", "End of MinusExpression")
  }

  def emitNotExpression(expression: NotExpression): Unit = {
    emit("", "Begin of NotExpression")
    emitExpression(expression.exp)
    emit(s"xor ${Reg.eax}, 1")
    emit("", "End of NotExpression")
  }

  def nullCheck(reg: Reg.Value): Unit = {
    emit(s"cmp $reg, 0", s"null check on register $reg")
    emit("je __exception")
  }

  def emitExpressionAddress(exp: Expression): Unit = {
    /**
      * given exp, throw error if expression does not have an address
      * otherwise, emit code to generate address
      */
    exp match {
      case variableExpression: VariableExpression => emitVariableExpressionAddress(variableExpression)
      case fieldAccessExpression: FieldAccessExpression => emitFieldAccessExpressionAddress(fieldAccessExpression)
      case arrayAccessExpression: ArrayAccessExpression => emitArrayAccessExpressionAddress(arrayAccessExpression)
      case parenthesesExpression: ParenthesesExpression => emitExpressionAddress(parenthesesExpression.exp)
      case _ => throw CodeGenerationException("lhs of assignment " + exp + " is not a l-value")
    }
  }

  // the sole purpose of this function is to get the address of the array access expression
  def emitArrayAccessExpressionAddress(a: ArrayAccessExpression): Unit = {
    emit("", "Begin of ArrayAccessExpressionAddress")
    emitExpression(a.exp1)
    emit(s"push ${Reg.eax}", "push the result of ArrayAccessExpression.exp1 onto stack")

    emitExpression(a.exp3)
    emit(s"pop ${Reg.ebx}", s"pop the result of ArrayAccessExpression.exp1 to ${Reg.ebx}")

    // ebx = ArrayAccessExpression.exp1, eax = ArrayAccessExpression.exp3
    // null check on ArrayAccessExpression.exp1
    nullCheck(Reg.ebx)

    // boundary check on ArrayAccessExpression.exp3, array length is stored at the 3rd offset of the array
    emit(s"cmp ${Reg.eax}, [${Reg.ebx} + ${arrayLengthOffset * 4}]", "check if array index is out of bounds")
    emit("jae __exception")
    // check on the lower bound
    emit(s"cmp ${Reg.eax}, 0")
    emit("jl __exception")

    // get the address
    emit(s"lea ${Reg.eax}, [${Reg.ebx} + ${Reg.eax} * 4 + $arrayStartOffset * 4]")
    emit("", "End of ArrayAccessExpressionAddress")
  }

  def emitArrayAccessExpression(a: ArrayAccessExpression): Unit = {
    emit("", "Begin of ArrayAccessExpression")

    emitArrayAccessExpressionAddress(a)

    // now we have the address in eax, access the address element directly
    emit(s"mov eax, [${Reg.eax}]", "access array element")

    emit("", "End of ArrayAccessExpression")
  }

  def emitAssignment(assignment: Assignment): Unit = {
    emit("", s"Begin of Assignment $assignment")
    emitExpressionAddress(assignment.lhs)
    // now Reg.eax holds the address of the lhs destination of the assignment
    emit(s"push ${Reg.eax}", "saves the assignment lhs address onto the stack")
    emitExpression(assignment.rhs)
    emit(s"pop ${Reg.ebx}", "pops the assignment lhs off the stack into ebx")
    // TODO: if LHS is an ArrayAccessExpression, type check
    emit(s"mov [${Reg.ebx}], ${Reg.eax}", "assign the result of rhs onto the address of lhs")
    emit("", "End of Assignment")
  }

  // the sole purpose of emitFieldAccessExpressionAddress is to emit the code for the address of both static/non-static field access expression
  def emitFieldAccessExpressionAddress(f: FieldAccessExpression): Unit = {
    def emitStaticFieldAccessExpressionAddress(f: FieldAccessExpression): Unit = {
      emit("Begin of StaticFieldAccessExpressionAddress")
      // create the label which we're searching for
      val statementScope = f.environment.get.asInstanceOf[StatementScope]
      val typeScope = statementScope.getScopeForType(statementScope.currentClassCName)
      val staticFieldLabel = createStaticFieldLabel(typeScope.cName.toString, f.name.toString)
      // now dereference the the label and save it to eax
      emit(s"mov ${Reg.eax}, [$staticFieldLabel]", "saving the deref result of static field label into eax")
      emit("End of StaticFieldAccessExpressionAddress")
    }
    def emitNonStaticFieldAccessExpressionAddress(f: FieldAccessExpression): Unit = {
      emit("", "Begin of NonStaticFieldAccessExpressionAddress")
      emitExpression(f.exp)
      nullCheck(Reg.eax)
      val statementScope = f.environment.get.asInstanceOf[StatementScope]
      val typeScope = statementScope.getScopeForType(statementScope.currentClassCName)
      val offset: Int = typeScope.gatherNonStaticFields(f.name)._3
      emit(s"add ${Reg.eax}, ${offset * 4}", s"adding the offset $offset to the base address of the class")
      emit("", "End of NonStaticFieldAccessExpressionAddress")
    }
    def emitFieldAccessArrayLengthFieldAddress(f: FieldAccessExpression): Unit = {
      // array length is read-only, and this should already be handled during type checking
      emit("", "Begin of FieldAccessArrayLengthFieldAddress")
      emitExpression(f.exp)
      nullCheck(Reg.eax)
      emit(s"add ${Reg.eax}, ${arrayLengthOffset * 4}", s"adding array length offset $arrayLengthOffset to the base address of the array")
      emit("", "End of FieldAccessArrayLengthFieldAddress")
    }
    val statementScope = f.environment.get.asInstanceOf[StatementScope]
    val typeScope = statementScope.getScopeForType(statementScope.currentClassCName)
    // special case when we are handling array.length
    if (f.exp.astType.get.isInstanceOf[ArrayType] && f.name == SimpleName("length")) {
      emitFieldAccessArrayLengthFieldAddress(f)
    } else if (typeScope.gatherFields(f.name)._2.contains(Modifier.STATIC)) {
      emitStaticFieldAccessExpressionAddress(f)
    } else {
      emitNonStaticFieldAccessExpressionAddress(f)
    }
  }

  def emitVariableExpressionAddress(expression: VariableExpression): Unit = {
    emit("", "Begin of VariableExpressionAddress")
    expression.variableLocation match {
      case VariableLocation.LocalVariable =>
        val offset: Int = 1 + expression.environment.get.asInstanceOf[StatementScope].findVariableOffset(expression.name.getSimpleName)
        //val offset = stack.variableIndex(expression.name)
        //    emit(s"mov [${Reg.ebp} + 4 * $offset], ${Reg.eax}", "variable assignment")

        if (expression.accessName.names.isEmpty) {
          emit(s"lea ${Reg.eax}, [${Reg.ebp} - 4 * $offset - 4]", s"move the variable address to ${Reg.eax}")
        } else {
          emit(s"mov ${Reg.eax}, [${Reg.ebp} - 4 * $offset - 4]", s"move the variable address to ${Reg.eax}")
        }

        var currentType = expression.baseType

        for ((fieldStr, index) <- expression.accessName.names.zipWithIndex) {
          currentType match {
            case arrayType: ArrayType =>
              // accessing field of array type
              // this must be the last fieldStr and the field must be length
              // which is checked by disambiguation
              if (index == expression.accessName.names.size - 1) {
                emit(s"lea ${Reg.eax}, [${Reg.eax} + 4]", "access array length address")
              } else {
                emit(s"mov ${Reg.eax}, [${Reg.eax} + 4]", "access array length")
              }
            case classType: ClassType =>
              val classScope = expression.environment.get.getScopeForType(classType.name.getQualifiedName)
              val fieldInfo = classScope.gatherNonStaticFields(SimpleName(fieldStr))
              val fieldOffset = 3 + fieldInfo._3
              currentType = fieldInfo._1

              if (index == expression.accessName.names.size - 1) {
                emit(s"lea ${Reg.eax}, [${Reg.eax} + ${fieldOffset * 4}]", "access field address")
              } else {
                emit(s"mov ${Reg.eax}, [${Reg.eax} + ${fieldOffset * 4}]", "access field")
              }
            case _ => throw CodeGenerationException("bad ambiguous name")
          }
        }

      case VariableLocation.LocalField =>
        emitThisExpression()
        val className = expression.environment.get.currentClassCName
        var classScope = expression.environment.get.getScopeForType(className)


        var isStatic = classScope.gatherFields(expression.name.getFirstSimpleName)._2.contains(Modifier.STATIC)
        var fieldInfo : (Type, Seq[Modifier.Value], Int) = null
        var fieldOffset = 0

        if (isStatic) {
          fieldInfo = (classScope.gatherFields(expression.name.getFirstSimpleName)._1, Seq(), 0)
          val staticFieldLabel = createStaticFieldLabel(classScope.cName.toString, expression.name.getFirstSimpleName.toString)
          if (expression.accessName.names.isEmpty) {
            emit(s"lea ${Reg.eax}, [$staticFieldLabel]", "read static field address")
          } else {
            emit(s"mov ${Reg.eax}, [$staticFieldLabel]", "read static field")
          }
        } else {
          fieldInfo = classScope.gatherNonStaticFields(expression.name.getFirstSimpleName)
          fieldOffset = 3 + fieldInfo._3

          if (expression.accessName.names.isEmpty) {
            emit(s"lea ${Reg.eax}, [${Reg.eax} + ${fieldOffset * 4}]", "access field address")
          } else {
            emit(s"mov ${Reg.eax}, [${Reg.eax} + ${fieldOffset * 4}]", "access field")
          }
        }

        for ((fieldStr, index) <- expression.accessName.names.zipWithIndex) {
          fieldInfo._1 match {
            case arrayType: ArrayType =>
              // accessing field of array type
              // this must be the last fieldStr and the field must be length
              // which is checked by disambiguation
              if (index == expression.accessName.names.size - 1) {
                emit(s"lea ${Reg.eax}, [${Reg.eax} + 4]", "access array length address")
              } else {
                emit(s"mov ${Reg.eax}, [${Reg.eax} + 4]", "access array length")
              }
            case classType: ClassType =>
              classScope = expression.environment.get.getScopeForType(classType.name.getQualifiedName)
              fieldInfo = classScope.gatherNonStaticFields(SimpleName(fieldStr))
              fieldOffset = 3 + fieldInfo._3

              if (index == expression.accessName.names.size - 1) {
                emit(s"lea ${Reg.eax}, [${Reg.eax} + ${fieldOffset * 4}]", "access field address")
              } else {
                emit(s"mov ${Reg.eax}, [${Reg.eax} + ${fieldOffset * 4}]", "access field")
              }
            case _ => throw CodeGenerationException("bad ambiguous name")
          }
        }
      case VariableLocation.ExternalField =>
        val environmentClassName = expression.environment.get.currentClassCName

        var currentType = expression.baseType

        for ((fieldStr, index) <- expression.accessName.names.zipWithIndex) {
          currentType match {
            case arrayType: ArrayType =>
              emit(s"mov ${Reg.eax}, [${Reg.eax} + 4]", "shouldn't appear")
            case classType: ClassType =>
              val classScope = expression.environment.get.getScopeForType(classType.name.getQualifiedName)
              if (index == 0) {
                val staticFieldLabel = createStaticFieldLabel(classScope.cName.toString, fieldStr)
                if (index == expression.accessName.names.size - 1) {
                  emit(s"lea ${Reg.eax}, [$staticFieldLabel]", "read static field address")
                } else {
                  emit(s"mov ${Reg.eax}, [$staticFieldLabel]", "read static field")
                }
              } else {
                val fieldInfo = classScope.gatherNonStaticFields(SimpleName(fieldStr))
                currentType = fieldInfo._1
                val fieldOffset = 3 + fieldInfo._3

                if (index == expression.accessName.names.size - 1) {
                  emit(s"lea ${Reg.eax}, [${Reg.eax} + ${fieldOffset * 4}]", "access field")
                } else {
                  emit(s"mov ${Reg.eax}, [${Reg.eax} + ${fieldOffset * 4}]", "access field")
                }
              }
            case _ => throw CodeGenerationException("bad ambiguous name")
          }
        }
    }
    emit("", "End of VariableExpressionAddress")
  }

  def emitFieldAccessExpression(f: FieldAccessExpression): Unit = {
    emit("", "Begin of FieldAccessExpression")
    emitFieldAccessExpressionAddress(f)
    emit(s"mov ${Reg.eax}, [${Reg.eax}]", "saving the dereferenced field access content into Reg.eax")
    emit("", "End of FieldAccessExpression")
  }

  val javaLangObject = QualifiedName(Seq("java", "lang", "Object"))
  val javaLangString = QualifiedName(Seq("java", "lang", "String"))
  def emitStringConcatenation(exp1: Expression, exp2: Expression): Unit ={
    // The value of first expression is in eax, the value of second expression is in ebx
    var isNull = false
    // The result of the function call is stored as a string in eax
    def getString(exp: Expression): Unit ={
      emitExpression(exp)
      // This is a this expression, it acts as a placeholder and will not be used in the static method call
      emit(s"push ${Reg.eax}", "(this) address placeholder")
      emit(s"push ${Reg.eax}", "push parameter")
      emit(s"lea ${Reg.esi}, [4 + ${Reg.esp}]", "save parameter ptr")
      exp.astType.get match{
        case PrimitiveType.INT => emit(s"call ${createStaticMethodLabel(javaLangString.toString, "valueOf", Seq(PrimitiveType(PrimitiveType.INT)))}")
        case PrimitiveType.CHAR => emit(s"call ${createStaticMethodLabel(javaLangString.toString, "valueOf", Seq(PrimitiveType(PrimitiveType.CHAR)))}")
        case PrimitiveType.SHORT => emit(s"call ${createStaticMethodLabel(javaLangString.toString, "valueOf", Seq(PrimitiveType(PrimitiveType.SHORT)))}")
        case PrimitiveType.BYTE => emit(s"call ${createStaticMethodLabel(javaLangString.toString, "valueOf", Seq(PrimitiveType(PrimitiveType.BYTE)))}")
        case PrimitiveType.BOOLEAN => emit(s"call ${createStaticMethodLabel(javaLangString.toString, "valueOf", Seq(PrimitiveType(PrimitiveType.BOOLEAN)))}")
        case ClassType(x) if x == javaLangString => // Do nothing if it is a string
        case ClassType(x) if x == javaLangObject => emit(s"call ${createStaticMethodLabel(javaLangString.toString, "valueOf", Seq(ClassType(javaLangObject)))}")
        case NullType() => isNull = true
        case _ =>
      }
      emit(s"add ${Reg.esp}, 8", "pop all of the arguments and this")
    }
    emit(s"push ${Reg.ebx}", "store the second exp value")
    // Resolve the first expression into a string
    getString(exp1)
    if(isNull){
      // Since there must be one java lang string involved in concatenation
      // If the first expression resolved to null, then the second expression must be a string already
      emit(s"pop ${Reg.eax}")
      return
    }
    // Resolve the second expression into a string
    emit(s"mov ${Reg.ebx}, ${Reg.eax}")
    emit(s"pop ${Reg.eax}", "extract the value of the second expression")
    emit(s"push ${Reg.ebx}", "store the first resolved string")
    getString(exp2)
    if(isNull){
      // If the second expression is null, then the first expression is a string
      emit(s"pop ${Reg.eax}")
      return
    }
    // Concatenate both expressions into a new string, the second resolved expression string will be the parameter
    // The first expression string will be used as the calling object
    // The first expression string is already in the stack and will be treated as (THIS)
    emit(s"push ${Reg.eax}", "push arguments onto the stack")
    emit(s"lea ${Reg.esi}, [4 + ${Reg.esp}]", "save parameter ptr")
    emit(s"call ${createClassMethodLabel(javaLangString.toString, "concat", Seq(ClassType(javaLangString)))}")
    emit(s"add ${Reg.esp}, 8", "pop all of the arguments and this")
  }

  def emitBinaryExpression(expression: BinaryExpression): Unit = {
    emit("", "Begin of BinaryExpression")
    expression.op match {
      case BinaryOperator.LAND =>
        emitExpression(expression.exp1)
        val endLabel = createLabel()
        emit(s"cmp ${Reg.eax}, 0")
        emit(s"je $endLabel")
        emitExpression(expression.exp2)
        emitLabel(endLabel)
      case BinaryOperator.LOR =>
        emitExpression(expression.exp1)
        val endLabel = createLabel()
        emit(s"cmp ${Reg.eax}, 0")
        emit(s"jne $endLabel")
        emitExpression(expression.exp2)
        emitLabel(endLabel)
      case _ =>
        emitExpression(expression.exp1)
        emit(s"push ${Reg.eax}", "push the binary exp1 evaluated result onto the stack")
        emitExpression(expression.exp2)
        emit(s"pop ${Reg.ebx}", "pop the binary exp1 evaluated result to ebx")
        // ebx = exp1, eax = exp2
        emit(s"xchg ${Reg.eax}, ${Reg.ebx}", "swap the content of eax and ebx")
        // eax = exp1, ebx = exp2
        expression.op match {
          case BinaryOperator.PLUS =>
            (expression.exp1.astType.get, expression.exp2.astType.get) match {
              case (ClassType(x), _) if x == javaLangString =>
                emitStringConcatenation(expression.exp1, expression.exp2)
                return
              case (_, ClassType(x)) if x == javaLangString =>
                emitStringConcatenation(expression.exp1, expression.exp2)
                return
              case _ =>
            }
            emit(s"add ${Reg.eax}, ${Reg.ebx}")
          case BinaryOperator.MINUS =>
            emit(s"sub ${Reg.eax}, ${Reg.ebx}")
          case BinaryOperator.MULTI =>
            emit(s"imul ${Reg.eax}, ${Reg.ebx}", "signed multiplication")
          case BinaryOperator.DIV =>
            emit(s"cmp ${Reg.ebx}, 0")
            emit("je __exception", "division by zero")
            emit("cdq", "sign extend eax into edx before division / modulo")
            emit(s"idiv ${Reg.ebx}", "signed division")
          case BinaryOperator.PERC =>
            emit(s"cmp ${Reg.ebx}, 0")
            emit("je __exception", "division by zero")
            emit("cdq", "sign extend eax into edx before division / modulo")
            emit(s"idiv ${Reg.ebx}", "signed division")
            emit(s"mov ${Reg.eax}, ${Reg.edx}", s"move the modulo into ${Reg.eax}")
          case BinaryOperator.EQ =>
            emit(s"cmp ${Reg.eax}, ${Reg.ebx}")
            emit(s"sete ${Reg.al}")
            // movzx instruction zero extends into a register of larger size
            emit(s"movzx ${Reg.eax}, ${Reg.al}")
          case BinaryOperator.NEQ =>
            emit(s"cmp ${Reg.eax}, ${Reg.ebx}")
            emit(s"setne ${Reg.al}")
            emit(s"movzx ${Reg.eax}, ${Reg.al}")
          case BinaryOperator.EOR =>
            emit(s"or ${Reg.eax}, ${Reg.ebx}")
          case BinaryOperator.EXOR =>
            emit(s"xor ${Reg.eax}, ${Reg.ebx}")
          case BinaryOperator.EAND =>
            emit(s"and ${Reg.eax}, ${Reg.ebx}")
          case BinaryOperator.INSTANCEOF => ??? // TODO: this is a bit different from the rest of binary expression
          case BinaryOperator.LT =>
            emit(s"cmp ${Reg.eax}, ${Reg.ebx}")
            emit(s"setle ${Reg.al}")
            emit(s"movzx ${Reg.eax}, ${Reg.al}")
          case BinaryOperator.GT =>
            emit(s"cmp ${Reg.eax}, ${Reg.ebx}")
            emit(s"setge ${Reg.al}")
            emit(s"movzx ${Reg.eax}, ${Reg.al}")
          case BinaryOperator.GREATER =>
            emit(s"cmp ${Reg.eax}, ${Reg.ebx}")
            emit(s"setg ${Reg.al}")
            emit(s"movzx ${Reg.eax}, ${Reg.al}")
          case BinaryOperator.LESS =>
            emit(s"cmp ${Reg.ebx}, ${Reg.eax}")
            emit(s"setl ${Reg.al}")
            emit(s"movzx ${Reg.eax}, ${Reg.al}")
          case _ => throw CodeGenerationException("Internal Error: Unhandled case in binary operator")
        }
    }
    emit("", "End Of BinaryExpression")
  }

  /**
    * emitStatement emits the x86 assembly for a Statement, and stores the result
    * into register eax
    * @param statement An ast node of type Statement
    */
  def emitStatement(statement: Statement): Unit = {
    statement match {
      case l: LocalVariableDeclarationStatement =>
        emitLocalVariableDeclarationStatement(l)
      case e: ExpressionStatement =>
        emitExpression(e.exp)
      case i: IfElseStatement =>
        emitIfElseStmt(i)
      case w: WhileStatement =>
        emitWhileStatement(w)
      case f: ForStatement =>
        emitForStatement(f)
      case r: ReturnStatement =>
        emitReturnStatement(r)
      case e: EmptyStatement =>
        emitEmptyStatement(e)
      case b: Block =>
        // all Block's children should be a Statement
        def emitBlockChild(ast: Ast): Unit = {
          ast match {
            case s: Statement => emitStatement(s)
            case _ => throw CodeGenerationException("Block child " + ast + " is not a Statement")
          }
        }
        allocateLocalVariable(b.environment.get.asInstanceOf[StatementScope].variableMap.size)
        b.children.foreach {
          stmt => emitBlockChild(stmt)
        }
        deallocateLocalVariable(b.environment.get.asInstanceOf[StatementScope].variableMap.size)
    }
  }

  /**
    * emitLocalVariableDeclarationStatement emits the x86 assembly for a LocalVariableDeclarationStatement
    *
    * @param localVariableDeclarationStatement An ast node of type localVariableDeclarationStatement
    *
    * @GeneratedCode
    *
    * // initializer is not empty
    * /* generated code for localVariableDeclarationStatement.initializer */
    * push eax
    *
    * OR
    *
    * // initializer is empty
    * mov eax, 0 // TODO: further check on java spec for the case missing default initializer, now I assume it's zero
    * push eax
    */
  def emitLocalVariableDeclarationStatement(localVariableDeclarationStatement: LocalVariableDeclarationStatement): Unit = {
    localVariableDeclarationStatement.initializer match {
      case Some(initializer) => emitExpression(initializer)
      case None => emit(s"mov ${Reg.eax}, 0", "missing initializer in LocalVariableDeclarationStatement, default to zero")
    }
    emit(s"push ${Reg.eax}", s"push local variable ${localVariableDeclarationStatement.name} onto the stack")
    val e = localVariableDeclarationStatement
    val offset: Int = 1 + e.environment.get.asInstanceOf[StatementScope].findVariableOffset(e.name.getSimpleName)
    emit(s"lea ${Reg.eax}, [${Reg.ebp} - ${4 * offset} - 4]", "variable assignment")
    emit(s"pop ${Reg.ebx}", "get initializer result")
    emit(s"mov [${Reg.eax}], ${Reg.ebx}", "assign to local variable")
  }

  def allocateLocalVariable(numVariables: Int): Unit = {
    if (numVariables > 0) emit(s"sub ${Reg.esp}, ${numVariables * 4}", "allocate local variable")
  }

  def deallocateLocalVariable(numVariables: Int): Unit = {
    if (numVariables > 0) emit(s"add ${Reg.esp}, ${numVariables * 4}", "deallocate local variable")
  }

  def prologue(parameterSize: Int, isStatic: Boolean): Unit = {
    // Parameters need to be pushed into the stack prior to method implementations
    emit("", "Push callee save registers onto the stack")
    emit(s"push ${Reg.ebx}")
    emit(s"push ${Reg.esi}")
    emit(s"push ${Reg.edi}")

    emit(s"push ${Reg.ebp}")
    emit(s"mov ${Reg.ebp}, ${Reg.esp}")
    // copy saved parameters
    for (i <- 0 until parameterSize) {
      if(i == 0 && isStatic) {
        // Prevents from dereferencing an invalid Res.esi
      } else {
        emit(s"mov ${Reg.ebx}, [${Reg.esi} - ${4 * i}]")
      }
      emit(s"mov [${Reg.esp} - ${4 * i} - 4], ${Reg.ebx}")
    }
    emit(s"sub ${Reg.esp}, ${4 * parameterSize}")
  }

  def epilogue(): Unit = {
    emit(s"mov ${Reg.esp}, ${Reg.ebp}", "copies the value of frame pointer (ebp) to stack pointer (esp)")
    emit(s"pop ${Reg.ebp}", "pops callee-saved frame pointer (ebp) off the stack ")

    emit("", "Pop callee save registers from the stack")
    emit(s"pop ${Reg.edi}")
    emit(s"pop ${Reg.esi}")
    emit(s"pop ${Reg.ebx}")

    emit("ret")
  }

  /**
    * emitReturnStatement emits the x86 assembly for a ReturnStatement
    * This implements the typical epilogue in x86 covered in the lecture, except we don't pop callee-save regs
    * @param returnStatement An ast node of type ReturnStatement
    *
    * @GeneratedCode
    * mov esp, ebp
    * pop ebp
    * ret
    */
  def emitReturnStatement(returnStatement: ReturnStatement): Unit = {
    returnStatement.returnExp match {
      case Some(exp) => emitExpression(exp)
      case None =>
    }
    emit("", "Begin of ReturnStatement")
    epilogue()
    emit("", "End of ReturnStatement")
  }

  /**
    * emitEmptyStatement emits the x86 assembly for an EmptyStatement
    * @param emptyStatement an ast node of type EmptyStatement
    *
    * @GeneratedCode
    * ; Empty Statement
    */
  def emitEmptyStatement(emptyStatement: EmptyStatement): Unit = {
    // for debugging purpose
    emit("", "Empty Statement")
  }

  /**
    * emitWhileStatement emits the x86 assembly for an WhileStatement
    * @param whileStatement an ast node of type WhileStatement
    *
    * @GeneratedCode:
    *
    * startLabel:
    * /* generated code for whileStatement.condition */
    * cmp eax, 0
    * je endLabel
    * /* generated code for whileStatement.loopStmt */
    * jmp startLabel
    * endLabel:
    */
  def emitWhileStatement(whileStatement: WhileStatement): Unit = {
    emit("", "Begin of WhileStatement")

    val startLabel = createLabel()
    emitLabel(startLabel)

    // check the while cond
    emitExpression(whileStatement.condition)
    emit(s"cmp ${Reg.eax}, 0", "compare the WhileStatement condition result with zero")

    // jump to end label if the cond is not true
    val endLabel = createLabel()
    emit(s"je $endLabel", "jump if zero")

    // emit the code for loopStmt
    emitStatement(whileStatement.loopStmt)

    emit(s"jmp $startLabel", "unconditional jump to the start label of WhileStatement")

    emitLabel(endLabel)

    emit("", "End of WhileStatement")
  }

  /**
    * emitForStatement emits the x86 assembly for ForStatement
    * @param forStatement an ast node of type ForStatement
    *
    * @GeneratedCode
    *
    * /* generated code for forStatement.initStmt */
    * startLabel:
    * /* generated code for forStatement.condition */
    * cmp eax, 0
    * je endLabel
    * /* generated code for forStatement.loopStmt */
    * /* generated code for forStatement.update */
    * jmp startLabel
    * endLabel:
    */
  def emitForStatement(forStatement: ForStatement): Unit = {
    emit("", "Begin of ForStatement")

    forStatement.initStmt match {
      case Some(initStmt) => emitStatement(initStmt)
      case None =>
    }

    val startLabel = createLabel()
    val endLabel = createLabel()
    emitLabel(startLabel)

    forStatement.condition match {
      case Some(cond) =>
        emitExpression(cond)
        emit(s"cmp ${Reg.eax}, 0", "compare the ForStatement condition result with zero")
        emit(s"je $endLabel")
      case None =>
    }

    // loop body
    emitStatement(forStatement.loopStmt)

    forStatement.updateStmt match {
      case Some(update) => emitStatement(update)
      case None =>
    }

    emit(s"jmp $startLabel", "unconditional jump to the ForStatement startLabel")
    emitLabel(endLabel)

    emit("", "End of ForStatement")
  }

  /**
    * emitIfElseStmt emits the x86 assembly for an IfElseStmt
    * @param ifElseStmt an ast node of type IfElseStmt
    *
    * @GeneratedCode:
    *
    * // else statement is empty
    * /* generated code for ifElseStmt.cond */
    * cmp eax, 0
    * je falseLabel ; jump if zero
    * /* generated code for ifElseStmt.then */
    * falseLabel:
    *
    * OR
    *
    * // else statement is not empty
    * /* generated code for ifElseStmt.cond */
    * cmp eax, 0
    * je falseLabel ; jump if zero
    * /* generated code for ifElseStmt.then */
    * jmp endLabel ; unconditional jump
    * falseLabel:
    * /* generated code for ifElseStmt.else */
    * endLabel:
    */


  def emitIfElseStmt(ifElseStmt: IfElseStatement): Unit = {
    emit("", "Begin of IfElseStmt")

    emitExpression(ifElseStmt.condition)

    // check if the condition evaluates to zero
    emit(s"cmp ${Reg.eax}, 0", "compare the if condition result with zero")

    // jump to false label if the cond is not true
    val falseLabel = createLabel()
    emit(s"je $falseLabel", "jump if zero")

    // emit the then stmt
    emitStatement(ifElseStmt.thenStmt)

    ifElseStmt.elseStmt match {
      case EmptyStatement() =>
        // else stmt is empty, emit the false label
        emitLabel(falseLabel)
      case stmt =>
        val endLabel = createLabel()
        emit(s"jmp $endLabel", "unconditional jump to end label")
        emitLabel(falseLabel)
        emitStatement(stmt)
        emitLabel(endLabel)
    }
    emit("", "End of IfElseStmt")
  }

  def emitStaticFieldsData(labels: Seq[StaticFieldLabel]): Unit = {
//    emit("section .data")
    emit("")
    labels.foreach(label => {
      emitLabel(label)
      emit("dd 0")
      emit("")
    })
  }

  //TODO what does "section .text" do
  def  emitStaticFields(cpUnits: Seq[CompilationUnit]): Unit ={
    emit("", "Begin Initialize Static Fields (Some of these initializations are missing because cast expression has not been implemented yet")
    //    emit("section .text", "Begin Initialize Static Fields (Some of these initializations are missing because cast expression has not been implemented yet")
    emit("")

    var staticFieldLabels: Seq[StaticFieldLabel] = Seq.empty

    def recur(curEnvironment : TypeScope, ast: Ast): Unit ={
      ast match {
        case compilationUnit: CompilationUnit =>
          compilationUnit.children.foreach(x => recur(curEnvironment, x))
        case classDecl: ClassDecl =>
          classDecl.children.foreach(x => recur(classDecl.environment.get.asInstanceOf[TypeScope], x))
        case fieldDecl: FieldDecl =>
          if(fieldDecl.modifiers.contains(Modifier.STATIC)) {
            val currentFieldLabel = createStaticFieldLabel(curEnvironment.cName.toString, fieldDecl.name.toString)
            staticFieldLabels = staticFieldLabels :+ currentFieldLabel
            emitLabel(currentFieldLabel)
            fieldDecl.initializer match {
              case Some(expression: Expression) =>
                emitExpression(expression)
                emit(s"dd, ${Reg.eax}")
              case None =>
                emit(s"dd , 0")
            }
          }

        case interfaceDecl: InterfaceDecl =>
        case x =>
      }
    }
    cpUnits.foreach(x =>recur(null, x))

    emit("")
    emit("", "Done Initializing Static Fields")
  }

  def  emitFieldAndMethodImplementation(cpUnits: Seq[CompilationUnit]): Unit ={
    emit("", "Begin Implementing Fields and Methods")
//    emit("section .text", "Begin Implementing Fields and Methods")
    emit("")

    // This function will generate the SIT for all the parent interfaces of the current class
    // Store all the interface methods in the SIT in order
    // If any of these methods contains a concrete method implementation, store the implementation address
    // Else store the address that leads to exception call because call on unimplemented methods will lead to error
    def createSIT(curClass: String, rootEnvironment: RootEnvironment, implementedMethods: mutable.LinkedHashMap[(SimpleName, Seq[Type]),(Int, QualifiedName)]) : Unit = {
      val interfaces = rootEnvironment.canonicalNameToInterfaceMap
      emitLabel(createVtableSITLabel(curClass))
      // The SIT table will be created in order
      interfaces.foreach(interface => {
        interface._2.methodMap.foreach(method => {
          if(implementedMethods.contains(method._1)){
            val implementedScope : TypeScope = rootEnvironment.getScopeForType(implementedMethods(method._1)._2)
            emit(s"dd ${createClassMethodLabel(implementedScope.cName.toString, method._1._1.toString, method._1._2)}")
          } else
            emit(s"dd ${createClassMethodLabel(interface._2.cName.toString, method._1._1.toString, method._1._2)}")
        })
      })
    }

    // This function will generate the subtype table (1D) true false value
    def createSubTypeTable(curClass: String, relationMap: mutable.LinkedHashMap[QualifiedName, Boolean]): Unit ={
      // Generate subtype table label
      emitLabel(createSubTypeTableLabel(curClass))
      relationMap.foreach(entry => {
        if(entry._2)
          emit("dd 1")
        else emit("dd 0")
      })
    }

    // Vtable contains the following structure

    // Vtable_ClassName Label:
    // SIT : Index 0
    // Subtype Table : Index 1
    // Method Implementation Addresses : Start from Index 2
    def createVTable(classDecl: ClassDecl): Unit = {

      val curClassEnvironment = classDecl.environment.get.asInstanceOf[TypeScope]
      val classLabel = createVtableLabel(curClassEnvironment.cName.toString)

      // Generate interface descriptor table
      val implementedMethods = curClassEnvironment.gatherNonStaticMethods

      createSIT(curClassEnvironment.cName.toString, curClassEnvironment.parent.asInstanceOf[RootEnvironment], implementedMethods)
      // getSubClassRelation will get all the parents of the current class
      // the sub type table will mark these parents as 1 (true) and other as 0 (false)
      createSubTypeTable(curClassEnvironment.cName.toString, curClassEnvironment.getSubClassRelation)

      // Generate VTabelLabel
      emitLabel(classLabel)
      // Generate SIT Table
      emit(s"dd ${createVtableSITLabel(curClassEnvironment.cName.toString)}")
      // Generate Subtype Table
      emit(s"dd ${createSubTypeTableLabel(curClassEnvironment.cName.toString)}")

      // Generate implementations for all non static methods and offsets
      implementedMethods.foreach(method => {
        // Output the implementation in order
        emit(s"dd ${createClassMethodLabel(method._2._2.toString, method._1._1.toString, method._1._2)}")
      })
    }

    def createArrayVtable(objectScope: TypeScope): Unit ={
      emitLabel(createPrimitiveArrayLabel())
      // Array's VTable does not have SIT nor Subtype Table, therefore, they are set to 0
      emit("dd 0")
      emit("dd 0")
      objectScope.methodMap.foreach(method => {
        // Output the implementation in order
        emit(s"dd ${createClassMethodLabel(objectScope.cName.toString, method._1._1.toString, method._1._2)}")
      })
    }

    def recur(curEnvironment : Environment, ast: Ast): Unit ={
      ast match {
        case compilationUnit: CompilationUnit =>
          compilationUnit.children.foreach(x => recur(curEnvironment, x))
        case classDecl: ClassDecl =>
          emit("")
          emit("")
          emit("")
          emit("", s"**********code for class ${classDecl.className}**********")
          emit("")
          emit("")
          emit("")
          createVTable(classDecl)
          classDecl.children.foreach(x => recur(classDecl.environment.get.asInstanceOf[TypeScope], x))
        case interfaceDecl: InterfaceDecl =>
          interfaceDecl.children.foreach(x => recur(interfaceDecl.environment.get.asInstanceOf[TypeScope], x))
        case fieldDecl: FieldDecl =>
          if(!fieldDecl.modifiers.contains(Modifier.STATIC)){
            val currentFieldLabel = createClassFieldLabel(curEnvironment.asInstanceOf[TypeScope].cName.toString, fieldDecl.name.toString)
            emitLabel(currentFieldLabel)
            fieldDecl.initializer match {
              case Some(expression: Expression) => emitExpression(expression)
              case _ => emit(s"mov ${Reg.eax}, 0") // null is defined to be 0
            }
            val className = fieldDecl.environment.get.currentClassCName
            val classScope = fieldDecl.environment.get.getScopeForType(className)
            val fieldInfo = classScope.gatherNonStaticFields(fieldDecl.name)
            val fieldOffset = 3 + fieldInfo._3

            emit(s"push ${Reg.eax}", "save field initialized value")
            emitThisExpression()
            emit(s"pop ${Reg.ebx}", "get field initialized value")
            emit(s"mov [${Reg.eax} + ${fieldOffset} * 4], ${Reg.ebx}", "assign initialized value to field")

            emit("jmp end")
          }
        case constructorDecl: ConstructorDecl =>{
          val curTypeScope = curEnvironment.asInstanceOf[TypeScope]
          emitLabel(createConstructorLabel(curTypeScope.cName.toString, constructorDecl.parameterLists.map(_.parameterType)))

          prologue(constructorDecl.parameterLists.size + 1, false)

          // Call parent's constructor
          if(curTypeScope.cName != curTypeScope.classParent){
            emit(s"call ${createConstructorLabel(curTypeScope.classParent.toString, Seq.empty)}")
          }

          // Initialize all fields
          curTypeScope.fieldMap.foreach(field => {
            if(!field._2._2.contains(Modifier.STATIC))
            emit(s"call ${createClassFieldLabel(curTypeScope.cName.toString, field._1.toString)}")

          })

          // TODO check the local variable decl counter is valid in ctr
          constructorDecl.children.foreach(x => recur(curEnvironment, x))

          epilogue()
        }
        case methodDecl: MethodDecl =>
          if(methodDecl.modifiers.contains(Modifier.STATIC)){
            val currentStaticMethodLabel = createStaticMethodLabel(curEnvironment.asInstanceOf[TypeScope].cName.toString, methodDecl.name.toString, methodDecl.parameters.map(_.parameterType))
            emitLabel(currentStaticMethodLabel)
          } else {
            if(curEnvironment.asInstanceOf[TypeScope].isInterface){
              val currentInterfaceMethodLabel = createInterfaceMethodLabel(curEnvironment.asInstanceOf[TypeScope].cName.toString, methodDecl.name.toString, methodDecl.parameters.map(_.parameterType))
              emitLabel(currentInterfaceMethodLabel)
            } else {
              val currentClassMethodLabel = createClassMethodLabel(curEnvironment.asInstanceOf[TypeScope].cName.toString, methodDecl.name.toString, methodDecl.parameters.map(_.parameterType))
              emitLabel(currentClassMethodLabel)
              // Abstract method's implementation is an exception
              if(methodDecl.modifiers.contains(Modifier.ABSTRACT)){
                emit("call __exception")
              }
            }

          }
          // Preallocate space and offset for local variables in the method
          prologue(methodDecl.parameters.size + 1, methodDecl.modifiers.contains(Modifier.STATIC))
          methodDecl.children.foreach(x => recur(curEnvironment, x))
          if(methodDecl.modifiers.contains(Modifier.ABSTRACT) || curEnvironment.asInstanceOf[TypeScope].isInterface)
            emit("call __exception", "Throw exception when implementing abstract method")
          if(methodDecl.mType.isInstanceOf[VoidType])
            epilogue()
        case statement: Statement => emitStatement(statement)
        case expression: Expression => throw CodeGenerationException("should not throw here")
        case x =>
      }
    }
    cpUnits.foreach(x =>recur(null, x))
    emit("end: ret")
    emit("")
    emit("", "Done Implementing Fields, Methods and Vtables")
    // Get the Java Lang Object Type Scope
    createArrayVtable(cpUnits.head.environment.get.asInstanceOf[RootEnvironment].getScopeForType(javaLangObject))
  }

  // Start from static test method, should locate in the first cpUnit
  def emitStartMethod(cpUnit: Ast): Unit = {
    var isFound: Boolean = false
    def recur(ast: Ast): Unit ={
      ast match{
        case compilationUnit: CompilationUnit =>
          compilationUnit.children.foreach(recur)
        case classDecl: ClassDecl =>
          val curType = classDecl.environment.get.asInstanceOf[TypeScope]
          // Does not contain the starting point method
          if(!curType.methodMap.contains(SimpleName("test"), Seq.empty)){
            throw CodeGenerationException("Cannot find starting method declaration")
          }
          val method = curType.methodMap(SimpleName("test"), Seq.empty)
          // Must return Int and is a static method
          if(method._1 != PrimitiveType(PrimitiveType.INT) || !method._2.contains(Modifier.STATIC)){
            throw CodeGenerationException("Cannot find starting method declaration")
          }
          // Call the starting method
          allocateLocalVariable(1)
          emit(s"mov ${Reg.esi}, ${Reg.esp}", "create esi for _start method")
          emit(s"call ${createStaticMethodLabel(curType.cName.toString, "test", Seq.empty)}")
          deallocateLocalVariable(1)
          isFound = true
        case _ =>
      }
    }
    recur(cpUnit)
    if(!isFound){
      throw CodeGenerationException("Cannot find starting method declaration")
    }
  }

  def emitNativeWrite(): Unit ={
    emitLabel(createStaticMethodLabel("java.io.PrintStream", "nativeWrite", Seq(PrimitiveType(PrimitiveType.INT))))
    epilogue()
    emit("call NATIVEjava.io.OutputStream.nativeWrite")
    prologue(1, true)
  }

  def codeGeneration(cpUnits: Seq[CompilationUnit]): Unit = {
    emit("extern __exception")
    emit("extern __malloc")
    emit("extern __debexit")
    emit("extern NATIVEjava.io.OutputStream.nativeWrite")
    emit("")
    emit("global _start", "entry point")
    emitStaticFields(cpUnits)
    emitFieldAndMethodImplementation(cpUnits)
    emitNativeWrite()
    emit("_start:")
    emitStartMethod(cpUnits.head)
    emit("call __debexit")

  }

  def joos1wA5(cpUnits: Seq[CompilationUnit]): Unit = {
    emitStart
    joos1wA4(cpUnits)
    codeGeneration(cpUnits)
    emitEnds

  }
}
