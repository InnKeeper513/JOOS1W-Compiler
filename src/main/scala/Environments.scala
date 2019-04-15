import Asts._
import Utils.topologicalSort
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Environments {
  /*
  Java Namespaces
  1. package
  2. type
  3. method
  4. expression

  Seven steps for name resolution
  1. build global environment (set of classes)
  2. resolve type names (type namespace)
  3. check class hierarchy
  4. disambiguate namespace of ambiguous names
  5. resolve "expressions"
  6. type checking
  7. resolve methods and instance fields




   */

  case class EnvironmentBuildingException(message: String = "") extends Throwable {
    override def getMessage: String = message
  }

  case class TypeLinkingException(message: String = "") extends Throwable {
    override def getMessage: String = message
  }

  case class HierarchyCheckingException(message: String = "") extends Throwable {
    override def getMessage: String = message
  }

  abstract class Environment {

    def validateVariableName(name: SimpleName): Unit

    def resolveTypeName(typeName: SimpleName): QualifiedName
      /*
      this method is overridden by the concrete class
      steps to resolve simple type name
      1. check current compilation unit's type name and then check single import type name
      2. check the types in the same package
      3. check on demand type import, if not exactly imported once, throw TypeLinkingException
      4. throw TypeLinkingException
       */

    def resolveTypeName(typeName: QualifiedName): QualifiedName
      /*
      this method is overridden by the concrete class
      steps to resolve qualified type name
      1. check if type name exists in the package
      2. throw TypeLinkingException
       */

    def resolveTypeName(typeName: Name): QualifiedName = {
      typeName match {
        case simpleName: SimpleName => resolveTypeName(simpleName)
        case qualifiedName: QualifiedName => resolveTypeName(qualifiedName)
      }
    }

    def resolveType(tipe: Type): Type = {
      tipe match {
        case classOrInterfaceType: ClassOrInterfaceType =>
          val cName = resolveTypeName(classOrInterfaceType.name)
          val scope = getScopeForType(cName)
          if (scope.isClass) {
            ClassType(cName)
          } else {
            InterfaceType(cName)
          }
        case classType: ClassType =>
          val cName = resolveTypeName(classType.name)
          ClassType(cName)
        case interfaceType: InterfaceType =>
          val cName = resolveTypeName(interfaceType.name)
          InterfaceType(cName)
        case arrayType: ArrayType =>
          ArrayType(resolveType(arrayType.aType))
        case x => x
      }
    }

    def getScopeForType(typeName: QualifiedName): TypeScope

    def currentClassCName: QualifiedName

    def getParents: Set[QualifiedName]

  }

  // environment for all the compilation units
  class RootEnvironment extends Environment {

    var currentPackage: QualifiedName = QualifiedName(Seq.empty)

    // maps from the package name to the type name
    var packageToClassMap: Map[QualifiedName, Set[SimpleName]] = Map.empty.withDefaultValue(Set.empty)
    var packageToInterfaceMap: Map[QualifiedName, Set[SimpleName]] = Map.empty.withDefaultValue(Set.empty)

    // maps fron canonical name to the type scope
    var canonicalNameToClassMap: mutable.LinkedHashMap[QualifiedName, TypeScope] = mutable.LinkedHashMap.empty
    var canonicalNameToInterfaceMap: mutable.LinkedHashMap[QualifiedName, TypeScope] = mutable.LinkedHashMap.empty

    override def validateVariableName(name: SimpleName): Unit = {}

    def getAllClassAndInterface: mutable.LinkedHashMap[QualifiedName, TypeScope] = {
      canonicalNameToClassMap ++ canonicalNameToInterfaceMap
    }

    // gather interfaces of all interfaces including itself
    def gatherInterfaces(className: QualifiedName): Seq[TypeScope] ={
      val curClass = getScopeForType(className)
      if(curClass.interfaceExtends.nonEmpty)
        curClass.interfaceExtends.flatMap(interface => gatherInterfaces(interface)) ++ Seq(curClass)
      else Seq(curClass)
    }

    // gather typescope of all classes including itself
    def gatherClasses(className: QualifiedName): Seq[TypeScope] ={
      val curClass = getScopeForType(className)
       if(curClass.classParent != className)
        gatherClasses(curClass.classParent) :+ curClass
      else Seq(curClass)
    }

    def addClass(className: SimpleName, classScope: TypeScope): Unit = {
      validateClassOrInterfaceName(className)
      packageToClassMap += (currentPackage -> (packageToClassMap(currentPackage) + className))
      canonicalNameToClassMap += (QualifiedName(currentPackage.names :+ className.name) -> classScope)
    }

    def addInterface(interfaceName: SimpleName, interfaceScope: TypeScope): Unit = {
      validateClassOrInterfaceName(interfaceName)
      packageToInterfaceMap += (currentPackage -> (packageToInterfaceMap(currentPackage) + interfaceName))
      canonicalNameToInterfaceMap += (QualifiedName(currentPackage.names :+ interfaceName.name) -> interfaceScope)
    }


    // map from type name to seq of canonical names
    var onDemandImportedTypes: Map[SimpleName, Set[QualifiedName]] = Map.empty.withDefaultValue(Set.empty)
    // map from type name to canonical name
    var singleImportedTypes: Map[SimpleName, QualifiedName] = Map.empty

    // No two classes or interfaces have the same canonical name.
    def validateClassOrInterfaceName(name: SimpleName): Unit = {
      if (packageToClassMap(currentPackage).contains(name) || packageToInterfaceMap(currentPackage).contains(name)) {
        throw EnvironmentBuildingException("No two classes or interfaces have the same canonical name.")
      }
    }

    def singleImport(name: Name): Unit = {
      val packageName = QualifiedName(name.getQualifiedName.names.dropRight(1))
      val typeName = name.getSimpleName
      /*
      No single-type-import declaration clashes with the class or interface declared in the same file.
      No two single-type-import declarations clash with each other.
       */
      if (singleImportedTypes.contains(typeName) && singleImportedTypes(typeName) != name.getQualifiedName) {
        throw TypeLinkingException("single type imports clash with each other")
      }

      if (!packageToClassMap(packageName).contains(typeName) && !packageToInterfaceMap(packageName).contains(typeName)) {
        throw TypeLinkingException("single type import referring to a non-existing package")
      }
      singleImportedTypes += (name.getSimpleName -> name.getQualifiedName)
    }

    def onDemandImport(name: Name): Unit = {
      // qualified name for the package
      val qualifiedName = name.getQualifiedName

      var packageExist = false
      for (existingPackageName: QualifiedName <- packageToClassMap.keySet ++ packageToInterfaceMap.keySet) {
        if (existingPackageName.names.size >= qualifiedName.names.size) {
          var result = true
          for (i <- qualifiedName.names.indices) {
            if (existingPackageName.names(i) != qualifiedName.names(i)) result = false
          }
          if (result) {
            packageExist = true
            for (typeName <- packageToClassMap(existingPackageName) ++ packageToInterfaceMap(existingPackageName)) {
              onDemandImportedTypes += (typeName -> (onDemandImportedTypes(typeName) + QualifiedName(existingPackageName.names :+ typeName.name) ))
            }
          }
        }
      }

      if (!packageExist) {
        throw TypeLinkingException("on demand import referring to a non-existing package")
      }
//      for (typeName <- packageToClassMap(qualifiedName) ++ packageToInterfaceMap(qualifiedName)) {
//        onDemandImportedTypes += (typeName -> (onDemandImportedTypes(typeName) + QualifiedName(name.getQualifiedName.names :+ typeName.name) ))
//      }
    }

    def enterPackage(packageName: QualifiedName): Unit = {
      currentPackage = packageName
      onDemandImportedTypes = Map.empty.withDefaultValue(Set.empty)
      singleImportedTypes = Map.empty
    }

    def leavePackage(): Unit = {
      currentPackage = QualifiedName(Seq.empty)
      onDemandImportedTypes = Map.empty.withDefaultValue(Set.empty)
      singleImportedTypes = Map.empty
    }

    override def resolveTypeName(typeName: SimpleName): QualifiedName ={
      /*
      1. check single import type name
      2. check the types in the same package
      3. check on demand type import, if not exactly imported once, throw TypeLinkingException
      4. check java.lang
      5. throw TypeLinkingException
       */
      if (singleImportedTypes.contains(typeName)) return singleImportedTypes(typeName)
      if (packageToClassMap(currentPackage).contains(typeName) || packageToInterfaceMap(currentPackage).contains(typeName)) return QualifiedName(currentPackage.names :+ typeName.name)

      // check clash
      if (onDemandImportedTypes.contains(typeName) && onDemandImportedTypes(typeName).size == 1 &&
        (packageToClassMap(QualifiedName(Seq("java", "lang"))).contains(typeName) ||
        packageToInterfaceMap(QualifiedName(Seq("java", "lang"))).contains(typeName))
      ) throw TypeLinkingException("on-demand-import clash with java.lang")

      if (onDemandImportedTypes.contains(typeName) && onDemandImportedTypes(typeName).size == 1) {
        val qualifiedName = onDemandImportedTypes(typeName).head
        // no import-on-demand declarations that are used may resolve to types.
        // don't need to check here
        // checkPrefixOfOtherType(qualifiedName, strict = true, checkDefaultPackage = false)
        return qualifiedName
      }
      // TODO for the following two cases, check if the Type for the returned name is public
      if (packageToClassMap(QualifiedName(Seq("java", "lang"))).contains(typeName)) return QualifiedName(Seq("java", "lang", typeName.name))
      if (packageToInterfaceMap(QualifiedName(Seq("java", "lang"))).contains(typeName)) return QualifiedName(Seq("java", "lang", typeName.name))
      throw TypeLinkingException("can not resolve type name " + typeName)
    }

    override def resolveTypeName(qualifiedName: QualifiedName): QualifiedName = {
      if (qualifiedName.names.length < 2) return resolveTypeName(qualifiedName.getSimpleName)
      val packageName = QualifiedName(qualifiedName.names.dropRight(1))
      val typeName = SimpleName(qualifiedName.names.last)
      /*
      1. check if type name exists in the package
      2. throw TypeLinkingException
       */
      if (packageToClassMap(packageName).contains(typeName) || packageToInterfaceMap(packageName).contains(typeName)) {
        /*
          When a fully qualified name resolves to a type, no strict prefix of the fully qualified name
          can resolve to a type in the same environment.
          */
        // TODO I think this will include the case for used import on demand declarations for the following rule
        // No package names or prefixes of package names of declared packages, single-type-import declarations or
        // import-on-demand declarations that are used may resolve to types, except for types in the default package.
        checkPrefixOfOtherType(qualifiedName, strict = true, checkDefaultPackage = true)
        return qualifiedName
      }
      throw TypeLinkingException("cannot resolve type name " + qualifiedName)
    }

    def checkPrefixOfOtherType(qualifiedName: QualifiedName, strict: Boolean, checkDefaultPackage: Boolean): Unit = {
      for (i <- 1 to (if (strict) qualifiedName.names.length - 1 else qualifiedName.names.length)) {
        val strictPrefix = QualifiedName(qualifiedName.names.take(i))
        var thrown = false
        try {
          val resolvedName = resolveTypeName(strictPrefix)
          if (!checkDefaultPackage && resolvedName.names.length == 1) thrown = true // default package
        } catch {
          case _ : TypeLinkingException => thrown = true
        }
        if (!thrown) throw TypeLinkingException("no strict prefix of the fully qualified name can resolve to a type in the same environment")
      }
    }

    override def getScopeForType(typeName: QualifiedName): TypeScope = {
      if (canonicalNameToInterfaceMap.contains(typeName)) return canonicalNameToInterfaceMap(typeName)
      if (canonicalNameToClassMap.contains(typeName)) return canonicalNameToClassMap(typeName)
      throw TypeLinkingException("this should never throw, please investigate.")
    }

    var classInheritGraph: ListBuffer[(QualifiedName, QualifiedName)] = ListBuffer.empty

    def addEdge(from: QualifiedName, to: QualifiedName): Unit = {
      if (from.toString == "java.lang.Object" && to.toString == "java.lang.Object") return
      classInheritGraph += ((from, to))
    }

    def checkCycle() : Unit = {
      try {
        topologicalSort(classInheritGraph, this)
      } catch {
        case e: Exception => throw HierarchyCheckingException(e.toString)
      }
    }

    def currentClassCName: QualifiedName = {
      throw new Exception("should not reach here")
    }

    def getParents: Set[QualifiedName] = {
      throw new Exception("should not reach here")
    }

  }

  class TypeScope(val typeName: SimpleName, val packageName: QualifiedName, val parent: Environment, val modifiers: Seq[Modifier.Value], val isClass: Boolean) extends Environment {

    val isInterface: Boolean = !isClass
    val cName: QualifiedName = QualifiedName(packageName.names :+ typeName.name)

    var classParent: QualifiedName = QualifiedName(Seq.empty)
    var classImplements: Seq[QualifiedName] = Seq.empty
    var interfaceExtends: Seq[QualifiedName] = Seq.empty

    override def validateVariableName(name: SimpleName): Unit = {}
    /************ for stage 1 ************/

    // TYPE ARE NOT CANONICAL AT THIS STAGE

    // map from name to type and modifiers
    var fieldMap: mutable.LinkedHashMap[SimpleName, (Type, Seq[Modifier.Value])] = mutable.LinkedHashMap.empty

    def getAllClassAndInterface: mutable.LinkedHashMap[QualifiedName, TypeScope] = {
      parent.asInstanceOf[RootEnvironment].getAllClassAndInterface
    }

    def getSubClassRelation: mutable.LinkedHashMap[QualifiedName, Boolean] = {
      val allClassInterface: mutable.LinkedHashMap[QualifiedName, TypeScope] = parent.asInstanceOf[RootEnvironment].canonicalNameToClassMap ++
        parent.asInstanceOf[RootEnvironment].canonicalNameToInterfaceMap
      val existingParents = (gatherClasses(classParent) ++ classImplements.flatMap(gatherInterfaces)).map(_.cName)
      allClassInterface.map(x => {
        if((existingParents ++ Seq(cName)).contains(x._1)){
          x._1 -> true
        } else x._1 -> false
      })
    }

    def gatherClasses(name: QualifiedName): Seq[TypeScope] ={
      parent.asInstanceOf[RootEnvironment].gatherClasses(name)
    }

    def gatherInterfaces(name: QualifiedName): Seq[TypeScope] = {
      parent.asInstanceOf[RootEnvironment].gatherInterfaces(name)
    }

    def addField(name: SimpleName, fType: Type, modifiers: Seq[Modifier.Value]): Unit = {
      if (fieldMap.contains(name)) {
        throw EnvironmentBuildingException("No two fields declared in the same class may have the same name.")
      }
      fieldMap += (name -> ((fType, modifiers)))
    }

    /************ for stage 2 ************/

    // TYPE SHOULD BE RESOLVED FIRST BEFORE CALLING THESE METHODS

    def addFieldResolved(name: SimpleName, fType: Type, modifiers: Seq[Modifier.Value]): Unit = {
      fieldMap += (name -> ((fType, modifiers)))
    }

    // signature to modifiers
    var constructorMap: mutable.LinkedHashMap[Seq[Type], Seq[Modifier.Value]] = mutable.LinkedHashMap.empty

    def addConstructor(constructorName: SimpleName, signature: Seq[Type], modifiers: Seq[Modifier.Value]): Unit = {
      if (typeName != constructorName) throw TypeLinkingException("Constructor name is not the same as type link")
      if (constructorMap.contains(signature)) throw TypeLinkingException("A class must not declare two constructors with the same parameter type")
      constructorMap += (signature -> modifiers)
    }

    // (name, signature) to (return type, modifiers)
    var methodMap: mutable.LinkedHashMap[(SimpleName, Seq[Type]), (Type, Seq[Modifier.Value])] = mutable.LinkedHashMap.empty

    def addMethod(name: SimpleName, methodType: Type, signature: Seq[Type], modifiers: Seq[Modifier.Value]): Unit = {
      /*
        1. method and field can have the same simple name
        2. An interface may have two or more fields with the same simple name
          if they are declared in different interfaces and inherited.
          An attempt to refer to any such field by its simple name results in a compile-time error
          if no attempt to refer to such field, then no error
       */
      if (methodMap.contains((name, signature))) throw TypeLinkingException("A type must not declare two methods with the same signature")
      methodMap += ((name, signature) -> ((methodType, modifiers)))
    }

    override def resolveTypeName(typeName: SimpleName): QualifiedName = {
      /*
      1. check current compilation unit's type name
       */
      if (typeName == this.typeName) QualifiedName(packageName.names :+ this.typeName.name)
      else parent.resolveTypeName(typeName)
    }


    override def resolveTypeName(typeName: QualifiedName): QualifiedName = {
      parent.resolveTypeName(typeName)
    }

    override def getScopeForType(typeName: QualifiedName): TypeScope = {
      parent.getScopeForType(typeName)
    }


    def validateReturnType() : Map[(SimpleName, Seq[Type]), Type] = {

      val msg = "A class or interface must not contain (declare or inherit) two methods with the same signature but different return types"

      var returnMap: Map[(SimpleName, Seq[Type]), Type] = Map.empty

      for ((nameAndParams, typeAndModifiers) <- methodMap) {
        returnMap += (nameAndParams -> typeAndModifiers._1)
      }

      if (isClass && cName != classParent) {
        for ((nameAndParams, tipe) <- getScopeForType(classParent).validateReturnType()) {
          if (returnMap.contains(nameAndParams) && returnMap(nameAndParams) != tipe)
            throw HierarchyCheckingException(msg)
          returnMap += (nameAndParams -> tipe)
        }
        for (iName <- classImplements) {
          for ((nameAndParams, tipe) <- getScopeForType(iName).validateReturnType()) {
            if (returnMap.contains(nameAndParams) && returnMap(nameAndParams) != tipe)
              throw HierarchyCheckingException(msg)
            returnMap += (nameAndParams -> tipe)
          }
        }
      } else {
        if (interfaceExtends.isEmpty) {
          var m: Map[(SimpleName, Seq[Type]), Type] = Map.empty
          m += ((SimpleName("equals"), Seq(resolveType(ClassType(SimpleName("Object"))))) -> PrimitiveType(PrimitiveType.BOOLEAN))
          m += ((SimpleName("toString"), Seq()) -> resolveType(ClassType(SimpleName("String"))))
          m += ((SimpleName("hashCode"), Seq()) -> PrimitiveType(PrimitiveType.INT))
          for ((nameAndParams, tipe) <- m) {
            if (returnMap.contains(nameAndParams) && returnMap(nameAndParams) != tipe)
              throw HierarchyCheckingException(msg)
            returnMap += (nameAndParams -> tipe)
          }
        } else {
          for (iName <- interfaceExtends) {
            for ((nameAndParams, tipe) <- getScopeForType(iName).validateReturnType()) {
              if (returnMap.contains(nameAndParams) && returnMap(nameAndParams) != tipe)
                throw HierarchyCheckingException(msg)
              returnMap += (nameAndParams -> tipe)
            }
          }
        }
      }

      returnMap
    }

    /**
      * this is a big method
      * it can be used to handle several tests
      *   - check if a concrete class has any inherited abstract method
      *   - check method replacement
      * @return mapOfMethods
      */
    def gatherMethods: mutable.LinkedHashMap[(SimpleName, Seq[Type]), (Type, Seq[Modifier.Value], TypeScope)] = {
      val result: mutable.LinkedHashMap[(SimpleName, Seq[Type]), (Type, Seq[Modifier.Value], TypeScope)] = mutable.LinkedHashMap.empty

      def test(m : mutable.LinkedHashMap[(SimpleName, Seq[Type]), (Type, Seq[Modifier.Value], TypeScope)]) : Unit = {
        for ((nameAndParams, typeAndModifiers) <- m) {
          if (result.contains(nameAndParams)) {
            if (
              result(nameAndParams)._2.contains(Modifier.PUBLIC) && typeAndModifiers._2.contains(Modifier.PROTECTED) && !typeAndModifiers._2.contains(Modifier.ABSTRACT)
            )
              throw HierarchyCheckingException("A protected method must not replace a public method")
            if (result(nameAndParams)._2.contains(Modifier.FINAL) || m(nameAndParams)._2.contains(Modifier.FINAL))
              throw HierarchyCheckingException("A method must not replace a final method")
            if (
              (result(nameAndParams)._2.contains(Modifier.STATIC) && !m(nameAndParams)._2.contains(Modifier.STATIC)) ||
              (!result(nameAndParams)._2.contains(Modifier.STATIC) && m(nameAndParams)._2.contains(Modifier.STATIC))
            ) throw HierarchyCheckingException("A nonstatic method must not replace a static method")
          }
        }
      }

      def merge(m : mutable.LinkedHashMap[(SimpleName, Seq[Type]), (Type, Seq[Modifier.Value], TypeScope)]) : Unit = {
        for ((nameAndParams, typeAndModifiers) <- m) {
          if (result.contains(nameAndParams)) {
            if (result(nameAndParams)._2.contains(Modifier.ABSTRACT) && typeAndModifiers._2.contains(Modifier.ABSTRACT)) {
              // if both abstract, update if the new one is public
              if (typeAndModifiers._2.contains(Modifier.PUBLIC))
                result += (nameAndParams -> typeAndModifiers)
            } else if (!typeAndModifiers._2.contains(Modifier.ABSTRACT)) {
              // if the new one is not abstract, use new one
              result += (nameAndParams -> typeAndModifiers)
            }
          } else {
            result += (nameAndParams -> typeAndModifiers)
          }
        }
      }

      // ask parent first
      if (isClass && cName != classParent) {
        for (iName <- classImplements) {
          val m = getScopeForType(iName).gatherMethods
          test(m)
          merge(m)
        }

        val m = getScopeForType(classParent).gatherMethods
        test(m)
        merge(m)
      } else {
        if (interfaceExtends.isEmpty) {
          val m: mutable.LinkedHashMap[(SimpleName, Seq[Type]), (Type, Seq[Modifier.Value], TypeScope)] = mutable.LinkedHashMap.empty
          m += ((SimpleName("equals"), Seq(resolveType(ClassType(SimpleName("Object"))))) -> ((PrimitiveType(PrimitiveType.BOOLEAN), Seq(Modifier.PUBLIC, Modifier.ABSTRACT), this)))
          m += ((SimpleName("toString"), Seq()) -> ((resolveType(ClassType(SimpleName("String"))), Seq(Modifier.PUBLIC, Modifier.ABSTRACT), this)))
          m += ((SimpleName("hashCode"), Seq()) -> ((PrimitiveType(PrimitiveType.INT), Seq(Modifier.PUBLIC, Modifier.ABSTRACT), this)))
          test(m)
          merge(m)
        } else {
          for (iName <- interfaceExtends) {
            val m = getScopeForType(iName).gatherMethods
            test(m)
            merge(m)
          }
        }
      }

      for ((nameAndParams, _) <- methodMap) {
        // special case
        if (cName != classParent && nameAndParams._1.name == "getClass") throw HierarchyCheckingException("A method must not replace a final method")
        if (result.contains(nameAndParams)) {
          if (result(nameAndParams)._2.contains(Modifier.PUBLIC) && methodMap(nameAndParams)._2.contains(Modifier.PROTECTED))
            throw HierarchyCheckingException("A protected method must not replace a public method")
          if (result(nameAndParams)._2.contains(Modifier.FINAL))
            throw HierarchyCheckingException("A method must not replace a final method")
          if (result(nameAndParams)._2.contains(Modifier.STATIC) && !methodMap(nameAndParams)._2.contains(Modifier.STATIC))
            throw HierarchyCheckingException("A nonstatic method must not replace a static method")
          if (!result(nameAndParams)._2.contains(Modifier.STATIC) && methodMap(nameAndParams)._2.contains(Modifier.STATIC))
            throw HierarchyCheckingException("A static method must not replace a nonstatic method")
        }
      }

      result ++ methodMap.map(method => (method._1 -> (method._2._1, method._2._2, this)))
    }

    def getTypeFullName: QualifiedName = {
      cName
    }

    def gatherFields: mutable.LinkedHashMap[SimpleName, (Type, Seq[Modifier.Value])] = {
      var result: mutable.LinkedHashMap[SimpleName, (Type, Seq[Modifier.Value])] = mutable.LinkedHashMap.empty
      if (isClass && cName != classParent) {
        result ++= getScopeForType(classParent).gatherFields
      }
      for ((name, typeAndModifier) <- fieldMap) {
        result += (name -> typeAndModifier)
      }
      result
    }

    def gatherNonStaticFields: mutable.LinkedHashMap[SimpleName, (Type, Seq[Modifier.Value], Int)] = {
      var result: mutable.LinkedHashMap[SimpleName, (Type, Seq[Modifier.Value], Int)] = mutable.LinkedHashMap.empty
      if (isClass && cName != classParent) {
        result ++= getScopeForType(classParent).gatherNonStaticFields
      }
      var counter = result.size
      for ((name, typeAndModifier) <- fieldMap.filter{field => !field._2._2.contains(Modifier.STATIC)}) {
        if (!result.contains(name)) {
          result += (name -> (typeAndModifier._1, typeAndModifier._2, counter))
          counter += 1
        }
      }
      result
    }

    // Collect all the methods (including parent methods in order)
    // Note: This will only gather these non-static methods from classes not interfaces
    // Method with same name and signature will be overriden, but their corresponding order in the vtable will be maintained
    // Method implementation is determined by their corresponding implementation label
    def gatherNonStaticMethods: mutable.LinkedHashMap[(SimpleName, Seq[Type]), (Int, QualifiedName)] = {
      var result:  mutable.LinkedHashMap[(SimpleName, Seq[Type]), (Int, QualifiedName)] = mutable.LinkedHashMap.empty
      if (isClass && cName != classParent) {
        result ++= getScopeForType(classParent).gatherNonStaticMethods
      }
      var counter = result.size
      for ((nameAndArgs, _) <- methodMap.filter{method => !method._2._2.contains(Modifier.STATIC)}) {
        if (!result.contains(nameAndArgs)) {
          result += (nameAndArgs -> (counter, cName))
          counter += 1
        } else {
          // Maintain the same counter, but change the belonging class || implementation
          result += (nameAndArgs -> (result(nameAndArgs)._1, cName))
        }
      }
      result
    }

    def resolveFieldName(name: SimpleName): Option[Type] = {
      val fields = gatherFields
      if (fields.contains(name)) Some(fields(name)._1)
      else None
    }

    def currentClassCName: QualifiedName = {
      cName
    }

    def getParents: Set[QualifiedName] = {
      var result: Set[QualifiedName] = Set.empty
      if (isClass && cName != classParent && classParent.names.nonEmpty) {
        val parentScope = getScopeForType(classParent)
        result ++= parentScope.getParents
        for (iName <- classImplements) {
          val interfaceScope = getScopeForType(iName)
          result ++= interfaceScope.getParents
        }
        result ++= (Set(classParent) ++ classImplements.toSet)
      } else {
        for (iName <- interfaceExtends) {
          val interfaceScope = getScopeForType(iName)
          result ++= interfaceScope.getParents
        }
        result ++= interfaceExtends.toSet
      }

      result
    }


    var definedFields: Set[SimpleName] = Set.empty

  }

  // environment for
  class StatementScope(val parent: Environment) extends Environment {

    var variableMap: mutable.LinkedHashMap[SimpleName, Type] = mutable.LinkedHashMap.empty

    var localVariableDeclarationCounter: Int = 0
    var stackIndex: Int = 0

    def getAllClassAndInterface: mutable.LinkedHashMap[QualifiedName, TypeScope] = {
      parent match{
        case statementScope: StatementScope => statementScope.getAllClassAndInterface
        case typeScope: TypeScope => typeScope.getAllClassAndInterface
      }
    }

    def getSubClassRelation: mutable.LinkedHashMap[QualifiedName, Boolean] = {
      parent match{
        case typeScope: TypeScope => typeScope.getSubClassRelation
        case statementScope: StatementScope => statementScope.getSubClassRelation
      }
    }

    def gatherClasses(name: QualifiedName): Seq[TypeScope] ={
      parent match{
        case typeScope: TypeScope => typeScope.gatherClasses(name)
        case statementScope: StatementScope => statementScope.gatherClasses(name)
      }
    }

    def gatherInterfaces(name: QualifiedName): Seq[TypeScope] = {
      parent match{
        case typeScope: TypeScope => typeScope.gatherInterfaces(name)
        case statementScope: StatementScope => statementScope.gatherInterfaces(name)
      }
    }

    // Track the declaration order of local variable
    def setLocalVariableIndex(): Unit = {
      parent match{
        case s : StatementScope =>
          s.setLocalVariableIndex()
        case t : TypeScope =>
          stackIndex = localVariableDeclarationCounter
          localVariableDeclarationCounter += 1
      }
    }

    def getParentStatementOffset: Int = {
      parent match {
        case statementScope: StatementScope =>
          statementScope.variableMap.size + statementScope.getParentStatementOffset
        case _: TypeScope => 0
      }
    }

    // Find the declaration order of local variable
    def findVariableOffset(variableName: SimpleName): Int = {
      if (variableMap.contains(variableName)) {
        getParentStatementOffset + variableMap.keys.toSeq.indexOf(variableName)
      } else {
        parent.asInstanceOf[StatementScope].findVariableOffset(variableName)
      }
    }

    def addVariable(name: SimpleName, vType: Type): Unit = {
      validateVariableName(name)
      variableMap += (name -> vType)
    }

    // No two local variables with overlapping scope have the same name.
    override def validateVariableName(name: SimpleName): Unit = {
      if (variableMap.contains(name)) {
        throw EnvironmentBuildingException(name + " overlap")
      }
      // ask parent if the expression name is valid (for nested StatementScope)
      parent.validateVariableName(name)
    }

    override def resolveTypeName(typeName: SimpleName): QualifiedName = {
      parent.resolveTypeName(typeName)
    }


    override def resolveTypeName(typeName: QualifiedName): QualifiedName = {
      parent.resolveTypeName(typeName)
    }

    override def getScopeForType(typeName: QualifiedName): TypeScope = {
      parent.getScopeForType(typeName)
    }

    def resolveVariable(name: SimpleName): Option[Type] = {
      if (variableMap.contains(name)) return Some(variableMap(name))
      parent match {
        case statementScope: StatementScope => statementScope.resolveVariable(name)
        case _ => None
      }
    }

    def resolveFieldName(name: SimpleName): Option[Type] = {
      parent match {
        case statementScope: StatementScope => statementScope.resolveFieldName(name)
        case typeScope: TypeScope => typeScope.resolveFieldName(name)
        case _ => None
      }
    }

    def currentClassCName: QualifiedName = {
      parent.currentClassCName
    }

    def getParents: Set[QualifiedName] = {
      parent.getParents
    }

    def gatherFields:  mutable.LinkedHashMap[SimpleName, (Type, Seq[Modifier.Value])] = {
      parent match {
        case statementScope: StatementScope => statementScope.gatherFields
        case typeScope: TypeScope => typeScope.gatherFields
        case _ => mutable.LinkedHashMap.empty
      }
    }

    def gatherNonStaticFields: mutable.LinkedHashMap[SimpleName, (Type, Seq[Modifier.Value], Int)] = {
      parent match {
        case statementScope: StatementScope => statementScope.gatherNonStaticFields
        case typeScope: TypeScope => typeScope.gatherNonStaticFields
        case _ => mutable.LinkedHashMap.empty
      }
    }

    def gatherNonStaticMethods: mutable.LinkedHashMap[(SimpleName, Seq[Type]), (Int, QualifiedName)] = {
      parent match {
        case statementScope: StatementScope => statementScope.gatherNonStaticMethods
        case typeScope: TypeScope => typeScope.gatherNonStaticMethods
        case _ => mutable.LinkedHashMap.empty
      }
    }

    def getTypeFullName: QualifiedName = {
      parent match{
        case statementScope: StatementScope => statementScope.getTypeFullName
        case typeScope: TypeScope => typeScope.getTypeFullName
      }
    }
  }

  /**
    * first stage of assignment 2: environment building
    * No two fields declared in the same class may have the same name.
    * No two local variables with overlapping scope have the same name.
    * No two classes or interfaces have the same canonical name.
    *
    *  only some method will be called during environment building
    *
    *  - enterPackage
    *  - leavePackage
    *  - addClass
    *  - addInterface
    *  - addField
    *  - addVariable
    *
    * @param cpUnits
    * @return
    */
  def buildEnvironment(cpUnits: Seq[CompilationUnit]): RootEnvironment = {
    val rootEnvironment = new RootEnvironment()

    def recur(parentEnvironment: Environment, ast: Ast): Environment = {
      // default environment for ast is the environment passed from the parent
      var result: Environment = parentEnvironment
      ast.environment = Some(result)

      // EXHAUSTIVE match, fill in environment for every ast
      ast match {
        case cpUnit: CompilationUnit =>
          cpUnit.children.foldLeft(parentEnvironment: Environment)(recur)
          parentEnvironment.asInstanceOf[RootEnvironment].leavePackage()

        case packageDecl: PackageDecl =>
          parentEnvironment.asInstanceOf[RootEnvironment].enterPackage(packageDecl.qualifiedName)

        case _: ImportDecl => // Do nothing for now

        case classDecl: ClassDecl =>
          // sees a classDecl, this means the scope is rootEnvironment

          val packageName = rootEnvironment.currentPackage
//          println(classDecl.className.toString)
          val classScope = new TypeScope(classDecl.className, packageName, parentEnvironment, classDecl.modifiers, isClass = true)
          ast.environment = Some(classScope)
          classDecl.children.foldLeft(classScope: Environment)(recur)
          parentEnvironment.asInstanceOf[RootEnvironment].addClass(classDecl.className, classScope)

        case interfaceDecl: InterfaceDecl =>
          val packageName = rootEnvironment.currentPackage
          val interfaceScope = new TypeScope(interfaceDecl.name, packageName, parentEnvironment, interfaceDecl.modifiers, isClass = false)
          ast.environment = Some(interfaceScope)
          interfaceDecl.children.foldLeft(interfaceScope: Environment)(recur)
          parentEnvironment.asInstanceOf[RootEnvironment].addInterface(interfaceDecl.name, interfaceScope)

        case constructorDecl: ConstructorDecl =>
          val constructorScope = new StatementScope(parentEnvironment)
          ast.environment = Some(constructorScope)
          // the children of constructorDecl should be a single `Block` Ast
          constructorDecl.children.foldLeft(constructorScope: Environment)(recur)

        case fieldDecl: FieldDecl =>
          parentEnvironment.asInstanceOf[TypeScope].addField(fieldDecl.name, fieldDecl.fType, fieldDecl.modifiers)
          val statementScope = new StatementScope(parentEnvironment)
          ast.environment = Some(statementScope)
          if (fieldDecl.initializer.nonEmpty) recur(statementScope, fieldDecl.initializer.get)

        case methodDecl: MethodDecl =>
          val methodScope = new StatementScope(parentEnvironment)
          ast.environment = Some(methodScope)
          methodDecl.children.foldLeft(methodScope: Environment)(recur)

        case methodDecl: InterfaceMethodDecl =>
          val methodScope = new StatementScope(parentEnvironment)
          ast.environment = Some(methodScope)
          methodDecl.children.foldLeft(methodScope: Environment)(recur)

        case block: Block =>
          val blockScope = new StatementScope(parentEnvironment)
          ast.environment = Some(blockScope)
          block.children.foldLeft(blockScope: Environment)(recur)
        case varDeclStmt: LocalVariableDeclarationStatement =>
//          val newScope = new StatementScope(parentEnvironment)
//          newScope.setLocalVariableIndex()
          // change return value to the new scope
//          ast.environment = Some(newScope)
//          result = newScope
//          newScope.addVariable(varDeclStmt.name, varDeclStmt.vType)
          if (varDeclStmt.initializer.nonEmpty) recur(parentEnvironment, varDeclStmt.initializer.get)
        case expressionStatement: ExpressionStatement =>
          recur(parentEnvironment, expressionStatement.exp)
        case ifStmt: IfElseStatement =>
          recur(parentEnvironment, ifStmt.condition)
          recur(parentEnvironment, ifStmt.thenStmt)
          recur(parentEnvironment, ifStmt.elseStmt)
        case whileStmt: WhileStatement =>
          recur(parentEnvironment, whileStmt.condition)
          recur(parentEnvironment, whileStmt.loopStmt)
        case forStmt: ForStatement =>
//          val newScope = new StatementScope(parentEnvironment)
//          ast.environment = Some(newScope)
//          if (forStmt.initStmt.getOrElse(EmptyStatement()).isInstanceOf[LocalVariableDeclarationStatement]) {
//            val varDeclStmt = forStmt.initStmt.get.asInstanceOf[LocalVariableDeclarationStatement]
//            newScope.addVariable(varDeclStmt.name, varDeclStmt.vType)
//          }
//          if (forStmt.initStmt.nonEmpty) recur(newScope, forStmt.initStmt.get)

//          var newScope = new StatementScope(parentEnvironment)
//          ast.environment = Some(newScope)
          if (forStmt.initStmt.nonEmpty) recur(parentEnvironment, forStmt.initStmt.get).asInstanceOf[StatementScope]
          if (forStmt.condition.nonEmpty) recur(parentEnvironment, forStmt.condition.get)
          if (forStmt.updateStmt.nonEmpty) recur(parentEnvironment, forStmt.updateStmt.get)
          recur(parentEnvironment, forStmt.loopStmt)
        case returnStatement: ReturnStatement =>
          if (returnStatement.returnExp.nonEmpty)
            recur(parentEnvironment, returnStatement.returnExp.get)
        case _: EmptyStatement =>
        case assignment: Assignment =>
          recur(parentEnvironment, assignment.lhs)
          recur(parentEnvironment, assignment.rhs)
        case binaryExpression: BinaryExpression =>
          recur(parentEnvironment, binaryExpression.exp1)
          recur(parentEnvironment, binaryExpression.exp2)
        case minusExpression: MinusExpression =>
          recur(parentEnvironment, minusExpression.exp)
        case notExpression: NotExpression =>
          recur(parentEnvironment, notExpression.exp)
        case castExpression: CastExpression =>
          recur(parentEnvironment, castExpression.exp)
        case _: VariableExpression =>
        case arrayCreationExpression: ArrayCreationExpression =>
          recur(parentEnvironment, arrayCreationExpression.sizeExp)
        case _: LiteralExpression =>
        case _: ThisExpression =>
        case classInstanceCreationExpression: ClassInstanceCreationExpression =>
          classInstanceCreationExpression.args = classInstanceCreationExpression.args.map(arg => {
            recur(parentEnvironment, arg)
            arg
          })
        case fieldAccessExpression: FieldAccessExpression =>
          recur(parentEnvironment, fieldAccessExpression.exp)
        case methodInvocationExpression: MethodInvocationExpression =>
          recur(parentEnvironment, methodInvocationExpression.exp1)
          methodInvocationExpression.argumentList = methodInvocationExpression.argumentList.map(arg => {
            recur(parentEnvironment, arg)
            arg
          })
        case arrayAccessExpression: ArrayAccessExpression =>
          recur(parentEnvironment, arrayAccessExpression.exp1)
          recur(parentEnvironment, arrayAccessExpression.exp3)
        case instanceOfExpression: InstanceOfExpression =>
          recur(parentEnvironment, instanceOfExpression.exp)
        case parenthesesExpression: ParenthesesExpression =>
          recur(parentEnvironment, parenthesesExpression.exp)
      }
      result
    }

    cpUnits.foldLeft(rootEnvironment: Environment)(recur)
    rootEnvironment
  }

  /**
    * second stage of assignment 2: type linking
    *
    * connect each use of a named type to the declaration of the type
    *   ( this is achieved by storing the canonical name of the type in the name )
    *
    *
    *
    * @param cpUnits
    */
  def linkType(cpUnits: Seq[CompilationUnit]): Unit = {

    def recur(ast: Ast): Unit = {

      /*

      A name is syntactically classified as a TypeName in these contexts:
        1. In a single-type-import declaration (§7.5.1)
        2. In an extends clause in a class declaration (§8.1.3)
        3. In an implements clause in a class declaration (§8.1.4)
        4. In an extends clause in an interface declaration (§9.1.3)
        5. As a Type (or the part of a Type that remains after all brackets are deleted) in any of the following contexts:
          1. In a field declaration (§8.3, §9.3)
          2. As the result type of a method (§8.4, §9.4)
          3. As the type of a formal parameter of a method or constructor (§8.4.1, §8.6.1, §9.4)
          4. As the type of an exception that can be thrown by a method or constructor (§8.4.4, §8.6.4, §9.4)
          5. As the type of a local variable (§14.3)
          6. As the type of an exception parameter in a catch clause of a try statement (§14.18)
          7. As the class type of an instance that is to be created in a class instance creation expression (§15.8)
          8. As the element type of an array to be created in an array creation expression (§15.9)
          9. As the type mentioned in the cast operator of a cast expression (§15.15)
          10. As the type that follows the instanceof relational operator (§15.19.2)

      find all names that are classified as TypeName and
      call Ast.environment.resolveTypeName(name) on them

     */

//      println(ast.toString)
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
          // No package names or prefixes of package names of declared packages may resolve to types
          environment.asInstanceOf[RootEnvironment].checkPrefixOfOtherType(packageName, strict = false, checkDefaultPackage = false)

          environment.asInstanceOf[RootEnvironment].enterPackage(packageName)

        // $1. add imported type information in the root environment, validate each type imported
        case importDecl: ImportDecl =>
          if (importDecl.onDemand) {
            environment.asInstanceOf[RootEnvironment].onDemandImport(importDecl.name)
          } else {// no single-type-import declarations may resolve to types
            environment.asInstanceOf[RootEnvironment].checkPrefixOfOtherType(importDecl.name.getQualifiedName, strict = true, checkDefaultPackage = false)
            environment.asInstanceOf[RootEnvironment].singleImport(importDecl.name)
          }

        case classDecl: ClassDecl =>
          classDecl.className.cName = Some(environment.resolveTypeName(classDecl.className))
          // $2. check extends clause
          classDecl.parent.cName = Some(environment.resolveTypeName(classDecl.parent))

          // add edge
          val classScope = environment.asInstanceOf[TypeScope]
          val rootScope = classScope.parent.asInstanceOf[RootEnvironment]
          rootScope.addEdge(classDecl.className.getCName, classDecl.parent.getCName)
          classScope.classParent = classDecl.parent.getCName

          // $3. check implements clause
          classDecl.interfaces = classDecl.interfaces.map(name => {
            name.cName = Some(environment.resolveTypeName(name))
            rootScope.addEdge(classDecl.className.getCName, name.getCName)
            name
          })
          classScope.classImplements = classDecl.interfaces.map(_.getCName)

          if (rootScope.singleImportedTypes.contains(classDecl.className) && rootScope.singleImportedTypes(classDecl.className) != classScope.cName)
            throw TypeLinkingException("No single-type-import declaration clashes with the class declared in the same file")

          classDecl.children.foreach(recur)

        case interfaceDecl: InterfaceDecl =>
          interfaceDecl.name.cName = Some(environment.resolveTypeName(interfaceDecl.name))
          // add edge
          val interfaceScope = environment.asInstanceOf[TypeScope]
          val rootScope = interfaceScope.parent.asInstanceOf[RootEnvironment]
          // $4. check implements clause
          interfaceDecl.interfaces = interfaceDecl.interfaces.map(name => {
            name.cName = Some(environment.resolveTypeName(name))
            rootScope.addEdge(interfaceDecl.name.getCName, name.getCName)
            name
          })

          interfaceScope.interfaceExtends = interfaceDecl.interfaces.map(_.getCName)

          if (rootScope.singleImportedTypes.contains(interfaceDecl.name) && rootScope.singleImportedTypes(interfaceDecl.name) != interfaceScope.cName)
            throw TypeLinkingException("No single-type-import declaration clashes with the class declared in the same file")

          interfaceDecl.children.foreach(recur)

        // $5.3
        case constructorDecl: ConstructorDecl =>
          constructorDecl.parameterLists = constructorDecl.parameterLists.map(param => {
            Parameter(environment.resolveType(param.parameterType), param.parameterName)
          })
          // add to type scope
          val statementScope = environment.asInstanceOf[StatementScope]
          val typeScope = statementScope.parent.asInstanceOf[TypeScope]
          typeScope.addConstructor(constructorDecl.constructorName, constructorDecl.parameterLists.map(_.parameterType), constructorDecl.modifiers)

          for (parameter: Parameter <- constructorDecl.parameterLists) {
            statementScope.addVariable(parameter.parameterName, parameter.parameterType)
          }

          constructorDecl.children.foreach(recur)

        // $5.1
        case fieldDecl: FieldDecl =>
          fieldDecl.fType = environment.resolveType(fieldDecl.fType)
          // add to type scope
          val typeScope = environment.asInstanceOf[StatementScope].parent.asInstanceOf[TypeScope]
          typeScope.addFieldResolved(fieldDecl.name, fieldDecl.fType, fieldDecl.modifiers)
          if (fieldDecl.initializer.nonEmpty) recur(fieldDecl.initializer.get)

        case methodDecl: MethodDecl =>
          // $5.2
          methodDecl.mType = environment.resolveType(methodDecl.mType)
          // $5.3
          methodDecl.parameters = methodDecl.parameters.map(param => {
            Parameter(environment.resolveType(param.parameterType), param.parameterName)
          })
          val statementScope = environment.asInstanceOf[StatementScope]
          val typeScope = statementScope.parent.asInstanceOf[TypeScope]
          typeScope.addMethod(methodDecl.name, methodDecl.mType, methodDecl.parameters.map(_.parameterType), methodDecl.modifiers)

          for (parameter: Parameter <- methodDecl.parameters) {
            statementScope.addVariable(parameter.parameterName, parameter.parameterType)
          }

          methodDecl.children.foreach(recur)

        case methodDecl: InterfaceMethodDecl =>
          // $5.2
          methodDecl.mType = environment.resolveType(methodDecl.mType)
          // $5.3
          methodDecl.parameters = methodDecl.parameters.map(param => {
            Parameter(environment.resolveType(param.parameterType), param.parameterName)
          })
          val statementScope = environment.asInstanceOf[StatementScope]
          val typeScope = statementScope.parent.asInstanceOf[TypeScope]
          // interface methods are abstract. just add the abstract modifier here
          typeScope.addMethod(methodDecl.name, methodDecl.mType, methodDecl.parameters.map(_.parameterType), methodDecl.modifiers :+ Modifier.ABSTRACT)

          for (parameter: Parameter <- methodDecl.parameters) {
            statementScope.addVariable(parameter.parameterName, parameter.parameterType)
          }

          methodDecl.children.foreach(recur)

        case block: Block =>
          block.children.foreach(recur)

        // $5.5
        case varDeclStmt: LocalVariableDeclarationStatement =>
          varDeclStmt.vType = environment.resolveType(varDeclStmt.vType)
          val statementScope = environment.asInstanceOf[StatementScope]
          statementScope.addVariable(varDeclStmt.name, varDeclStmt.vType)
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
        case _ : EmptyStatement =>
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
        // $5.9
        case castExpression: CastExpression =>
          castExpression.cType = environment.resolveType(castExpression.cType)
          recur(castExpression.exp)
        case _: VariableExpression =>
        // $5.8
        case arrayCreationExpression: ArrayCreationExpression =>
          arrayCreationExpression.aType = environment.resolveType(arrayCreationExpression.aType)
          recur(arrayCreationExpression.sizeExp)
        case _: LiteralExpression =>
        case _: ThisExpression =>
        // $5.7
        case classInstanceCreationExpression: ClassInstanceCreationExpression =>
          classInstanceCreationExpression.cType = environment.resolveType(classInstanceCreationExpression.cType).asInstanceOf[ClassType]
          classInstanceCreationExpression.args.foreach(recur)
        case fieldAccessExpression: FieldAccessExpression =>
          recur(fieldAccessExpression.exp)
        case methodInvocationExpression: MethodInvocationExpression =>
          recur(methodInvocationExpression.exp1)
          methodInvocationExpression.argumentList.foreach(recur)
        case arrayAccessExpression: ArrayAccessExpression =>
          recur(arrayAccessExpression.exp1)
          recur(arrayAccessExpression.exp3)
        // $5.10
        case instanceOfExpression: InstanceOfExpression =>
          instanceOfExpression.iType = environment.resolveType(instanceOfExpression.iType)
          recur(instanceOfExpression.exp)
        case parenthesesExpression: ParenthesesExpression =>
          recur(parenthesesExpression.exp)
      }
    }

    cpUnits.foreach(recur)
  }

  /**
    * third stage of assignment 2: hierarchy checking
    * @param cpUnits
    */
  def checkHierarchy(cpUnits: Seq[CompilationUnit]) : Unit = {

    def recur(ast: Ast): Unit = {

      val environment = ast.environment.get

      ast match {
        case cpUnit: CompilationUnit =>
          // reset the importedTypes, prepare for import declarations
          cpUnit.children.foreach(recur)
          // after processing every children of cpUnit, leave the package
          environment.asInstanceOf[RootEnvironment].leavePackage()

        case packageDecl: PackageDecl =>
          val packageName = packageDecl.qualifiedName
          environment.asInstanceOf[RootEnvironment].enterPackage(packageName)

        case _: ImportDecl =>

        case classDecl: ClassDecl =>
          val qualifiedExtendName = classDecl.parent.getCName
          val parentTypeScope = environment.getScopeForType(qualifiedExtendName)
          if (parentTypeScope.isInterface) throw HierarchyCheckingException("A class must not extend an interface")
          if (parentTypeScope.modifiers.contains(Modifier.FINAL)) throw HierarchyCheckingException("A class must not extend a final class")

          if (parentTypeScope.cName != QualifiedName(Seq("java", "lang", "Object"))) {
//            for (signature <- environment.asInstanceOf[TypeScope].constructorMap.keySet) {
              if (!parentTypeScope.constructorMap.contains(Seq.empty)) {
                throw HierarchyCheckingException("......")
              }
//            }
          }

          var interfaces: Set[QualifiedName] = Set.empty
          classDecl.interfaces.foreach(name => {
            val qualifiedName = name.getCName
            if (interfaces.contains(qualifiedName)) throw HierarchyCheckingException("An interface must not be repeated in an implements clause")
            interfaces += qualifiedName
            val typeScope = environment.getScopeForType(qualifiedName)
            if (typeScope.isClass) throw HierarchyCheckingException("A class must not implement a class")
          })

          val currentTypeScope = environment.asInstanceOf[TypeScope]

          val methods = currentTypeScope.gatherMethods
          val abstractMethods = methods.values.count(_._2.contains(Modifier.ABSTRACT))
          if (abstractMethods != 0 && !classDecl.modifiers.contains(Modifier.ABSTRACT)) {
            throw HierarchyCheckingException("A class that contains any abstract methods must be abstract")
          }

          // check if return type is conflicting with parents
          currentTypeScope.validateReturnType()

          classDecl.children.foreach(recur)

        case interfaceDecl: InterfaceDecl =>
          var interfaces: Set[QualifiedName] = Set.empty
          interfaceDecl.interfaces.foreach(name => {
            val qualifiedName = name.getCName
            if (interfaces.contains(qualifiedName)) throw HierarchyCheckingException("An interface must not be repeated in an extends clause")
            interfaces += qualifiedName
            val typeScope = environment.getScopeForType(qualifiedName)
            if (typeScope.isClass) throw HierarchyCheckingException("An interface must not extends a class")
          })

          val currentTypeScope = environment.asInstanceOf[TypeScope]
          // check if return type is conflicting with parents
          currentTypeScope.validateReturnType()

          currentTypeScope.gatherMethods

          interfaceDecl.children.foreach(recur)

        case _ =>
      }
    }

    cpUnits.foreach(recur)
  }

  /**
    * wrap for statement and localVariableDeclaration
    */
  def wrapForStmtAndLocalVarWithBlock(cpUnits: Seq[CompilationUnit]) : Unit = {

    def recur(ast: Ast) : Ast = {
      ast match {
        case cpUnit: CompilationUnit =>
          // reset the importedTypes, prepare for import declarations
          cpUnit.children = cpUnit.children.map(recur)

        case packageDecl: PackageDecl =>

        case _: ImportDecl =>

        case classDecl: ClassDecl =>
          classDecl.children = classDecl.children.map(recur)

        case interfaceDecl: InterfaceDecl =>

        // $5.3
        case constructorDecl: ConstructorDecl =>
          constructorDecl.children = constructorDecl.children.map(recur)

        // $5.1
        case fieldDecl: FieldDecl =>

        case methodDecl: MethodDecl =>
          methodDecl.children = methodDecl.children.map(recur)

        case methodDecl: InterfaceMethodDecl =>

        case block: Block =>
          block.children = block.children.map(recur)

          def fold : (Ast, Seq[Ast]) => Seq[Ast] = (newStmt: Ast, before: Seq[Ast]) => {
            var now: Seq[Ast] = Seq(newStmt) ++ before
            newStmt match {
              case localVariableDeclarationStatement: LocalVariableDeclarationStatement =>
                val block = Block()
                block.children = now
                Seq(block)
              case _ => now
            }
          }

          block.children = block.children.foldRight(Seq.empty : Seq[Ast])(fold)

        // $5.5
        case varDeclStmt: LocalVariableDeclarationStatement =>

        case expressionStatement: ExpressionStatement =>

        case ifStmt: IfElseStatement =>
          ifStmt.thenStmt = recur(ifStmt.thenStmt).asInstanceOf[Statement]
          ifStmt.elseStmt = recur(ifStmt.elseStmt).asInstanceOf[Statement]

        case whileStmt: WhileStatement =>
          whileStmt.loopStmt = recur(whileStmt.loopStmt).asInstanceOf[Statement]

        case forStmt: ForStatement =>
          forStmt.loopStmt = recur(forStmt.loopStmt).asInstanceOf[Statement]
          val block = Block()
          block.children = Seq(forStmt)
          return block

        case returnStatement: ReturnStatement =>

        case _ : EmptyStatement =>

        case expression: Expression =>
      }
      ast
    }

    cpUnits.foreach(recur)
  }

  def joos1wA2(cpUnits: Seq[CompilationUnit]) : Unit = {
    wrapForStmtAndLocalVarWithBlock(cpUnits)
    val rootEnvironment = buildEnvironment(cpUnits)
    linkType(cpUnits)
    // check cycle first so that we wont get infinite loop when checking hierarchy
    rootEnvironment.checkCycle()
    checkHierarchy(cpUnits)
  }

}
