import Asts.Type

object Label {
  private var counter: Long = 0
  /**
    * getCounter returns the counter and post increment the counter by one
    * @return
    */
  def getCounter: Long = {
    val count = counter
    Label.counter += 1
    count
  }
  /**
    * createLabel creates a label with the name labelName and returns the Label
    * @return
    */
  def createLabel(): Label = {
    new Label(getCounter)
  }

  def createStaticFieldLabel(className: String, fieldName: String): StaticFieldLabel = {
    new StaticFieldLabel(className, fieldName)
  }

  def createStaticMethodLabel(className: String, methodName: String, params: Seq[Type]): StaticMethodLabel = {
    new StaticMethodLabel(className, methodName, params)
  }

  def createClassFieldLabel(className: String, fieldName: String): ClassFieldLabel = {
    new ClassFieldLabel(className, fieldName)
  }

  def createClassMethodLabel(className: String, methodName: String, params: Seq[Type]): ClassMethodLabel = {
    new ClassMethodLabel(className, methodName, params)
  }

  def createVtableLabel(className: String): VtableLabel = {
    new VtableLabel(className)
  }
  def createInterfaceLabel(interfaceName: String): InterfaceLabel = {
    new InterfaceLabel(interfaceName)
  }

  def createInterfaceMethodLabel(interfaceName: String, methodName: String, params: Seq[Type]): InterfaceMethodLabel = {
    new InterfaceMethodLabel(interfaceName, methodName, params)
  }

  def createVtableSITLabel(className: String): VtableSITLabel = {
    new VtableSITLabel(className)
  }

  def createSubTypeTableLabel(className: String): SubTypeTableLabel ={
    new SubTypeTableLabel(className)
  }

  def createSubTypeTableEntryLabel(className: String, entryName: String): SubTypeTableEntryLabel  ={
    new SubTypeTableEntryLabel(className, entryName)
  }

  def createConstructorLabel(className: String, params: Seq[Type]): ConstructorLabel = {
    new ConstructorLabel(className, params)
  }

  def createPrimitiveArrayLabel(): PrimitiveArrayLabel = {
    new PrimitiveArrayLabel()
  }

  def createReferenceArrayLabel(className: String): ReferenceArrayLabel = {
    new ReferenceArrayLabel(className)
  }

  def createExceptionCallLabel(): ExceptionCallLabel = {
    new ExceptionCallLabel()
  }
}

class Label(counter: Long) {
  override def toString: String = s"L$counter"
}

class StaticFieldLabel(className: String, fieldName: String) extends Label(Label.getCounter) {
  override def toString: String = s"${className}#Static#Field#$fieldName"
}

class StaticMethodLabel(className: String, methodName: String, params: Seq[Type]) extends Label(Label.getCounter) {
  override def toString: String = s"${className}#Static#Method#${methodName}#${params.mkString("#")}"
}

class ClassFieldLabel(className: String, fieldName: String) extends Label(Label.getCounter) {
  override def toString: String = s"Class#${className}#Field#$fieldName"
}

class ClassMethodLabel(className: String, methodName: String, params: Seq[Type]) extends Label(Label.getCounter) {
  override def toString: String = s"Class#${className}#Method#${methodName}#${params.mkString("#")}"
}

//  VTable Base Label
class VtableLabel(className: String) extends Label(Label.getCounter) {
  override def toString: String = s"vtable#${className}"
}

class VtableSITLabel(className: String) extends Label(Label.getCounter){
  override def toString: String = s"vtable#SIT#${className}"
}

class InterfaceLabel(interfaceName: String) extends Label(Label.getCounter){
  override def toString: String = s"Interface#${interfaceName}"
}

class InterfaceMethodLabel(interfaceName: String, methodName: String, params: Seq[Type]) extends Label(Label.getCounter){
  override def toString: String = s"Interface#${interfaceName}#Method#${methodName}#${params.mkString("#")}"
}

class SubTypeTableLabel(className: String) extends Label(Label.getCounter){
  override def toString: String = s"vtable#${className}#SubTypeTable"
}

class SubTypeTableEntryLabel(className: String, entryName: String) extends Label(Label.getCounter){
  override def toString: String = s"vtable#${className}#SubTypeTable#Entry#${entryName}"
}

class ConstructorLabel(className: String, params: Seq[Type]) extends Label(Label.getCounter){
  override def toString: String = s"Class#${className}#Constructor#${params.mkString("#")}"
}

class PrimitiveArrayLabel() extends Label(Label.getCounter){
  override def toString: String = "vtable#Primitive#Array"
}

class ReferenceArrayLabel(className: String) extends Label(Label.getCounter){
  override def toString: String = s"vtable#${className}#Array"
}

class ExceptionCallLabel() extends Label(Label.getCounter){
  override def toString: String = "__exception"
}