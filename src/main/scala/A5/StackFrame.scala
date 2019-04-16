package A5

import A2.Asts.Name


/**
  * StackFrame defines a stack frame
  */
class StackFrame {
  var stackVariables: Seq[Name] = Seq.empty
  var stackSize: Int = 0
  // The below methods are problematic, consider the case, which will cause the program to fail
  /*
  {
  int a
  }
  {
  int a
  }
   */

  def addVariable(variable: Name): Unit = {
    stackSize += 1
    stackVariables = stackVariables :+ variable
  }
  def variableIndex(variable: Name): Int = {
    stackVariables.indexOf(variable)
  }
  def thisIndex(): Int = {
    stackVariables.size
  }
}