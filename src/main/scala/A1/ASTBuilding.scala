package A1

import A1.Grammars.Tree

object ASTBuilding {

  def ASTBuild(root: Tree): Tree = {

    if(root.children.isEmpty){
      root
    } else if (root.children.length == 1){
      ASTBuild(root.children.head)
    } else {
      var new_children: Seq[Tree] = Seq.empty
      root.children.foreach(child => {
        new_children = new_children :+ ASTBuild(child)
      })
      new Tree(root.lhs,new_children)
    }
  }

  def ASTBuildJoos1w(root: Tree): Tree = {
    ASTBuild(root)
  }

}