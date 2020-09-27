/*
 * CS3210 - Principles of Programming Languages - Fall 2020
 * Instructor: Thyago Mota
 * Description: Activity 09 - Tree
 */

import scala.collection.mutable.ArrayBuffer

class Tree(var label: String) {

  	private val branches: ArrayBuffer[Tree] = new ArrayBuffer[Tree]

  	def add(branch: Tree): Unit = branches.append(branch)

  	private def print(current: Tree, tabs: String): String = {
    	var out = ""
    	if (current == null)
      		out
    	else {
      		out += tabs + current.label + "\n"
      		for (branch <- current.branches)
        		out += print(branch, tabs + "    ")
      		out
    	}
  	}

  	override def toString = print(this, "")
}

