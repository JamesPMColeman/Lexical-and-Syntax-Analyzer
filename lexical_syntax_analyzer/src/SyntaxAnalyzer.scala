import SyntaxAnalyzer.{GRAMMAR_FILENAME, SLR_TABLE_FILENAME}
import Token.Value

import scala.collection.mutable.ArrayBuffer

/*
 * CS3210 - Principles of Programming Languages - Fall 2020
 * Instructor: Thyago Mota
 * Description: Activity 09 - Syntax Analyzer
 */

/*
expression  = term expression'
expression' = ( ´+´  | ´-´ ) term expression' | epsilon
term        = factor term'
term'       = ( ´*´ | ´/´ ) factor term' | epsilon
factor      = identifier | literal | ´(´ expression ´)´
identifier  = letter { ( letter | digit ) }
letter      = ´a´ | ´b´ | ´c´ | ´d´ | ´e´ | ´f´ | ´g´ | ´h´ | ´i´ | ´j´ | ´k´ | ´l´ | ´m´
| ´n´ | ´o´ | ´p´ | ´q´ | ´r´ | ´s´ | ´t´ | ´u´ | ´v´ | ´w´ | ´x´ | ´y´ | ´z´
literal     = digit { digit }
digit       = ´0´ | ´1´ | ´2´ | ´3´ | ´4´ | ´5´ | ´6´ | ´7´ | ´8´ | ´9´
 */

class SyntaxAnalyzer(private var source: String) {

  	private val it = new LexicalAnalyzer(source).iterator
  	private var lexemeUnit: LexemeUnit = null
  	private val grammar = new Grammar(GRAMMAR_FILENAME)
  	private val slrTable = new SLRTable(SLR_TABLE_FILENAME)

  	private def getLexemeUnit() = {
    	if (lexemeUnit == null)
      		lexemeUnit = it.next()
    	if (SyntaxAnalyzer.DEBUG)
      		println("lexemeUnit: " + lexemeUnit)
  	}

  	def parse(): Tree = {

    // create a stack of trees
    	val trees: ArrayBuffer[Tree] = new ArrayBuffer[Tree]

    // initialize the parser's stack of (state, symbol) pairs
    	val stack: ArrayBuffer[String] = new ArrayBuffer[String]
    	stack.append("0")

    // main parser loop
    	while (true) {

      		if (SyntaxAnalyzer.DEBUG)
        		println("stack: " + stack.mkString(","))

      // update lexeme unit (if needed)
      		getLexemeUnit()

      // get current state
      		var state = stack.last.strip().toInt
      		if (SyntaxAnalyzer.DEBUG)
        		println("state: " + state)

      // get current token
      		val token = lexemeUnit.getToken()
				if (SyntaxAnalyzer.DEBUG)
					println("Token: " + token)

      // get action
      		val action = slrTable.getAction(state, token)
      			if (SyntaxAnalyzer.DEBUG)
        			println("action: " + action)

      // if action is undefined, throw an exception
      		if (action.length == 0)
        		throw new Exception("Syntax Analyzer Error!")

      // implement the "shift" operation if the action's prefix is "s"
      		if (action(0) == 's') {

        // update the parser's stack
        		stack.append(token + "")
        		stack.append(action.substring(1))

        // create a new tree with the lexeme
				val tree = if (lexemeUnit.getToken() == SyntaxAnalyzer.TOKEN_IDENTIFIER) 
					new Tree("identifier: " + lexemeUnit.getLexeme()) else
        			new Tree(lexemeUnit.getLexeme())
        // push the new tree onto the stack of trees
		// handle Integer and Boolean being leaves of type
				val typeTree = new Tree("type")
				if (tree.label == "Integer" || tree.label == "Boolean") {
					typeTree.add(tree)
					trees.append(typeTree)
				}
				else
        			trees.append(tree)

        // update lexemeUnit to null to acknowledge reading the input
        		lexemeUnit = null
      		}
      // implement the "reduce" operation if the action's prefix is "r"
      		else if (action(0) == 'r') {
      			if (SyntaxAnalyzer.REDUCE)
        			println("R action: " + action)

        // get the production to use
        		val index = action.substring(1).toInt
        		val lhs = grammar.getLHS(index)
        		val rhs = grammar.getRHS(index)
      			if (SyntaxAnalyzer.REDUCE)
        			println("Index: " + index + "\nLHS: " + lhs + "\nRHS: " + rhs(0) +
							"\nRHS Length: " + rhs.length)

        // update the parser's stack
				if (rhs(0) != "#") {
					stack.trimEnd(rhs.length * 2)
					state = stack.last.strip().toInt
      				if (SyntaxAnalyzer.REDUCE)
        				println("State: " + state + "\nStack: " + stack.mkString(","))
				}
				stack.append(lhs)
      			if (SyntaxAnalyzer.REDUCE)
        			println("State: " + state + "\nStack: " + stack.mkString(","))
				stack.append(slrTable.getGoto(state, lhs))
      			if (SyntaxAnalyzer.REDUCE)
        			println("State: " + state + "\nStack: " + stack.mkString(","))

        // create a new tree with the "lhs" variable as its label
				if (rhs(0) != "#" && lhs != "ID" && lhs != "VD’" && lhs != "S’") {
					val newTree = new Tree(lhs)
					if (SyntaxAnalyzer.TREES)
						println("Entering New Reduction >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" +
								"\nTrees_list: " + trees + "\n======================" + "\nTrees Length: " +
								trees.length + "\nRHS length: " + rhs.length)
					var snip = 0
					if (lhs == "B") {
						for (tree <- trees)
							if (tree.label == "VS")
								snip += 1
						snip += 1
					}	
					else if (lhs == "BL") {
						for (tree <- trees)
							if (tree.label == "S")
								snip += 2
						snip += 1 
					}
					else if (lhs == "ID") {
						for (tree <- trees) 
							if (tree.label.contains("id"))
								snip += 1
						snip -= 1
					}
					else if (lhs == "VD") {
						for (tree <- trees) 
							if (tree.label.contains("id") || tree.label.contains("ty"))
								snip += 1
					}
					else if (lhs == "VS") {
						for (tree <- trees) 
							if (tree.label.contains("VD"))
								snip += 2
					}
					else 
						snip = rhs.length		
				// add "rhs.length" trees from the right-side of "trees" as children of "newTree"
					for (tree <- trees.drop(trees.length - snip)) {
						println("Tree node: " + tree.label)
						newTree.add(tree)
					}
					if (true)
						println("Trees_list: " + trees + "\n======================")
				//	if (SyntaxAnalyzer.TREES)
				//		println(newTree + "\n======================")

				// drop "rhs.length" trees from the right-side of "trees"
					trees.trimEnd(snip)
			//		if (SyntaxAnalyzer.TREES)
			//			println(newTree + "\n======================")
					if (SyntaxAnalyzer.TREES)
						println("Trees_list: " + trees + "\n======================")
				// append "newTree" to the list of "trees" 
					trees.append(newTree)
					if (SyntaxAnalyzer.TREES)
						println("Trees_list: " + trees + "\n======================")
      			}
			}
      // implement the "accept" operation
      		else if (action.equals("acc")) {

        // create a new tree with the "lhs" of the first production ("start symbol")
       /* 		val newTree = new Tree(grammar.getLHS(0))

        // add all trees as children of "newTree"
        		for (tree <- trees)
          			newTree.add(tree)
		*/
        // return "newTree"
        		return trees(0) 
			}
      		else
        		throw new Exception("Syntax Analyzer Error!")
    	}
    	throw new Exception("Syntax Analyzer Error!")
  	}
}

object SyntaxAnalyzer {
    
  	val GRAMMAR_FILENAME   = "../grammar.txt"
  	val SLR_TABLE_FILENAME = "../parse_table.csv"
	
	val TOKEN_PROGRAM         = 1
    val TOKEN_IDENTIFIER      = 2
    val TOKEN_INT_LITERAL     = 3
    val TOKEN_VAR             = 4
    val TOKEN_TYPE            = 5
    val TOKEN_BOOLEAN         = 6
    val TOKEN_BEGIN           = 7
    val TOKEN_READ            = 8
    val TOKEN_WRITE           = 9
    val TOKEN_IF              = 10
    val TOKEN_THEN            = 11
    val TOKEN_ELSE            = 12
    val TOKEN_WHILE           = 13
    val TOKEN_DO              = 14
    val TOKEN_COLON           = 15
    val TOKEN_SEMI_COLON      = 16
    val TOKEN_ASSIGN          = 17
    val TOKEN_EQUAL           = 18
    val TOKEN_PLUS            = 19
    val TOKEN_MINUS           = 20
    val TOKEN_MULTIPLIER      = 21
    val TOKEN_GREATER_THAN    = 22
    val TOKEN_LESS_THAN       = 23
    val TOKEN_GREATER_EQUAL   = 24
    val TOKEN_LESS_EQUAL      = 25
    val TOKEN_END             = 26
	val TOKEN_PERIOD		  = 27
    val TOKEN_EOF             = 0

  	val DEBUG  = false
	val REDUCE = false
	val TREES  = true

  	def main(args: Array[String]): Unit = {
    // check if source file was passed through the command-line
    	if (args.length != 1) {
      		print("Missing source file!")
      		System.exit(1)
    	}

    	val syntaxAnalyzer = new SyntaxAnalyzer(args(0))
    	val parseTree = syntaxAnalyzer.parse()
    	print(parseTree)
  	}	
}
