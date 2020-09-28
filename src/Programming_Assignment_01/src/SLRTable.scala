import scala.io.Source

/*
 * James Coleman
 * CS3210
 * Programming Assignment 01 SLRTable
 * September 26th, 2020
 *
 */

/***                                                            ***
 ***                                                            ***
 *** Template from Dr Mota via https://github.com/thyagomota/   ***
 *** 20FCS3210/tree/master/activity_05_lexical_analyzer/src     ***
 ***                                                            ***
 ***                                                            ***/

/* |||||||| Grammar |||||||| 

    program      = ´program´ identifier body ´.´ 
    identifier   = letter { ( letter | digit ) } 
    body         = [ var_sct ] block 
    var_sct      = ´var´ var_dcl { ´;´ var_dcl } 
    var_dcl      = identifier { identifier } ´:´ type 
    type         = ´Integer´ | ´Boolean´ 
    block        = ´begin´ stmt { ´;´ stmt } ´end´ 
    stmt         = assgm_stmt | read_stmt | write_stmt | if_stmt | while_stmt | block 
    assgm_stmt   = identifier ´:=´ expr 
    read_stmt    = ´read´ identifier 
    write_stmt   = ´write´ ( identifier | literal ) 
    if_stmt      = ´if´ bool_expr ´then´ stmt [ ´else´ stmt ] 
    while_stmt   = ´while´ bool_expr ´do´ stmt expr = arithm_expr | bool_expr 
    arithm_expr  = arithm_expr ( ´+´ | ´-´ ) term | term 
    term         = term ´*´ factor | factor 
    factor       = identifier | int_literal 
    literal      = int_literal | bool_literal 
    int_literal  = digit { digit } 
    bool_litreal = ´true´ | ´false´ 
    bool_expr    = bool_literal 
                 | arithm_expr ( ´>´ | ´>=´ | ´=´ | ´<=´ | ´<´ ) arithm_expr 
    letter       = ´a´ | ´b´ | ´c´ | ´d´ | ´e´ | ´f´ | ´g´ | ´h´ | ´i´ | ´j´ | ´k´ | ´l´ 
                 | ´m´ | ´n´ | ´o´ | ´p´ | ´q´ | ´r´ | ´s´ | ´t´ | ´u´ | ´v´ | ´w´ | ´x´ 
                 | ´y´ | ´z´ | also upper case letters 
    digit        = ´0´ | ´1´ | ´2´ | ´3´ | ´4´ | ´5´ | ´6´ | ´7´ | ´8´ | ´9´
 
*/

class SLRTable(private var source: String) {

	private val actions: scala.collection.mutable.Map[(Int, Int), String] =  scala.collection.mutable.Map()
  	private val gotos: scala.collection.mutable.Map[(Int, String), String] =  scala.collection.mutable.Map()

  	val input = Source.fromFile(source).getLines()
  	val header = input.next().split(",")
	val eof = header.indexOf("0")
	val tokens = for (i <- 1 to eof) yield header(i).toInt
	val variables = for (i <- eof + 1 until header.length) yield header(i)
	if (SyntaxAnalyzer.DEBUG) {
		println("Input: " + input)
  		println("Header: " + header)
  		println("End of File" + eof)
		println("Tokens: " + tokens)
  		println("Variables: " + variables)
	}
	while (input.hasNext) {
    	val line = input.next() + " "
    	val row = line.split(",")
    	val state = row(0).toInt
    	for (i <- 0 until tokens.length) {
      		val token = tokens(i)
      		val key = (state, token)
      		val value = row(i + 1)
      		actions(key) = value
    	}
    	for (i <- 0 until variables.length) {
      		val variable = variables(i)
      		val key = (state, variable)
      		val value = row(tokens.length + i + 1) 
      		gotos(key) = value
    	}
  	}

  	def getAction(state: Int, token: Int) = actions((state, token))

  	def getGoto(state: Int, variable: String) = gotos((state, variable))

  	override def toString: String = {
    	var out = "actions:\n"
    	for ((key, value) <- actions)
      		out += key + " -> " + value + "\n"
    	out += "gotos:\n"
    	for ((key, value) <- gotos)
      		out += key + " -> " + value + "\n"
    	out
  	}
}
