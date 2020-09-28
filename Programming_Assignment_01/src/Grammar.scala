import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/*
 * James Coleman
 * CS3210
 * Programming Assignment 01 Grammar
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

class Grammar(private var source: String) {

 	private var productions = new ArrayBuffer[String]
  	for (line <- Source.fromFile(source).getLines)
    	productions += line

  	def getLHS(index: Integer): String = {
    	val production = productions(index)
    	production.split("->")(0).strip()
  	}

  	def getRHS(index: Integer) = {
    	val production = productions(index)
    	production.split("->")(1).strip().split(" ")
  	}

  	override def toString: String = {
    	var out = ""
    	for (i <- 0 until productions.length)
      		out += i + ". " + getLHS(i) + " -> " + getRHS(i).mkString(" ") + "\n"
    	out
  	}
}
