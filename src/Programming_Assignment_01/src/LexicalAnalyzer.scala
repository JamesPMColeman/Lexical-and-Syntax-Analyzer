import scala.io.Source

/*
 * James Coleman
 * CS3210
 * Programming Assignment 01
 * September 26th, 2020
 *
 */

/***							 								***
 *** 															***
 *** Template from Dr Mota via https://github.com/thyagomota/	***
 *** 20FCS3210/tree/master/activity_05_lexical_analyzer/src 	***
 ***															***
 ***															***/


/* |||||||| Grammar |||||||| 

	program 	 = ´program´ identifier body ´.´ 
	identifier   = letter { ( letter | digit ) } 
	body 		 = [ var_sct ] block 
	var_sct 	 = ´var´ var_dcl { ´;´ var_dcl } 
	var_dcl 	 = identifier { identifier } ´:´ type 
	type 		 = ´Integer´ | ´Boolean´ 
	block 		 = ´begin´ stmt { ´;´ stmt } ´end´ 
	stmt 		 = assgm_stmt | read_stmt | write_stmt | if_stmt | while_stmt | block 
	assgm_stmt 	 = identifier ´:=´ expr 
	read_stmt 	 = ´read´ identifier 
	write_stmt   = ´write´ ( identifier | literal ) 
	if_stmt 	 = ´if´ bool_expr ´then´ stmt [ ´else´ stmt ] 
	while_stmt   = ´while´ bool_expr ´do´ stmt expr = arithm_expr | bool_expr 
	arithm_expr  = arithm_expr ( ´+´ | ´-´ ) term | term 
	term 		 = term ´*´ factor | factor 
	factor 		 = identifier | int_literal 
	literal 	 = int_literal | bool_literal 
	int_literal  = digit { digit } 
	bool_litreal = ´true´ | ´false´ 
	bool_expr 	 = bool_literal 
				 | arithm_expr ( ´>´ | ´>=´ | ´=´ | ´<=´ | ´<´ ) arithm_expr 
	letter 		 = ´a´ | ´b´ | ´c´ | ´d´ | ´e´ | ´f´ | ´g´ | ´h´ | ´i´ | ´j´ | ´k´ | ´l´ 
				 | ´m´ | ´n´ | ´o´ | ´p´ | ´q´ | ´r´ | ´s´ | ´t´ | ´u´ | ´v´ | ´w´ | ´x´ 
				 | ´y´ | ´z´ | also upper case letters 
	digit 		 = ´0´ | ´1´ | ´2´ | ´3´ | ´4´ | ´5´ | ´6´ | ´7´ | ´8´ | ´9´
 
*/

class LexicalAnalyzer(private var source: String) extends Iterable[LexemeUnit] {

	private var input = ""
  	for (line <- Source.fromFile(source).getLines) {
   		input += line + "\n"
	}

  	// determines the class of a given character
  	private def getCharClass(c: Char): CharClass.Value = {
    	if (LexicalAnalyzer.LETTERS.contains(c))
      		CharClass.LETTER
    	else if (LexicalAnalyzer.DIGITS.contains(c))
			CharClass.DIGIT
		else if (LexicalAnalyzer.SYMBOLS.contains(c))
			CharClass.SYMBOL
    	else if (LexicalAnalyzer.BLANKS.contains(c))
      		CharClass.BLANK
    	else
      		CharClass.OTHER
  	}	

  	// reads the input until a non-blank character is found, returning the input updated
  	private def readBlanks: Unit = {
    	var foundNonBlank = false
    	while (input.length > 0 && !foundNonBlank) {
      		val c = input(0)
      		if (getCharClass(c) == CharClass.BLANK)
        		input = input.substring(1)
      		else
        		foundNonBlank = true
	   	}
  	}

	def iterator: Iterator[LexemeUnit] = {
		new Iterator[LexemeUnit] {

			override def hasNext: Boolean = {
				readBlanks
				input.length > 0
			}

	      	override def next(): LexemeUnit = {
    	    	if (!hasNext)
        	  		new LexemeUnit("", SyntaxAnalyzer.TOKEN_EOF)
        		else {
          			var lexeme = ""
	          		readBlanks
    	      		if (input.length == 0)
        	    		new LexemeUnit(lexeme, SyntaxAnalyzer.TOKEN_EOF)
          			else {
            			var c = input(0)
            			var charClass = getCharClass(c)
	
            // Recognize special words and identifiers
						if (charClass == CharClass.LETTER) {
							lexeme += c
							input = input.substring(1)
							var lettersDigitsLeft = true
							while (input.length > 0 && lettersDigitsLeft) {
								c = input(0)
								charClass = getCharClass(c)
								if (charClass == CharClass.LETTER ||
									charClass == CharClass.DIGIT) {
									lexeme += c
									input = input.substring(1)
								}		
								else lettersDigitsLeft = false
							}							
							lexeme match {
								case "program"  => return new LexemeUnit(lexeme, SyntaxAnalyzer.TOKEN_PROGRAM)
								case "var"      => return new LexemeUnit(lexeme, SyntaxAnalyzer.TOKEN_VAR)
								case "begin"    => return new LexemeUnit(lexeme, SyntaxAnalyzer.TOKEN_BEGIN)
								case "read"     => return new LexemeUnit(lexeme, SyntaxAnalyzer.TOKEN_READ) 
								case "write" 	=> return new LexemeUnit(lexeme, SyntaxAnalyzer.TOKEN_WRITE)
								case "if"   	=> return new LexemeUnit(lexeme, SyntaxAnalyzer.TOKEN_IF)
								case "then"   	=> return new LexemeUnit(lexeme, SyntaxAnalyzer.TOKEN_THEN)
								case "else"   	=> return new LexemeUnit(lexeme, SyntaxAnalyzer.TOKEN_ELSE)
								case "while"  	=> return new LexemeUnit(lexeme, SyntaxAnalyzer.TOKEN_WHILE)
								case "do"  		=> return new LexemeUnit(lexeme, SyntaxAnalyzer.TOKEN_DO)
								case "true"  	=> return new LexemeUnit(lexeme, SyntaxAnalyzer.TOKEN_BOOLEAN)
								case "false"	=> return new LexemeUnit(lexeme, SyntaxAnalyzer.TOKEN_BOOLEAN)
								case "Integer" 	=> return new LexemeUnit(lexeme, SyntaxAnalyzer.TOKEN_TYPE)
								case "Boolean" 	=> return new LexemeUnit(lexeme, SyntaxAnalyzer.TOKEN_TYPE)
								case "end"	 	=> return new LexemeUnit(lexeme, SyntaxAnalyzer.TOKEN_END)
								case default    => return new LexemeUnit(lexeme, SyntaxAnalyzer.TOKEN_IDENTIFIER)
							}
							println("Statement Reached")
						}
						else if (charClass == CharClass.SYMBOL) {
							lexeme += c
							input = input.substring(1)
							var symbolsLeft = true
							while (input.length > 0 && symbolsLeft) {
								c = input(0)
								charClass = getCharClass(c)
								if (charClass == CharClass.SYMBOL) {
									lexeme += c
									input = input.substring(1)
								}
								else symbolsLeft = false
							}
							lexeme match {
								case ":" => return new LexemeUnit(lexeme, SyntaxAnalyzer.TOKEN_COLON)
								case ";" => return new LexemeUnit(lexeme, SyntaxAnalyzer.TOKEN_SEMI_COLON)
								case "." => return new LexemeUnit(lexeme, SyntaxAnalyzer.TOKEN_PERIOD)
								case "+" => return new LexemeUnit(lexeme, SyntaxAnalyzer.TOKEN_PLUS)
								case "-" => return new LexemeUnit(lexeme, SyntaxAnalyzer.TOKEN_MINUS)
								case "*" => return new LexemeUnit(lexeme, SyntaxAnalyzer.TOKEN_MULTIPLIER)
								case ">" => return new LexemeUnit(lexeme, SyntaxAnalyzer.TOKEN_GREATER_THAN)
								case "<" => return new LexemeUnit(lexeme, SyntaxAnalyzer.TOKEN_LESS_THAN)
								case "=" => return new LexemeUnit(lexeme, SyntaxAnalyzer.TOKEN_EQUAL)
								case "<=" => return new LexemeUnit(lexeme, SyntaxAnalyzer.TOKEN_LESS_EQUAL)
								case ">=" => return new LexemeUnit(lexeme, SyntaxAnalyzer.TOKEN_GREATER_EQUAL)
								case ":=" => return new LexemeUnit(lexeme, SyntaxAnalyzer.TOKEN_ASSIGN)
							} 
						}
						else if (charClass == CharClass.DIGIT) {
							lexeme += c
							input = input.substring(1)
							var digitsLeft = true
							while (input.length > 0 && digitsLeft) {
								c = input(0)
								charClass = getCharClass(c)
								if (charClass == CharClass.DIGIT) {
									lexeme += c
									input = input.substring(1)
								}
								else digitsLeft = false
							}
							return new LexemeUnit(lexeme, SyntaxAnalyzer.TOKEN_INT_LITERAL)
						}	
					}
			// throw an exception if an unrecognizable symbol is found
					throw new Exception("Lexical Analyzer Error: unrecognizable symbol found!")	
				}
	  		} // end next
		} // end 'new' iterator
	} // end iterator method
} // end LexicalAnalyzer class

object LexicalAnalyzer {
  	val LETTERS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
	val DIGITS = "0123456789"
	val SYMBOLS = ":;.=+-*><"
  	val BLANKS  = " \n\t"

  	def main(args: Array[String]): Unit = {
    // check if source file was passed through the command-line
    	if (args.length != 1) {
      	print("Missing source file!")
      	System.exit(1)
    }

    val lex = new LexicalAnalyzer(args(0))
    val it = lex.iterator
    while (it.hasNext) {
      	val lexemeUnit = it.next()
      	println(lexemeUnit)
    }
  } // end main method
} // end LexicalAnalyzer object
