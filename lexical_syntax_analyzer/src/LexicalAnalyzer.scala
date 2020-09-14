import scala.io.Source

/*
 * James Coleman
 * CS3210
 * Homework3 Lexecal Analyze class and Main class
 * September 6th, 2020
 */

/*** 								***
 *** 								***
 *** Template from Dr Mota via https://github.com/thyagomota/	***
 *** 20FCS3210/tree/master/activity_05_lexical_analyzer/src 	***
 ***								***
 ***								***/


/*

	stmt	   = `declare` identifier { option }
	identifier = `$` letter { letter }
	letter 	   = `a` | ... | `z` | `A` | ... | `Z`
	option 	   = mode | scale | precision | base
	mode       = `real` | `complex`
	scale	   = `fixed` | `floating`
	precision  = `single` | `double`
	base 	   = `binary` | `decimal`

 */

class LexicalAnalyzer(private var source: String) extends Iterable[LexemeUnit] {

  private var input = ""
  for (line <- Source.fromFile(source).getLines)
    input += line + "\n"

  // determines the class of a given character
  private def getCharClass(c: Char): CharClass.Value = {
    if (LexicalAnalyzer.LETTERS.contains(c))
      CharClass.LETTER
    else if (LexicalAnalyzer.DOLLARS.contains(c))
      CharClass.DOLLAR
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
          new LexemeUnit("", Token.EOF)
        else {
          var lexeme = ""
          readBlanks
          if (input.length == 0)
            new LexemeUnit(lexeme, Token.EOF)
          else {
            var c = input(0)
            var charClass = getCharClass(c)

            // Recognize a dollar sign followed by at least 
	    // one letter as an identifier
            if (charClass == CharClass.DOLLAR) {
              lexeme += c
              input = input.substring(1)
	      c = input(0)
	      charClass = getCharClass(c)
	      if (charClass == CharClass.LETTER) {
		lexeme += c
		input = input.substring(1)
		var lettersLeft = true
 		while (input.length > 0 && lettersLeft) {
	          c = input(0)
	          charClass = getCharClass(c)
		  if (charClass == CharClass.LETTER) {
                    lexeme += c
                    input = input.substring(1)
                  }
		  else lettersLeft = false
		}
                return new LexemeUnit(lexeme, Token.IDENTIFIER)
              }
	    }
            // Recognize special words
            if (charClass == CharClass.LETTER) {
              lexeme += c
              input = input.substring(1)
	      var lettersLeft = true
	      while (input.length > 0 && lettersLeft) {
		c = input(0)
		charClass = getCharClass(c)
		if (charClass == CharClass.LETTER) {
		  lexeme += input(0)
		  input = input.substring(1)
		}
		else lettersLeft = false
	      }
	      lexeme match {
                case "declare"  => return new LexemeUnit(lexeme, Token.DECLARE)
                case "real"     => return new LexemeUnit(lexeme, Token.REAL)
                case "complex"  => return new LexemeUnit(lexeme, Token.COMPLEX)
                case "fixed"    => return new LexemeUnit(lexeme, Token.FIXED) 
                case "floating" => return new LexemeUnit(lexeme, Token.FLOATING)
                case "single"   => return new LexemeUnit(lexeme, Token.SINGLE)
                case "double"   => return new LexemeUnit(lexeme, Token.DOUBLE)
                case "binary"   => return new LexemeUnit(lexeme, Token.BINARY)
                case "decimal"  => return new LexemeUnit(lexeme, Token.DECIMAL)
	      	case lexeme     =>  throw new Exception(
		  "Lexical Analyzer Error: unrecognizable symbol '" + 
		  lexeme + "' found!"
                )
	      }
            }
            // throw an exception if an unrecognizable symbol is found
            throw new Exception("Lexical Analyzer Error: unrecognizable symbol found!")
          }
        }
      } // end next
    } // end 'new' iterator
  } // end iterator method
} // end LexicalAnalyzer class

object LexicalAnalyzer {
  val LETTERS = "abcdefghijklmnopqrstuvwxyz"
  val DOLLARS = "$"
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
