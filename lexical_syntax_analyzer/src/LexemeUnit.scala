/*
 * James Coleman
 * CS3210
 * Homework3 Lexem Unit class
 * September 6th, 2020
 */

 /***                                                            ***
  *** Template from Dr Mota via https://github.com/thyagomota/   ***
  *** 20FCS3210/tree/master/activity_05_lexical_analyzer/src     ***  
  ***                                                            ***/

class LexemeUnit(private var lexeme: String, private var token: Token.Value) {

  def getLexeme() = lexeme

  def getToken() = token

  override def toString: String = "(" + lexeme + "," + token + ")"
}
