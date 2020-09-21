/*
 * James Coleman
 * CS3210
 * Homework3 Character class
 * September 6th 2020
 */

 /***                                                            ***
  *** Template from Dr Mota via https://github.com/thyagomota/   ***
  *** 20FCS3210/tree/master/activity_05_lexical_analyzer/src     ***  
  ***                                                            ***/

object CharClass extends Enumeration {
  	val LETTER      = Value
    val DIGIT		= Value
	val PUNCTUATION = Value
  	val OPERATORS   = Value
  	val BLANK       = Value
  	val OTHER       = Value
}
