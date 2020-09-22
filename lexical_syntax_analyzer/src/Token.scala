/*
 * James Coleman
 * CS3210
 * Homework 3 Token Object
 * September 6th, 2020
 */

 /***                                                            ***
  *** Template from Dr Mota via https://github.com/thyagomota/   ***
  *** 20FCS3210/tree/master/activity_05_lexical_analyzer/src     ***  
  ***                                                            ***/

object Token extends Enumeration {
  	val PROGRAM    		= Value
  	val IDENTIFIER 		= Value
	val INT_LITERAL		= Value
  	val VAR        		= Value
  	val TYPE	   		= Value
  	val BOOLEAN		    = Value
  	val BEGIN      		= Value
  	val READ       		= Value
  	val WRITE      		= Value
  	val IF         		= Value
   	val THEN       		= Value
  	val ELSE	   		= Value
	val WHILE      		= Value
  	val DO         		= Value
  	val COLON      		= Value
  	val SEMI_COLON 		= Value
	val ASSIGN     		= Value
  	val EQUAL      		= Value
  	val PLUS       		= Value
  	val MINUS      		= Value
  	val MULTIPLIER  	= Value
  	val GREATER_THAN    = Value
    val LESS_THAN		= Value
  	val GREATER_EQUAL  	= Value
	val LESS_EQUAL		= Value
  	val END		   		= Value
	val EOF		   		= Value
}
