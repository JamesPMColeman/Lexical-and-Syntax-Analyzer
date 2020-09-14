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
  val DECLARE    = Value
  val IDENTIFIER = Value
  val REAL       = Value
  val COMPLEX    = Value
  val FIXED      = Value
  val FLOATING   = Value
  val SINGLE     = Value
  val DOUBLE     = Value
  val BINARY     = Value
  val DECIMAL    = Value
  val EOF	 = Value
}
