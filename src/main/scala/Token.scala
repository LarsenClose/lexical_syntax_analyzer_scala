/*
 * CS3210 - Principles of Programming Languages - Fall 2020
 * Instructor: Thyago Mota
 * Description: Prg01 - Token
 * Student: Larsen Close
 */

// TODO: update this enumeration with the token possible values
object Token extends Enumeration {

// implied or constructed tokens
  val EOF             = Value
  val IDENTIFIER      = Value
  val INT_LITERAL     = Value

// there are no parens in this grammar
  // val OPEN_PAR        = Value
  // val CLOSE_PAR       = Value

// operator
  val ADD_OP          = Value
  val SUB_OP          = Value
  val MUL_OP          = Value
  val DIV_OP          = Value

// combinable operators
  val GREATER_THAN      = Value
  val LESS_THAN         = Value
  val EQUAL_TO          = Value


  val GREATER_EQUAL   = Value
  val LESS_EQUAL      = Value

  val COLON           = Value // combinable punctuator

// punctuators
  val PERIOD          = Value
  val COMMA           = Value
  val SEMI_COLON      = Value

// key word tokens
  val PROGRAM         = Value


  val BOOL_LITERAL    = Value

// regrouping for syntax perspective
  val WALRUS          = Value
  val READ_STMT       = Value
  val WRITE_STMT      = Value
  val IF_STMT         = Value
  val WHILE_STMT      = Value
  val BEGIN_STMT      = Value



  val END_STMT        = Value
  val DO_STMT         = Value

  val THEN_STMT       = Value
  val ELSE_STMT       = Value
  val VAR_STMT        = Value
  val TYPE_STMT       = Value
  val VAR             = Value

}
