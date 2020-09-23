/*
 * CS3210 - Principles of Programming Languages - Fall 2020
 * Instructor: Thyago Mota
 * Description: Prg01 - Token
 * Student: Larsen Close
 */

// TODO: update this enumeration with the token possible values
object Token extends Enumeration {
  val EOF = Value
  val IDENTIFIER = Value
  val INT_LITERAL = Value
  val VAR_STMT = Value
  val ADD_OP = Value
  val SUB_OP = Value
  val MUL_OP = Value
  val DIV_OP = Value
  val COMPARISON = Value
  val COLON = Value
  val PERIOD = Value
  val COMMA = Value
  val SEMI_COLON = Value
  val PROGRAM = Value
  val BOOL_LITERAL = Value
  val WALRUS = Value
  val READ_STMT = Value
  val WRITE_STMT = Value
  val IF_STMT = Value
  val WHILE_STMT = Value
  val BEGIN_STMT = Value
  val END_STMT = Value
  val DO_STMT = Value
  val THEN_STMT = Value
  val ELSE_STMT = Value
  val TYPE_STMT = Value
}
