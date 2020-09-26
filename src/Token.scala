/*
 * CS3210 - Principles of Programming Languages - Fall 2020
 * Instructor: Thyago Mota
 * Description: Prg01 - Token
 * Dominick Licciardi
 */

// TODO: update this enumeration with the token possible values
object Token extends Enumeration {
  val EOF = Value
  val PROGRAM = Value
  val IDENTIFIER = Value
  val VAR = Value
  val BEGIN = Value
  val READ = Value
  val WRITE = Value
  val END = Value
  val ADD_OP = Value
  val SUB_OP = Value
  val MULT = Value
  val DIV = Value
  val INTEGER = Value
  val BOOLEAN = Value
  val IF = Value
  val THEN = Value
  val ELSE = Value
  val WHILE = Value
  val DO = Value
  val INT_LITERAL = Value
  val BOOL_LITERAL = Value
  val LETTER = Value
  val COLON = Value
  val SEMICOLON = Value
  val PERIOD = Value
  val DEFINED = Value
  val COMMA = Value
  val TRUE = Value
  val FALSE = Value

}