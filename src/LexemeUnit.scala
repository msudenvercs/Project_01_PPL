/*
 * CS3210 - Principles of Programming Languages - Fall 2020
 * Instructor: Thyago Mota
 * Description: Prg01 - LexemeUnit
 * Dominick Licciardi
 */

class LexemeUnit(private var lexeme: String, private var token: Token.Value) {

  def getLexeme() = lexeme

  def getToken() = token

  override def toString: String = "(" + lexeme + "," + token + ")"
}
