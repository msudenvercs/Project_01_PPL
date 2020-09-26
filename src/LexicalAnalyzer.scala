// import LexicalAnalyzer.{OPERATOR_PUNCTUATOR_TO_TOKEN, WORD_TO_TOKEN}
import scala.io.Source

/*
 * CS3210 - Principles of Programming Languages - Fall 2020
 * Instructor: Thyago Mota
 * Description: Prg01 - Lexical Analyzer
 * Dominick Licciardi:
 */

/*
'' denotes terminal symbol, [] optional, {} repetition
program =  ́program ́ identifier body  ́. ́
identifier = letter { ( letter | digit ) }
body = [ var_sct ] block
var_sct =  ́var ́ var_dcl {  ́; ́ var_dcl }
var_dcl = identifier { identifier }  ́: ́ type
type =  ́Integer ́ |  ́Boolean ́
block =  ́begin ́ stmt {  ́; ́ stmt } end
stmt = assgm_stmt | read_stmt | write_stmt | if_stmt | while_stmt | block
assgm_stmt = identifier  ́:= ́ expr
read_stmt =  ́read ́ identifier
write_stmt =  ́write ́ ( identifier | literal )
if_stmt =  ́if ́ bool_expr  ́then ́ stmt [  ́else ́ stmt ]
while_stmt =  ́while ́ bool_expr  ́do ́ stmt
expr = arithm_expr | bool_expr

// arithm_expr = arithm_expr (  ́+ ́ |  ́- ́ ) term | term
arithm_expr = term arithm_expr'
arithm_expr' = ( '+'| '-' ) term arithm_exp' | epsilon

// term = term  ́* ́ factor | factor
term = factor term'
term' = '*' factor term' | epsilon

factor = identifier | int_literal
literal = int_literal | bool_literal
int_literal = digit { digit }
bool_litreal =  ́true ́ |  ́false ́
bool_expr = bool_literal | arithm_expr (  ́> ́ |  ́>= ́ |  ́= ́ |  ́<= ́ |  ́< ́ ) arithm_expr
letter =  ́a ́ |  ́b ́ |  ́c ́ |  ́d ́ |  ́e ́ |  ́f ́ |  ́g ́ |  ́h ́ |  ́i ́ |  ́j ́ |  ́k ́ |  ́l ́ |  ́m ́ |
 ́n ́ |  ́o ́ |  ́p ́ |  ́q ́ |  ́r ́ |  ́s ́ |  ́t ́ |  ́u ́ |  ́v ́ |  ́w ́ |  ́x ́ |  ́y ́ |  ́z ́
digit =  ́0 ́ |  ́1 ́ |  ́2 ́ |  ́3 ́ |  ́4 ́ |  ́5 ́ |  ́6 ́ |  ́7 ́ |  ́8 ́ |  ́9 ́
 */

class LexicalAnalyzer(private var source: String) extends Iterable[LexemeUnit] {

  private var input = ""
  for (line <- Source.fromFile(source).getLines)
    input += line + "\n"

  // determines the class of a given character
  private def getCharClass(input_char: Char): CharClass.Value = {
    if (LexicalAnalyzer.LETTERS.contains(input_char))
      CharClass.LETTER
    else if (LexicalAnalyzer.DIGITS.contains(input_char))
      CharClass.DIGIT
    else if (LexicalAnalyzer.BLANKS.contains(input_char))
      CharClass.BLANK
    else if (input_char == '+' || input_char == '-' || input_char == '*' || input_char == '/')
      CharClass.OPERATOR
    else if (input_char == '>' || input_char == '<' || input_char == '=')
      CharClass.GTLTET
    else if (input_char == '.' || input_char == ',' || input_char == ';' || input_char == ':')
      CharClass.PUNCTUATOR
    else if (input_char == '(' || input_char == ')')
      CharClass.DELIMITER
    else
      CharClass.OTHER
  }

  // reads the input until a non-blank character is found, returning the input updated
  private def readBlanks: Unit = {
    var foundNonBlank = false
    while (input.length > 0 && !foundNonBlank) {
      val input_char = input(0)
      if (getCharClass(input_char) == CharClass.BLANK)
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
            var input_char = input(0)
            var charClass = getCharClass(input_char)

            // TODO: finish the code

            // TODO: recognize reserved words
            if (charClass == CharClass.LETTER) {
              lexeme += input_char
              input = input.substring(1) // consume the character at (0)
              var noMoreChars = false
              while (input.length() > 0 && !noMoreChars) {
                input_char = input(0)
                charClass = getCharClass(input_char)
                if (charClass == CharClass.LETTER || charClass == CharClass.DIGIT) {
                  lexeme += input_char
                  input = input.substring(1)
                }
                else
                  noMoreChars = true
              }
              lexeme match {
                case "program" => return new LexemeUnit(lexeme, Token.PROGRAM)
                case "read" => return new LexemeUnit(lexeme, Token.READ)
                case "while" => return new LexemeUnit(lexeme, Token.WHILE)
                case "do" => return new LexemeUnit(lexeme, Token.DO)
                case "if" => return new LexemeUnit(lexeme, Token.IF)
                case "else" => return new LexemeUnit(lexeme, Token.ELSE)
                case "write" => return new LexemeUnit(lexeme, Token.WRITE)
                case "end" => return new LexemeUnit(lexeme, Token.END)
                case "var" => return new LexemeUnit(lexeme, Token.VAR)
                case "Integer" => return new LexemeUnit(lexeme, Token.INTEGER)
                case "Boolean" => return new LexemeUnit(lexeme, Token.BOOLEAN)
                case "begin" => return new LexemeUnit(lexeme, Token.BEGIN)
                case "true" => return new LexemeUnit(lexeme, Token.BOOL_LITERAL)
                case "false" => return new LexemeUnit(lexeme, Token.BOOL_LITERAL)
                case "then" => return new LexemeUnit(lexeme, Token.THEN)
                case _ => return new LexemeUnit(lexeme, Token.IDENTIFIER)

              }
            }

            // TODO: recognize operators
            if (charClass == CharClass.OPERATOR) {
              input = input.substring(1)
              lexeme += input_char
              input_char match {
                case '+' => return new LexemeUnit(lexeme, Token.ADD_OP)
                case '-' => return new LexemeUnit(lexeme, Token.SUB_OP)
                case '*' => return new LexemeUnit(lexeme, Token.MULT)
                case '/' => return new LexemeUnit(lexeme, Token.DIV)
                case _ => return new LexemeUnit(lexeme, Token.IDENTIFIER)

              }
            }

            // TODO: recognize operators
            if (charClass == CharClass.GTLTET) {
              input = input.substring(1)
              lexeme += input_char
              var noMoreChars = false
              while (input.length() > 0 && !noMoreChars) {
                input_char = input(0)
                charClass = getCharClass(input_char)
                if (charClass == CharClass.GTLTET) {
                  lexeme += input_char
                  input = input.substring(1)

                }
                else
                  noMoreChars = true
              }
              input_char match {
                case '+' => return new LexemeUnit(lexeme, Token.ADD_OP)
                case '-' => return new LexemeUnit(lexeme, Token.SUB_OP)
                case '*' => return new LexemeUnit(lexeme, Token.MULT)
                case '/' => return new LexemeUnit(lexeme, Token.DIV)
                case _ => return new LexemeUnit(lexeme, Token.BOOLEAN)

              }
            }

            // TODO: recognize := as DEFINED and the PUNCTUATORS
            if (charClass == CharClass.PUNCTUATOR) {
              lexeme += input_char
              input = input.substring(1) // consume the character at (0)
              var noMoreChars = false
              while (input.length() > 0 && !noMoreChars) {
                input_char = input(0)
                charClass = getCharClass(input_char)
                  if (charClass == CharClass.GTLTET) {
                  lexeme += input_char
                  input = input.substring(1)
                    return new LexemeUnit(lexeme, Token.DEFINED)
                }
                else
                  noMoreChars = true
              }
              lexeme match {
                case "." => return new LexemeUnit(lexeme, Token.PERIOD)
                case ";" => return new LexemeUnit(lexeme, Token.SEMICOLON)
                case ":" => return new LexemeUnit(lexeme, Token.COLON)
                case "," => return new LexemeUnit(lexeme, Token.COMMA)
                case _ => return new LexemeUnit(lexeme, Token.IDENTIFIER)
              }
            }

            // TODO: recognize operators
            if (charClass == CharClass.DIGIT) {
              input = input.substring(1)
              lexeme += input_char
              input_char match {
                case '0' => return new LexemeUnit(lexeme, Token.IDENTIFIER)
                case '1' => return new LexemeUnit(lexeme, Token.IDENTIFIER)
              }
            }

            // throw an exception if an unrecognizable symbol is found
            throw new Exception("Lexical Analyzer Error: un-recognizable symbol found!")
          }
        }
      } // end next
    } // end 'new' iterator
  } // end iterator method
} // end LexicalAnalyzer class

object LexicalAnalyzer {
  val LETTERS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val DIGITS  = "0123456789"
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