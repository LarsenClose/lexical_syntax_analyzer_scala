import LexicalAnalyzer.{OPERATOR_PUNCTUATOR_TO_TOKEN, WORD_TO_TOKEN}

import scala.io.Source

/*
 * CS3210 - Principles of Programming Languages - Fall 2020
 * Instructor: Thyago Mota
 * Description: Prg01 - Lexical Analyzer
 * Student: Larsen Close
 */

class LexicalAnalyzer(private var source: String) extends Iterable[LexemeUnit] {

  private var input = ""
  for (line <- Source.fromFile(source).getLines)
    input += line + "\n"

  // determines the class of a given character
  private def getCharClass(c: Char): CharClass.Value = {
    if (LexicalAnalyzer.LETTERS.contains(c))
      CharClass.LETTER
    else if (LexicalAnalyzer.DIGITS.contains(c))
      CharClass.DIGIT
    else if (LexicalAnalyzer.BLANKS.contains(c))
      CharClass.BLANK
    else if (c == '+' || c == '-' || c == '*' || c == '/' ||  c == '>' || c == '<' || c == '=')
      CharClass.OPERATOR
    else if (c == '.' || c == ',' || c == ';' || c == ':' )
      CharClass.PUNCTUATOR

  // there are no parens in this grammar
    // else if (c == '(' || c == ')')
    //   CharClass.DELIMITER
    //
    else
      CharClass.OTHER
  }

  // reads the input until a non-blank  character is found, returning the input updated
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

            // TODO: finish the code

            // check all input starting with at least one letter
            // allow digits
            // once finished check if it's a keyword, if so return it
            // else return identifier
           if (charClass == CharClass.LETTER) {
             input = input.substring(1)
             lexeme += c
             var noMoreLetterDigits = false
             while (!noMoreLetterDigits) {
               if (input.length == 0)
                 noMoreLetterDigits = true
               else {
                 c = input(0)
                 charClass = getCharClass(c)
                 if (charClass == CharClass.LETTER || charClass == CharClass.DIGIT) {
                   input = input.substring(1)
                   lexeme += c
                 }
                 else
                   noMoreLetterDigits = true
               }
             }
             if (WORD_TO_TOKEN contains lexeme) {
              return new LexemeUnit(lexeme, WORD_TO_TOKEN(lexeme))
            } else {
             return new LexemeUnit(lexeme, Token.IDENTIFIER)
           }
         }

           // check input starting with a digit
           // allow combination with other digits
           // return as INT_LITERAL
           if (charClass == CharClass.DIGIT) {
             input = input.substring(1)
             lexeme += c
             var noMoreDigits = false
             while (!noMoreDigits) {
               if (input.length == 0)
                 noMoreDigits = true
               else {
                 c = input(0)
                 charClass = getCharClass(c)
                 if (charClass == CharClass.DIGIT) {
                   input = input.substring(1)
                   lexeme += c
                 }
                 else
                   noMoreDigits = true
               }
             }
             return new LexemeUnit(lexeme, Token.INT_LITERAL)
           }

           // check input starting with a operator or punctuator
           // allow combination with more operators or punctuators
           // return as INT_LITERAL
           if (charClass == CharClass.OPERATOR | charClass == CharClass.PUNCTUATOR) {
             input = input.substring(1)
             lexeme += c
             var noMore = false
             while (!noMore) {
               if (input.length == 0)
                 noMore = true
               else {
                 c = input(0)
                 charClass = getCharClass(c)
                 if (charClass == CharClass.OPERATOR | charClass == CharClass.PUNCTUATOR) {
                   input = input.substring(1)
                   lexeme += c
                 }
                 else
                   noMore = true
               }
             }
             if (OPERATOR_PUNCTUATOR_TO_TOKEN contains lexeme) {
              return new LexemeUnit(lexeme, OPERATOR_PUNCTUATOR_TO_TOKEN(lexeme))
            } else
              throw new Exception("Lexical Analyzer Error: unrecognizable symbol(s) found!")
           }



 //
 //           // there are no parens
 //           if (charClass == CharClass.DELIMITER) {
 //             input = input.substring(1)
 //             lexeme += c
 //             c match {
 //               case '(' => return new LexemeUnit(lexeme, Token.OPEN_PAR)
 //               case ')' => return new LexemeUnit(lexeme, Token.CLOSE_PAR)
 //             }
 //           }

            // throw an exception if an unrecognizable symbol is found
            throw new Exception("Lexical Analyzer Error: unrecognizable symbol found!")
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

  val WORD_TO_TOKEN = Map(
  "program"  -> Token.PROGRAM,

  "var"      -> Token.VAR,

  "read"     -> Token.READ_STMT,
  "write"    -> Token.WRITE_STMT,


  "begin"    -> Token.BEGIN_STMT,
  "end"      -> Token.END_STMT,

  "while"    -> Token.WHILE_STMT,
  "do"       -> Token.DO_STMT,

  "if"       -> Token.IF_STMT,
  "then"     -> Token.THEN_STMT,
  "else"     -> Token.ELSE_STMT,

  "true"     -> Token.BOOL_LITERAL,
  "false"    -> Token.BOOL_LITERAL,

  "var"      -> Token.VAR_STMT,
  "Integer"  -> Token.TYPE_STMT,
  "Boolean"  -> Token.TYPE_STMT,

)
  val OPERATOR_PUNCTUATOR_TO_TOKEN = Map(
// arithmetic operators
  "+"         -> Token.ADD_OP,
  "-"         -> Token.SUB_OP,
  "*"         -> Token.MUL_OP,
  "/"         -> Token.DIV_OP,
// combinable
  ">"         -> Token.GREATER_THAN,
  "<"         -> Token.LESS_THAN,
  "="         -> Token.EQUAL_TO,

  ":"         -> Token.COLON, // combinable punctuator

// puncuators
  "."         -> Token.PERIOD,
  ","         -> Token.COMMA,
  ";"         -> Token.SEMI_COLON,

  // combined
  ">="         -> Token.GREATER_EQUAL,
  "<="         -> Token.LESS_THAN,
  ":="         -> Token.WALRUS,
  )




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
