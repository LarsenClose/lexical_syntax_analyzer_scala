/*
 * CS3210 - Principles of Programming Languages - Fall 2020
 * Instructor: Thyago Mota
 * Description: Prg01 - Syntax Analyzer
 * Student: Larsen Close
 */

class SyntaxAnalyzer(private var source: String) {

  private var it = new LexicalAnalyzer(source).iterator
  private var lexemeUnit: LexemeUnit = null

  private def getLexemeUnit() = {
    if (lexemeUnit == null)
      lexemeUnit = it.next()
  }

  def parse(): Tree = {
    parseProgram()
  }

  private def parseProgram() = {

    val tree = new Tree("program")
    getLexemeUnit()

    if (lexemeUnit.getToken() != Token.PROGRAM) {
      throw new Exception("Syntax Analyzer Error: program expected!")
    }
    tree.add(new Tree(lexemeUnit.getLexeme()))
    lexemeUnit = null
    getLexemeUnit()

    tree.add(parseIdentifier())

    tree.add(parseBody())

    if (lexemeUnit.getToken() == Token.PERIOD) {
      tree.add(new Tree(lexemeUnit.getLexeme()))
      lexemeUnit = null
      getLexemeUnit()
    } else { throw new Exception("Syntax Analyzer Error: '.' expected!") }

    tree
  }

  private def parseBody(): Tree = {

    val tree = new Tree("body")
    getLexemeUnit()

    if (lexemeUnit.getToken() == Token.VAR_STMT) {
      tree.add(parseVariableSection())
    } else { throw new Exception("Syntax Analyzer Error: 'var' expected!") }

    tree.add(parseBlock())

    tree
  }

  private def parseBlock(): Tree = {

    val tree = new Tree("block")
    getLexemeUnit()

    tree.add(new Tree(lexemeUnit.getLexeme()))
    lexemeUnit = null
    getLexemeUnit()

    var run = true
    while (run) {

      if (lexemeUnit.getToken() == Token.SEMI_COLON) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
      }

      tree.add(parseStatement())

      if (
        lexemeUnit.getToken() == Token.END_STMT || lexemeUnit
          .getToken() == Token.EOF
      ) { run = false }

    }

    if (lexemeUnit.getToken() == Token.END_STMT) {
      tree.add(new Tree(lexemeUnit.getLexeme()))
      lexemeUnit = null
      getLexemeUnit()
    } else { throw new Exception("Syntax Analyzer Error: 'end' expected!") }

    tree
  } // end parseBlock

  private def parseVariableSection(): Tree = {

    val tree = new Tree("var_sct")
    getLexemeUnit()

    tree.add(new Tree(lexemeUnit.getLexeme()))
    lexemeUnit = null
    getLexemeUnit()

    var run = true
    while (run) {

      if (lexemeUnit.getToken() == Token.SEMI_COLON) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
      }

      tree.add(parseVariableDeclaration())

      if (
        lexemeUnit.getToken() == Token.BEGIN_STMT || lexemeUnit
          .getToken() == Token.EOF
      ) { run = false }

    }

    tree
  }

  private def parseVariableDeclaration(): Tree = {

    val tree = new Tree("var_dct")
    getLexemeUnit()

    var run = true
    while (run) {

      tree.add(parseIdentifier())

      if (
        lexemeUnit.getToken() == Token.COLON || lexemeUnit
          .getToken() == Token.EOF
      ) {
        run = false
      }
    }

    if (lexemeUnit.getToken() == Token.COLON) {
      tree.add(new Tree(lexemeUnit.getLexeme()))
      lexemeUnit = null
      getLexemeUnit()
    } else { throw new Exception("Syntax Analyzer Error: ':' expected!") }

    tree.add(parseType())

    tree
  }

  private def parseType(): Tree = {

    val tree = new Tree("type")
    getLexemeUnit()

    if (lexemeUnit.getToken() == Token.TYPE_STMT) {
      tree.add(new Tree(lexemeUnit.getLexeme()))
      lexemeUnit = null
      getLexemeUnit()
    } else { throw new Exception("Syntax Analyzer Error: 'type' expected!") }
    tree
  }

  private def parseStatement(): Tree = {

    val tree = new Tree("stmt")
    getLexemeUnit()

    if (lexemeUnit.getToken() == Token.IDENTIFIER) {
      tree.add(parseWalrus())
    } else if (lexemeUnit.getToken() == Token.READ_STMT) {
      tree.add(parseReadStatement())
    } else if (lexemeUnit.getToken() == Token.WRITE_STMT) {
      tree.add(parseWriteStatement())
    } else if (lexemeUnit.getToken() == Token.IF_STMT) {
      tree.add(parseIfStatement())
    } else if (lexemeUnit.getToken() == Token.WHILE_STMT) {
      tree.add(parseWhiletatement())
    } else if (lexemeUnit.getToken() == Token.BEGIN_STMT) {
      tree.add(parseBlock())
    } else {throw new Exception("Syntax Analyzer Error: 'statement' expected!")
    }
    tree
  }

  private def parseWhiletatement(): Tree = {

    val tree = new Tree("while_stmt")
    getLexemeUnit()

    tree.add(new Tree(lexemeUnit.getLexeme()))
    lexemeUnit = null
    getLexemeUnit()

    tree.add(parseBooleanExpression())

    getLexemeUnit()

    if (lexemeUnit.getToken() == Token.DO_STMT) {
      tree.add(new Tree(lexemeUnit.getLexeme()))
      lexemeUnit = null
      getLexemeUnit()
    } else { throw new Exception("Syntax Analyzer Error: 'do' expected!") }

    tree.add(parseStatement())
    tree
  }

  private def parseIfStatement(): Tree = {

    val tree = new Tree("if_stmt")
    getLexemeUnit()

    if (lexemeUnit.getToken() == Token.IF_STMT) {
      tree.add(new Tree(lexemeUnit.getLexeme()))
      lexemeUnit = null
      getLexemeUnit()
    } else { throw new Exception("Syntax Analyzer Error: 'if' expected!") }

    tree.add(parseBooleanExpression)
    getLexemeUnit()

    if (lexemeUnit.getToken() == Token.THEN_STMT) {
      tree.add(new Tree(lexemeUnit.getLexeme()))
      lexemeUnit = null
      getLexemeUnit()
    } else { throw new Exception("Syntax Analyzer Error: 'then' expected!") }

    tree.add(parseStatement())
    getLexemeUnit()

    if (lexemeUnit.getToken() == Token.ELSE_STMT) {
      tree.add(new Tree(lexemeUnit.getLexeme()))
      lexemeUnit = null
      getLexemeUnit()
      tree.add(parseStatement())
    }
    tree
  }

  private def parseWriteStatement(): Tree = {

    val tree = new Tree("write_stmt")
    getLexemeUnit()

    tree.add(new Tree(lexemeUnit.getLexeme()))
    lexemeUnit = null
    getLexemeUnit()

    if (lexemeUnit.getToken() == Token.IDENTIFIER) {
      tree.add(parseIdentifier())
    }

    tree
  }

  private def parseReadStatement(): Tree = {

    val tree = new Tree("read_stmt")
    getLexemeUnit()
    tree.add(new Tree(lexemeUnit.getLexeme()))
    lexemeUnit = null
    getLexemeUnit()

    tree.add(parseIdentifier())

    tree
  }

  private def parseWalrus(): Tree = {

    val tree = new Tree("assgm_stmt")
    getLexemeUnit()

    tree.add(parseIdentifier())

    getLexemeUnit()
    tree.add(new Tree(lexemeUnit.getLexeme()))
    lexemeUnit = null
    getLexemeUnit()

    tree.add(parseExpression())

    tree
  }

  private def parseExpression(): Tree = {

    val tree = new Tree("expr")
    getLexemeUnit()

    if (lexemeUnit.getToken() == Token.BOOL_LITERAL) {
      tree.add(parseBooleanExpression())
    } else if (
      lexemeUnit.getToken() == Token.IDENTIFIER || lexemeUnit
        .getToken() == Token.INT_LITERAL
    ) {
      tree.add(parseArithmeticExpression())
    }

    tree
  }

  private def parseBooleanExpression(): Tree = {

    val tree = new Tree("bool_expr")
    getLexemeUnit()

    if (lexemeUnit.getToken() == Token.BOOL_LITERAL) {
      tree.add(new Tree(lexemeUnit.getLexeme()))
      lexemeUnit = null
      getLexemeUnit()
      return tree
    }

    if (
      lexemeUnit.getToken() == Token.INT_LITERAL || lexemeUnit
        .getToken() == Token.IDENTIFIER
    ) {

      tree.add(parseArithmeticExpression)
      getLexemeUnit()

      if (lexemeUnit.getToken() == Token.COMPARISON) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
      } else {
        throw new Exception("Syntax Analyzer Error: 'comparison' expected!")
      }
      tree.add(parseArithmeticExpression)

    }

    tree
  }

  private def parseArithmeticExpression(): Tree = {

    val tree = new Tree("arithm_expr")
    getLexemeUnit()

    if (
      lexemeUnit.getToken() == Token.IDENTIFIER || lexemeUnit
        .getToken() == Token.INT_LITERAL
    ) {
      tree.add(parseTerm())
    }

    if (
      lexemeUnit.getToken() == Token.ADD_OP || lexemeUnit
        .getToken() == Token.SUB_OP
    ) {
      tree.add(parseArithmeticExpressionPrime())
      tree
    }

    tree.add(new Tree("arithm_expr'"))

    tree
  }

  private def parseArithmeticExpressionPrime(): Tree = {
    val tree = new Tree("arithm_expr'")

    tree.add(new Tree(lexemeUnit.getLexeme()))
    lexemeUnit = null
    getLexemeUnit()

    if (
      lexemeUnit.getToken() == Token.IDENTIFIER || lexemeUnit
        .getToken() == Token.INT_LITERAL
    ) {
      tree.add(parseTerm())
    }
    if (
      lexemeUnit.getToken() == Token.ADD_OP || lexemeUnit
        .getToken() == Token.SUB_OP
    ) {
      tree.add(parseArithmeticExpressionPrime())
      tree
    }

    tree.add(new Tree("arithm_expr'"))

    tree
  }

  private def parseTerm(): Tree = {

    val tree = new Tree("term")
    getLexemeUnit()

    if (
      lexemeUnit.getToken() == Token.IDENTIFIER || lexemeUnit
        .getToken() == Token.INT_LITERAL
    ) {
      tree.add(parseFactor())
    }
    if (lexemeUnit.getToken() == Token.MUL_OP) {
      tree.add(new Tree(lexemeUnit.getLexeme()))
      lexemeUnit = null
      getLexemeUnit()
      tree.add(parseTerm())
    }

    tree.add(new Tree("term'"))
    tree
  }

  private def parseFactor(): Tree = {

    val tree = new Tree("factor")
    getLexemeUnit()

    if (lexemeUnit.getToken() == Token.IDENTIFIER) {
      tree.add(parseIdentifier())
    } else if (lexemeUnit.getToken() == Token.INT_LITERAL) {
      tree.add(parseLiteral())
    } else { throw new Exception("Syntax Analyzer Error: Factor expected!") }

    tree
  }

  private def parseIdentifier(): Tree = {
    if (lexemeUnit.getToken() != Token.IDENTIFIER) {
      throw new Exception("Syntax Analyzer Error: identifier expected!")
    }
    val tree = new Tree("identifier: '" + lexemeUnit.getLexeme() + "'")
    lexemeUnit = null
    getLexemeUnit()

    tree
  }

  private def parseLiteral(): Tree = {
    if (lexemeUnit.getToken() != Token.INT_LITERAL) {
      throw new Exception("Syntax Analyzer Error: integer literal expected!")
    }
    val tree = new Tree("int_literal: '" + lexemeUnit.getLexeme() + "'")
    lexemeUnit = null
    getLexemeUnit()

    tree
  }
} //end SyntaxAnalyzer class

object SyntaxAnalyzer {
  def main(args: Array[String]): Unit = {
    // check if source file was passed through the command-line
    if (args.length != 1) {
      print("Missing source file!")
      System.exit(1)
    }

    val syntaxAnalyzer = new SyntaxAnalyzer(args(0))
    val parseTree = syntaxAnalyzer.parse()
    print(parseTree)
  }
}
