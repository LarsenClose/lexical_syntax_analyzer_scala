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

  // TODO: finish the syntax analyzer
  // program = `program` identifier body `.`
  private def parseProgram() = {
    // create a tree with label "program"
    val tree = new Tree("program")
    getLexemeUnit()

    if (lexemeUnit.getToken() != Token.PROGRAM) {
  //    throw new Exception("Syntax Analyzer Error: program expected!")
    }
    tree.add(new Tree(lexemeUnit.getLexeme()))
    lexemeUnit = null
    getLexemeUnit()

    if (lexemeUnit.getToken() != Token.IDENTIFIER) {
      // throw new Exception("Syntax Analyzer Error: identifier expected!")
    }
    tree.add(parseIdentifier())

    while (lexemeUnit.getToken() != Token.EOF || lexemeUnit.getToken() != Token.PERIOD) {
      tree.add(parseBody())
      }

    getLexemeUnit()
    if (lexemeUnit.getToken() != Token.PERIOD) {
      // throw new Exception("Syntax Analyzer Error: '.' expected!")
    }

      tree.add(new Tree(lexemeUnit.getLexeme()))
    // return the tree
    tree
  }


private def parseBody(): Tree = {

  val tree = new Tree("body")
  getLexemeUnit()

  if (lexemeUnit.getToken() == Token.VAR_STMT) {
    tree.add(parseVariableSection())
  }
  else {
    tree.add(parseBlock())
  }
  tree
}

private def parseVariableSection(): Tree = {

  val tree = new Tree("var_sct")
  getLexemeUnit()

  tree.add(new Tree(lexemeUnit.getLexeme()))
  lexemeUnit = null
  getLexemeUnit()


  var done = false

  while (lexemeUnit.getToken() != Token.EOF) {
      tree.add(parseVariableDeclaration())

      if (lexemeUnit.getToken() == Token.SEMI_COLON) {
          tree.add(new Tree(lexemeUnit.getLexeme()))
          lexemeUnit = null
          getLexemeUnit()
        }
        else
          done = true
    }
  tree
}

private def parseVariableDeclaration(): Tree = {

  val tree = new Tree("var_dct")
  getLexemeUnit()

    while (lexemeUnit.getToken() == Token.IDENTIFIER){
        tree.add(parseIdentifier())
        lexemeUnit = null
        getLexemeUnit()
    }
    if (lexemeUnit.getToken() != Token.COLON) {
      // throw new Exception("Syntax Analyzer Error: colon expected!")
    }
    tree.add(new Tree(lexemeUnit.getLexeme()))
    lexemeUnit = null
    getLexemeUnit()
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
  }
  else {
    // throw new Exception("Syntax Analyzer Error: type expected!")
  }
  tree
}


private def parseBlock(): Tree = {

  val tree = new Tree("block")
  getLexemeUnit()

  if (lexemeUnit.getToken() != Token.BEGIN_STMT) {
    // throw new Exception("Syntax Analyzer Error: block begin expected!")
  }
  tree.add(new Tree(lexemeUnit.getLexeme()))
  lexemeUnit = null
  getLexemeUnit()

  var done = false
  while (lexemeUnit.getToken() != Token.EOF || done == true) {
      tree.add(parseStatement())

      if (lexemeUnit.getToken() == Token.SEMI_COLON) {
          tree.add(new Tree(lexemeUnit.getLexeme()))
          lexemeUnit = null
          getLexemeUnit()
        }
        else
          done = true

    }
    if (lexemeUnit.getToken() != Token.END_STMT) {
      // throw new Exception("Syntax Analyzer Error: block end expected!")
    }
    tree.add(new Tree(lexemeUnit.getLexeme()))
    lexemeUnit = null
    getLexemeUnit()

    tree

} // end parseBlock

private def parseStatement(): Tree = {

  val tree = new Tree("stmt")
  getLexemeUnit()

    lexemeUnit.getToken() match{
      case Token.IDENTIFIER   => tree.add(parseWalrus())
      case Token.READ_STMT    => tree.add(parseReadStatement())
      case Token.WRITE_STMT   => tree.add(parseWriteStatement())
      case Token.IF_STMT      => tree.add(parseIfStatement())
      case Token.WHILE_STMT   => tree.add(parseWhiletatement())
      case Token.BEGIN_STMT   => tree.add(parseBlock())
      // case _                  => throw new Exception("Syntax Analyzer Error: statement expected!")
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


  if (lexemeUnit.getToken() != Token.DO_STMT) {
    // throw new Exception("Syntax Analyzer Error: do expected!")
  }
  getLexemeUnit()
  tree.add(new Tree(lexemeUnit.getLexeme()))
  lexemeUnit = null
  getLexemeUnit()

  tree.add(parseStatement())

  tree
}

private def parseIfStatement(): Tree = {

  val tree = new Tree("if_stmt")
  getLexemeUnit()
  tree.add(new Tree(lexemeUnit.getLexeme()))
  lexemeUnit = null
  getLexemeUnit()

  tree.add(parseBooleanExpression)

  getLexemeUnit()
  if (lexemeUnit.getToken() != Token.THEN_STMT) {
    // throw new Exception("Syntax Analyzer Error: then statement expected!")
  }
  tree.add(new Tree(lexemeUnit.getLexeme()))
  lexemeUnit = null
  getLexemeUnit()


  tree.add(parseStatement())
  getLexemeUnit()
  if (lexemeUnit.getToken() == Token.ELSE_STMT) {
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

  if (lexemeUnit.getToken() != Token.IDENTIFIER) {
    // throw new Exception("Syntax Analyzer Error: identifier expected!")
  }
  tree.add(parseIdentifier())
  tree
}

private def parseReadStatement(): Tree = {

  val tree = new Tree("read_stmt")
  getLexemeUnit()
  tree.add(new Tree(lexemeUnit.getLexeme()))
  lexemeUnit = null
  getLexemeUnit()

  if (lexemeUnit.getToken() != Token.IDENTIFIER) {
    // throw new Exception("Syntax Analyzer Error: identifier expected!")
  }
  tree.add(parseIdentifier())
  tree
}


private def parseWalrus(): Tree = {

  val tree = new Tree("assgm_stmt")
  getLexemeUnit()

  tree.add(parseIdentifier())
  lexemeUnit = null
  getLexemeUnit()
  if (lexemeUnit.getToken() != Token.WALRUS) {
    // throw new Exception("Syntax Analyzer Error: walrus expected!")
  }
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
  }
  if (lexemeUnit.getToken() == Token.IDENTIFIER || lexemeUnit.getToken() == Token.INT_LITERAL) {
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
  }
  tree.add(parseArithmeticExpression)
  getLexemeUnit()
  lexemeUnit.getToken() match{
    case Token.GREATER_THAN   => tree.add(new Tree(lexemeUnit.getLexeme()))
    case Token.LESS_THAN      => tree.add(new Tree(lexemeUnit.getLexeme()))
    case Token.EQUAL_TO       => tree.add(new Tree(lexemeUnit.getLexeme()))
    case Token.GREATER_EQUAL  => tree.add(new Tree(lexemeUnit.getLexeme()))
    case Token.LESS_EQUAL     => tree.add(new Tree(lexemeUnit.getLexeme()))
    // case _                    => throw new Exception("Syntax Analyzer Error: comparison operator expected!")
  }
  lexemeUnit = null
  getLexemeUnit()
  tree.add(parseArithmeticExpression)
  tree
}

private def parseArithmeticExpression(): Tree = {

  val tree = new Tree("arithm_expr")
  getLexemeUnit()

  if (lexemeUnit.getToken() == Token.IDENTIFIER || lexemeUnit.getToken() == Token.INT_LITERAL) {
      tree.add(parseTerm())
  }
  if (lexemeUnit.getToken() == Token.ADD_OP || lexemeUnit.getToken() == Token.SUB_OP) {
      tree.add(new Tree(lexemeUnit.getLexeme()))
      lexemeUnit = null
      getLexemeUnit()
      tree.add(parseArithmeticExpression())
  }
  tree.add(new Tree("arithm_expr'"))
  tree
}


private def parseTerm(): Tree = {

  val tree = new Tree("term")
  getLexemeUnit()

  if (lexemeUnit.getToken() == Token.IDENTIFIER || lexemeUnit.getToken() == Token.INT_LITERAL) {
      tree.add(parseFactor())
  }
  if (lexemeUnit.getToken() == Token.MUL_OP){
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

  if (lexemeUnit.getToken() == Token.IDENTIFIER){
    tree.add(parseIdentifier())
    lexemeUnit = null
    getLexemeUnit()
  }

  if (lexemeUnit.getToken() == Token.INT_LITERAL){
      tree.add(parseLiteral())
      lexemeUnit = null
      getLexemeUnit()
    }

  tree
}


  private def parseIdentifier() = new Tree("identifier: '" + lexemeUnit.getLexeme() + "'")
  private def parseLiteral() = new Tree("int_literal: '" + lexemeUnit.getLexeme() + "'")
}

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
