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

    if (lexemeUnit.getToken() == Token.PROGRAM) {}

    // while (lexemeUnit.getToken() != Token.EOF) {
    //   tree.add(parseSyntaxRule())
    //   if (lexemeUnit.getToken() == Token.NEW_LINE) {
    //     tree.add(new Tree(lexemeUnit.getLexeme()))
    //     lexemeUnit = null
    //     getLexemeUnit()
    //   }
    // }

    // return the tree
    tree
  }
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
