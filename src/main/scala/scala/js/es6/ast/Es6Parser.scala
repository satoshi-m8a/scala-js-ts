package scala.js.es6.ast

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.syntactical.StdTokenParsers

class Es6Parser extends StdTokenParsers with ImplicitConversions {

  type Tokens = Es6Lexer
  val lexical = new Tokens


  lazy val ClassDeclaration:Parser[_] = ""

  lazy val VariableDeclaration: Parser[_] = ""

  lazy val FunctionDeclaration: Parser[_] = ""

  //TODO
  lazy val GeneratorDeclaration = opt(stringLit)

  //TODO
  lazy val LexicalDeclaration = opt(stringLit)

  /**
    * ECMA-262 A.3 Statements
    */
  lazy val Statement = Declaration | VariableDeclaration

  lazy val Declaration: Parser[_] = HoistableDeclaration | ClassDeclaration | LexicalDeclaration

  lazy val HoistableDeclaration = FunctionDeclaration | GeneratorDeclaration

}

