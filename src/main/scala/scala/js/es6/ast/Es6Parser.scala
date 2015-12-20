package scala.js.es6.ast

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StdTokenParsers

class Es6Parser extends StdTokenParsers with PackratParsers {

  type Tokens = Es6Lexer
  val lexical = new Tokens


  lazy val FunctionBody = FunctionStatementList

  lazy val FunctionStatementList:PackratParser[_] = opt(stringLit)

  lazy val ClassDeclaration: PackratParser[_] = opt(stringLit)

  lazy val VariableDeclaration: PackratParser[_] = opt(stringLit)

  lazy val FunctionDeclaration: PackratParser[_] = opt(stringLit)

  //TODO
  lazy val GeneratorDeclaration = opt(stringLit)

  //TODO
  lazy val LexicalDeclaration = opt(stringLit)

  lazy val AssignmentExpression = stringLit

  lazy val Initializer: PackratParser[_] = "=" ~ AssignmentExpression

  lazy val StringLiteral = stringLit

  lazy val NumericLiteral = numericLit

  /**
    * ECMA-262 A.3 Statements
    */
  lazy val Statement = Declaration | VariableDeclaration

  lazy val Declaration: Parser[_] = HoistableDeclaration | ClassDeclaration | LexicalDeclaration

  lazy val HoistableDeclaration = FunctionDeclaration | GeneratorDeclaration

}

