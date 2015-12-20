package scala.js.ts.ast

import org.scalatest.{Matchers, FlatSpec}


class ParserSpec extends FlatSpec with Matchers {

  val parser = new Parser

  def parse[T](p: parser.Parser[T])(input: String) = {
    parser.phrase(p)(new parser.lexical.Scanner(input)) match {
      case parser.Success(result, _) => Right(result)
      case parser.NoSuccess(errorMessage, next) => {
        Left(s"$errorMessage on line ${next.pos.line} on column ${next.pos.column}")
      }
    }
  }

  "parser" should "parse InterfaceDeclaration" in {
    parse(parser.InterfaceDeclaration) {
      """
        |interface i {
        |}
      """.stripMargin
    } shouldBe a[Right[_, _]]

    it should "parse AmbientModuleDeclaration" in {
      parse(parser.AmbientModuleDeclaration) {
        """
          |declare module "mname" {
          | interface i {
          |
          | }
          |}
        """.stripMargin
      } shouldBe a[Right[_, _]]
    }
  }
}
