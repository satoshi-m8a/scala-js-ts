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

  "parser" should "parse NumericLiteral" in {
    parse(parser.NumericLiteral) {
      """12"""
    } shouldBe a[Right[_, _]]
  }

  it should "parse VariableDeclaration #1" in {
    parse(parser.VariableDeclaration) {
      """
        |x:number
      """.stripMargin
    } shouldBe a[Right[_, _]]
  }

  it should "parse VariableDeclaration #2" in {
    parse(parser.VariableDeclaration) {
      """
        |x
      """.stripMargin
    } shouldBe a[Right[_, _]]
  }

  it should "parse PropertyName" in {
    val p = parse(parser.PropertyName) _
    p("""x""") shouldBe a[Right[_, _]]
    p("12") shouldBe a[Right[_, _]]
    p("\"ss\"") shouldBe a[Right[_, _]]
  }

  it should "parse InterfaceDeclaration" in {
    parse(parser.InterfaceDeclaration) {
      """
        |interface i {
        | x:number;
        | type?: string;
        | add(callbacks: Function[]): string;
        |}
      """.stripMargin
    } shouldBe a[Right[_, _]]
  }

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

  it should "parse UnionType #1" in {
    parse(parser.UnionType) {
      """number|string"""
    } shouldBe a[Right[_, _]]
  }

  it should "parse UnionType #2" in {
    parse(parser.UnionType) {
      """number|string|boolean"""
    } shouldBe a[Right[_, _]]
  }

  it should "parse FunctionType" in {
    parse(parser.FunctionType) {
      """<T>(t:T) => void"""
    } shouldBe a[Right[_, _]]
  }

  it should "parse PropertySignature" in {
    parse(parser.PropertySignature) {
      "x?:number"
    } shouldBe a[Right[_, _]]
  }

  it should "parse ConstructSignature" in {
    parse(parser.ConstructSignature) {
      """
        |new ():void
      """.stripMargin
    } shouldBe a[Right[_, _]]
  }

  it should "parse IndexSignature" in {
    parse(parser.IndexSignature) {
      """[x:number]:void"""
    } shouldBe a[Right[_, _]]
  }

  it should "parse ObjectType" in {
    parse(parser.ObjectType) {
      """
        |{
        | x:boolean;
        | f: () => void;
        | xx?;
        | [x:number]:string;
        | x():void;
        |}
      """.stripMargin
    } shouldBe a[Right[_, _]]
  }

  it should "parse RestParameter" in {
    parse(parser.RestParameter) {
      """...values:any[]"""
    } shouldBe a[Right[_, _]]
  }

  it should "parse ParameterList #1" in {
    val p = parse(parser.ParameterList) _
    p("x:any") shouldBe a[Right[_, _]]
    p("x:any,y:any") shouldBe a[Right[_, _]]
  }

  it should "parse ParameterList #2" in {
    val p = parse(parser.ParameterList) _
    p("x?:any") shouldBe a[Right[_, _]]
    p("x?:any,y?:any") shouldBe a[Right[_, _]]
  }

  it should "parse ParameterList #3" in {
    val p = parse(parser.ParameterList) _
    p("x:any,y?:any") shouldBe a[Right[_, _]]
  }

  it should "parse ParameterList #4" in {
    val p = parse(parser.ParameterList) _
    p("...values:any[]") shouldBe a[Right[_, _]]
    p("x:any,...values:any[]") shouldBe a[Right[_, _]]
    p("x:any,y?:any,...values:any[]") shouldBe a[Right[_, _]]
  }

  it should "parse ParameterList #5" in {
    val p = parse(parser.ParameterList) _
    p("x:string[]") shouldBe a[Right[_, _]]
    p("...values: any[]") shouldBe a[Right[_, _]]
    p("value?: T, ...values: any[]") shouldBe a[Right[_, _]]
    p("v1: T, v2?:T,...values:any[]") shouldBe a[Right[_, _]]
  }

  it should "parse MethodSignature" in {
    parse(parser.MethodSignature) {
      """
        |x(options?: Params):void
      """.stripMargin
    } shouldBe a[Right[_, _]]
  }

  it should "parse CallSignature" in {
    parse(parser.CallSignature) {
      """
        |():void
      """.stripMargin
    } shouldBe a[Right[_, _]]
  }

  it should "parse OptionalParameter" in {
    parse(parser.OptionalParameter) {
      """
        |options?: Params
      """.stripMargin
    } shouldBe a[Right[_, _]]
  }

  it should "parse ArrayType" in {
    val p = parse(parser.ArrayType) _
    p("string[]") shouldBe a[Right[_, _]]
    p("(string | number)[]") shouldBe a[Right[_, _]]
    p("Function[]") shouldBe a[Right[_, _]]
  }

  it should "parse ParenthesizedType" in {
    parse(parser.ParenthesizedType) {
      """
        |(string | number)
      """.stripMargin
    } shouldBe a[Right[_, _]]
  }
}
