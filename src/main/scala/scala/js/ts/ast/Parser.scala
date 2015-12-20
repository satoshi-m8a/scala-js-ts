package scala.js.ts.ast

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.syntactical.StdTokenParsers

class Parser extends StdTokenParsers with ImplicitConversions {

  type Tokens = Lexer
  val lexical = new Tokens

  lexical.reserved ++= List(
    "declare", "module", "var", "interface"
  )

  lexical.delimiters ++= List(
    "{", "}", ";", ":", "<", ">", ","
  )

  lazy val LineTerminator = "\n"

  //TODO
  lazy val AssignmentExpression = stringLit

  //TODO
  lazy val FunctionBody = opt(stringLit)

  //TODO
  lazy val ClassBody = opt(stringLit)

  //TODO
  lazy val Initializer = "=" ~ opt(stringLit)

  //TODO
  lazy val BindingPattern = opt(stringLit)

  //TODO

  //TODO
  lazy val LexicalDeclaration = opt(stringLit)

  //TODO
  lazy val GeneratorDeclaration = opt(stringLit)

  //TODO
  lazy val AmbientLexicalDeclaration = opt(stringLit)

  lazy val BindingIdentifier = ident

  lazy val IdentifierReference = ident

  lazy val IdentifierName = ident

  lazy val ExportClause = "{" ~ rep1sep(ExportClauseEntry, ",") ~ "}"

  lazy val ExportClauseEntry = ident | ((ident <~ "as") ~ ident)

  lazy val FromClause = "from" ~> stringLit

  lazy val ImportDeclaration = "import" ~ (stringLit | ((ImportAllAs | ImportClause) ~ FromClause)) ~ ";"

  lazy val ImportAllAs = "*" ~ "as" ~ ident

  lazy val ImportClause = "{" ~ rep1sep(ImportClauseEntry, ",") ~ "}"

  lazy val ImportClauseEntry = ident | ((ident | "default") <~ "as") ~ ident

  lazy val VariableStatement = VariableDeclaration

  /**
    * ECMA-262 A.3 Statements
    */
  lazy val Statement = Declaration | VariableDeclaration

  //Modified
  lazy val Declaration = HoistableDeclaration | ClassDeclaration | LexicalDeclaration | InterfaceDeclaration | TypeAliasDeclaration | EnumDeclaration

  lazy val HoistableDeclaration = FunctionDeclaration | GeneratorDeclaration


  /**
    * A.1 Types
    */
  lazy val TypeParameters = "<" ~> TypeParameterList <~ ">"

  lazy val TypeParameterList = rep1sep(TypeParameter, ",")

  lazy val TypeParameter = BindingIdentifier ~ opt(Constraint)

  lazy val Constraint = "extends" ~> Type

  lazy val TypeArguments = "<" ~> rep1sep(TypeArgument, ",") <~ ">"

  lazy val TypeArgument = Type

  lazy val Type: Parser[_] = UnionOrIntersectionOrPrimaryType | FunctionType | ConstructorType

  lazy val UnionOrIntersectionOrPrimaryType: Parser[_] = UnionType | IntersectionOrPrimaryType

  lazy val IntersectionOrPrimaryType: Parser[_] = IntersectionType | PrimaryType

  lazy val PrimaryType = ParenthesizedType | PredefinedType | TypeReference | ObjectType | ArrayType | TupleType | TypeQuery

  lazy val ParenthesizedType = "(" ~> Type <~ ")"

  lazy val PredefinedType = ("any" | "number" | "boolean" | "string" | "symbol" | "void")

  lazy val TypeReference = (TypeName <~ not(LineTerminator)) ~ opt(TypeArguments)

  lazy val TypeName = rep1sep(ident, ".")

  lazy val NamespaceName = rep1sep(ident, ".")

  lazy val ObjectType = "{" ~> opt(TypeBody) <~ "}"

  lazy val TypeBody = TypeMemberList <~ opt(";" | ",")

  lazy val TypeMemberList = rep1sep(TypeMember, ";" | ",")

  lazy val TypeMember = PropertySignature | CallSignature | ConstructSignature | IndexSignature | MethodSignature

  lazy val ArrayType: Parser[_] = PrimaryType <~ not(LineTerminator) ~ "[" ~ "]"

  lazy val TupleType = "[" ~> TupleElementTypes <~ "]"

  lazy val TupleElementTypes = rep1sep(TupleElementType, ",")

  lazy val TupleElementType = Type

  lazy val UnionType = (UnionOrIntersectionOrPrimaryType <~ "|") ~ IntersectionOrPrimaryType

  lazy val IntersectionType = (IntersectionOrPrimaryType <~ "&") ~ PrimaryType

  lazy val FunctionType = ((opt(TypeParameters) <~ "(") ~ opt(ParameterList) <~ ")" ~ "=>") ~ Type

  lazy val ConstructorType = ("new" ~> opt(TypeParameters) <~ "(") ~ (opt(ParameterList) <~ ")" ~ "=>") ~ Type

  lazy val TypeQuery = "typeof" ~ TypeQueryExpression

  lazy val TypeQueryExpression: Parser[_] = IdentifierReference | (TypeQueryExpression ~ "." ~ IdentifierName)

  lazy val PropertySignature = PropertyName ~ opt("?") ~ opt(TypeAnnotation)

  lazy val PropertyName = (ident | stringLit | numericLit)

  lazy val TypeAnnotation = ":" ~> Type


  lazy val CallSignature = (opt(TypeParameters) <~ "(") ~ (opt(ParameterList) <~ ")") ~ opt(TypeAnnotation)

  lazy val ParameterList = (RequiredParameterList | OptionalParameterList | RestParameter) |
    (RequiredParameterList ~ "," ~ OptionalParameterList) |
    (RequiredParameterList ~ "," ~ RestParameter) |
    (OptionalParameterList ~ "," ~ RestParameter) |
    (RequiredParameterList ~ "," ~ OptionalParameterList ~ "," ~ RestParameter)

  lazy val RequiredParameterList = rep1sep(RequiredParameter, ",")

  lazy val RequiredParameter = (opt(AccessibilityModifier) ~ BindingIdentifierOrPattern ~ opt(TypeAnnotation)) |
    (BindingIdentifier ~ ":" ~ stringLit)

  lazy val AccessibilityModifier = ("public" | "private" | "protected")

  lazy val BindingIdentifierOrPattern = BindingIdentifier | BindingPattern

  lazy val OptionalParameterList = rep1sep(OptionalParameter, ",")

  lazy val OptionalParameter =
    (opt(AccessibilityModifier) ~ BindingIdentifierOrPattern ~ "?" ~ opt(TypeAnnotation)) |
      (opt(AccessibilityModifier) ~ BindingIdentifierOrPattern ~ opt(TypeAnnotation) ~ Initializer) |
      (BindingIdentifier ~ "?" ~ ":" ~ stringLit)

  lazy val RestParameter = "..." ~> BindingIdentifier ~ opt(TypeAnnotation)

  lazy val ConstructSignature = "new" ~ opt(TypeParameters) ~ "(" ~ opt(ParameterList) ~ ")" ~ opt(TypeAnnotation)


  lazy val IndexSignature = "[" ~ BindingIdentifier ~ ":" ~ ("string" | "number") ~ "]" ~ TypeAnnotation

  lazy val MethodSignature = PropertyName ~ opt("?") ~ CallSignature

  lazy val TypeAliasDeclaration = "type" ~ BindingIdentifier ~ opt(TypeParameters) ~ "=" ~ Type ~ ";"

  /**
    * A.3 Statements
    */
  //Modified
  lazy val VariableDeclaration = SimpleVariableDeclaration | DestructuringVariableDeclaration

  lazy val SimpleVariableDeclaration = BindingIdentifier ~ opt(TypeAnnotation) ~ opt(Initializer)

  lazy val DestructuringVariableDeclaration = BindingPattern ~ opt(TypeAnnotation) ~ Initializer

  /**
    * A.4 Functions
    */
  //Modified
  lazy val FunctionDeclaration = "function" ~ opt(BindingIdentifier) ~ CallSignature ~ (("{" ~ FunctionBody ~ "}") | ";")

  /**
    * A.5 Interfaces
    */
  lazy val InterfaceDeclaration = "interface" ~> BindingIdentifier ~ opt(TypeParameters) ~ opt(InterfaceExtendsClause) ~ ObjectType

  lazy val InterfaceExtendsClause = "extends" ~ ClassOrInterfaceTypeList

  lazy val ClassOrInterfaceTypeList = rep1sep(ClassOrInterfaceType, ",")

  lazy val ClassOrInterfaceType = TypeReference

  /**
    * A.6 Classes
    */
  //Modified
  lazy val ClassDeclaration = "class" ~ opt(BindingIdentifier) ~ opt(TypeParameters) ~ ClassHeritage ~ "{" ~ ClassBody ~ "}"

  //Modified
  lazy val ClassHeritage = opt(ClassExtendsClause) ~ opt(ImplementsClause)

  lazy val ClassExtendsClause = "extends" ~ ClassType

  lazy val ClassType = TypeReference

  lazy val ImplementsClause = "implements" ~ ClassOrInterfaceTypeList

  /**
    * A.7 Enums
    */
  lazy val EnumDeclaration = opt("const") ~ "enum" ~ BindingIdentifier ~ "{" ~ opt(EnumBody) ~ "}"

  lazy val EnumBody = EnumMemberList ~ opt(",")

  lazy val EnumMemberList = rep1sep(EnumMember, ",")

  lazy val EnumMember = PropertyName | (PropertyName ~ "=" ~ EnumValue)

  lazy val EnumValue = AssignmentExpression

  /**
    * A.8 Namespaces
    */
  lazy val NamespaceDeclaration = "namespace" ~ IdentifierPath ~ "{" ~ NamespaceBody ~ "}"

  lazy val IdentifierPath = rep1sep(BindingIdentifier, ".")

  lazy val NamespaceBody: Parser[_] = opt(NamespaceElements)

  lazy val NamespaceElements = rep1(NamespaceElement)

  lazy val NamespaceElement = Statement | LexicalDeclaration | FunctionDeclaration | GeneratorDeclaration | ClassDeclaration | InterfaceDeclaration | TypeAliasDeclaration | EnumDeclaration | NamespaceDeclaration | AmbientDeclaration | ImportAliasDeclaration | ExportNamespaceElement

  lazy val ExportNamespaceElement = "export" ~ (VariableStatement | LexicalDeclaration | FunctionDeclaration | GeneratorDeclaration | ClassDeclaration | InterfaceDeclaration | TypeAliasDeclaration | EnumDeclaration | NamespaceDeclaration | AmbientDeclaration | ImportAliasDeclaration)


  lazy val ImportAliasDeclaration = "import" ~ BindingIdentifier ~ "=" ~ EntityName ~ ";"

  lazy val EntityName = rep1(NamespaceName)

  /**
    * A.9 Scripts and Modules
    */
  //TODO ImplementationSourceFile
  lazy val SourceFile = DeclarationSourceFile

  lazy val DeclarationSourceFile = DeclarationScript | DeclarationModule

  lazy val DeclarationScript = opt(DeclarationScriptElements)

  lazy val DeclarationScriptElements = rep1(DeclarationScriptElement)

  lazy val DeclarationScriptElement = DeclarationElement | AmbientModuleDeclaration

  lazy val DeclarationElement = InterfaceDeclaration | TypeAliasDeclaration | NamespaceDeclaration | AmbientDeclaration | ImportAliasDeclaration

  lazy val DeclarationModule = opt(DeclarationModuleElements)

  lazy val DeclarationModuleElements = rep1(DeclarationModuleElement)

  lazy val DeclarationModuleElement = DeclarationElement | ImportDeclaration | ImportAliasDeclaration | ExportDeclarationElement | ExportDefaultDeclarationElement | ExportListDeclaration | ExportAssignment

  lazy val ExportDeclarationElement = "export" ~ (InterfaceDeclaration | TypeAliasDeclaration | AmbientDeclaration | ImportAliasDeclaration)

  lazy val ExportDefaultDeclarationElement = "export" ~ "default" ~ (AmbientFunctionDeclaration | AmbientClassDeclaration | (ident ~ ";"))

  lazy val ExportListDeclaration = "export" ~ (("*" ~ FromClause ~ ";") | (ExportClause ~ FromClause ~ ";") | (ExportClause ~ ";"))

  lazy val ExportAssignment = "export" ~ "=" ~> ident <~ ";"

  /**
    * A.10 Ambients
    */
  lazy val AmbientDeclaration = "declare" ~ (AmbientVariableDeclaration | AmbientFunctionDeclaration | AmbientClassDeclaration | AmbientEnumDeclaration | AmbientNamespaceDeclaration)

  lazy val AmbientVariableDeclaration = ("var" | "let" | "const") ~ AmbientBindingList <~ ";"

  lazy val AmbientBindingList = rep1sep(AmbientBinding, ",")

  lazy val AmbientBinding = BindingIdentifier ~ opt(TypeAnnotation)

  lazy val AmbientFunctionDeclaration = "function" ~ BindingIdentifier ~ CallSignature ~ ";"

  lazy val AmbientClassDeclaration = "class" ~ BindingIdentifier ~ opt(TypeParameters) ~ "{" ~ AmbientClassBody ~ "}"

  lazy val AmbientClassBody = opt(AmbientClassBodyElements)

  lazy val AmbientClassBodyElements = rep1(AmbientClassBodyElement)

  lazy val AmbientClassBodyElement = AmbientConstructorDeclaration | AmbientPropertyMemberDeclaration | IndexSignature

  lazy val AmbientConstructorDeclaration = "constructor" ~ "(" ~ opt(ParameterList) ~ ")" ~ ";"

  lazy val AmbientPropertyMemberDeclaration = opt(AccessibilityModifier) ~ opt("static") ~ PropertyName ~ opt(TypeAnnotation | CallSignature) ~ ";"

  lazy val AmbientEnumDeclaration = EnumDeclaration

  lazy val AmbientNamespaceDeclaration = "namespace" ~ IdentifierPath ~ "{" ~ AmbientNamespaceBody ~ "}"

  lazy val AmbientNamespaceBody = opt(AmbientNamespaceElements)

  lazy val AmbientNamespaceElements = rep1(AmbientNamespaceElement)

  lazy val AmbientNamespaceElement: Parser[_] = opt("export") ~ (AmbientVariableDeclaration | AmbientLexicalDeclaration | AmbientFunctionDeclaration | AmbientClassDeclaration | InterfaceDeclaration | AmbientEnumDeclaration | AmbientNamespaceDeclaration | ImportAliasDeclaration)

  lazy val AmbientModuleDeclaration = "declare" ~ "module" ~ stringLit ~ "{" ~ DeclarationModule ~ "}"

}

object Parser {
  def parse(input: String) = {
    val parser = new Parser

    parser.phrase(parser.DeclarationSourceFile)(new parser.lexical.Scanner(input)) match {
      case parser.Success(result, _) => Some(result)
      case parser.NoSuccess(errorMessage, next) => None
    }
  }
}