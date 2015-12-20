package scala.js.ts.ast

import scala.js.es6.ast.Es6Parser

class Parser extends Es6Parser {

  override val lexical = new Tokens

  lexical.reserved ++= List(
    "declare", "module", "var", "let", "const", "interface", "import", "from", "as",
    "any", "number", "boolean", "string", "symbol", "void",
    "new", "type", "default", "typeof", "extends",
    "public", "private", "protected", "function", "Function",
    "export"
  )

  lexical.delimiters ++= List(
    "{", "}", ";", ":", "<", ">", ",", "|", "=>", "=", "(", ")", "?", "[", "]", "...", "&"
  )

  lazy val LineTerminator = "\n"

  //TODO
  lazy val ClassBody = opt(stringLit)

  //TODO
  lazy val BindingPattern: PackratParser[_] = opt(stringLit)


  //TODO
  lazy val AmbientLexicalDeclaration = opt(stringLit)

  lazy val BindingIdentifier: PackratParser[_] = ident | "type"

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


  //Modified
  override lazy val Declaration: PackratParser[_] = HoistableDeclaration | ClassDeclaration | LexicalDeclaration | InterfaceDeclaration | TypeAliasDeclaration | EnumDeclaration


  /**
    * A.1 Types
    */
  lazy val TypeParameters = "<" ~> TypeParameterList <~ ">"

  lazy val TypeParameterList = rep1sep(TypeParameter, ",")

  lazy val TypeParameter = BindingIdentifier ~ opt(Constraint)

  lazy val Constraint = "extends" ~> Type

  lazy val TypeArguments = "<" ~> rep1sep(TypeArgument, ",") <~ ">"

  lazy val TypeArgument: PackratParser[_] = Type

  //not equivalent to original
  lazy val Type: PackratParser[_] = (UnionType | IntersectionType | PrimaryType) | ConstructorType | FunctionType

  lazy val UnionOrIntersectionOrPrimaryType: PackratParser[_] =  UnionType | PrimaryType | IntersectionType

  lazy val IntersectionOrPrimaryType: PackratParser[_] = PrimaryType | IntersectionType

  lazy val PrimaryType: PackratParser[_] = TypeQuery | TupleType | ArrayType | ObjectType | TypeReference | PredefinedType | ParenthesizedType

  lazy val ParenthesizedType: PackratParser[_] = "(" ~> Type <~ ")"

  //not equivalent to original
  lazy val PredefinedType = "Function" | "any" | "number" | "boolean" | "string" | "symbol" | "void"

  lazy val TypeReference = (TypeName <~ not(LineTerminator)) ~ opt(TypeArguments)

  lazy val TypeName = rep1sep(ident, ".")

  lazy val NamespaceName = rep1sep(ident, ".")

  lazy val ObjectType = "{" ~> opt(TypeBody) <~ "}"

  lazy val TypeBody = TypeMemberList <~ opt(";" | ",")

  lazy val TypeMemberList = rep1sep(TypeMember, ";" | ",")

  lazy val TypeMember: PackratParser[_] = MethodSignature | IndexSignature | ConstructSignature | CallSignature | PropertySignature

  lazy val ArrayType: PackratParser[_] = PrimaryType <~ not(LineTerminator) ~ "[" ~ "]"

  lazy val TupleType = "[" ~> TupleElementTypes <~ "]"

  lazy val TupleElementTypes = rep1sep(TupleElementType, ",")

  lazy val TupleElementType = Type

  lazy val UnionType: PackratParser[_] = ((UnionOrIntersectionOrPrimaryType ~> "|") ~ IntersectionOrPrimaryType)

  lazy val IntersectionType: PackratParser[_] = (IntersectionOrPrimaryType <~ "&") ~ PrimaryType

  lazy val FunctionType: PackratParser[_] = ((opt(TypeParameters) <~ "(") ~ opt(ParameterList) <~ ")" ~ "=>") ~ Type

  lazy val ConstructorType: PackratParser[_] = ("new" ~> opt(TypeParameters) <~ "(") ~ (opt(ParameterList) <~ ")" ~ "=>") ~ Type

  lazy val TypeQuery: PackratParser[_] = "typeof" ~ TypeQueryExpression

  lazy val TypeQueryExpression: PackratParser[_] = IdentifierReference | (TypeQueryExpression ~ "." ~ IdentifierName)

  lazy val PropertySignature: PackratParser[_] = PropertyName ~ opt("?") ~ opt(TypeAnnotation)

  //not equivalent to original
  lazy val PropertyName = (IdentifierName | StringLiteral | NumericLiteral | "type")

  lazy val TypeAnnotation: PackratParser[_] = ":" ~> Type

  lazy val CallSignature = (opt(TypeParameters) <~ "(") ~ (opt(ParameterList) <~ ")") ~ opt(TypeAnnotation)

  lazy val ParameterList: PackratParser[_] =
    (repsep(RequiredParameter, ",") ~ ",".? ~ repsep(OptionalParameter, ",") ~ ",".?  ~ opt(RestParameter))


  lazy val RequiredParameterList: PackratParser[_] = (RequiredParameterList ~ "," ~ RequiredParameter) | RequiredParameter

  lazy val RequiredParameter: PackratParser[_] = (opt(AccessibilityModifier) ~ BindingIdentifierOrPattern ~ opt(TypeAnnotation)) |
    (BindingIdentifier ~ ":" ~ stringLit)

  lazy val AccessibilityModifier: PackratParser[_] = ("public" | "private" | "protected")

  lazy val BindingIdentifierOrPattern: PackratParser[_] = BindingIdentifier | BindingPattern

  lazy val OptionalParameterList: PackratParser[_] = (OptionalParameterList ~ "," ~ OptionalParameter) | OptionalParameter

  lazy val OptionalParameter: PackratParser[_] =
    (opt(AccessibilityModifier) ~ BindingIdentifierOrPattern ~ "?" ~ opt(TypeAnnotation)) |
      (opt(AccessibilityModifier) ~ BindingIdentifierOrPattern ~ opt(TypeAnnotation) ~ Initializer) |
      (BindingIdentifier ~ "?" ~ ":" ~ stringLit)

  lazy val RestParameter: PackratParser[_] = "..." ~> BindingIdentifier ~ opt(TypeAnnotation)

  lazy val ConstructSignature = "new" ~ opt(TypeParameters) ~ "(" ~ opt(ParameterList) ~ ")" ~ opt(TypeAnnotation)


  lazy val IndexSignature = "[" ~ BindingIdentifier ~ ":" ~ ("string" | "number") ~ "]" ~ TypeAnnotation

  lazy val MethodSignature = PropertyName ~ opt("?") ~ CallSignature

  lazy val TypeAliasDeclaration = "type" ~ BindingIdentifier ~ opt(TypeParameters) ~ "=" ~ Type ~ ";"

  /**
    * A.3 Statements
    */
  //Modified
  override lazy val VariableDeclaration: PackratParser[_] = SimpleVariableDeclaration | DestructuringVariableDeclaration

  lazy val SimpleVariableDeclaration = BindingIdentifier ~ opt(TypeAnnotation) ~ opt(Initializer)

  lazy val DestructuringVariableDeclaration = BindingPattern ~ opt(TypeAnnotation) ~ Initializer

  /**
    * A.4 Functions
    */
  //Modified
  override lazy val FunctionDeclaration: PackratParser[_] = "function" ~ opt(BindingIdentifier) ~ CallSignature ~ (("{" ~ FunctionBody ~ "}") | ";")

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
  override lazy val ClassDeclaration: PackratParser[_] = "class" ~ opt(BindingIdentifier) ~ opt(TypeParameters) ~ ClassHeritage ~ "{" ~ ClassBody ~ "}"

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

  lazy val NamespaceBody: PackratParser[_] = opt(NamespaceElements)

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

  lazy val AmbientNamespaceElement: PackratParser[_] = opt("export") ~ (AmbientVariableDeclaration | AmbientLexicalDeclaration | AmbientFunctionDeclaration | AmbientClassDeclaration | InterfaceDeclaration | AmbientEnumDeclaration | AmbientNamespaceDeclaration | ImportAliasDeclaration)

  lazy val AmbientModuleDeclaration = "declare" ~ "module" ~ stringLit ~ "{" ~ DeclarationModule ~ "}"

}

object Parser {
  def parse(input: String) = {
    val parser = new Parser

    parser.phrase(parser.DeclarationSourceFile)(new parser.lexical.Scanner(input)) match {
      case parser.Success(result, _) => Right(result)
      case parser.NoSuccess(errorMessage, next) => {
        Left(s"$errorMessage on line ${next.pos.line} on column ${next.pos.column}")
      }
    }
  }
}