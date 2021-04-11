package lt.vu.mif.bentkus.bachelor.compiler.parser.scala

import lt.vu.mif.bentkus.bachelor.compiler.lexer.{Lexer, LexerToken}
import lt.vu.mif.bentkus.bachelor.compiler.lexer.LexerToken._
import lt.vu.mif.bentkus.bachelor.compiler.parser._
import lt.vu.mif.bentkus.bachelor.compiler.parser.Parser._

import scalaz.{Monad, MonadPlus}
import scalaz.syntax.monadPlus._

sealed trait Expression

object Expression {
  sealed trait Statement extends Expression

  case class Package(name: String) extends Statement
  case class Import(name: String) extends Statement
  case class DefObject(name: String, statements: Seq[Statement] = Seq.empty) extends Statement

  trait ScalaType extends Expression
  case class SimpleType(name: String) extends ScalaType
  case class GenericType(name: String, generics: Seq[ScalaType]) extends ScalaType

  case class DefMethod(
    name: String,
    returnType: ScalaType,
    arguments: Seq[DefMethodArgument] = Seq.empty,
    body: Option[Expr] = None) extends Statement

  case class DefMethodArgument(name: String, argumentType: ScalaType)

  sealed trait Expr extends Statement

  case class Ident(name: String) extends Expr
  case class Func(name: String, arguments: Seq[Expr]) extends Expr
  case class Num(value: String) extends Expr
  case class Stri(value: String) extends Expr

  case class ExprOp(char: Char, left: Expr, right: Expr) extends Expr
}

object Scala {
  import Expression._

  def symbol(ch: Char): Parser[LexerToken] = token(sat(Symbol(ch.toString)))

  def symbol(chs: Seq[Char]): Parser[LexerToken] = token(sat2 {
    case Symbol(ch) =>
      chs.contains(ch)
    case _ =>
      false
  })

  def identifierWithName(value: String): Parser[LexerToken] =
    token(sat(Identifier(value)))

  def identifierWithNames(values: Seq[String]): Parser[LexerToken] = token {
    sat2(values.map(Identifier(_)).contains)
  }

  val identifier = token(sat[Identifier])

  val `[` = symbol('[')
  val `]` = symbol(']')
  val `(` = symbol('(')
  val `)` = symbol(')')
  val `{` = symbol('{')
  val `}` = symbol('}')
  val `.` = symbol('.')
  val `,` = symbol(',')
  val `:` = symbol(':')
  val `=` = symbol('=')

  val fullIdentifier = token(sepBy(sat[Identifier], `.`))

  def typeDef: Parser[ScalaType] = for {
    name <- identifier
    generic <- one {
      for {
        _ <- `[`
        generics <- sepBy(typeDef, `,`)
        _ <- `]`
      } yield generics
    }
  } yield {
    generic.map { types =>
      GenericType(name.value, types)
    } getOrElse {
      SimpleType(name.value)
    }
  }

  val `import`: Parser[Import] = for {
    _ <- identifierWithName("import")
    name <- fullIdentifier
    fullname = name.map(_.value).mkString(".")
  } yield Import(fullname)

  val defObject: Parser[DefObject] = for {
    _ <- identifierWithName("object")
    name <- identifier
    _ <- `{`
    statements <- objectStatements
    _ <- `}`
  } yield DefObject(name.value, statements)

  val objectStatement =
    defMethod

  val objectStatements =
    many(objectStatement)

  def defMethodArgument: Parser[DefMethodArgument] = for {
    name <- identifier
    _ <- `:`
    argumentType <- typeDef
  } yield DefMethodArgument(name.value, argumentType)

  def methodArgumentsGroup: Parser[Seq[DefMethodArgument]] = for {
    _ <- `(`
    args <- sepBy(defMethodArgument, `,`)
    _ <- `)`
  } yield args

  def methodArguments: Parser[Seq[DefMethodArgument]] = for {
    args <- one(methodArgumentsGroup)
  } yield args.getOrElse(Seq.empty)

  def defMethod: Parser[DefMethod] = for {
    _ <- identifierWithName("def")
    name <- identifier
    arguments <- methodArguments
    _ <- `:`
    returnType <- typeDef
    _ <- `=`
    expr <- expr.all
  } yield DefMethod(name.value, returnType, arguments, body = Some(expr))

  object expr {
    val number: Parser[Expr] = for {
      number <- token(sat[Number])
    } yield Num(number.value)

    val ident: Parser[Expr] = for {
      ident <- identifier
    } yield Ident(ident.value)

    /*
    val function: Parser[Expr] = for {
      func <- sat[Identifier]
      _ <- `(`
      arguments <- sepBy(expr.all, `,`)
      _ <- `)`
    } yield Func(func.value, arguments)
    */

    val string: Parser[Expr] = for {
      string <- token(sat[Str])
    } yield Stri(string.unquoted)

    val function: Parser[Expr] = for {
      name <- identifier
      args <- one(expressionGroup)
    } yield {
      args.map { args =>
        Func(name.value, args)
      } getOrElse {
        Ident(name.value)
      }
    }

    val expressionGroup = for {
      _ <- `(`
      expressions <- sepBy(expr.all, `,`)
      _ <- `)`
    } yield expressions

    def grouped: Parser[Expr] = for {
      _ <- `{`
      all <- all
      _ <- `}`
    } yield all

    // TODO: supports only numbers :/
    def op: Parser[Expr] = for {
      left <- number
      Symbol(ch) <- symbol('+') +++ symbol('-') +++ symbol('*') +++ symbol('/')
      right <- number
    } yield ExprOp(ch(0), left, right)

    def all: Parser[Expr] =
      op +++
      number +++
      string +++
      function +++
      grouped
  }

  val statement: Parser[Expression] =
    `import`.asInstanceOf[Parser[Expression]] +++
    defObject.asInstanceOf[Parser[Expression]]

  val main: Parser[List[Expression]] = many(token(statement))

}
