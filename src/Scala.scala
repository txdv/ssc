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

  case class DefMethod(name: String) extends Statement

  sealed trait Expr extends Statement

  case class Ident(name: String) extends Expr
}

object Scala {
  import Expression._

  def symbol(ch: Char): Parser[LexerToken] = token(sat(Symbol(ch.toString)))

  def identifierWithName(value: String): Parser[LexerToken] = token(sat(Identifier(value)))

  val identifier = token(sat[Identifier])

  val `{` = symbol('{')
  val `}` = symbol('}')
  val `.` = symbol('.')
  val `:` = symbol(':')
  val `=` = symbol('=')

  val fullIdentifier = token(sepBy(sat[Identifier], `.`))

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

  def defMethod: Parser[DefMethod] = for {
    _ <- identifierWithName("def")
    name <- identifier
    _ <- `:`
    returnType <- identifierWithName("Unit")
    _ <- `=`
    _ <- expr.all
  } yield DefMethod(name.value)


  object expr {
    val ident: Parser[Expr] = for {
      ident <- identifier
    } yield Ident(ident.value)

    def grouped: Parser[Expr] = for {
      _ <- `{`
      all <- all
      _ <- `}`
    } yield all

    def all: Parser[Expr] =
      ident +++ grouped
  }

  val statement: Parser[Expression] =
    `import`.asInstanceOf[Parser[Expression]] +++
    defObject.asInstanceOf[Parser[Expression]]

  val main: Parser[List[Expression]] = many(token(statement))

}
