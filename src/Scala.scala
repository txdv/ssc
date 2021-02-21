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
  case class DefObject(name: String) extends Statement
}

object Scala {
  import Expression._

  def symbol(ch: Char): Parser[LexerToken] = token(sat(Symbol(ch.toString)))

  def identifierWithName(value: String): Parser[LexerToken] = token(sat(Identifier(value)))

  val `{` = symbol('{')
  val `}` = symbol('}')
  val `.` = symbol('.')

  val ident = token(sat[Identifier])

  val fullIdentifier = token(sepByN(ident, `.`))

  val `import`: Parser[Import] = for {
    ident <- identifierWithName("import")
    name <- fullIdentifier
    fullname = name.map(_.value).mkString(".")
  } yield Import(fullname)

  val statement: Parser[Expression] =
    `import`.asInstanceOf[Parser[Expression]]


  def main: Parser[Expression] = statement
}
