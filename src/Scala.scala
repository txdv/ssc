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

  def identifier(value: String): Parser[LexerToken] = token(sat(Identifier(value)))

  lazy val `{` = symbol('{')
  lazy val `}` = symbol('}')
  lazy val `.` = symbol('.')

  lazy val ident = token(sat[Identifier])

  val importStatement: Parser[Import] = for {
    ident <- ident
    if ident.value == "import"
  } yield Import(ident.value)

  def main: Parser[Expression] = for {
    ident <- identifier("import")
    name <- token(sepByN(identifier("a"), `.`))
    fullname = name.map(_.value).mkString(".")
  } yield Import(fullname)
}
