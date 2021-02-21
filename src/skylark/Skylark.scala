package lt.vu.mif.bentkus.bachelor.compiler.parser.skylark

import lt.vu.mif.bentkus.bachelor.compiler.lexer.{Lexer, LexerToken}
import lt.vu.mif.bentkus.bachelor.compiler.lexer.LexerToken._
import lt.vu.mif.bentkus.bachelor.compiler.parser._
import lt.vu.mif.bentkus.bachelor.compiler.parser.Parser._
import scalaz.{Monad, MonadPlus}
import scalaz.syntax.monadPlus._

sealed trait Expression

case class EString(value: String) extends Expression
case class EList(expressions: Seq[Expression]) extends Expression
case class EFunction(name: String, arguments: Seq[Argument]) extends Expression
case class EInt(value: Int) extends Expression
case class EBoolean(value: Boolean) extends Expression
case class EOperator(name: String, left: Expression, right: Expression) extends Expression
case class EIdentifier(name: String) extends Expression
case class EHash(elements: Seq[EHashElement]) extends Expression

case class EAssignment(name: String, value: Expression) extends Expression

case class EHashElement(key: Expression, value: Expression)

sealed trait Argument
case class NamedArgument(name: String, expression: Expression) extends Argument
case class UnnamedArgument(expression: Expression) extends Argument

object Skylark {

  def char(ch: Char): Parser[LexerToken] = token(sat(Symbol(ch.toString)))

  lazy val `[` = char('[')
  lazy val `]` = char(']')
  lazy val `(` = char('(')
  lazy val `)` = char(')')
  lazy val `{` = char('{')
  lazy val `}` = char('}')
  lazy val `:` = char(':')
  lazy val comma = char(',')

  lazy val identifier = token(sat[Identifier])

  def isOperator(ch: LexerToken): Boolean =
    ch == Symbol("+") ||
    ch == Symbol("%")

  lazy val expressions = for {
    funcs <- many(token(expr))
  } yield funcs

  lazy val expr: Parser[Expression] =
    chainl1(term, operation)

  lazy val operation = for {
    op <- char('+') +++ char('%')
    p <- Monad[Parser].pure((left: Expression, right: Expression) => EOperator(op.toString, left, right).asInstanceOf[Expression])
  } yield p

  lazy val term: Parser[Expression] =
    int +++
    string +++
    list +++
    hash +++
    assignment +++
    function

  lazy val int: Parser[Expression] = for {
    number <- token(sat[Number])
  } yield EInt(number.value.toInt)

  lazy val string: Parser[Expression] = for {
    string <- token(sat[Str])
  } yield EString(string.value)

  lazy val list: Parser[Expression] = for {
    _ <- `[`
    expressions <- token(sepByN(expr, comma))
    _ <- `]`
  } yield EList(expressions)

  lazy val hash: Parser[Expression] = for {
    _ <- `{`
    elements <- token(sepByN(hashElement, comma))
    _ <- `}`
  } yield EHash(elements)

  lazy val hashElement: Parser[EHashElement] = for {
    key <- expr
    _ <- `:`
    value <- expr
  } yield EHashElement(key, value)

  lazy val function: Parser[Expression] = for {
    name <- identifier
    args <- one(expressionGroup)
  } yield {
    args.map { args =>
      EFunction(name.value, args)
    } getOrElse {
      EIdentifier(name.value)
    }
  }

  lazy val expressionGroup = for {
    _ <- `(`
    args <- arguments
    _ <- `)`
  } yield args

  lazy val arguments = for {
    result <- sepByN(argument, comma)
  } yield result

  lazy val argument = for {
    arg <- namedArgument +++ unnamedArgument
  } yield arg

  lazy val unnamedArgument: Parser[Argument] = for {
    expr <- expr
  } yield UnnamedArgument(expr)

  lazy val namedArgument: Parser[Argument] = for {
    name <- identifier
    _ <- char('=')
    expr <- expr
  } yield NamedArgument(name.value, expr)

  lazy val assignment: Parser[Expression] = namedArgument.map { arg =>
    val narg = arg.asInstanceOf[NamedArgument]
    EAssignment(narg.name, narg.expression)
  }
}
