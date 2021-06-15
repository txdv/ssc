package ssc.parser.scala

import ssc.lexer.{Lexer, LexerToken}
import ssc.lexer.LexerToken._
import ssc.parser._
import ssc.parser.Parser._

import scalaz.{Monad, MonadPlus}
import scalaz.syntax.monadPlus._

sealed trait AST

object AST {
  sealed trait Statement extends AST

  case class Package(name: String) extends Statement
  case class Import(name: String) extends Statement
  case class ObjectDecl(name: String, statements: Seq[Statement] = Seq.empty) extends Statement

  sealed trait ScalaType extends AST
  case class SimpleType(name: String) extends ScalaType
  case class GenericType(name: String, generics: Seq[ScalaType]) extends ScalaType

  case class VarDecl(name: String, scalaType: ScalaType)

  case class MethodDecl(
    name: String,
    returnType: ScalaType,
    arguments: Seq[MethodDeclArgument] = Seq.empty,
    body: Option[Expr] = None) extends Statement

  case class MethodDeclArgument(name: String, argumentType: ScalaType)

  sealed trait Expr extends Statement {
    val depth: Int
  }

  case class Ident(name: String) extends Expr {
    val depth: Int = 1
  }

  case class Func(name: String, arguments: Seq[Expr]) extends Expr {
    val namespace: Seq[String] = {
      val total = name.split("\\.")
      total.take(total.length - 1)
    }

    val methodName: String = {
      val total = name.split("\\.")
      total.last
    }

    val depth: Int = 1
  }
  case class Num(value: String) extends Expr {
    val depth: Int = 1
  }
  case class Stri(value: String) extends Expr {
    val depth: Int = 1
  }
  case class Bool(value: Boolean) extends Expr {
    val depth: Int = 1
  }

  case class ExprOp(char: String, left: Expr, right: Expr) extends Expr {
    val depth: Int = left.depth + right.depth
  }

  case class If(cond: Expr, left: Expr, right: Expr) extends Expr {
    val depth: Int = 1
  }
}

object Scala {
  import AST._

  def symbol(ch: Char): Parser[LexerToken] = token(sat(Symbol(ch.toString)))

  def symbol(chs: Seq[Char]): Parser[LexerToken] = token(sat2 {
    case Symbol(ch) =>
      chs.contains(ch)
    case _ =>
      false
  })

  def strSymbol(strings: Seq[String]): Parser[LexerToken] = token {
    sat2 {
      case Symbol(string) =>
        strings.contains(string)
      case _ =>
        false
    }
  }

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

  val fullIdentifier = sepBy1(identifier, `.`)

  val fidentifier: Parser[Ident] = for {
    ident <- fullIdentifier
    fullname = ident.map(_.value).mkString(".")
  } yield Ident(fullname)

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

  val defObject: Parser[ObjectDecl] = for {
    _ <- identifierWithName("object")
    name <- identifier
    _ <- `{`
    statements <- objectStatements
    _ <- `}`
  } yield ObjectDecl(name.value, statements)

  val objectStatement =
    defMethod

  val objectStatements =
    many(objectStatement)

  def defMethodArgument: Parser[MethodDeclArgument] = for {
    name <- identifier
    _ <- `:`
    argumentType <- typeDef
  } yield MethodDeclArgument(name.value, argumentType)

  def methodArgumentsGroup: Parser[Seq[MethodDeclArgument]] = for {
    _ <- `(`
    args <- sepBy(defMethodArgument, `,`)
    _ <- `)`
  } yield args

  def methodArguments: Parser[Seq[MethodDeclArgument]] = for {
    args <- one(methodArgumentsGroup)
  } yield args.getOrElse(Seq.empty)

  def defMethod: Parser[MethodDecl] = for {
    _ <- identifierWithName("def")
    name <- identifier
    arguments <- methodArguments
    _ <- `:`
    returnType <- typeDef
    _ <- `=`
    expr <- expr.all
  } yield MethodDecl(name.value, returnType, arguments, body = Some(expr))

  val varDecl: Parser[VarDecl] = for {
    _ <- identifierWithName("val")
    name <- identifier
    _ <- `:`
    `type` <- typeDef
  } yield VarDecl(name.value, `type`)

  object expr {
    val number: Parser[Expr] = for {
      number <- token(sat[Number])
    } yield Num(number.value)

    val bool: Parser[Expr] = for {
      bool <- token(sat[LexerToken.Bool])
    } yield Bool(bool.value == "true")

    val string: Parser[Expr] = for {
      string <- token(sat[Str])
    } yield Stri(string.unquoted)

    val function: Parser[Expr] = for {
      fullname <- fidentifier
      args <- one(expressionGroup)
    } yield {
      args.map { args =>
        Func(fullname.asInstanceOf[Ident].name, args)
      } getOrElse {
        fullname
      }
    }

    val ifExpr: Parser[Expr] = for {
      _ <- token(sat(LexerToken.If))
      _ <- `(`
      cond <- all
      _ <- `)`
      left <- all
      _ <- token(sat(LexerToken.Else))
      right <- all
    } yield If(cond, left, right).asInstanceOf[Expr]

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

    val constants: Parser[Expr] =
      number +++
      string +++
      bool +++
      function +++
      ifExpr

    val emptyToken = Parser.parserMonad.empty[LexerToken]

    def ops2(symbols: Seq[String]): Parser[(Expr, Expr) => Expr] = for {
      Symbol(sym) <- strSymbol(symbols)
      p <- lift { (a, b) =>
        ExprOp(sym, a, b)
      }
    } yield p


    def ops(symbols: Seq[Char]): Parser[(Expr, Expr) => Expr] = for {
      Symbol(sym) <- symbols.foldLeft(emptyToken) { case (parser, ch) =>
        parser +++ symbol(ch)
      }
      p <- lift { (a, b) =>
        ExprOp(sym, a, b)
      }
    } yield p

    val op: Parser[Expr] = chainl1(term1, ops2(Seq("==")))
    lazy val term1: Parser[Expr] = chainl1(term, ops(Seq('+', '-')))
    lazy val term: Parser[Expr] = chainl1(factor, ops(Seq('*', '/')))

    lazy val factor: Parser[Expr] = constants +++ (for {
      _ <- symbol('(')
      e <- op
      _ <- symbol(')')
    } yield e)

    def all: Parser[Expr] = {
      op +++
      grouped
    }
  }

  val statement: Parser[AST] =
    `import`.asInstanceOf[Parser[AST]] +++
    defObject.asInstanceOf[Parser[AST]]

  val main: Parser[List[AST]] = many(token(statement))

}
