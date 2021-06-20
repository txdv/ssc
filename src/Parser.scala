package ssc.parser

import scalaz.{Monad, MonadPlus}
import scalaz.syntax.monadPlus._
import scala.reflect.ClassTag
import ssc.lexer.{Lexer, LexerToken}
import ssc.lexer.LexerToken._

case class Parser[+A](run: List[LexerToken] => List[(A, List[LexerToken])]){
  val t: List[LexerToken] => List[(A, List[LexerToken])] = { input =>
    val output = run(input)
    //println(s"$input -> $output")
    output
  }
  
  def +++(other: Any): Parser[A] = {
    other match {
      case b: Parser[A] =>
        Parser[A](cs => t(cs) ++ b.asInstanceOf[Parser[A]].t(cs) match {
          case Nil => Nil
          case x :: _ => List(x)
        })
    }
  }
}

object Parser {
  implicit val parserMonad = new MonadPlus[Parser] {
    override def empty[A]: Parser[A] = Parser[A](_ => Nil)

    override def plus[A](a: Parser[A], b: => Parser[A]): Parser[A] = Parser(cs => a.run(cs) ++ b.run(cs))

    override def point[B](a: => B): Parser[B] = Parser[B](cs => List((a, cs)))

    override def bind[B, C](fa: Parser[B])(f: B => Parser[C]): Parser[C] = Parser[C](cs => {
      fa.run(cs).flatMap { case (a, ls) => f(a).run(ls) }
    })
  }

  def any: Parser[LexerToken] = Parser[LexerToken] {
    case Nil => Nil
    case x :: xs => List((x, xs))
  }

  /*
  def sat(value: LexerToken): Parser[LexerToken] = for {
    c <- any
    if c == value
  } yield c
  */
  def sat(value: LexerToken): Parser[LexerToken] = sat2(_ == value)

  def sat2(f: LexerToken => Boolean): Parser[LexerToken] = for {
    c <- any
    if f(c)
  } yield c

  // TODO: maybe shorten this
  def sat[T <: LexerToken](implicit classTag: ClassTag[T]): Parser[T] = for {
    c <- any
    if c.getClass == classTag.runtimeClass
  } yield c.asInstanceOf[T]

  /*
  def sat2[T <: LexerToken](f: LexerToken => Boolean)(implicit classTag: ClassTag[T]): Parser[T] = for {
    c <- any
    if c.getClass == classTag.runtimeClass && f(c)
  } yield c.asInstanceOf[T]

  def sat3[T <: LexerToken](token: LexerToken)(implicit classTag: ClassTag[T]): Parser[T] = sat2(_ == token)
  */

  lazy val space: Parser[List[LexerToken]] =
    many(sat[Whitespace].map(_.asInstanceOf[LexerToken]) +++
    sat[Comment].map(_.asInstanceOf[LexerToken]))

  def token[A](parser: Parser[A]): Parser[A] = for {
    a <- parser
    _ <- space
  } yield a


  /*
  def char(char: Char): Parser[Char] = sat(char == _)

  def string(str: String): Parser[List[Char]] = string(str.toList)

  def string(str: List[Char]): Parser[List[Char]] = str match {
    case Nil => Monad[Parser].pure[List[Char]]("".toList)
    case x :: xs => for {
      c <- char(x)
      cs <- string(xs)
    } yield c :: cs
  }
k
*/
  def lift[A](value: A): Parser[A] =
    Monad[Parser].pure[A](value)

  def one[A](parser: Parser[A]): Parser[Option[A]] = (for {
    a <- parser
  } yield Option(a)) +++ lift(None)

  def many[A](parser: Parser[A]): Parser[List[A]] = {
    many1(parser) +++ lift(Nil)
  }

  def many1[A](parser: Parser[A]): Parser[List[A]] = for {
    a <- parser
    as <- many(parser)
  } yield a :: as

  def sepBy[A, B](parser: Parser[A], separator: Parser[B]): Parser[List[A]] = {
    sepBy1(parser, separator) +++ lift(Nil)
  }

  def sepBy1[A, B](parser: Parser[A], separator: Parser[B]): Parser[List[A]] = for {
    a <- parser
    as <- many(for { _ <- separator; p <- parser } yield p)
  } yield a :: as

  def sepByT[A, B](parser: Parser[A], separator: Parser[B]): Parser[List[A]] = {
    sepByT1(parser, separator) +++ lift(Nil)
  }

  def sepByT1[A, B](parser: Parser[A], separator: Parser[B]): Parser[List[A]] = for {
    a <- parser
    _ <- separator
    as <- many(for { p <- parser; _ <- separator } yield p)
  } yield a :: as

  def chainl[A](parser: Parser[A], opParser: Parser[(A, A) => A], value: A): Parser[A] = {
    chainl1(parser, opParser) +++ lift(value)
  }

  def chainl1[A](parser: Parser[A], opParser: Parser[(A, A) => A]): Parser[A] = {
    def rest(v: A): Parser[A] = (for {
      f <- opParser
      b <- parser
      r <- rest(f(v, b))
    } yield r) +++ lift(v)

    for {
      a <- parser
      b <- rest(a)
    } yield b
  }
/*
  val space: Parser[List[Char]] = many(sat(_ == ' '))

  val whitespace: Parser[List[Char]] = many(sat(_.isWhitespace))

  def token[A](parser: Parser[A], space: Parser[List[Char]] = space): Parser[A] = for {
    a <- parser
    _ <- space
  } yield a

  def symbol(cs: List[Char]): Parser[List[Char]] = token (string(cs))
  */

  def parseTokens[A](parser: Parser[A], tokens: List[LexerToken]): List[(A, List[LexerToken])] = {
    for {
      _ <- space
      p <- parser
    } yield p
  }.run(tokens)

  def parse[A](parser: Parser[A], content: Array[Byte]): List[(A, List[LexerToken])] = {
    val tokens = Lexer.lexAll(content)
    parseTokens[A](parser, tokens.toList)
  }
}
