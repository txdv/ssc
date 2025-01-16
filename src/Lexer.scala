package ssc.lexer

import ssc.span._

import scala.reflect.ClassTag

sealed trait LexerToken {
  val value: String

  def getString: String = value
}

object LexerToken {
  case class Comment(value: String) extends LexerToken
  case class Number(value: String) extends LexerToken
  case class Symbol(value: String) extends LexerToken
  case class Identifier(value: String) extends LexerToken
  case class Bool(value: String) extends LexerToken
  case class Whitespace(value: String) extends LexerToken
  case class Str(value: String) extends LexerToken {
    val unquoted = value.substring(1, value.length - 1)
  }

  case object If extends LexerToken {
    val value: String = "if"

  }
  case object Else extends LexerToken {
    val value: String = "else"
  }

  case class Operator(value: String) extends LexerToken
}

import LexerToken._


object SpanExtensions {
  implicit class Converter(span: Span) {

    import span._

    def to[T <: LexerToken](implicit classTag: ClassTag[T]): T = {
      val klass = classTag.runtimeClass
      (if (klass == classOf[Identifier]) {
        Identifier(getString)
      } else if (klass == classOf[Whitespace]) {
        Whitespace(getString)
      } else if (klass == classOf[Number]) {
        Number(getString)
      } else if (klass == classOf[Symbol]) {
        Symbol(getString)
      } else if (klass == classOf[Str]) {
        Str(getString)
      } else if (klass == classOf[Comment]) {
        Comment(getString)
      } else {
        throw new Exception
      }).asInstanceOf[T]
    }
  }
}

import SpanExtensions._


object Lexer {
  private val singleChars = Set("(", ")", "[", "]", "=", ",", ".", "+", "-", "*", "%", "{", "}", ":")

  private val special = Set("?", "!")

  val keywords = Map(
    "if" -> If,
    "else" -> Else,
    "true" -> Bool("true"),
    "false" -> Bool("false"),
    "==" -> Symbol("=="),
  )

  //val keywordLetters = keywords.map { case (k, v) => k(0) }.toArray

  def isKeyword(buffer: Span): Boolean = {
    keywords.find { case (prefix, _) =>
      buffer.startsWith(prefix)
    }.nonEmpty
  }

  // TODO: question marks are weird in scala
  // do something more appropriate
  def isIdentifier(ch: Char): Boolean =
    ch.isLetterOrDigit || Seq('_').contains(ch)

  def lex(buffer: Span): (LexerToken, Span) = {
    var pos = buffer.start

    val ch = buffer.char

    if (ch.isWhitespace) {
      buffer.takeWhile(_.isWhitespace).map(_.to[Whitespace])
    } else if (isKeyword(buffer)) {
      val (prefix, token) = keywords.find { case (prefix, _) =>
        buffer.startsWith(prefix)
      }.get
      (token, buffer.split(token.value.length).suffix)
    } else if (special.contains(ch.toString)) {
      buffer.takeWhile(c => special.contains(c.toString)).map(_.to[Identifier])
    } else if (ch.isLetter) {
      buffer.takeWhile(isIdentifier).map(_.to[Identifier])
    } else if (singleChars.contains(ch.toString)) {
      buffer.takeChar.map(_.to[Symbol])
    } else if (ch == '"') {
      // TODO: support escaping
      val SpanSplit(prefix, suffix) = buffer.takeChar
      (prefix + suffix.takeWhile(_ != '"').move(1)).map(_.to[Str])
    } else if (ch.isDigit) {
      buffer.takeWhile(_.isDigit).map(_.to[Number])
    } else if (ch == '#') {
      val SpanSplit(prefix, suffix) = buffer.takeChar
      (prefix + suffix.takeWhile(_ != '\n').move(1)).map(_.to[Comment])
    } else {
      throw new Exception(s"can't parse symbol '$ch' which starts at ${buffer.start}: ${buffer.getString}")
    }
  }

  def lexAll(buffer: Span): Seq[LexerToken] = {
    var result = Vector.empty[LexerToken]
    var buf: Span = buffer

    do {
      val lexres = lex(buf)
      result = result :+ lexres._1
      buf = lexres._2
    } while (!buf.isEmpty)

    result
  }

  def lexAll(bytes: Array[Byte]): Seq[LexerToken] = {
    lexAll(Span(bytes))
  }

  def lexAll(string: String): Seq[LexerToken] = {
    lexAll(string.getBytes)
  }
}
