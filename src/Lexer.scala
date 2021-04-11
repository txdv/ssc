package lt.vu.mif.bentkus.bachelor.compiler.lexer

import lt.vu.mif.bentkus.bachelor.compiler.span._

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
  private val singleChars = Set('(', ')', '[', ']', '=', ',', '.', '+', '-', '*', '%', '{', '}', ':')

  private val special = Set('?', '!')

  // TODO: question marks are weird in scala
  // do something more appropriate
  def isIdentifier(ch: Char): Boolean =
    ch.isLetterOrDigit || Seq('_').contains(ch)

  def lex(buffer: Span): (LexerToken, Span) = {
    var pos = buffer.start

    val ch = buffer.char

    if (ch.isWhitespace) {
      buffer.takeWhile(_.isWhitespace).map(_.to[Whitespace])
    } else if (special.contains(ch)) {
      buffer.takeWhile(special.contains).map(_.to[Identifier])
    } else if (ch.isLetter) {
      buffer.takeWhile(isIdentifier).map(_.to[Identifier])
    } else if (singleChars.contains(ch)) {
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
