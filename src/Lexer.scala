package lt.vu.mif.bentkus.bachelor.compiler.lexer

import scala.reflect.ClassTag

import java.io.File
import java.nio.file.Files

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
  case class Str(value: String) extends LexerToken
}

import LexerToken._


case class Span(start: Int, end: Int, buffer: Array[Byte]) {

  def char(pos: Int): Char =
    buffer(pos).asInstanceOf[Char]

  def takeWhile(predicate: Char => Boolean): SpanSplit = {
    var i = start
    while (i < end && predicate(char(i))) {
      i += 1
    }
    split(i - start)
  }

  def split(prefix: Int) =
    SpanSplit(Span(start, start + prefix, buffer), Span(start + prefix, end, buffer))

  def takeChar = split(1)

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

  def getString: String = getString(end - start)

  def getString(number: Int): String = {
    val end = scala.math.min(number, buffer.size)
    new String(buffer, start, end)
  }

  def char: Char = char(start)

  def isEmpty: Boolean = start >= end

  def withStart(f: Int => Int): Span =
    copy(start = f(start))

  def withEnd(f: Int => Int): Span =
    copy(end = f(end))

  def +(that: Span): Span = {
    if (buffer == that.buffer && end == that.start) {
      Span(start, that.end, buffer)
    } else {
      throw new Exception
    }
  }

  def +(that: SpanSplit) = that + this
}

case class SpanSplit(prefix: Span, suffix: Span) {
  def map[T](f: Span => T): (T, Span) = (f(prefix), suffix)

  def move(step: Int): SpanSplit =
    SpanSplit(prefix.withEnd(_ + step), suffix.withStart(_ + step))

  def +(that: Span): SpanSplit = {
    SpanSplit(that + prefix, suffix)
  }
}

object Span {
  def apply(buffer: Array[Byte]): Span = {
    Span(0, buffer.length, buffer)
  }
}

object Lexer {
  private val singleChars = Set('(', ')', '[', ']', '=', ',', '.', '+', '%', '{', '}', ':')

  def isIdentifier(ch: Char): Boolean =
    ch.isLetterOrDigit || ch == '_'

  def lex(buffer: Span): (LexerToken, Span) = {
    var pos = buffer.start

    val ch = buffer.char

    if (ch.isWhitespace) {
      buffer.takeWhile(_.isWhitespace).map(_.to[Whitespace])
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
    val (identifier, rest) = lex(buffer)

    Seq(identifier) ++ {
      if (rest.isEmpty)
        Seq.empty[LexerToken]
      else
        lexAll(rest)
    }
  }

  def lexAll(bytes: Array[Byte]): Seq[LexerToken] = {
    lexAll(Span(bytes))
  }

  def lexAll(string: String): Seq[LexerToken] = {
    lexAll(string.getBytes)
  }
}

object LexerApp extends App {
  val file = args.headOption.getOrElse {
    println("Please provide file to parse")
    System.exit(1)
    ???
  }

  val path = new File(file).toPath
  val bytes = Files.readAllBytes(path)

  val lexems = Lexer.lexAll(Span(bytes))
  println(lexems)
  lexems.foreach { byte =>
    print(byte.getString)
  }
}
