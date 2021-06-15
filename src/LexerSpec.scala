package ssc.lexer

import org.scalatest._
import flatspec._
import matchers._
import LexerToken._

class LexerSpec extends AnyFlatSpec with should.Matchers {
  import Lexer.{lexAll => lex}

  "Lexer" should "lex identifiers and whitespaces" in {
    Lexer.lexAll("hello world") should be (Seq(
      Identifier("hello"), Whitespace(" "), Identifier("world")))
  } 

  it should "lex tabs and spaces as whitespace" in {
    Lexer.lexAll("\t \t") should be (Seq(Whitespace("\t \t")))
  } 

  it should "treat a string with a leading non number with a number suffix as an identifier" in {
    Lexer.lexAll("a123") should be (Seq(Identifier("a123")))
  }

  it should "treat string leading with number as a Number and the letters after an identifier" in {
    Lexer.lexAll("123abc") should be (Seq(Number("123"), Identifier("abc")))
  }

  it should "treat quoted strings as Str tokens" in {
    Lexer.lexAll("\"hello\"") should be (Seq(Str("\"hello\"")))
  }

  it should "treat strings starting with # as a comment" in {
    Lexer.lexAll("# hello world") should be (Seq(Comment("# hello world")))
  }

  it should "fail compilation with unexaustive check" in {
    val a: LexerToken = Comment("comment")

    a match {
      case Str(value) =>
      case _ =>
    }
  }

  it should "treat ??? as identifier" in {
    Lexer.lexAll("???") should be (Seq(Identifier("???")))
  }

  it should "lex true" in {
    lex("true") should be (Seq(Bool("true")))
  }

  it should "lex false" in {
    lex("true") should be (Seq(Bool("true")))
  }

  it should "lex if" in {
    lex("if") should be (Seq(If))
  }

  it should "else" in {
    lex("else") should be (Seq(Else))
  }

  it should "==" in {
    lex("==") should be (Seq(Symbol("==")))
  }
}
