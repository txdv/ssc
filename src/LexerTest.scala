package lt.vu.mif.bentkus.bachelor.compiler.lexer

import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._
import LexerToken.{
  IdentifierToken => Identifier,
  NumberToken => Number,
  WhitespaceToken => Whitespace,
}

class LexerSpec extends AnyFlatSpec with should.Matchers {

  "Lexer" should "lex identifiers and whitespaces" in {
    Lexer.lexAll("hello world") should be (Seq(
      Identifier("hello"), Whitespace(" "), Identifier("world")))
  } 

  it should "lex tabs and spaces as whitespace" in {
    Lexer.lexAll("\t \t") should be (Seq(Whitespace("\t \t")))
  } 

  it should "lex a leading symbol with a number suffix as an identifier" in {
    Lexer.lexAll("a123") should be (Seq(Identifier("a123")))
  }

  it should "lex a leading number as a Number and the letters after an identifier" in {
    Lexer.lexAll("123abc") should be (Seq(Number("123"), Identifier("abc")))
  }
}
