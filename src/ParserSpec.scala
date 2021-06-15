package ssc.parser

import ssc.Util
import ssc.lexer.LexerToken

import org.scalatest._
import flatspec._
import matchers._
import ssc.lexer.LexerToken._

class ParserSpec extends AnyFlatSpec with should.Matchers {

  "Parser" should "parse any token with any" in {
    Util.check {
      Parser.parse(Parser.any, "some".getBytes)
    } should be (Some(Identifier("some")))
  }

}
