package lt.vu.mif.bentkus.bachelor.compiler.parser

import lt.vu.mif.bentkus.bachelor.compiler.Util
import lt.vu.mif.bentkus.bachelor.compiler.lexer.LexerToken

import org.scalatest._
import flatspec._
import matchers._
import lt.vu.mif.bentkus.bachelor.compiler.lexer.LexerToken._

class ParserSpec extends AnyFlatSpec with should.Matchers {

  "Parser" should "parse any token with any" in {
    Util.check {
      Parser.parse(Parser.any, "some".getBytes)
    } should be (Some(Identifier("some")))
  }

}
