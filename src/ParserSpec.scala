package lt.vu.mif.bentkus.bachelor.compiler.parser

import org.scalatest._
import flatspec._
import matchers._
import lt.vu.mif.bentkus.bachelor.compiler.lexer.LexerToken._

class ParserSpec extends AnyFlatSpec with should.Matchers {

  "Parser" should "parse any token with any" in {
    //val List((success, rest)) = Parser.parse(Parser.any, List(Comment("asd")))
    val success = Parser.parse(Parser.any, List(Comment("asd")))
    println(success)
    1 should be (2)
  }

}
