package lt.vu.mif.bentkus.bachelor.compiler.parser.scala

import lt.vu.mif.bentkus.bachelor.compiler.Util
import lt.vu.mif.bentkus.bachelor.compiler.lexer.LexerToken
import lt.vu.mif.bentkus.bachelor.compiler.lexer.LexerToken._
import lt.vu.mif.bentkus.bachelor.compiler.parser.Parser

import org.scalatest._
import flatspec._
import matchers._
import Expression.Import

class ScalaSpec extends AnyFlatSpec with should.Matchers {

  "Scala" should "parse simple import" in {
    Util.check {
      Parser.parse(Scala.main, "import some.other.value123".getBytes)
    } should be (Some(Import("some.other.value123")))
  }

  it should "parse imports only on the same line" in {
    Util.check {
      Parser.parse(Scala.main, "import some.other\n.value123".getBytes)
    } should be (None)
  }


}
