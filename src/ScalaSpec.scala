package lt.vu.mif.bentkus.bachelor.compiler.parser.scala

import lt.vu.mif.bentkus.bachelor.compiler.Util
import lt.vu.mif.bentkus.bachelor.compiler.lexer.LexerToken
import lt.vu.mif.bentkus.bachelor.compiler.lexer.LexerToken._
import lt.vu.mif.bentkus.bachelor.compiler.parser.Parser

import org.scalatest._
import flatspec._
import matchers._
import Expression._

class ScalaSpec extends AnyFlatSpec with should.Matchers {

  "import" should "parse a simple import" in {
    Util.check {
      Parser.parse(Scala.`import`, "import some.other.value123\n".getBytes)
    } should be (Some(Import("some.other.value123")))
  }

  "import" should "parse an import only if the imported name is on one line" in {
    Util.check {
      Parser.parse(Scala.`import`, "import some.other\n.value123".getBytes)
    } should be (None)
  }

  "main" should "parse a singular import" in {
    Util.check {
      Parser.parse(Scala.main, "import some.other.value123".getBytes)
    } should be (Some(List(Import("some.other.value123"))))
  }

  "main" should "parse a multiple imports" in {
    Util.check {
      Parser.parse(Scala.main, "import some.other.value123\nimport some.other.value2".getBytes)
    } should be (Some(List(Import("some.other.value123"), Import("some.other.value2"))))
  }

  "defObject" should "parse empty object definition" in {
    Util.check {
      Parser.parse(Scala.defObject, "object empty{}".getBytes)
    } should be (Some(DefObject("empty")))
  }

  "main" should "parse multiple statements" in {
    Util.error {
      Parser.parse(Scala.main, "import a.b.c\nobject empty{}\n".getBytes)
    } should be (Some(List(Import("a.b.c"), DefObject("empty"))))
  }
}
