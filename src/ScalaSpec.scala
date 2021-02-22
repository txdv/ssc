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

  "defMethod" should "parse simple method definition" in {
    Util.check {
      Parser.parse(Scala.defMethod, "def method_name: Unit = ???".getBytes)
    } should be (Some(DefMethod("method_name")))
  }

  "expr" should "parse symbol identifier reference" in {
    Util.check {
      Parser.parse(Scala.expr.ident, "???".getBytes)
    } should be (Some(Ident("???")))

  }

  "defMethod" should "parse method with expr surrounded in { }" in {
    val src = """
      |def method_name: Unit = {
      |  ???
      |}""".stripMargin.getBytes

    Util.check {
      Parser.parse(Scala.defMethod, src)
    } should be (Some(DefMethod("method_name")))
  }

  "defObject" should "parse empty object definition" in {
    Util.check {
      Parser.parse(Scala.defObject, "object empty{}".getBytes)
    } should be (Some(DefObject("empty")))
  }

  "defObject" should "parse multiple statements" in {
    Util.check {
      Parser.parse(Scala.main, "import a.b.c\nobject empty{}\n".getBytes)
    } should be (Some(List(Import("a.b.c"), DefObject("empty"))))
  }

  "defObject" should "parse an empty method in an object" in {
    val src = """
      |object Main {
      |  def method: Unit = ???
      |}
      |""".stripMargin

    Util.check {
      Parser.parse(Scala.defObject, src.getBytes)
    } should not be (None)
  }
}
