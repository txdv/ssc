package lt.vu.mif.bentkus.bachelor.compiler.parser.scala

import lt.vu.mif.bentkus.bachelor.compiler.Util
import lt.vu.mif.bentkus.bachelor.compiler.lexer.LexerToken
import lt.vu.mif.bentkus.bachelor.compiler.lexer.LexerToken._
import lt.vu.mif.bentkus.bachelor.compiler.parser.Parser

import org.scalatest._
import flatspec._
import matchers._
import Expression._

object Helper {
  implicit class StringOps(val string: String) {
    def ast[A](parser: Parser[A]): Option[A] = Util.check {
      Parser.parse(parser, string.stripMargin.getBytes)
    }

  }
}

class ScalaSpec extends AnyFlatSpec with should.Matchers {
  import Helper._

  "import" should "parse a simple import" in {
    "import some.other.value123\n".ast(Scala.`import`) should be (Some(
      Import("some.other.value123")
    ))
  }

  "import" should "parse an import only if the imported name is on one line" in {
    "import some.other\n.value123".ast(Scala.`import`) should be (None)
  }

  "main" should "parse a singular import" in {
    "import some.other.value123".ast(Scala.main) should be (Some(
      List(Import("some.other.value123"))
    ))
  }

  "main" should "parse a multiple imports" in {
    val src = "import some.other.value123\nimport some.other.value2"
    src.ast(Scala.main) should be (Some(
      List(Import("some.other.value123"), Import("some.other.value2"))
    ))
  }

  "defMethod" should "parse simple method definition" in {
    "def method_name: Unit = ???".ast(Scala.defMethod) should be (Some(
      DefMethod("method_name")
    ))
  }

  "expr" should "parse symbol identifier reference" in {
    "???".ast(Scala.expr.ident) should be (Some(Ident("???")))
  }

  "defMethod" should "parse method with expr surrounded in { }" in {
    """
      |def method_name: Unit = {
      |  ???
      |}""".ast(Scala.defMethod) should be (Some(
        DefMethod("method_name")
      ))
  }

  "defObject" should "parse empty object definition" in {
    "object empty{}".ast(Scala.defObject) should be (Some(DefObject("empty")))
  }

  "defObject" should "parse multiple statements" in {
    "import a.b.c\nobject empty{}\n".ast(Scala.main) should be (Some(
      List(Import("a.b.c"), DefObject("empty"))
    ))
  }

  "defObject" should "parse an empty method in an object" in {
    """
      |object Main {
      |  def methodName: Unit = ???
      |}
      |""".ast(Scala.defObject) should be (Some(
        DefObject("Main", Seq(DefMethod("methodName")))
      ))
  }
}
