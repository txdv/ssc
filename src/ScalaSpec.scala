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
    def ast[A](parser: Parser[A], debug: Boolean = false): Option[A] = Util.check {
      val result = Parser.parse(parser, string.stripMargin.getBytes)

      if (debug) {
        println(s"result: $result")
        Util.error(result)

      }
      result
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

  "defMethod" should "parse method with expr surrounded in { }" in {
    """
      |def method_name: Unit = {
      |  ???
      |}""".ast(Scala.defMethod) should be (Some(
        DefMethod("method_name")
      ))
  }

  "expr" should "parse symbol identifier reference" in {
    "???".ast(Scala.expr.ident) should be (Some(Ident("???")))
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

  "number" should "parse integer number" in {
    "123".ast(Scala.expr.number) should be (Some(Num("123")))
  }

  "function" should "parse a simple function call with no parameters" in {
    "method()".ast(Scala.expr.function) should be (Some(
      Func("method", arguments = Seq.empty)
    ))
  }

  "function" should "parse a simple function call with one parameter" in {
    """method(a)""".ast(Scala.expr.function) should be (Some(
      Func("method", Seq(Ident("a")))
    ))

    "func(1)".ast(Scala.expr.function) should be (Some(
      Func("func", Seq(Num("1")))
    ))
  }

  "function" should "parse a simple function call with two parameters" in {
    """method(a, b)""".ast(Scala.expr.function) should be (Some(
      Func("method", Seq(Ident("a"), Ident("b")))
    ))

    """method(a, 2)""".ast(Scala.expr.function) should be (Some(
      Func("method", Seq(Ident("a"), Num("2")))
    ))
  }

  "function" should "parse functions as arguments" in {
    "func(func(1))".ast(Scala.expr.function) should be (Some(
      Func("func", Seq(Func("func", Seq(Num("1")))))
    ))
  }

  "expr.all" should "parse values correctly" in {
    "1".ast(Scala.expr.all) should be (Some(Num("1")))
    "a".ast(Scala.expr.all) should be (Some(Ident("a")))
    "a(1, 2)".ast(Scala.expr.all) should be (Some(Func("a", arguments = Seq(Num("1"), Num("2")))))
    """println("Hello World!")""".ast(Scala.expr.all) should be (Some(
      Func("println", Seq(Stri("Hello World!")))
    ))
  }

  /*
  "defObject" should "parse scala hello world" in {
    """
      |object Main {
      |  def main(arg: Array[String]): Unit = {
      |    println("Hello World!")
      |  }
      |}
      |""".ast(Scala.defObject) should not be (None)
  }
  */
}
