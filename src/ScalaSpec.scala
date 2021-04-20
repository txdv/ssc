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
  val `???` = Ident("???")
  val `Int` = SimpleType("Int")
  val `String` = SimpleType("String")
  val `Unit` = SimpleType("Unit")


  import Helper._

  "import" should "parse a simple import" in {
    "import some.other.value123\n".ast(Scala.`import`) should be (Some(
      Import("some.other.value123")
    ))
  }

  "import" should "parse import with new lines inbetween" in {
    "import some\n.other\n.value123\n".ast(Scala.`import`) should be (Some(
      Import("some.other.value123")
    ))
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

  "typeDef" should "parse simple types" in {
    "Int".ast(Scala.typeDef) should be (Some(`Int`))
    "String".ast(Scala.typeDef) should be (Some(`String`))
  }

  "typeDef" should "parse generic types" in {
    "Option[Int]".ast(Scala.typeDef) should be (Some(
      GenericType("Option", Seq(`Int`))
    ))
    "List[String]".ast(Scala.typeDef) should be (Some(
      GenericType("List", Seq(`String`))
    ))
  }

  "defMethod" should "parse simple method definition" in {
    "def method_name: Unit = ???".ast(Scala.defMethod) should be (Some(
      DefMethod("method_name", `Unit`, body = Option(`???`))
    ))
  }

  "defMethod" should "parse method with expr surrounded in { }" in {
    """
      |def method_name: Unit = {
      |  ???
      |}""".ast(Scala.defMethod) should be (Some(
        DefMethod("method_name", `Unit`, body = Option(`???`))
      ))
  }

  "defMethod" should "parse method with arguments and types" in {
    val src = """def method1(arg1: Int, arg2: String): Unit = ???"""
    src.ast(Scala.defMethod) should be (Some(
      DefMethod("method1", `Unit`, Seq(
        DefMethodArgument("arg1", `Int`),
        DefMethodArgument("arg2", `String`),
      ),
      body = Option(`???`))
    ))
  }

  "defMethodArgument" should "parse name and type tuple" in {
    val expected = DefMethodArgument("name", `Int`)
    """name:Int""".ast(Scala.defMethodArgument) should be (Some(expected))
  }

  "methodArguments" should "parse method with arguments and types" in {
     """(a: Int)""".ast(Scala.methodArguments) should be (Some(
       Seq(DefMethodArgument("a", `Int`))
     ))
  }

  "methodArguments" should "parse method with two arguments" in {
     """(a: Int, b: String)""".ast(Scala.methodArguments) should be (Some(
       Seq(DefMethodArgument("a", `Int`), DefMethodArgument("b", `String`))
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
    val expected = DefObject("Main", Seq(
      DefMethod("methodName", `Unit`, body = Option(`???`))
    ))
    """
      |object Main {
      |  def methodName: Unit = ???
      |}
      |""".ast(Scala.defObject) should be (Some(expected))
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

  "main" should "parse multiple object definitions" in {
    val src = """
      |object Main {
      |  def method1: Unit = ???
      |  def method2: Unit = ???
      |  def method3: Unit = ???
      |}
      |"""

    val expected = DefObject("Main", Seq(
      DefMethod("method1", `Unit`, body = Option(`???`)),
      DefMethod("method2", `Unit`, body = Option(`???`)),
      DefMethod("method3", `Unit`, body = Option(`???`)),
    ))

    src.ast(Scala.defObject) should be (Some(expected))
  }

  "main" should "parse 'hello world' example without arguments in main method" in {
    val src = """
      |object Main {
      |  def main: Unit = {
      |    println("Hello World!")
      |  }
      |}
      |"""

    val expected = DefObject("Main", Seq(
      DefMethod("main", `Unit`, body = Option(
        Func("println", Seq(Stri("Hello World!")))
      ))
    ))

    src.ast(Scala.defObject) should be (Some(expected))
  }

  "main" should "parse 'hello world' example" in {
    val src = """
      |object Main {
      |  def main(args: Array[String]): Unit = {
      |    println("Hello World!")
      |  }
      |}
      |"""

    val args = DefMethodArgument("args", GenericType("Array", Seq(SimpleType("String"))))

    val expected = DefObject("Main", Seq(
      DefMethod(
        name = "main",
        returnType = `Unit`,
        arguments = Seq(args),
        body = Option(
          Func("println", Seq(Stri("Hello World!")))
        )
      )
    ))

    src.ast(Scala.defObject) should be (Some(expected))
  }

  "expr.op" should "parse 1 + 2" in {
    "1 + 2".ast(Scala.expr.all) should be (Some {
      ExprOp("+", Num("1"), Num("2"))
    })
  }

  "expr.op" should "parse 1 * 2" in {
    "1 * 2".ast(Scala.expr.all) should be (Some {
      ExprOp("*", Num("1"), Num("2"))
    })
  }

  "expr.op" should "parse \"1\" * \"2\"" in {
    "\"1\" * \"2\"".ast(Scala.expr.all) should be (Some {
      ExprOp("*", Stri("1"), Stri("2"))
    })
  }

  "expr.op" should "parse booleans" in {
    "true".ast(Scala.expr.all) should be (Some(Expression.Bool(true)))
    "false".ast(Scala.expr.all) should be (Some(Expression.Bool(false)))
  }

  "expr.function" should "parse println(1 + 2)" in {
    val expected = Func("println", Seq(ExprOp("+", Num("1"), Num("2"))))
    "println(1 + 2)".ast(Scala.expr.function) should be (Some(expected))
  }

  "expr.ifExpr" should "parse if (true) 1 else 2" in {
    val expected = Expression.If(Expression.Bool(true), Num("1"), Num("2"))
    //"if (true) 1 else 2".ast(Scala.expr.ifExpr) should be (Some(expected))
    "if (true) 1 else 2".ast(Scala.expr.ifExpr) should be (Some(expected))
  }
}
