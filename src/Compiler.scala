package lt.vu.mif.bentkus.bachelor.compiler

import lt.vu.mif.bentkus.bachelor.compiler.classfile.Version
import lt.vu.mif.bentkus.bachelor.compiler.classfile.higher.{
  ClassAccessFlag,
  AccessFlag,
  Class,
  Code,
  ConstString,
  FieldRef,
  JavaType,
  Method,
  MethodRef,
  Op,
}
import lt.vu.mif.bentkus.bachelor.compiler.lexer.Lexer
import lt.vu.mif.bentkus.bachelor.compiler.parser.Parser
import lt.vu.mif.bentkus.bachelor.compiler.parser.scala.{Scala, Expression}
import lt.vu.mif.bentkus.bachelor.compiler.parser.scala.Expression.{
  DefObject,
  DefMethod,
  Expr
}
import lt.vu.mif.bentkus.bachelor.compiler.span.Span
import lt.vu.mif.bentkus.bachelor.compiler.misc.PrettyPrint

import java.io.File
import java.nio.file.Files

object MainApp extends App {
  def convert(obj: DefObject): Class = {
    Class(
      version = Version(0, 59),
      thisClass = JavaType.Class(obj.name),
      superClass = JavaType.Class("java/lang/Object"),
      methods = {
        val defMethods = obj.statements.filter(_.isInstanceOf[DefMethod]).map(_.asInstanceOf[DefMethod])

        val defcon = Method.DefaultConstructor

        defcon +: defMethods.map { defMethod =>
          val sig =
            Seq(convert(defMethod.returnType)) ++
            defMethod.arguments.map(arg => convert(arg.argumentType))
          Method(
            defMethod.name,
            signature = sig,
            access = Set(AccessFlag.Public, AccessFlag.Static),
            code = defMethod.body.map(convertBody))
        }
      },
      attributes = Seq.empty,
      access = Set(ClassAccessFlag.Public, ClassAccessFlag.Super))
  }

  def convert(stype: Expression.ScalaType): JavaType = {
    stype match {
      case Expression.SimpleType("Unit") =>
        JavaType.Void
      case Expression.SimpleType("String") =>
        JavaType.Class("java/lang/String")
      case Expression.GenericType("Array", Seq(generic)) =>
        JavaType.Array(convert(generic))
      case _ =>
        println(stype)
        ???
    }
  }

  def convertBody(expr: Expr): Code = {
    import Expression._

    val code = expr match {
      case Func("println", Seq(Stri(arg))) =>
        val printStream = JavaType.Class("java/io/PrintStream")

        val systemOut = FieldRef(
          JavaType.Class("java/lang/System"),
          "out",
          Seq(printStream))

        val method = MethodRef(
          printStream,
          "println",
          Seq(JavaType.Void, JavaType.Class("java/lang/String")))

        Seq(
          Op.getstatic(systemOut),
          Op.ldc(ConstString(arg)),
          Op.invoke(method, Op.invoke.virtual),
          Op.Return,
        )
      case _ =>
        ???
    }


    Code(2, 1, code)
  }

  def convert(method: DefMethod): Method = {
    Method(
      method.name,
      signature = null,
      access = Set.empty,
      code = None)
  }

  def readFile(filename: String): Seq[Expression] = {

    val content = Files.readAllBytes(new File(filename).toPath)

    val state = Parser.parseTokens(Scala.main, Lexer.lexAll(Span(content)).toList)

    val result = state
      .find { case (_, tokens) => tokens.isEmpty }
      .map { case (tree, tokens) => tree }

    result.getOrElse {
      println("Failed parsing:")
      state.zipWithIndex.foreach { case ((tree, tokens), i) =>
        println(s"$i.")
        println(s"\ttree: ${tree}")
        println(s"\ttokens: ${tokens.mkString(",")}")
      }
      throw new Exception("Failed parsing")
    }
  }

  {
    val statements = MainApp.readFile(args.head)
    val defObject = statements.head.asInstanceOf[DefObject]
    val jclass = convert(defObject)
    PrettyPrint.pformat(defObject)
    PrettyPrint.pformat(jclass)
    val m = new classfile.higher.Materializer
    val (head, body) = m.bytes(jclass)
    printBuffer(head)
    printBuffer(body)

    {
      val fname = args.head.replaceFirst("\\.scala$", ".class")
      val fc = new java.io.FileOutputStream(fname).getChannel()
      fc.write(head)
      fc.write(body)
      fc.close()

    }
  }

  def printBuffer(bb: java.nio.ByteBuffer): Unit = {
    val start = bb.position()
    val end = bb.limit()
    val arr = bb.array()

    val longest = Math.max(String.format("%x", end).size, 2)

    (start until end).foreach { i =>
      if (i % 16 == 0) {
        val prefix = String.format(s"%0${longest}x", Int.box(i))
        val nl = if (i > 0) "\n" else ""
        print(s"$nl$prefix: ")
      } else if (i % 8 == 0 && i > 0) {
        print("   ")
      } else if (i % 4 == 0 && i > 0) {
        print(" ")
      }

      val value = arr(i)
      val str = String.format("%02x ", Byte.box(value))
      print(str)
    }
    println
  }
}
