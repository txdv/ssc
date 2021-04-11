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

  def const(expr: Expr): Op = {
    import Expression._
    expr match {
      case Stri(arg) =>
        Op.ldc(ConstString(arg))
      case Num(a) =>
        Op.iconst(a.toInt)
      case _ =>
        println(expr)
        System.exit(0)
        ???
    }
  }

  def eval(expr: Expr): Expr = {
    import Expression._
    expr match {
      case ExprOp('+', Num(a), Num(b)) =>
        Num((a.toInt + b.toInt).toString)
      case ExprOp('*', Num(a), Num(b)) =>
        Num((a.toInt * b.toInt).toString)
      case ExprOp('-', Num(a), Num(b)) =>
        Num((a.toInt - b.toInt).toString)
      case ExprOp('/', Num(a), Num(b)) =>
        Num((a.toInt / b.toInt).toString)
      case ExprOp('+', Stri(a), Stri(b)) =>
        Stri(a + b)
      case _ =>
        expr
    }
  }

  def convertBody(expr: Expr): Code = {
    import Expression._

    val code = expr match {
      case Func("println", Seq(arg)) =>
        val printStream = JavaType.Class("java/io/PrintStream")

        val systemOut = FieldRef(
          JavaType.Class("java/lang/System"),
          "out",
          Seq(printStream))

        val argExpr = eval(arg)

        val methodType = argExpr match {
          case _: Stri =>
            JavaType.Class("java/lang/String")
          case _: Num =>
            JavaType.Int
          case _ =>
            ???
        }

        val method = MethodRef(
          printStream,
          "println",
          Seq(JavaType.Void, methodType))

        Seq(
          Op.getstatic(systemOut),
          const(argExpr),
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

    val content = Benchmark.gauge2("read") {
      Files.readAllBytes(new File(filename).toPath)
    }

    val lex = Benchmark.gauge2("lex") {
      Lexer.lexAll(Span(content)).toList
    }

    val state = Benchmark.gauge2("parse") {
      Parser.parseTokens(Scala.main, lex)
    }

    val result = state
      .find { case (_, tokens) => tokens.isEmpty }
      .map { case (tree, tokens) => tree }

    result.getOrElse {
      println("Failed parsing:")
      state.zipWithIndex.foreach { case ((tree, tokens), i) =>
        println(s"$i.")
        println(s"\ttree: ${tree}")
        //println(s"\ttokens: ${tokens.mkString(",")}")
        PrettyPrint.pformat(tokens)
      }
      throw new Exception("Failed parsing")
    }
  }

  {
    val statements = MainApp.readFile(args.head)
    val defObject = statements.head.asInstanceOf[DefObject]
    val jclass = Benchmark.gauge2("ast") {
      convert(defObject)
    }
    PrettyPrint.pformat(defObject)
    PrettyPrint.pformat(jclass)
    val m = new classfile.higher.Materializer
    val (head, body) = Benchmark.gauge2("class") {
      m.bytes(jclass)
    }
    printBuffer(head)
    printBuffer(body)

    {
      val fname = args.head.replaceFirst("\\.scala$", ".class")
      val fc = new java.io.FileOutputStream(fname).getChannel()
      fc.write(head)
      fc.write(body)
      fc.close()
      Benchmark.print
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

import scala.concurrent.duration._
object Benchmark {
  var measurements = Seq.empty[(String, Duration)]

  def gauge(f: => Unit): Duration = {
    val now = System.nanoTime
    f
    val measurement = (System.nanoTime - now).nanos

    measurement
  }

  def gauge(name: String, f: => Unit): Duration = {
    val m = gauge(f)
    measurements = measurements :+ (name -> m)
    m
  }

  def gauge2[T](name: String)(f: => T): T = {
    val now = System.nanoTime
    val result = f
    val measurement = (System.nanoTime - now).nanos
    measurements = measurements :+ (name -> measurement)
    result
  }

  def print(): Unit = {
    measurements.foreach { case (k -> v) =>
      println(s"$k\t${v.toMillis}")

    }

  }
}
