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
  ClassFile,
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
import lt.vu.mif.bentkus.bachelor.compiler.classfile.types.runtime.Types


import java.io.File
import java.nio.file.Files

object ScalaCompiler {
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
        JavaType.String
      case Expression.GenericType("Array", Seq(generic)) =>
        JavaType.Array(convert(generic))
      case _ =>
        println(stype)
        ???
    }
  }

  def eval(expr: Expr): Expr = {
    evalCache(expr, Vector.empty)
  }

  def evalCache(expr: Expr, cache: Seq[Int]): Expr = {
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
      case ExprOp('+', Stri(a), Num(b)) =>
        Stri(a + b)
      case ExprOp('+', Stri(a), Bool(value)) =>
        Stri(a + value)
      /*
       * TODO:
       * When we have something like
       *        +
       *       / \
       *      +   2
       *     / \
       * f(1)   1
       * we should be able to optimize to f(1) + 3
       */
      case ExprOp(op, left, right) =>
        val hashCode = expr.hashCode
        if (cache.contains(hashCode)) {
          expr
        } else {
          val newCache = cache :+ hashCode

          val res = ExprOp(op, eval(left), eval(right))

          evalCache(res, newCache)

        }
      case Func(name, args) =>
        Func(name, args.map(eval))
      case _ =>
        expr
    }
  }

  val math = Types.resolve("java.lang.Math")

  def genops(expr: Expr): Seq[Op] = {
    import Expression._
    expr match {
      case Func(name, args) =>
        val methods = math.methods.filter(_.name == name)
        val method = methods.head

        if (args.size != method.signature.size - 1) {
          throw new Exception(s"Expected ${args.size} arguments, but got ${method.signature.size}")
        }

        args.flatMap(genops) :+
          Op.invoke(method, Op.invoke.static)
      case Stri(arg) =>
        Seq(Op.ldc(ConstString(arg)))
      case Num(a) =>
        val num = a.toInt
        if (num <= 255) {
          Seq(Op.bipush(num.toByte))
        } else {
          Seq(Op.iconst(a.toInt))
        }
      case Bool(value) =>
        Seq(if (value) Op.iconst(1) else Op.iconst(0))
      case ExprOp('+', left, right) =>
        guessType(left) match {
          case JavaType.Int =>
            genops(left) ++ genops(right) :+ Op.iadd
          case _ =>
            ???

        }
      case _ =>
        ???
    }
  }

  def a(b: Int): Int = {
    b + 42
  }

  def guessType(expr: Expr): JavaType = {
    import Expression._
    expr match {
      case _: Stri =>
        JavaType.String
      case _: Num =>
        JavaType.Int
      case _: Bool =>
        JavaType.Boolean
      case Func(name, arguments) =>
        val method = math.methods.find(_.name == name).get
        method.returnType
      case ExprOp(_, left, _) =>
        guessType(left)
      case _ =>
        println("HERE:")
        println(expr)
        ???
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

        val methodType = guessType(argExpr)

        val method = MethodRef(
          printStream,
          "println",
          Seq(JavaType.Void, methodType))

        Seq.empty ++
          Seq(Op.getstatic(systemOut)) ++
          genops(argExpr) ++
          Seq(Op.invoke(method, Op.invoke.virtual)) ++
          Seq(Op.Return)
      case _ =>
        ???
    }


    Code(stackSize = 5, 1, code)
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

    parseBytes(content)
  }

  def parseBytes(content: Array[Byte]): Seq[Expression] = {
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

  def main(args: Array[String]): Unit = {
    val statements = readFile(args.head)
    val defObject = statements.find(_.isInstanceOf[DefObject]).get.asInstanceOf[DefObject]
    val jclass = Benchmark.gauge2("ast") {
      convert(defObject)
    }
    PrettyPrint.pformat(defObject)
    PrettyPrint.pformat(jclass)
    val m = new classfile.higher.Materializer
    val cf = Benchmark.gauge2("class") {
      m.bytes(jclass)
    }

    val fname = args.head.replaceFirst("\\.scala$", ".class")
    writeFile(fname, cf)

    Benchmark.print
  }

  def debug(classFile: ClassFile): Unit = {
    printBuffer(classFile.head)
    printBuffer(classFile.body)
  }

  def writeFile(name: String, classFile: ClassFile): Unit = {
    val fc = new java.io.FileOutputStream(name).getChannel()
    fc.write(classFile.head)
    fc.write(classFile.body)
    fc.close()
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
