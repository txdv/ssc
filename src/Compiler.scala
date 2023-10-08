package ssc

import ssc.classfile.Version
import ssc.classfile.higher.{AccessFlag, Class, ClassAccessFlag, ClassFile, Code, ConstString, FieldRef, JavaType, Method, MethodRef, Op, ScalaType, StackElement, StackFrame}
import ssc.lexer.Lexer
import ssc.parser.Parser
import ssc.parser.scala.{AST, Scala}
import ssc.parser.scala.AST.{ClassDecl, Expr, GenericType, MethodDecl, MethodDeclArgument, ObjectDecl, ScalaType, SimpleType}
import ssc.span.Span
import ssc.misc.PrettyPrint
import ssc.classfile.types.runtime.Types
import ssc.jar.ScalaSignature

import java.io.File
import java.nio.file.Files

object ScalaCompiler {
  def convert(obj: ObjectDecl): Class = {
    Class(
      version = Version(0, 55),
      thisClass = JavaType.Class(obj.name),
      superClass = JavaType.Object,
      methods = {
        val defMethods = obj.statements.filter(_.isInstanceOf[MethodDecl]).map(_.asInstanceOf[MethodDecl])

        val defcon = Method.DefaultConstructor

        defcon +: defMethods.map { defMethod =>
          val sig =
            Seq(convert(defMethod.returnType)) ++
            defMethod.arguments.map(arg => convert(arg.argumentType))
          Method(
            defMethod.name,
            signature = sig,
            access = Set(AccessFlag.Public, AccessFlag.Static),
            code = defMethod
              .body.map(statement => convertBody(statement, defMethod.arguments))
              .map(_ + Code.op(Op.Return)))
        }
      },
      attributes = Seq.empty,
      access = Set(ClassAccessFlag.Public, ClassAccessFlag.Super))
  }

  def convert(stype: AST.ScalaType): JavaType = {
    stype match {
      case AST.SimpleType("Unit") =>
        JavaType.Void
      case AST.SimpleType("String") =>
        JavaType.String
      case AST.GenericType("Array", Seq(generic)) =>
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
    import AST._
    expr match {
      case ExprOp("+", Num(a), Num(b)) =>
        Num((a.toInt + b.toInt).toString)
      case ExprOp("*", Num(a), Num(b)) =>
        Num((a.toInt * b.toInt).toString)
      case ExprOp("-", Num(a), Num(b)) =>
        Num((a.toInt - b.toInt).toString)
      case ExprOp("/", Num(a), Num(b)) =>
        Num((a.toInt / b.toInt).toString)
      case ExprOp("+", Stri(a), Stri(b)) =>
        Stri(a + b)
      case ExprOp("+", Stri(a), Num(b)) =>
        Stri(a + b)
      case ExprOp("+", Stri(a), Bool(value)) =>
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
      case If(Bool(true), left, _) =>
        left
      case If(Bool(false), _, right) =>
        right
      case Func(name, args) =>
        Func(name, args.map(eval))
      case _ =>
        expr
    }
  }

  import ssc.classfile.types.runtime.ClassObj

  case class Context(map: Map[String, ClassObj]) {
    def findMethods(f: AST.Func): Seq[MethodRef] = {
      for {
        namespace <- f.namespace.headOption.toSeq
        types <- map.get(namespace).toSeq
        methods <- types.methods.filter(_.name == f.methodName)
      } yield methods
    }

    def find(f: AST.Func): Option[MethodRef] = {
      None
    }
  }

  val ns = Context(Map(
    "Math" -> Types.resolve("java.lang.Math"),
  ))


  def genops(expr: Expr, stack: Seq[StackFrame] = Seq.empty, locals: Seq[MethodDeclArgument]): Code = {
    import AST._
    expr match {
      case f@Func(name, args) =>
        val symbol = symbolsInContext.get(name).toSeq.flatten.find { case (method, _) =>
          method.name == name && method.arguments.size == args.size
        }

        symbol match {
          case Some((method, predef)) =>
            val klass = predef // we gonna generalise this later on
            val klassRef = javaRef(klass)

            val signature = method.returnArguments.map {
              case SimpleType("scala.Any") =>
                JavaType.Object
              case SimpleType("scala.Unit") =>
                JavaType.Void
              case other =>
                println(other)
                ???
            }

            val methodRef = MethodRef(klassRef, method.name, signature)

            // TODO: multiple argument support
            val argExpr = eval(args.head)

            klass match {
              case objectDecl: ObjectDecl =>
                val module = FieldRef(klassRef, "MODULE$", Seq(klassRef))

                {
                  Code.op(Op.getstatic(module)) +
                    genops(argExpr, Seq(
                      StackFrame(offset = 0, Seq(StackElement.Type(module.signature.head)))
                    ), locals) +
                    maybeConvertToObject(argExpr, locals) +
                    Code.op(Op.invoke(methodRef, Op.invoke.virtual))
                }
              case other =>
                ???
            }
          case other =>
            ns.findMethods(f).headOption match {
              case None =>
                ???
              case Some(method) =>
                val args = f.arguments

                args.map(a => genops(a, stack, locals)).foldLeft(Code.empty)(_ + _) +
                  Code.op(Op.invoke(method, Op.invoke.static))
            }
        }
      case Stri(arg) =>
        Code.op(Op.ldc(ConstString(arg)))
      case Num(a) =>
        val num = a.toInt
        if (num >= -1 && num <= 5) {
          Code.op(Op.iconst(num))
        } else if (num <= 255) {
          Code.op(Op.bipush(num.toByte))
        } else {
          Code.op(Op.iconst(a.toInt))
        }
      case Bool(value) =>
        val op = if (value) Op.iconst(1) else Op.iconst(0)
        Code.op(op)
      case ExprOp("+", left, right) if guessType(left, locals) == JavaType.Int =>
        // TODO: right might not be Int
        println("HERE")
        genops(left, stack, locals) +
          genops(right, stack, locals) +
          Code.op(Op.iadd)
      case ExprOp("==", left, right) if sameType(left, right, locals)(JavaType.Int) =>
        val l = genops(left, stack, locals)
        val r = genops(right, stack, locals)
        val pre = l + r

        val newStack =
          stack.map(_.addOffset(7)) ++
          stack.map(_.add(8, StackElement.Type(JavaType.Int)))

        pre +
          Code.ops(Seq(
            Op.if_icmpne(7),
            Op.iconst(1),
            Op.goto(4),
            Op.iconst(0),
          )).withStackMap(newStack)
      case ExprOp("==", left, right) =>
        val pre = {
          maybeBox(genops(left, stack, locals), left, locals) +
            maybeBox(genops(right, stack, locals), right, locals)
        }

        pre +
          Op.invoke(equalsMethod, Op.invoke.virtual) + {
            Code.empty +
              Op.ifeq(7) +
              Op.iconst(1) +
              Op.goto(4) +
              Op.iconst(0)
          }.withStackMap {
            stack.map(_.addOffset(7)) ++
            stack.map(_.add(8, StackElement.Type(JavaType.Int)))
        }
      case a@If(ExprOp("==", left, right), leftBranch, rightBranch) =>
        // some code reusage from above?

        val pre = genops(left, stack, locals) + genops(right, stack, locals)

        val l = genops(leftBranch, stack, locals)
        val r = genops(rightBranch, stack, locals)

        val newStack =
          stack.map(_.addOffset(3 + 3 + l.codeSize)) ++
            stack.map(_.add(3 + 3 + l.codeSize + r.codeSize, StackElement.Type(guessType(leftBranch, locals))))

          val result = pre + {
            {
              Code.empty +
                Op.if_icmpne(3 + 3 + l.codeSize) +
                l +
                Op.goto(3 + r.codeSize) +
                r
            }.withStackMap(newStack)
          }

        //println(result)

        result
      case Ident("???") =>
        val predef = JavaType.Class("scala/Predef$")
        val nothing = JavaType.Class("scala/runtime/Nothing$")
        val method = MethodRef(predef, "$qmark$qmark$qmark", Seq(nothing))

        Code.ops(Seq(
          Op.getstatic(FieldRef(predef, "MODULE$", Seq(predef))),
          Op.invoke(method, Op.invoke.virtual),
        )).copy(localsCount = 1, stackSize = 1)
      case Ident(path) =>
        path.split("\\.") match {
          case Array(field, method) =>
            locals.find(_.name == field) match {
              case Some(result) if result.argumentType.isArray && method == "length" =>
                Code.empty +
                  Op.aload(0) +
                  Op.arraylength
              case Some(some) =>
                println(s"don't know what to do with $some")
                ???
              case None =>
                println(s"no identifier in scope '$field'")
                ???
            }
          case _ =>
            println(s"trying to reference $path")
            ???

        }
      case err =>
        println(s"error: $err")
        ???
    }
  }

  private val equalsMethod = MethodRef(JavaType.Object, "equals", Seq(JavaType.Boolean, JavaType.Object))

  def guessType(expr: Expr, locals: Seq[MethodDeclArgument]): JavaType = {
    import AST._
    expr match {
      case _: Stri =>
        JavaType.String
      case _: Num =>
        JavaType.Int
      case _: Bool =>
        JavaType.Boolean
      case f: Func =>
        //val method = math.methods.find(_.name == name).get
        val method = ns.findMethods(f).head
        method.returnType
      case ExprOp("==", _, _) =>
        JavaType.Boolean
      case ExprOp(_, left, _) =>
        guessType(left, locals)
      case If(_, left, right) =>
        guessType(left, locals)
      case Ident("args.length") =>
        // TODO: quick hack
      JavaType.Int
      case Ident("???") =>
        ScalaType.Nothing
      case Ident(something) =>
        guessIdentType(something.split("\\.").toList, locals)
      case _ =>
        println(locals)
        println(s"can't guess the type of $expr")
        ???
    }
  }

  def guessIdentType(identifier: List[String], locals: Seq[MethodDeclArgument]): JavaType = {
    println("identifier")
    identifier match {
      case ident :: tail =>
        locals.find(decl => decl.name == ident) match {
          case None =>
            println(s"no such identifier in scope $ident")
            ???
          case Some(methodDecl) =>
            methodDecl.argumentType match {
              case GenericType(typeName, generics) =>
                loadedClasses.collectFirst {
                  case classDecl: ClassDecl if classDecl.name == s"scala.$typeName" =>
                    guessIdentType(tail, classDecl)
                } getOrElse {
                  ???
                }
              case _ =>
                ???
            }
        }
      case _ =>
        ???
    }
  }

  def guessIdentType(identifier: List[String], classDecl: ClassDecl): JavaType = {
    identifier match {
      case last :: Nil =>
        classDecl.statements.collectFirst {
          case methodDecl:MethodDecl if methodDecl.name == last && methodDecl.arguments.isEmpty =>
            scalaTypeToJavaType(methodDecl.returnType)
        } getOrElse {
          ???

        }
      case _ =>
        ???
    }
  }

  def scalaTypeToJavaType(scalaType: ScalaType): JavaType = {
    scalaType match {
      case SimpleType("scala.Int") =>
        JavaType.Int
      case _ =>
        ???
    }
  }

  def sameType(left: Expr, right: Expr, locals: Seq[MethodDeclArgument])(t: JavaType): Boolean = {
    val l = guessType(left, locals)
    val r = guessType(right, locals)
    l == r && r == t
  }

  def convertBody(statement: AST.Statement, locals: Seq[MethodDeclArgument]): Code = {
    statement match {
      case expr: Expr =>
        genops(expr, locals = locals)
      case AST.Multi(all) =>
        all.map(statement => convertBody(statement, locals)).foldLeft(Code.empty)(_ + _)
      case b: AST.VarDecl =>
        println(b)
        ???
      case _ =>
        ???
    }
  }

  private val predefJarPath = new File(Predef.getClass.getProtectionDomain.getCodeSource.getLocation.toURI).getPath

  private val loadedClasses = Benchmark.gauge2("unjar") {
    val files = List(
      "scala/Predef.class",
      "scala/Array.class",
      "scala/Array$.class",
    )
    ScalaSignature.load(predefJarPath, files)
  }.toArray.toSeq


  private val symbolsInContext: Map[String, Seq[(MethodDecl, AST.Decl)]] = {
    /*
    loadedClasses.foreach {
      case objectDecl: ObjectDecl =>
        println(objectDecl.name)
      case classDecl: ClassDecl =>
        println(classDecl.name)
      case other =>
        println(other)
    }
    */

    val objs = loadedClasses.collect {
      case obj: ObjectDecl =>
        obj
    }

    objs.flatMap { klass =>
      klass.methods.map { method =>
        method -> klass
      }
    }.groupBy { _._1.name }
  }

  private def javaRef(decl: AST.Decl): JavaType.Class = decl match {
    case objectDecl: ObjectDecl =>
      JavaType.Class(objectDecl.name.replace(".", "/") + "$")
    case classDecl: AST.ClassDecl =>
      JavaType.Class(classDecl.name.replace(".", "/"))
  }


  val boxesRunTime = JavaType.Class("scala/runtime/BoxesRunTime")
  val boxToInteger = MethodRef(boxesRunTime, "boxToInteger", Seq(JavaType.Integer, JavaType.Int))
  val boxToBoolean = MethodRef(boxesRunTime, "boxToBoolean", Seq(JavaType.JavaBoolean, JavaType.Boolean))

  private def maybeBox(code: Code, expr: Expr, locals: Seq[MethodDeclArgument]): Code = {
    code + maybeConvertToObject(expr, locals)
  }
  private def maybeConvertToObject(expr: Expr, locals: Seq[MethodDeclArgument]): Option[Code] = {
    guessType(expr, locals) match {
      case JavaType.Int =>
        Some(Code.op(Op.invoke(boxToInteger, Op.invoke.static)))
      case JavaType.Boolean =>
        Some(Code.op(Op.invoke(boxToBoolean, Op.invoke.static)))
      case _ =>
        None
    }
  }

  def convert(method: MethodDecl): Method = {
    Method(
      method.name,
      signature = null,
      access = Set.empty,
      code = None)
  }

  def readFile(filename: String): Seq[AST] = {
    val content = Benchmark.gauge2("read") {
      Files.readAllBytes(new File(filename).toPath)
    }

    parseBytes(content)
  }

  def parseBytes(content: Array[Byte]): Seq[AST] = {
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

  def main2(args: Array[String]): Unit = {
    val statements = readFile(args.head)
    val defObject = statements.find(_.isInstanceOf[ObjectDecl]).get.asInstanceOf[ObjectDecl]
    val jclass = Benchmark.gauge2("ast") {
      convert(defObject)
    }
    //PrettyPrint.pformat(defObject)
    //PrettyPrint.pformat(jclass)
    val m = new classfile.higher.Materializer
    val cf = Benchmark.gauge2("class") {
      m.bytes(jclass)
    }

    Benchmark.gauge2("write") {
      val fname = cf.name
      writeFile(fname, cf)
    }

    Benchmark.print
  }

  object Cache {
    import java.nio.ByteBuffer
    import java.nio.file.{Files, Path, StandardOpenOption}

    val path = Path.of("class_cache")

    def save(values: Seq[Int]): Unit = {
      val bb = java.nio.ByteBuffer.allocate(values.length * 4)
      values.foreach(bb.putInt)
      Files.write(path, bb.array, Seq(StandardOpenOption.CREATE):_*)
    }

    def load(): Seq[Int] = {
    /*
      try {
        val bytes = Files.readAllBytes(path)
        val bb = ByteBuffer.wrap(bytes)
        (1 to (bytes.size / 4)).map { _ => bb.getInt }
      } catch {
        case _: java.nio.file.NoSuchFileException =>
          Seq.empty
      }
    */
      Seq.empty
    }
  }

  def main(args: Array[String]): Unit = {
    val targetFile = args.head

    val statements = readFile(args.head)
    val defObjects = statements.filter(_.isInstanceOf[ObjectDecl]).map(_.asInstanceOf[ObjectDecl])

    val cachedHashes = Cache.load()
    val hashes = defObjects.map { defObject => defObject.hashCode }.sorted
    Cache.save(hashes)

    val needRecompile = hashes.diff(cachedHashes)

    val classes = Benchmark.gauge2("ast") {
      defObjects
        .filter { defObject => needRecompile.contains(defObject.hashCode) }
        .map(convert)
    }

    val classFiles = Benchmark.gauge2("class") {
      classes.map { klass =>
        val m = new classfile.higher.Materializer
        m.bytes(klass)
      }
    }

    val root = new File(targetFile).getParent

    Benchmark.gauge2("write") {
      classFiles.foreach { classFile =>
        val fname = classFile.name
        writeFile(s"$root/$fname.class", classFile)

      }
    }

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
