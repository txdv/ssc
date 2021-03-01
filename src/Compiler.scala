package lt.vu.mif.bentkus.bachelor.compiler

import lt.vu.mif.bentkus.bachelor.compiler.parser.Parser
import lt.vu.mif.bentkus.bachelor.compiler.lexer.{Lexer, Span}
import lt.vu.mif.bentkus.bachelor.compiler.parser.scala.{Scala, Expression}
import lt.vu.mif.bentkus.bachelor.compiler.parser.scala.Expression.DefObject
import lt.vu.mif.bentkus.bachelor.compiler.classfile.Version
import lt.vu.mif.bentkus.bachelor.compiler.classfile.higher.{Class, JavaType, AccessFlag}

import java.io.File
import java.nio.file.Files

object MainApp extends App {
  def convert(expr: DefObject): Class = {
    Class(
      version = Version(0, 59),
      thisClass = JavaType.Class(expr.name),
      superClass = JavaType.Class("java/lang/Object"),
      methods = Seq.empty,
      attributes = Seq.empty)
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

  println {
    val statements = MainApp.readFile(args.head)
    val defObject = statements.head.asInstanceOf[DefObject]
    println(convert(defObject))
  }
}
