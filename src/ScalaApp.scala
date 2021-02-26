package lt.vu.mif.bentkus.bachelor.compiler.parser.scala

import lt.vu.mif.bentkus.bachelor.compiler.parser.Parser
import lt.vu.mif.bentkus.bachelor.compiler.lexer.{Lexer, Span}

object ScalaApp extends App {
  import java.io.File
  import java.nio.file.Files

  val content = Files.readAllBytes(new File(args(0)).toPath)

  val state = Parser.parseTokens(Scala.main, Lexer.lexAll(Span(content)).toList)

  val result = state
    .find { case (_, tokens) => tokens.isEmpty }
    .map { case (tree, tokens) => tree }


  result.map { success =>
    println(success)
  } getOrElse {
    println("Failed parsing:")
    state.zipWithIndex.foreach { case ((tree, tokens), i) =>
      println(s"$i.")
      println(s"\ttree: ${tree}")
      println(s"\ttokens: ${tokens.mkString(",")}")
    }
  }
}
